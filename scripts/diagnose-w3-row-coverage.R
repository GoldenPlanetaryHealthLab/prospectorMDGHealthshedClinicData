# Run from the repository root through scripts/mdg-healthshed-clinic-data exec Rscript.
# Read-only stage-0 diagnosis: collect keys/counts, never measurement tables.
# Uses existing targets and source files; builds no targets and writes no products.
suppressPackageStartupMessages({
  library(dplyr)
  library(duckplyr)
  library(targets)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
})

local({
  baseline <- dget("src/archived-comparison-baseline.R")
  archive <- tryCatch(tar_read(s0_pipeline_raw_zip), error = function(e) {
    stop("The target store predates the ZIP-only source boundary; rebuild through the compute-node launcher.")
  })
  stopifnot(identical(normalizePath(archive), normalizePath(baseline$archive_zip)))
  scratch <- tempfile("w3-key-coverage-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  reference_path <- unzip(archive, files = baseline$wave3_archive_member, exdir = scratch)
  keys <- c("organisationunitid", "periodname")
  only <- function(x, y) anti_join(x, y, by = keys)
  same_keys <- function(x, y) nrow(only(x, y)) == 0L && nrow(only(y, x)) == 0L
  read_keys <- function(path) {
    # Legacy readr defaults, projecting just the two character grouping keys.
    x <- read_csv(path, col_types = cols_only(
      organisationunitid = col_character(), periodname = col_character()
    ), show_col_types = FALSE, progress = FALSE)
    stopifnot(nrow(problems(x)) == 0L)
    distinct(x)
  }
  hash <- function(path) {
    result <- system2("sha256sum", shQuote(path), stdout = TRUE)
    stopifnot(is.null(attr(result, "status")), length(result) == 1L)
    substr(result, 1L, 64L)
  }
  stopifnot(identical(hash(reference_path), baseline$wave3_sha256))
  archived_counts <- read_csv_duckdb(
    reference_path, prudence = "stingy", options = list(all_varchar = TRUE)
  ) |> count(organisationunitid, periodname, name = "archived_rows") |> collect()
  current_counts <- read_parquet_duckdb(
    tar_read(s0_clinic_RMA_W3_clean_parquet), prudence = "stingy"
  ) |> count(organisationunitid, periodname, name = "current_rows") |> collect()
  archived_keys <- select(archived_counts, all_of(keys))
  current_keys <- select(current_counts, all_of(keys))
  archived_only <- only(archived_keys, current_keys)
  current_only <- only(current_keys, archived_keys)
  print(tibble(
    source = c("most recent archived data", "current pipeline"),
    rows = c(sum(archived_counts$archived_rows), sum(current_counts$current_rows)),
    keys = c(nrow(archived_keys), nrow(current_keys)),
    exclusive_keys = c(nrow(archived_only), nrow(current_only))
  ))
  print(bind_rows(archived_only = archived_only, current_only = current_only,
                  .id = "side") |>
          count(side, year = sub(".* ", "", periodname), name = "keys"))
  shared <- inner_join(archived_counts, current_counts, by = keys)
  stopifnot(all(shared$archived_rows == shared$current_rows))

  current_paths <- tar_read(s0_clinic_RMA_W3_raw_paths_chr)
  if (!all(grepl("/archive_sources/madagascar_clinic_data_cleaning/data/raw/", current_paths, fixed = TRUE))) {
    stop("Stored W3 source paths are from the former input selection; rebuild before ZIP-only comparison.")
  }
  current_per_file <- lapply(current_paths, function(path) {
    mutate(read_keys(path), source_file = sub(".*RMA_W3_FULL/", "", path))
  })
  current_raw <- bind_rows(current_per_file) |> distinct(across(all_of(keys)))
  message("Read all ", length(current_paths), " current raw files with legacy readr key semantics.")

  listing <- unzip(baseline$archive_zip, list = TRUE)
  raw_prefix <- "madagascar_clinic_data_cleaning/data/raw/from Symphonia/RMA_W3_FULL/"
  members <- listing$Name[startsWith(listing$Name, raw_prefix) &
                            endsWith(listing$Name, ".csv")]
  archived_per_file <- lapply(members, function(member) {
    path <- unzip(baseline$archive_zip, files = member, exdir = scratch)
    out <- mutate(read_keys(path), source_file = substring(member, nchar(raw_prefix) + 1L))
    unlink(path)
    out
  })
  archived_raw <- bind_rows(archived_per_file) |> distinct(across(all_of(keys)))
  message("Read all ", length(members), " archived raw files with legacy readr key semantics.")
  group_keys <- read_parquet_duckdb(
    tar_read(s0_clinic_RMA_W3_group_max_parquet), prudence = "stingy"
  ) |> select(all_of(keys)) |> collect()
  coverage <- tibble(
    check = c("archived raw keys absent from archived W3", "archived W3 keys absent from archived raw",
              "current raw keys absent from group-max output", "group-max keys absent from current raw",
              "current raw keys absent from cleaned W3", "cleaned W3 keys absent from current raw"),
    keys = c(nrow(only(archived_raw, archived_keys)), nrow(only(archived_keys, archived_raw)),
             nrow(only(current_raw, group_keys)), nrow(only(group_keys, current_raw)),
             nrow(only(current_raw, current_keys)), nrow(only(current_keys, current_raw)))
  )
  print(coverage, width = Inf)
  stopifnot(all(coverage$keys == 0L),
            same_keys(only(archived_raw, current_raw), archived_only),
            same_keys(only(current_raw, archived_raw), current_only))
  print(tibble(
    check = c("archived-only output keys present in current raw",
              "archived-only output keys present in archived raw",
              "current-only output keys present in current raw",
              "current-only output keys present in archived raw"),
    keys = c(nrow(semi_join(archived_only, current_raw, by = keys)),
             nrow(semi_join(archived_only, archived_raw, by = keys)),
             nrow(semi_join(current_only, current_raw, by = keys)),
             nrow(semi_join(current_only, archived_raw, by = keys)))
  ), width = Inf)

  # Exercise the actual stored DuckDB reader on the selected 2021 and 2022 sources.
  selected <- current_paths[grepl("/RMA 202[12]/", current_paths)]
  stopifnot(length(selected) > 0L)
  reader <- tar_read(s0_clinic_RMA_read_nonlogical_fn)
  duck_keys <- bind_rows(lapply(selected, function(path) {
    suppressMessages(reader(path)) |> select(all_of(keys)) |> distinct() |> collect()
  })) |> distinct()
  readr_keys <- bind_rows(current_per_file[grepl("/RMA 202[12]/", current_paths)]) |>
    distinct(across(all_of(keys)))
  print(tibble(reader_difference = c("readr only", "DuckDB only"),
               keys = c(nrow(only(readr_keys, duck_keys)), nrow(only(duck_keys, readr_keys)))))
  stopifnot(same_keys(readr_keys, duck_keys))

  xwalk_member <- "madagascar_clinic_data_cleaning/data/raw/from Symphonia/mapping GESIS DHIS questions - mapping DHIS2 GESIS 2010 2022.csv"
  archived_xwalk_path <- unzip(baseline$archive_zip, files = xwalk_member, exdir = scratch)
  current_xwalk_path <- tar_read(s0_gesis_xwalk_raw_csv)
  stopifnot(identical(hash(archived_xwalk_path), hash(current_xwalk_path)))
  xwalk_raw <- read_csv(current_xwalk_path, show_col_types = FALSE, progress = FALSE)
  parse_issues <- problems(xwalk_raw)
  if (nrow(parse_issues)) {
    message("Crosswalk readr parsing issues (the legacy script uses the same reader):")
    print(parse_issues |> mutate(column_name = names(xwalk_raw)[col]) |>
            count(column_name, expected, name = "parse_issues"))
  }
  xwalk <- xwalk_raw |>
    select(UID...12, code_fs, TYPE, FS) |> rename(organisationunitid = UID...12) |>
    distinct() |> filter(!is.na(organisationunitid))
  stored_xwalk <- tar_read(s0_gesis_xwalk_nonmissing_uid_df)
  stopifnot(nrow(anti_join(xwalk, stored_xwalk, by = names(xwalk))) == 0L,
            nrow(anti_join(stored_xwalk, xwalk, by = names(xwalk))) == 0L)
  matches <- count(xwalk, organisationunitid, name = "matches")
  expanded_rows <- function(x) {
    sum((left_join(x, matches, by = "organisationunitid") |>
           mutate(matches = coalesce(matches, 1L)))$matches)
  }
  expanded <- tibble(side = c("archived_only", "current_only"),
                     keys = c(nrow(archived_only), nrow(current_only)),
                     joined_rows = c(expanded_rows(archived_only), expanded_rows(current_only)))
  print(expanded)
  expected_delta <- expanded$joined_rows[expanded$side == "archived_only"] -
    expanded$joined_rows[expanded$side == "current_only"]
  stopifnot(expected_delta == sum(archived_counts$archived_rows) - sum(current_counts$current_rows))
  message("All observed row-count differences are explained by raw input key coverage and the shared crosswalk. ",
          "No key loss is observed between current raw inputs, group maxima, and cleaned W3. ",
          "Measurement-value equivalence has not been tested.")
})
