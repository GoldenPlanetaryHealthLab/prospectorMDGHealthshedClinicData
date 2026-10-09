# Run from the repository root:
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-w3-parquet-transforms.R
# Synthetic inputs only: no pipeline targets or staged data are executed.
library(dplyr)
library(duckplyr)

local({
  scratch <- tempfile("check-w3-parquet-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  tangled <- file.path(scratch, "page18.R")
  knitr::purl(
    "notebooks/pipeline/18_page-prep-w3.qmd",
    output = tangled, documentation = 0L, quiet = TRUE
  )
  assignments <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-"))
  }, as.list(parse(tangled)))
  names(assignments) <- vapply(assignments, function(expr) as.character(expr[[2L]]), "")
  fixture_env <- new.env(parent = environment())
  for (expr in assignments) {
    if (is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
      eval(expr, envir = fixture_env)
    }
  }
  # Evaluate the real notebook commands, redirecting their explicit output paths.
  fixture_env$here <- function(path) file.path(scratch, path)
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)

  first <- data.frame(
    organisationunitid = c("A", "A", "B"),
    periodname = c("early", "early", "late"),
    periodcode = c(202201, 202201, 202301),
    all_missing = NA_real_, check.names = FALSE
  )
  first[["T13: AME 0-6 mois"]] <- c(1, 4, 2)
  first[["T13: AME premiere heure 0-6 mois"]] <- c(5, 3, 8)
  first[["T13: AME 1ère heure"]] <- c(2, 1, 3)
  first[["T13: AME premiere heure 1ère heure"]] <- c(4, 6, 9)
  first[["MIDs pour groupe à risque (migrants/réfugiés/personnes déplacées)"]] <- c(1, 2, 3)
  first[["CHU CHRR T26 MIDs pour groupe à risque (migrants/réfugiés/personnes déplacées)"]] <- c(4, 5, 6)
  second <- first[c(1L, 3L), ]
  second[["T13: AME 0-6 mois"]] <- c(7, 1)
  second$extra_measure <- c(11, 12)

  paths <- file.path(scratch, c("first.parquet", "second.parquet"))
  compute_parquet(as_duckdb_tibble(first), paths[[1L]])
  compute_parquet(as_duckdb_tibble(second), paths[[2L]])
  fixture_env$s0_clinic_RMA_W3_within_file_max_parquet <- paths
  fixture_env$s0_gesis_xwalk_nonmissing_uid_df <- tibble(
    organisationunitid = c("A", "B"), code_fs = c("001", "002"),
    TYPE = c("CSB1", "CSB2"), FS = c("Clinic A", "Clinic B")
  )

  # Base R supplies the independent reference, including all-missing groups.
  combined <- bind_rows(first, second)
  expected <- do.call(rbind, lapply(split(combined, combined$organisationunitid), function(x) {
    values <- lapply(x[setdiff(names(x), c("organisationunitid", "periodname"))], function(v) {
      if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    })
    data.frame(organisationunitid = x$organisationunitid[[1L]],
               periodname = x$periodname[[1L]], values, check.names = FALSE)
  }))
  setwd(scratch)
  eval(assignments$s0_clinic_RMA_W3_group_max_parquet, envir = fixture_env)
  eval(assignments$s0_clinic_RMA_W3_clean_parquet, envir = fixture_env)
  group_path <- fixture_env$s0_clinic_RMA_W3_group_max_parquet
  clean_path <- fixture_env$s0_clinic_RMA_W3_clean_parquet
  stopifnot(file.exists(group_path), file.exists(clean_path))
  actual <- read_parquet_duckdb(group_path, prudence = "stingy") |>
    arrange(organisationunitid) |> collect() |> as.data.frame()
  stopifnot(isTRUE(all.equal(actual, expected, check.attributes = FALSE)))

  replacements <- c(
    "T13: AME 0-6 mois" = "T13: AME premiere heure 0-6 mois",
    "T13: AME 1ère heure" = "T13: AME premiere heure 1ère heure",
    "MIDs pour groupe à risque (migrants/réfugiés/personnes déplacées)" =
      "CHU CHRR T26 MIDs pour groupe à risque (migrants/réfugiés/personnes déplacées)"
  )
  for (name in names(replacements)) {
    later <- expected$periodcode > 202300
    expected[[name]][later] <- expected[[replacements[[name]]]][later]
  }
  expected <- expected[setdiff(names(expected), unname(replacements))]
  expected <- left_join(expected, fixture_env$s0_gesis_xwalk_nonmissing_uid_df,
                        by = "organisationunitid")
  clean <- read_parquet_duckdb(clean_path, prudence = "stingy") |> collect() |> as.data.frame()
  stopifnot(identical(names(clean)[[1L]], "V1"), setequal(as.numeric(clean$V1), 0:1))
  clean <- clean |> arrange(organisationunitid) |> select(-V1)
  stopifnot(isTRUE(all.equal(clean, expected, check.attributes = FALSE)))
  message("W3 Parquet targets match the independent reference for unioned schemas, maxima, missing values, and crosswalk corrections.")
})
