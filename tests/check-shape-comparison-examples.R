# Run through scripts/mdg-healthshed-clinic-data exec Rscript.
# Execute the Manual's shape summaries on synthetic inputs only.
# No tar_read(), tar_make(), archive extraction, or scientific data reads.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(duckplyr)
  library(sf)
})

local({
  scratch <- tempfile("shape-examples-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  pages <- list.files("notebooks/pipeline", pattern = "^(18|19|20|21|22|23|24|25)_.*[.]qmd$", full.names = TRUE)
  chunks <- list()
  for (page in pages) {
    lines <- readLines(page, warn = FALSE)
    opening <- NULL
    for (i in seq_along(lines)) {
      if (is.null(opening)) {
        if (grepl("^```(r|\\{r\\})$", lines[[i]])) opening <- i
      } else if (identical(lines[[i]], "```")) {
        code <- lines[seq.int(opening + 1L, i - 1L)]
        parsed <- parse(text = code) # Also validate the unexecuted setup examples.
        marker <- grep("^# Shape check: ", code, value = TRUE)
        if (length(marker)) {
          stopifnot(length(marker) == 1L)
          name <- sub("^# Shape check: ", "", marker)
          stopifnot(is.null(chunks[[name]]))
          chunks[[name]] <- parsed
        }
        opening <- NULL
      }
    }
  }
  env <- new.env(parent = environment())
  run <- function(name) {
    stopifnot(!is.null(chunks[[name]]))
    message("Checking ", name)
    results <- list()
    invisible(capture.output({
      for (expr in chunks[[name]]) results[[length(results) + 1L]] <- eval(expr, env)
    }))
    results
  }

  env$old_w3 <- read_sql_duckdb(
    "SELECT * FROM (VALUES ('202001'), (NULL::VARCHAR)) t(periodcode)", prudence = "stingy")
  env$new_w3 <- read_sql_duckdb(
    "SELECT * FROM (VALUES (202001), (202001), (202002)) t(periodcode)", prudence = "stingy")
  w3_periods <- run("w3-periods")[[1L]]
  stopifnot(identical(w3_periods$difference, c(1L, 1L, -1L)))
  env$old_dictionary <- data.frame(filename = c("file1", "file2"), waves_available = "1, 2")
  env$new_dictionary <- data.frame(filename = c("file1", "file1"), waves_available = "1, 2")
  groups <- run("dictionary-groups")[[1L]]
  stopifnot(identical(groups$difference, c(1L, -1L)))

  # Actual stingy file readers: reversed variable order, an unexpected variable,
  # repeated clinic-months, and a missing period must remain visible.
  first <- tibble(clinic_ID = c("a", "a", "b"), cAnnee = 2020L,
    cPeriode = c(1L, 1L, NA_integer_), fname = 6L, V2 = c(0, NA, -1), V1 = 1:3)
  second <- tibble(clinic_ID = "c", cAnnee = 2021L, cPeriode = 2L, fname = 6L, V4 = 4)
  env$retained_paths <- file.path(scratch, c("file1.parquet", "file2.parquet"))
  compute_parquet(as_duckdb_tibble(first), env$retained_paths[[1L]])
  compute_parquet(as_duckdb_tibble(second), env$retained_paths[[2L]])
  env$current_dictionary <- tibble(var = c("V1", "V2", "V3"), filename = c("file1", "file1", "file2"))
  env$diagnosis_keys <- c("clinic_ID", "cAnnee", "cPeriode", "fname")
  run("diagnosis-partitions")
  stopifnot(identical(env$diagnosis_shapes$rows, c(3L, 1L)),
    identical(env$diagnosis_shapes$variable_names_match, c(TRUE, FALSE)),
    identical(env$diagnosis_shapes$variable_order_matches, c(FALSE, FALSE)))
  run("diagnosis-select")
  run("diagnosis-schema")
  multiplicity <- run("diagnosis-multiplicity")[[1L]]
  periods <- run("diagnosis-periods")[[1L]]
  stopifnot(identical(multiplicity$reports, c(1L, 2L)),
    identical(multiplicity$clinic_month_keys, c(1L, 1L)),
    sum(periods$rows) == 3L, anyNA(periods$cPeriode))

  # Empty geometries and an added attribute produce visible differences.
  polygon <- st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  geometry <- st_sfc(polygon, st_geometrycollection(), crs = 4326)
  env$old_healthsheds <- st_sf(fs_uid = c("a", "b"), geometry = geometry)
  env$new_healthsheds <- st_sf(fs_uid = c("a", "b", NA_character_), origin = "fixture",
    geometry = c(geometry, st_sfc(st_geometrycollection(), crs = 4326)))
  env$new_sectorization <- st_sf(fs_uid = c("a", "a"),
    fkt_instat_clean = c("001", NA_character_), match_type = c("manual", NA_character_),
    geometry = geometry)
  healthshed_summary <- run("healthsheds-dimensions")[[2L]]
  stopifnot(healthshed_summary$difference[healthshed_summary$metric == "rows"] == 1L,
    healthshed_summary$difference[healthshed_summary$metric == "empty_geometries"] == 1L)
  run("healthsheds-schema")
  run("healthsheds-types")
  run("sectorization-dimensions")

  env$old_rows <- data.frame(fs_uid = c("a", "b"), which_use = c("gps", NA_character_))
  env$new_rows <- data.frame(fs_uid = c("a", "a", NA_character_),
    which_use = c("gps", "who", NA_character_), origin = "fixture")
  locations <- run("locations-dimensions")[[2L]]
  sources <- run("locations-sources")[[1L]]
  run("locations-schema")
  stopifnot(locations$difference[locations$metric == "extra_uid_rows"] == 1L,
    sources$`most recent archived data`[sources$which_use %in% "who"] == 0L,
    sources$difference[sources$which_use %in% "who"] == 1L, anyNA(sources$which_use))

  env$old_time <- data.frame(clinic_ID = c("a", "b"), year = 2020L, month = 1L, fname = c(6L, 0L))
  env$new_time <- tibble(clinic_ID = c("a", "a", "b"), year = 2020L,
    month = c(1L, 1L, 2L), fname = c(6L, 6L, NA_integer_), origin = "fixture")
  env$old_spatial <- data.frame(clinic_ID = c("a", "b"))
  env$new_spatial <- tibble(clinic_ID = c("a", "c"), origin = "fixture")
  run("metadata-dimensions")
  run("metadata-schema")
  calendar <- run("metadata-periods")[[1L]]
  repeated <- filter(env$metadata_shapes, product == "reporting", metric == "extra_key_rows")
  stopifnot(repeated$difference == 1L, anyNA(calendar$reported),
    calendar$difference[calendar$month == 1L & calendar$reported %in% TRUE] == 1L,
    calendar$`current pipeline`[calendar$month == 1L & calendar$reported %in% FALSE] == 0L)
  message("Shape examples passed: stingy summaries, schema/order mismatches, missing groups/periods, repeated keys, and empty geometry.")
})
