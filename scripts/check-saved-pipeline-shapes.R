# Audit saved products for pages 22--25; never calls tar_make() or reads Feather.
# Run from the repository root:
# scripts/mdg-healthshed-clinic-data exec Rscript scripts/check-saved-pipeline-shapes.R
# Aggregate evidence is written under ignored data/intermediates/validation/.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(duckplyr)
  library(sf)
})

local({
  started <- Sys.time()
  report_dir <- file.path("data/intermediates/validation", format(started, "saved-shapes-%Y%m%dT%H%M%SZ", tz = "UTC"))
  dir.create(report_dir, recursive = TRUE)
  statuses <- list()
  record <- function(name, x) {
    stopifnot(!inherits(x, "sf"), !any(vapply(x, inherits, logical(1), what = "sfc")))
    x <- tibble::as_tibble(x)
    duckplyr::compute_parquet(duckplyr::as_duckdb_tibble(x), file.path(report_dir, paste0(name, ".parquet")))
    invisible(x)
  }
  check <- function(name, ok, detail) {
    status <- if (isTRUE(ok)) "PASS" else "REVIEW"
    statuses[[length(statuses) + 1L]] <<- tibble(check = name, status = status, detail = detail)
    cat(status, name, detail, sep = " | "); cat("\n")
  }
  # Execute the actual notebook summaries, capturing their printed output.
  # No row-level EDA, scientific procedures, or target builds are evaluated.
  chunks <- list()
  setups <- list()
  for (page in list.files("notebooks/pipeline", "^(22|23|24|25)_.*[.]qmd$", full.names = TRUE)) {
    lines <- readLines(page, warn = FALSE)
    opening <- NULL
    for (i in seq_along(lines)) {
      if (is.null(opening)) {
        if (identical(lines[[i]], "```r")) opening <- i
      } else if (identical(lines[[i]], "```")) {
        code <- lines[seq.int(opening + 1L, i - 1L)]
        marker <- grep("^# Shape check: ", code, value = TRUE)
        if (length(marker)) chunks[[sub("^# Shape check: ", "", marker)]] <- parse(text = code)
        if (any(grepl("^old_rows <- plain\\(old\\)", code))) setups$locations <- parse(text = code)
        if (any(grepl("^old_time <- read_old\\(", code))) setups$metadata <- parse(text = code)
        opening <- NULL
      }
    }
  }
  env <- new.env(parent = environment())
  evaluate <- function(expressions) {
    values <- list()
    invisible(capture.output({
      for (expr in expressions) values[[length(values) + 1L]] <- eval(expr, env)
    }))
    values
  }
  run <- function(name) {
    stopifnot(!is.null(chunks[[name]]))
    message("Running notebook check: ", name)
    values <- evaluate(chunks[[name]])
    if (!grepl("-(setup|select)$", name)) {
      for (i in seq_along(values)) if (is.data.frame(values[[i]])) record(paste0(name, "-", i), values[[i]])
    }
    invisible(values)
  }
  literal <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
  query <- function(sql) duckplyr::read_sql_duckdb(sql, prudence = "stingy") |> collect()
  source_sql <- function(path) paste0("read_parquet(", literal(path), ")")
  key_projection <- "clinic_ID,cAnnee,cPeriode,fname"
  compare_keys <- function(a, b) query(paste0(
    "SELECT (SELECT count(*) FROM ((", a, ") EXCEPT ALL (", b,
    "))) AS left_only, (SELECT count(*) FROM ((", b, ") EXCEPT ALL (", a, "))) AS right_only"))

  run("diagnosis-setup")
  run("diagnosis-partitions")
  shapes <- env$diagnosis_shapes
  expected_groups <- unique(na.omit(env$current_dictionary$filename))
  check("diagnosis partitions", setequal(shapes$partition, expected_groups) && !anyDuplicated(shapes$partition),
        paste(nrow(shapes), "retained partitions;", nrow(env$current_dictionary), "dictionary variables"))
  check("diagnosis schemas", all(shapes$key_columns_present & shapes$variable_names_match & shapes$variable_order_matches),
        paste(sum(shapes$variable_order_matches), "ordered schemas match their dictionary groups"))
  check("diagnosis row counts", length(unique(shapes$rows)) == 1L && all(shapes$rows > 0),
        paste("range", min(shapes$rows), "to", max(shapes$rows)))
  run("diagnosis-select")
  run("diagnosis-schema")
  run("diagnosis-multiplicity")
  run("diagnosis-periods")

  # Compare full key multisets in DuckDB; collect only scalar discrepancy counts.
  reference <- paste0("SELECT ", key_projection, " FROM ", source_sql(env$selected_path))
  key_checks <- bind_rows(lapply(env$retained_paths, function(path) {
    keys <- paste0("SELECT ", key_projection, " FROM ", source_sql(path))
    differences <- compare_keys(keys, reference)
    counts <- query(paste0("SELECT count(*) AS keys, coalesce(sum(n-1),0)::INTEGER AS extra_rows, ",
      "count(*) FILTER (WHERE n>1) AS repeated_keys FROM (SELECT clinic_ID,cAnnee,cPeriode,count(*) n FROM ",
      source_sql(path), " GROUP BY clinic_ID,cAnnee,cPeriode)"))
    missing <- query(paste0("SELECT count(*) AS missing_key_rows FROM ", source_sql(path),
      " WHERE clinic_ID IS NULL OR trim(clinic_ID)='' OR cAnnee IS NULL OR cPeriode IS NULL OR fname IS NULL"))
    tibble(partition = tools::file_path_sans_ext(basename(path)),
      left_only = differences$left_only, right_only = differences$right_only,
      keys = counts$keys, repeated_keys = counts$repeated_keys, extra_rows = counts$extra_rows,
      missing_key_rows = missing$missing_key_rows)
  }))
  record("diagnosis-key-agreement", key_checks)
  check("diagnosis key agreement", all(key_checks$left_only == 0 & key_checks$right_only == 0),
        "all retained partitions compared with file1, including multiplicity and fname")
  check("diagnosis key validity", all(key_checks$missing_key_rows == 0 & key_checks$repeated_keys == 0),
        paste(sum(key_checks$missing_key_rows), "missing key rows;", sum(key_checks$repeated_keys), "repeated keys across partitions"))

  run("healthsheds-setup")
  run("healthsheds-dimensions")
  run("healthsheds-schema")
  run("healthsheds-types")
  run("sectorization-dimensions")
  hs <- env$healthshed_shape(env$new_healthsheds)
  check("healthshed UID keys", hs$missing_uids == 0 && hs$clinics == hs$rows,
        paste(hs$rows, "rows;", hs$clinics, "unique UIDs"))
  check("healthshed validity", hs$invalid_geometries == 0 && hs$unknown_validity == 0,
        paste(hs$invalid_geometries, "invalid;", hs$unknown_validity, "unknown"))
  check("healthshed geometry presence", hs$empty_geometries == 0,
        paste(hs$empty_geometries, "empty geometries"))

  stopifnot(!is.null(setups$locations), !is.null(setups$metadata))
  evaluate(setups$locations)
  run("locations-dimensions")
  run("locations-schema")
  source_counts <- run("locations-sources")[[1L]]
  check("location shape vs archive", nrow(env$old_rows) == nrow(env$new_rows) && setequal(names(env$old_rows), names(env$new_rows)),
        paste(nrow(env$old_rows), "archived rows;", nrow(env$new_rows), "current rows"))
  check("location source counts vs archive", all(source_counts$difference == 0),
        paste(sum(source_counts$difference != 0), "source categories differ"))

  evaluate(setups$metadata)
  run("metadata-dimensions")
  run("metadata-schema")
  run("metadata-periods")
  metadata <- env$metadata_shape(env$new_time, c("clinic_ID", "year", "month"))
  spatial <- env$metadata_shape(env$new_spatial, "clinic_ID")
  check("reporting metadata keys", metadata$missing_key_rows == 0 && metadata$extra_key_rows == 0,
        paste(metadata$rows, "rows;", metadata$unique_keys, "unique clinic-months;", metadata$extra_key_rows, "extra rows"))
  check("spatial metadata keys", spatial$missing_key_rows == 0 && spatial$extra_key_rows == 0,
        paste(spatial$rows, "rows;", spatial$unique_keys, "unique clinics"))
  reporting_path <- targets::tar_read(s7_clinic_reporting_metadata_parquet)
  reporting_keys <- paste0("SELECT DISTINCT clinic_ID,year AS cAnnee,month AS cPeriode,fname FROM ",
                           source_sql(reporting_path), " WHERE fname<>0")
  reporting_agreement <- compare_keys(reference, reporting_keys)
  record("reporting-diagnosis-key-agreement", reporting_agreement)
  check("reported keys vs diagnoses", all(reporting_agreement == 0),
        paste(reporting_agreement$left_only, "diagnosis-only;", reporting_agreement$right_only, "reporting-only"))
  diagnosis_clinics <- query(paste0("SELECT DISTINCT clinic_ID FROM ", source_sql(env$selected_path)))$clinic_ID
  check("spatial population vs diagnoses", setequal(diagnosis_clinics, env$new_spatial$clinic_ID),
        paste(length(diagnosis_clinics), "diagnosis clinics;", spatial$clinics, "spatial metadata clinics"))

  record("checks", bind_rows(statuses))
  meta <- targets::tar_meta(fields = c(name, data, time, error))
  record("target-snapshot", meta)
  dput(list(started_utc = format(started, tz = "UTC", usetz = TRUE),
    finished_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    R = as.character(getRversion()), packages = vapply(c("targets", "duckplyr", "duckdb", "sf"),
      function(p) as.character(packageVersion(p)), ""),
    baseline = dget("src/archived-comparison-baseline.R"),
    baseline_hash_reverified = FALSE,
    code_md5 = tools::md5sum(c("scripts/check-saved-pipeline-shapes.R",
      list.files("notebooks/pipeline", "^(22|23|24|25)_.*[.]qmd$", full.names = TRUE)))),
    file.path(report_dir, "provenance.R"))
  cat("Evidence:", normalizePath(report_dir), "\n")
  cat("Scope: saved products, current shape/key checks, RDS/CSV archive comparisons; no Feather or measurement-value comparison.\n")
})
