# Run from the repository root:
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-data-storage-convention.R
# Inspect graph declarations and use synthetic data only; do not build targets.
library(dplyr)
library(duckplyr)

local({
  manifest <- targets::tar_manifest(fields = c(name, format, command))
  path_targets <- grepl("_(parquet|csv|xlsx|zip|yml)$", manifest$name) |
    manifest$name == "s0_pipeline_inputs_raw_chr"
  untracked <- manifest$name[path_targets & manifest$format != "file"]
  if (length(untracked)) {
    stop("File paths must be file targets: ", paste(untracked, collapse = ", "))
  }

  # Source formats are preserved. Every other persisted data artifact is Parquet.
  source_targets <- manifest$name == "s0_pipeline_inputs_raw_chr" |
    manifest$name == "s0_pipeline_stagecoach_yml" |
    grepl("_raw_(csv|xlsx|zip)$", manifest$name) |
    manifest$name == "s2_gesis_snips_verified_csv"
  artifacts <- manifest$format == "file" & !source_targets
  stopifnot(
    any(artifacts),
    all(grepl("_parquet$", manifest$name[artifacts])),
    !any(manifest$name %in% c("s2_clinic_ids_csv", "s2_clinic_ids_rds")),
    manifest$format[match("s2_clinic_ids_df", manifest$name)] == "rds",
    manifest$format[match("s1_gesis_xwalk_clean_df", manifest$name)] == "rds",
    "s2_clinic_ids_parquet" %in% manifest$name
  )

  scratch <- tempfile("check-data-storage-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  tangled <- file.path(scratch, "page20.R")
  knitr::purl(
    "notebooks/pipeline/20_page-construct-clinic-ids.qmd",
    output = tangled, documentation = 0L, quiet = TRUE
  )
  assignments <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("s2_clinic_ids_parquet"))
  }, as.list(parse(tangled)))
  stopifnot(length(assignments) == 1L)
  command <- assignments[[1L]][[3L]]
  output <- file.path(scratch, "crosswalk.parquet")
  # Redirect only the destination in the real notebook command.
  command[[2L]] <- substitute(path <- destination, list(destination = output))
  fixture_env <- new.env(parent = environment())
  fixture_env$s2_clinic_ids_df <- tibble(
    clinic_ID = c("uid-A", NA_character_, "0002 - sante"),
    cAnnee = c(2010L, 2015L, 2019L), cPeriode = c(1L, NA_integer_, 12L),
    cCodeNiv = c("001", "002", NA_character_),
    cCodeStru = c("0001", "0002", "0003"),
    correct_cCodeStru = c("0001", "0002", "0002"),
    cNom = c("CSB Santé", "CSB Sœurs", "CSB Ampitafà"),
    fname = c(1L, 2L, NA_integer_),
    type_map = c("CSB1", NA_character_, "CSB2"),
    ID_checked = c(TRUE, FALSE, NA)
  )
  result <- eval(command, envir = fixture_env)
  stopifnot(identical(result, output), file.exists(result))
  actual <- read_parquet_duckdb(result, prudence = "stingy") |> collect()
  stopifnot(isTRUE(all.equal(actual, fixture_env$s2_clinic_ids_df)))
  message("Storage convention verified: tracked source files, Parquet artifacts, and lossless clinic-ID round trip.")
})
