# Synthetic source-boundary and graph checks; no scientific targets are built.
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-archive-source-boundary.R
local({
  scratch <- tempfile("archive-source-boundary-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  tangled <- file.path(scratch, "stage-inputs.R")
  knitr::purl("notebooks/pipeline/16_page-stage-inputs.qmd", output = tangled, quiet = TRUE)
  assignments <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")),
                        as.list(parse(tangled)))
  names(assignments) <- vapply(assignments, function(x) as.character(x[[2L]]), "")
  env <- new.env(parent = environment())
  for (name in c("s0_pipeline_archive_members_fn", "s0_pipeline_archive_inputs_fn")) {
    eval(assignments[[name]], env)
  }
  raw <- "madagascar_clinic_data_cleaning/data/raw/"
  expected <- paste0(raw, c(
    "from Symphonia/RMA_W3_FULL/RMA 2021/source.csv",
    "from Symphonia/RMA_W3_FULL/RMA 2022/source.csv",
    "from Symphonia/mapping GESIS DHIS questions - mapping DHIS2 GESIS 2010 2022.csv",
    "symphonia_verified/CSB1 CSB2 PUBLIC PRIVATE.xlsx",
    "symphonia_verified/gesis_missing_uid_in_mapping.xlsx",
    "symphonia_verified/VERIFIER fs_uid_year_with_multiple_gesis.csv",
    "symphonia_verified/VERIFIER clinic_codes_in_gesis_but_not_in_mapping.csv",
    "symphonia_verified/GESIS_snips_public_multipleLongGESIS VERIFIED.csv"
  ))
  excluded <- c("__MACOSX/madagascar_clinic_data_cleaning/data/raw/from Symphonia/RMA_W3_FULL/._source.csv",
                paste0(raw, "from Symphonia/RMA_W3_FULL/.DS_Store"),
                "madagascar_clinic_data_cleaning/data/clean/not_a_source.csv")
  fixture <- file.path(scratch, "fixture")
  for (name in c(expected, excluded)) {
    path <- file.path(fixture, name)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("unchanged source bytes", path)
  }
  original <- getwd()
  on.exit(setwd(original), add = TRUE)
  setwd(fixture)
  archive <- file.path(scratch, "freeze.zip")
  zip::zip(archive, c(expected, excluded))
  setwd(original)
  stopifnot(setequal(env$s0_pipeline_archive_members_fn(archive), expected))
  output <- file.path(scratch, "extracted")
  dir.create(output)
  stale <- file.path(output, "stale-drive-source.csv")
  writeLines("not selected", stale)
  paths <- env$s0_pipeline_archive_inputs_fn(archive, output)
  stopifnot(length(paths) == length(expected) + 1L, !stale %in% paths,
            identical(paths[[1L]], archive),
            all(vapply(paths[-1L], function(p) identical(readLines(p), "unchanged source bytes"), TRUE)))
  setwd(fixture)
  incomplete <- file.path(scratch, "incomplete.zip")
  zip::zip(incomplete, expected[-3L])
  setwd(original)
  stopifnot(inherits(try(env$s0_pipeline_archive_members_fn(incomplete), silent = TRUE), "try-error"))

  graph <- targets::tar_manifest(fields = c(name, command, format))
  stopifnot(graph$format[match("s0_pipeline_raw_zip", graph$name)] == "file",
            grepl("s0_pipeline_raw_zip", graph$command[match("s0_pipeline_inputs_raw_chr", graph$name)]),
            !any(grepl("RMA_clinic_data_gdrive|golden_googledrive_rclone", graph$command)))
  config <- dget("src/archived-comparison-baseline.R")
  stopifnot(grepl(config$archive_zip,
                 graph$command[match("s0_pipeline_raw_zip", graph$name)], fixed = TRUE),
            !any(grepl("gdrive|rclone", unlist(config), ignore.case = TRUE)))
  message("ZIP-only source boundary verified: explicit members, original bytes, no stale/Drive discovery, ",
          "required-input failures, tracked archive, and one comparison source.")
})
