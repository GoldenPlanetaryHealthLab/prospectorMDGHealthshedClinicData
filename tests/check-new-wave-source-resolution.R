# Synthetic manifest checks only; no source data is read or pipeline executed.
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-new-wave-source-resolution.R
local({
  scratch <- tempfile("new-wave-manifest-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  page <- here::here("notebooks", "pipeline", "26_page-prep-2025-2026.qmd")
  helpers <- knitr::purl(page, output = file.path(scratch, "helpers.R"),
                          documentation = 0L, quiet = TRUE)
  for (expr in parse(helpers)) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) eval(expr)
  }
  fixture <- list(
    project = list(project_working_dir = "/host/project", input_data_dir = "/host/project/staged/inputs"),
    sources = list(`01_gold_mine` = list(enabled = TRUE, items = list(
      list(name = "harvard_2025_2026", path_regex = "/upstream/main-delivery", required = TRUE),
      list(name = "july_2025_candidate", path_regex = "/upstream/july-delivery", required = FALSE)
    )))
  )
  manifest <- file.path(scratch, "stagecoach_manifest.yml")
  write_fixture <- function(x) yaml::write_yaml(x, manifest)
  write_fixture(fixture)
  # Starting in the notebook directory must not change staging resolution.
  original <- getwd()
  on.exit(setwd(original), add = TRUE)
  setwd(here::here("notebooks", "pipeline"))
  roots <- s8_new_wave_roots_fn(manifest)
  stopifnot(identical(roots$root, c(
    here::here("staged", "inputs", "01_gold_mine", "harvard_2025_2026", "main-delivery"),
    here::here("staged", "inputs", "01_gold_mine", "july_2025_candidate", "july-delivery")
  )), !any(grepl("/host/|/upstream/", roots$root)))
  for (kind in c("missing", "duplicate", "disabled", "outside_project")) {
    invalid <- fixture
    if (kind == "missing") invalid$sources[["01_gold_mine"]]$items <- invalid$sources[["01_gold_mine"]]$items[1L]
    if (kind == "duplicate") invalid$sources[["01_gold_mine"]]$items <- c(invalid$sources[["01_gold_mine"]]$items, invalid$sources[["01_gold_mine"]]$items[1L])
    if (kind == "disabled") invalid$sources[["01_gold_mine"]]$enabled <- FALSE
    if (kind == "outside_project") invalid$project$input_data_dir <- "/different/project/inputs"
    write_fixture(invalid)
    stopifnot(inherits(try(s8_new_wave_roots_fn(manifest), silent = TRUE), "try-error"))
  }
  setwd(original)
  actual <- s8_new_wave_roots_fn()
  stopifnot(identical(actual$source_id, c("harvard_2025_2026", "july_2025_candidate")),
            !any(grepl("RMA_clinic_data_gdrive", actual$root, fixed = TRUE)))
  message("New-wave source roots follow Stagecoach items and here() from the notebook directory; ",
          "ambiguous, missing, disabled, and non-project configurations fail explicitly.")
})
