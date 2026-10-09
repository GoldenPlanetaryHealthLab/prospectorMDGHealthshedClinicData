# Run from the repository root:
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-clinic-month-parquet-transforms.R
# Synthetic inputs only; no pipeline targets or staged data are executed.
library(dplyr)
library(duckplyr)

local({
  scratch <- tempfile("check-clinic-month-parquet-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  fixture_env <- new.env(parent = environment())
  assignments <- list()
  for (page in c("18_page-prep-w3", "20_page-construct-clinic-ids")) {
    tangled <- file.path(scratch, paste0(page, ".R"))
    knitr::purl(file.path("notebooks/pipeline", paste0(page, ".qmd")),
                output = tangled, documentation = 0L, quiet = TRUE)
    for (expr in parse(tangled)) {
      if (!is.call(expr) || !identical(expr[[1L]], as.name("<-"))) next
      assignments[[as.character(expr[[2L]])]] <- expr[[3L]]
      if (is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
        eval(expr, envir = fixture_env)
      }
    }
  }
  run_target_command <- function(name) {
    command <- assignments[[name]]
    output <- file.path(scratch, "outputs", paste0(name, ".parquet"))
    # Redirect only the output path in the real notebook command.
    command[[which(names(command) == "output_path")]] <- output
    result <- eval(command, envir = fixture_env)
    stopifnot(identical(result, output), file.exists(result))
    assign(name, result, envir = fixture_env)
    read_parquet_duckdb(result, prudence = "stingy") |> collect() |> as.data.frame()
  }

  rows <- data.frame(
    fixture_id = 1:9, cNom = paste("CSB Santé", 1:9),
    cAnnee = c(2013L, 2012L, 2014L, rep(2015L, 4), 2016L, NA_integer_),
    cPeriode = c(1L, 1L, 1L, 2L, 7L, 7L, NA_integer_, 1L, 1L),
    cCodeNiv = 123L,
    cCodeStru = c(1001L, 1001L, 110543027L, 630242008L, 1001L, 1001L,
                  1001L, 9000L, NA_integer_),
    fname = c(1L, 1L, 1L, 2L, 1L, 2L, NA_integer_, 1L, 1L),
    .inventory_row = 1:9, .inventory_file = "2010_01.csv"
  )
  # Reverse source-row order and the input paths, and repeat an identity across
  # files. The inventory must preserve the repeat and restore source order.
  first <- rows[9:1, ]
  second <- rows[5L, ]
  second$.inventory_file <- "2010_02.csv"
  paths <- file.path(scratch, c("first.parquet", "second.parquet"))
  compute_parquet(as_duckdb_tibble(first), paths[[1L]])
  compute_parquet(as_duckdb_tibble(second), paths[[2L]])
  fixture_env$s2_clinic_months_deduplicated_parquet <- rev(paths)
  expected <- rbind(rows, second)
  inventory <- run_target_command("s2_clinic_months_inventory_parquet")
  stopifnot(isTRUE(all.equal(inventory, expected, check.attributes = FALSE)))

  message("Clinic-month Parquet inventory preserves source order and repeated identities across files.")
})
