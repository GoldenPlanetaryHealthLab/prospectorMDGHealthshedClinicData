# Exercise the plotting/comparison script on unequal synthetic datasets.
# No targets, archive, or research data reads.
suppressPackageStartupMessages({library(dplyr);library(duckplyr)})
local({
  scratch <- tempfile("guide-fixtures-");dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  work <- file.path(scratch,"work");assets <- file.path(scratch,"assets")
  dir.create(work)
  sha <- function(path) substr(system2("sha256sum",shQuote(path),stdout=TRUE),1L,64L)
  old <- data.frame(clinic_ID=c("A","B","C"),cAnnee=2024L,cPeriode=1:3,fname=6L,
    V114=c(1,2,-1),V121=c(NA,0,-99),V135=c(4,NA,6))
  # Change one value, change missing to zero, remove a key, add a historical
  # key, and append 2025. Row order must not affect alignment.
  current <- old[c(2,1),];current$V114[current$clinic_ID=="A"] <- 9
  current$V121[current$clinic_ID=="A"] <- 0
  extra <- old[1,];extra$clinic_ID <- "D";extra$cPeriode <- 4L
  extension <- extra;extension$clinic_ID <- "E";extension$cAnnee <- 2025L
  current <- bind_rows(current,extra,extension)
  old_path <- file.path(work,"archive-selected.parquet")
  path <- file.path(work,"current.parquet")
  compute_parquet(as_duckdb_tibble(old),old_path)
  compute_parquet(as_duckdb_tibble(current),path)
  request <- list(current_path=path,current_sha256=sha(path),captured_utc="fixture",
    baseline=list(archive_sha256=paste(rep("a",64),collapse=""),label="synthetic fixture"))
  request_path <- file.path(work,"request.json")
  jsonlite::write_json(request,request_path,auto_unbox=TRUE)
  jsonlite::write_json(list(request_sha256=sha(request_path),projection_sha256=sha(old_path),
    archive_sha256=request$baseline$archive_sha256,member="fixture",member_sha256=sha(old_path),pyarrow="fixture"),
    file.path(work,"archive-receipt.json"),auto_unbox=TRUE)
  run <- function() {
    output <- suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
      shQuote(c("scripts/build-data-guide.R","plot",work,assets)),stdout=TRUE,stderr=TRUE))
    list(status=if(is.null(attr(output,"status"))) 0L else attr(output,"status"),output=output)
  }
  result <- run()
  if(result$status!=0L) stop(paste(result$output,collapse="\n"))
  checks <- read_parquet_duckdb(file.path(work,"keyed-comparison.parquet"),prudence="stingy") |> collect()
  stopifnot(all(checks$shared_keys==2),all(checks$archive_only==1),all(checks$current_only==1),
    checks$different_cells[checks$var=="V114"]==1,
    checks$different_cells[checks$var=="V121"]==1,
    checks$different_cells[checks$var=="V135"]==0,
    all(checks$equal_cells+checks$different_cells==checks$shared_keys))
  totals <- read_parquet_duckdb(file.path(work,"monthly-aggregates.parquet"),prudence="stingy") |> collect()
  # All missing and negative-only totals must stay missing, rather than zero.
  stopifnot(is.na(totals$total[totals$source=="Frozen archive" & totals$var=="V121" & totals$month==1]),
    is.na(totals$total[totals$source=="Frozen archive" & totals$var=="V114" & totals$month==3]),
    all(file.exists(file.path(assets,c("archive-comparison.png","trend-extension.png","reporting-coverage.png","provenance.json")))))
  # The script must reject an injected duplicate key before any many-to-many join.
  compute_parquet(as_duckdb_tibble(bind_rows(old,old[1,])),old_path)
  receipt_path <- file.path(work,"archive-receipt.json")
  receipt <- jsonlite::read_json(receipt_path)
  receipt$projection_sha256 <- sha(old_path)
  jsonlite::write_json(receipt,receipt_path,auto_unbox=TRUE)
  stopifnot(run()$status!=0L)
  message("Data-guide diagnostics detect changed values, missing/extra keys and duplicate keys; preserve missing/sentinel distinctions and create aggregate plots. No research data read.")
})
