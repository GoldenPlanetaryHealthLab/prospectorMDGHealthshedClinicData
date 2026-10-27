# Explicit bounded source smoke check: one small CSV per approved delivery.
# No targets, production manifests, or pipeline products are created.
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-new-wave-source-sample.R
local({
  scratch <- tempfile("new-wave-source-sample-");dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  helper <- knitr::purl("notebooks/pipeline/26_page-prep-2025-2026.qmd",
    output=file.path(scratch,"helpers.R"),documentation=0L,quiet=TRUE)
  for(expr in parse(helper)) if(is.call(expr) && identical(expr[[1L]],as.name("<-")) &&
    is.call(expr[[3L]]) && identical(expr[[3L]][[1L]],as.name("function"))) eval(expr)
  roots <- s8_new_wave_roots_fn()
  results <- lapply(seq_len(nrow(roots)),function(i) {
    inventory <- s8_new_wave_inventory_fn(roots[i,],hash=FALSE)
    choices <- inventory[inventory$extension=="csv" & inventory$bytes>0 & inventory$bytes<=1000000,]
    stopifnot(nrow(choices)>0L)
    chosen <- choices[which.min(choices$bytes),]
    hash <- unname(tools::md5sum(chosen$path))
    shape <- s8_new_wave_shape_fn(chosen$path)
    stopifnot(shape$missing_uid==0L,shape$missing_period==0L,shape$invalid_period==0L)
    output <- s8_new_wave_write_raw_fn(chosen$path,file.path(scratch,paste0("sample",i,".parquet")),
      roots$source_id[[i]],paste(roots$source_id[[i]],chosen$relative_path,sep="/"),hash)
    relation <- duckplyr::read_parquet_duckdb(output,prudence="stingy")
    expected <- c(s8_new_wave_header_fn(chosen$path),"_source_file","_source_row","_source_id","_source_md5")
    stopifnot(identical(names(relation),expected),identical(unname(tools::md5sum(chosen$path)),hash))
    dplyr::mutate(shape,source_id=roots$source_id[[i]],sample_bytes=chosen$bytes,.before=1L)
  })
  print(dplyr::bind_rows(results))
  message("Both bounded source samples passed raw-reader, key, schema and hash checks. Sources were unchanged; no targets ran.")
})
