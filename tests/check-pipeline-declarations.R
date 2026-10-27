# Parse declarations and dependency edges without loading or running targets.
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-pipeline-declarations.R
local({
  scratch <- tempfile("pipeline-declarations-");dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  parsed <- as.list(parse("_targets.R"))
  graph <- Filter(function(x) is.call(x) && identical(x[[1L]],as.name("list")),parsed)
  stopifnot(length(graph)==1L)
  entries <- as.list(graph[[1L]])[-1L]
  commands <- list(); patterns <- list(); files <- character(); iterations <- list()
  add <- function(name,command) {
    if(name %in% names(commands)) stop("Duplicate declaration: ",name)
    commands[[name]] <<- command
  }
  for(entry in entries) {
    if(identical(entry[[1L]],quote(tarchetypes::tar_tangle))) {
      args <- as.list(entry[[2L]])[-1L]
      stopifnot(all(vapply(args,is.character,TRUE)))
      page <- do.call(file.path,args)
      output <- knitr::purl(page,output=file.path(scratch,paste0(basename(page),".R")),documentation=0L,quiet=TRUE)
      for(expr in parse(output)) if(is.call(expr) && identical(expr[[1L]],as.name("<-")) && is.symbol(expr[[2L]])) {
        name <- as.character(expr[[2L]])
        if(grepl("^(s[0-9][[:alnum:]]*_|schecks_|srelease_)",name)) add(name,expr[[3L]])
      }
    } else if(identical(entry[[1L]],as.name("tar_target"))) {
      name <- as.character(entry[[2L]]);add(name,entry[[3L]])
      args <- as.list(entry)
      if("pattern" %in% names(args)) patterns[[name]] <- all.vars(args$pattern)
      if("format" %in% names(args) && identical(args$format,"file")) files <- c(files,name)
      if("iteration" %in% names(args)) iterations[[name]] <- args$iteration
    }
  }
  names <- names(commands)
  dependencies <- lapply(commands,function(x) intersect(all.names(x,functions=TRUE),names))
  unknown <- unique(unlist(lapply(commands,function(x) {
    symbols <- all.names(x,functions=TRUE)
    setdiff(symbols[grepl("^(s[0-9][[:alnum:]]*_|schecks_|srelease_)",symbols)],names)
  })))
  stopifnot(length(unknown)==0L)
  remaining <- names
  while(length(remaining)) {
    ready <- remaining[vapply(dependencies[remaining],function(x) !any(x %in% remaining),TRUE)]
    if(!length(ready)) stop("Dependency cycle among: ",paste(remaining,collapse=", "))
    remaining <- setdiff(remaining,ready)
  }
  for(name in names(patterns)) for(dimension in patterns[[name]]) {
    stopifnot(dimension %in% names)
    if(dimension %in% files && !dimension %in% names(patterns)) stop("Static file bundle used as branch dimension: ",dimension)
  }
  stopifnot(identical(iterations$s8_new_wave_file_records_list,"list"),
    identical(iterations$srelease_resources_list,"list"),
    all(c("s8_new_wave_source_inventory_df","s8_new_wave_parameters_list","srelease_parameters_list","s8_new_wave_inputs_raw_csv","s9_new_wave_typed_parquet","s10_new_wave_diagnosis_parquet",
      "s10_final_diagnosis_parquet","schecks_final_shape_df","srelease_manifest_parquet","srelease_datapackage_json") %in% names))
  stopifnot(!any(c("s8_new_wave_config_raw_yml","s8_new_wave_manifest_raw_csv","srelease_config_raw_yml") %in% names))
  inventory_entry <- Filter(function(x) identical(x[[1L]],as.name("tar_target")) &&
    identical(x[[2L]],as.name("s8_new_wave_source_inventory_df")),entries)[[1L]]
  stopifnot(identical(as.list(inventory_entry)$cue,quote(tar_cue(mode = "always"))))
  cat("Parsed",length(commands),"declarations; dependency graph is acyclic, all source references resolve, and branch dimensions are declared. No targets ran.\n")
})
