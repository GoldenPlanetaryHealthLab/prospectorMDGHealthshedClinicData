# Synthetic, bounded checks only. No tar_make(), tar_read(), or graph execution.
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-new-wave-pipeline.R
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(duckplyr);library(sf)})
local({
  scratch <- tempfile("new-wave-fixtures-"); dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  env <- new.env(parent=environment())
  pages <- c("pipeline/22_page-clean-clinic-diagnosis.qmd","pipeline/23_page-construct-healthsheds.qmd",
    "pipeline/26_page-prep-2025-2026.qmd","pipeline/27_page-clean-2025-2026.qmd",
    "pipeline/28_page-geospatial-aggregate-2025-2026.qmd","pipeline/30_page-validate-migration.qmd",
    "releases/19_page-package-release.qmd","releases/21_page-verify-release.qmd")
  notebook_values <- c("s8_new_wave_parameters_list", "s9_new_wave_mapping_review_df",
    "s10_new_wave_identity_review_df", "s10_new_wave_na_corrections_df", "srelease_parameters_list")
  for(page in pages) {
    r <- knitr::purl(file.path("notebooks",page),output=file.path(scratch,basename(paste0(page,".R"))),documentation=0L,quiet=TRUE)
    for(expr in parse(r)) if(is.call(expr) && identical(expr[[1L]],as.name("<-"))) {
      definition <- is.call(expr[[3L]]) && identical(expr[[3L]][[1L]],as.name("function"))
      setting <- is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in% notebook_values
      if(setting) stopifnot(is.call(expr[[3L]]),
        identical(expr[[3L]][[1L]],as.name("list")) || identical(expr[[3L]][[1L]],quote(tibble::tibble)))
      if(definition || setting) eval(expr,env)
    }
  }
  use <- function(name,...) { message("Checking ",name);get(name,envir=env)(...) }
  fail <- function(expr) stopifnot(inherits(tryCatch({force(expr);NULL},error=function(e) e),"error"))
  read <- function(path) read_parquet_duckdb(path,prudence="stingy") |> collect()
  write <- function(x,name) { p <- file.path(scratch,paste0(name,".parquet"));compute_parquet(as_duckdb_tibble(as.data.frame(x)),p);p }
  config <- env$s8_new_wave_parameters_list
  roots <- tibble(source_id=c("harvard_2025_2026","july_2025_candidate"),root=file.path(scratch,c("main","july")))
  for(root in roots$root) dir.create(file.path(root,"tableau 4"),recursive=TRUE)
  first <- tibble(organisationunitid=c("A","A","B","C"),periodcode=c("202501","202501","202501","202503"),
    periodname=c("January 2025","January 2025","January 2025","March 2025"),
    organisationunitname=c("Clinic A","Clinic A","Clinic B","Clinic C"),orgunitlevel2="Region",orgunitlevel3="District",
    organisationunitcode=c("0007","0007","",""),`T4: Fever`=c("1","4","bad","NA"),
    `T4: Cough`=c("NA","0","0","NA"),BadNegative=c("-2","-3","0","NA"),Fridge=c("10","9","0","100"))
  second <- first[c(1L,3L),];second$`T4: Fever`<-c("7","0");second$`T4: Cough`<-c(NA_character_,"2")
  second$organisationunitname <- c("Other A","Clinic B")
  second$organisationunitcode[[1L]] <- "0099"
  paths <- file.path(roots$root,"tableau 4/a.csv")
  readr::write_csv(first,paths[[1L]]);readr::write_csv(second,paths[[2L]])
  inventory <- use("s8_new_wave_sources_fn",roots)
  stopifnot(nrow(inventory)==2L,!"content_md5" %in% names(inventory))
  # Discovery finds additions; file content tracking is left to file targets.
  added <- file.path(roots$root[[1L]],"tableau 4/added.csv")
  file.copy(paths[[1L]],added)
  stopifnot(nrow(use("s8_new_wave_sources_fn",roots))==3L)
  unlink(added)
  raw <- vapply(seq_len(nrow(inventory)),function(i) use("s8_new_wave_write_raw_fn",inventory$path[[i]],
    file.path(scratch,paste0("raw",i,".parquet")),inventory$source_id[[i]],inventory$source_file[[i]]),"")
  snapshot <- use("s8_new_wave_snapshot_fn",raw,inventory)
  stopifnot(nrow(snapshot)==2L,identical(snapshot$content_md5,unname(tools::md5sum(paths))))
  original <- readBin(paths[[1L]],"raw",n=file.info(paths[[1L]])$size)
  cat("\n",file=paths[[1L]],append=TRUE)
  fail(use("s8_new_wave_write_raw_fn",paths[[1L]],file.path(scratch,"changed.parquet"),"x","x",snapshot$content_md5[[1L]]))
  stopifnot(identical(inventory,use("s8_new_wave_sources_fn",roots)))
  writeBin(original,paths[[1L]])
  headers <- use("s8_new_wave_schema_fn",raw)
  schema <- tibble(w3_description=c("T4: Fever","T4: Cough","Unavailable 1","Unavailable 2","BadNegative","Fridge","Fridge 2"),
    w3_colname=c("X1","X2","X3","X4","X1067","X1708","X1709"))
  old_headers <- tibble(table_key=character(),source_name=character())
  candidates <- use("s9_new_wave_candidates_fn",headers,schema$w3_description,old_headers)
  review <- env$s9_new_wave_mapping_review_df
  mapping <- use("s9_new_wave_mapping_fn",headers,candidates,review,schema$w3_description,config)
  stopifnot(all(mapping$applied))
  stopifnot(identical(use("s9_new_wave_normalize_fn",c("Écho – 2", " ECHO-2 ")),c("echo-2","echo-2")),
    length(use("s9_new_wave_remainders_fn","prefix masculin","prefix"))==0L,
    identical(use("s9_new_wave_remainders_fn","unwrapped illness",c("ce nouveaux cas","t4:")),character()),
    identical(use("s9_new_wave_remainders_fn","ce nouveaux cas fever","ce nouveaux cas"),"fever"))
  ambiguous_headers <- tibble(table_key="tableau 4/a.csv",source_name="echo",is_metadata=FALSE)
  ambiguous <- use("s9_new_wave_candidates_fn",ambiguous_headers,c("ÉCHO","ECHO"),old_headers)
  stopifnot(ambiguous$n_options==2L,ambiguous$method=="ambiguous",is.na(ambiguous$target_name))
  wrapper_headers <- tibble(table_key="tableau 4/wrappers.csv",source_name=c("ce nouveaux cas Fever","ce nouveaux cas Cough"),is_metadata=FALSE)
  wrapper_old <- tibble(table_key="tableau 4/wrappers.csv",source_name=c("T4: Fever","T4: Cough"))
  wrappers <- use("s9_new_wave_candidates_fn",wrapper_headers,schema$w3_description,wrapper_old)
  stopifnot(all(wrappers$method=="M4 wrapper"),setequal(wrappers$target_name,c("T4: Fever","T4: Cough")))
  # A table can have a shared wrapper plus an outlier with no matching prefix.
  mixed_headers <- bind_rows(wrapper_headers,tibble(table_key="tableau 4/wrappers.csv",source_name="Other illness",is_metadata=FALSE))
  mixed <- use("s9_new_wave_candidates_fn",mixed_headers,schema$w3_description,wrapper_old)
  stopifnot(all(mixed$method[mixed$source_name!="Other illness"]=="M4 wrapper"),
    mixed$method[mixed$source_name=="Other illness"]=="unmatched",
    is.na(mixed$target_name[mixed$source_name=="Other illness"]))
  table_headers <- tibble(table_key="tableau 4/new.csv",source_name="T4:- Fever",is_metadata=FALSE)
  table_match <- use("s9_new_wave_candidates_fn",table_headers,schema$w3_description,old_headers)
  stopifnot(table_match$method=="M5 table code",table_match$target_name=="T4: Fever")
  aliases <- use("s9_new_wave_aliases_fn")
  alias_headers <- tibble(table_key="tableau 13/new.csv",source_name=names(aliases)[[1L]],is_metadata=FALSE)
  alias_match <- use("s9_new_wave_candidates_fn",alias_headers,unname(aliases),old_headers)
  stopifnot(alias_match$method=="M2 script0",alias_match$target_name==unname(aliases[[1L]]))
  fail(use("s9_new_wave_mapping_fn",headers,candidates,tibble(),schema$w3_description,config))
  conflict_headers <- headers[headers$source_name=="T4: Fever",]
  conflict_headers$source_name[[2L]] <- "t4: fever"
  conflicts <- use("s9_new_wave_candidates_fn",conflict_headers,schema$w3_description,old_headers)
  conflict_mapping <- use("s9_new_wave_mapping_fn",conflict_headers,conflicts,review,schema$w3_description,config)
  stopifnot(all(!conflict_mapping$applied[conflict_mapping$periodcode==202501L]),
    all(conflict_mapping$conflict[conflict_mapping$periodcode==202501L]),
    all(conflict_mapping$applied[conflict_mapping$periodcode==202503L]))
  override <- tibble(source_id="*",relative_path="*",source_name="t4: fever",target_name=NA_character_,
    valid_from="202501",valid_to="202612",decision="reject",reviewer="fixture",reason="fixture rejection")
  resolved <- use("s9_new_wave_mapping_fn",conflict_headers,conflicts,override,schema$w3_description,config)
  stopifnot(sum(resolved$applied)==2L)
  fail(use("s9_new_wave_mapping_fn",conflict_headers,conflicts,bind_rows(override,override),schema$w3_description,config))
  typed <- lapply(seq_along(raw),function(i) use("s9_new_wave_parse_fn",raw[[i]],mapping,config,file.path(scratch,paste0("typed",i,".parquet"))))
  typed_paths <- vapply(typed,function(x) x[["typed"]],"")
  stopifnot(sum(read(typed[[1L]][["numeric_audit"]])$invalid_numeric)==1L)
  reduced <- use("s9_new_wave_reduce_fn",typed_paths,file.path(scratch,"reduced.parquet"))
  values <- read(reduced)
  stopifnot(values$value[values$organisationunitid=="A" & values$target_name=="T4: Fever"]==7,
    all(is.na(values$value[values$organisationunitid=="C" & values$target_name=="T4: Fever"])))
  reports <- use("s9_new_wave_reports_fn",raw,reduced,schema,config,file.path(scratch,"reports.parquet"))
  report_rows <- read(reports)
  stopifnot(report_rows$organisationunitname[report_rows$organisationunitid=="A"]=="Clinic A",
    report_rows$metadata_conflicts[report_rows$organisationunitid=="A"]>0L,
    sum(report_rows$reported)==2L,!report_rows$reported[report_rows$organisationunitid=="C"])
  identity_review <- env$s10_new_wave_identity_review_df
  identity <- use("s10_new_wave_identity_fn",reports,identity_review,"A",file.path(scratch,"identity.parquet"))
  stopifnot(read(identity)$identity_status[read(identity)$source_uid=="B"]=="new_uid")
  identity_override <- tibble(source_uid="B",clinic_ID="B-reviewed",valid_from="202501",valid_to="202501",
    decision="accept",reviewer="fixture",reason="fixture alias")
  reviewed <- use("s10_new_wave_identity_fn",reports,identity_override,"A",file.path(scratch,"reviewed.parquet"))
  stopifnot(read(reviewed)$clinic_ID[read(reviewed)$source_uid=="B"]=="B-reviewed")
  fail(use("s10_new_wave_identity_fn",reports,bind_rows(identity_override,identity_override),"A",file.path(scratch,"invalid-id.parquet")))
  rules <- tibble(wave="wave3B",var=paste0("V",1:5),filename="file1",
    terms=list("X1",c("X1","X2"),c("X3","X4"),"X2","X1067"))
  dictionary <- select(rules,var,filename)
  corrections <- env$s10_new_wave_na_corrections_df
  dir.create(file.path(scratch,"new"));dir.create(file.path(scratch,"old"));dir.create(file.path(scratch,"final"))
  new_path <- use("s10_new_wave_diagnosis_fn","file1",reduced,reports,identity,rules,schema,corrections,config,file.path(scratch,"new/file1.parquet"))
  new <- read(new_path) |> arrange(clinic_ID)
  stopifnot(nrow(new)==2L,identical(new$V1,c(7,0)),identical(new$V2,c(7,2)),
    all(is.na(new$V3)),identical(new$V4,c(0,2)),is.na(new$V5[[1L]]))
  exclude_review <- identity_override;exclude_review$decision<-"exclude";exclude_review$clinic_ID<-NA_character_
  excluded <- use("s10_new_wave_identity_fn",reports,exclude_review,"A",file.path(scratch,"excluded-id.parquet"))
  excluded_output <- use("s10_new_wave_diagnosis_fn","file1",reduced,reports,excluded,rules,schema,corrections,config,file.path(scratch,"excluded.parquet"))
  stopifnot(identical(read(excluded_output)$clinic_ID,"A"),sum(read(excluded)$decision=="exclude")==1L)
  bad_correction <- tibble(file="file1",var="V1",period_from="202501",period_to="202501",reason="fixture",reviewer="fixture")
  fail(use("s10_new_wave_diagnosis_fn","file1",reduced,reports,identity,rules,schema,bad_correction,config,file.path(scratch,"invalid-correction.parquet")))
  unknown_file <- bad_correction;unknown_file$file<-"file999"
  fail(use("s10_new_wave_diagnosis_fn","file1",reduced,reports,identity,rules,schema,unknown_file,config,file.path(scratch,"unknown-file.parquet")))
  zero_correction <- bad_correction;zero_correction$var<-"V3"
  corrected <- use("s10_new_wave_diagnosis_fn","file1",reduced,reports,identity,rules,schema,zero_correction,config,file.path(scratch,"correction.parquet"))
  stopifnot(all(is.na(read(corrected)$V3)))
  old <- new[1L,];old$cAnnee<-2024L;old$cPeriode<-12L
  old_path <- file.path(scratch,"old/file1.parquet");compute_parquet(as_duckdb_tibble(old),old_path)
  final_path <- use("s10_final_diagnosis_fn","file1",old_path,new_path,file.path(scratch,"final/file1.parquet"))
  stopifnot(nrow(read(final_path))==3L)
  fail(use("s10_final_diagnosis_fn","file1",new_path,new_path,file.path(scratch,"overlap.parquet")))
  old_time <- tibble(clinic_ID="A",year=2024L,month=12L,fname=6L,clinic_database_code=c("001","002"),database_code_origin="historical")
  new_time <- use("s10_new_wave_reporting_fn",reports,identity,config)
  stopifnot(new_time$database_code_ambiguous[new_time$clinic_ID=="A"],
    is.na(new_time$clinic_database_code[new_time$clinic_ID=="A"]),
    identical(report_rows$source_code_candidates[[which(report_rows$organisationunitid=="A")]],c("0007","0099")))
  time <- use("s10_final_reporting_fn",old_time,new_time)
  ambiguous_time <- time$data |> filter(clinic_ID=="A",year==2024L)
  stopifnot(nrow(ambiguous_time)==1L,is.na(ambiguous_time$clinic_database_code),ambiguous_time$database_code_ambiguous,
    nrow(time$audit)==2L,!anyDuplicated(time$data[c("clinic_ID","year","month")]),
    all(!time$data$source_covered[time$data$month==2L]))
  polygon <- st_as_sfc("POLYGON ((0 0,1 0,1 1,0 1,0 0))",crs=4326)
  sheds <- st_sf(fs_uid="A",geometry=polygon)
  old_spatial <- tibble(clinic_ID="A",n_month=1L,first_report="y2024_m12",last_report="y2024_m12",
    fs_name="Clinic A",clinic_x=0.5,clinic_y=0.5,coordinate_status="available")
  spatial <- use("s10_final_spatial_fn",time$data,old_spatial,new_time,sheds,config)
  stopifnot(setequal(spatial$clinic_ID,c("A","B")),spatial$has_healthshed[spatial$clinic_ID=="A"],
    !spatial$has_healthshed[spatial$clinic_ID=="B"],is.na(spatial$clinic_x[spatial$clinic_ID=="B"]))
  shape <- use("schecks_final_shape_fn",final_path,dictionary,time$data,spatial,sheds)
  stopifnot(all(shape$ok))
  bad_dictionary <- dictionary;bad_dictionary$var[[1L]]<-"V999"
  bad_shape <- use("schecks_final_shape_fn",final_path,bad_dictionary,time$data,spatial,sheds)
  stopifnot(any(!bad_shape$ok))
  seams <- use("schecks_seam_fn","file1",final_path,dictionary,file.path(scratch,"seams.parquet"))
  stopifnot(nrow(read(seams))==10L)
  geometry <- use("srelease_geometry_fn",sheds,2022L,file.path(scratch,"geometry.parquet"))
  restored <- st_as_sfc(read(geometry)$geometry_wkt,crs=read(geometry)$crs_wkt[[1L]])
  stopifnot(all(st_equals(polygon,restored,sparse=FALSE)))
  release_config <- env$srelease_parameters_list
  release_config$version <- "1.0.0"
  resources <- use("srelease_resources_fn",final_path,write(dictionary,"dictionary"),write(time$data,"time"),write(spatial,"spatial"),
    geometry,write(shape,"shape"),write(mapping,"mapping"),identity,write(time$audit,"codes"),reduced,seams,
    write(tibble(component="R",value=as.character(getRversion())),"provenance"),write(snapshot,"source-inventory"),
    typed[[1L]][["numeric_audit"]],typed[[1L]][["key_audit"]],write(tibble(source_uid=character(),applied=logical()),"predecessors"),
    write(review,"review"),reports)
  package_root <- file.path(scratch,"packages")
  contract <- use("srelease_contract_fn",resources,shape,release_config,package_root)
  stopifnot(identical(contract,use("srelease_contract_fn",resources,shape,release_config,package_root)))
  files <- vapply(resources,function(r) use("srelease_copy_fn",r,contract,release_config,package_root),"")
  checks <- bind_rows(lapply(seq_along(resources),function(i) use("srelease_verify_fn",resources[[i]],files[[i]],contract,release_config)))
  stopifnot(all(checks$rows_equal),all(checks$schema_equal))
  manifest_out <- use("srelease_manifest_fn",checks,resources,release_config,file.path(package_root,release_config$version,"manifest.parquet"))
  stopifnot(file.exists(manifest_out),identical(files[[1L]],use("srelease_copy_fn",resources[[1L]],contract,release_config,package_root)))
  schemas <- use("srelease_schema_fn",files,file.path(package_root,release_config$version),file.path(package_root,release_config$version,"resource_schemas.parquet"))
  stopifnot(file.exists(schemas),identical(schemas,use("srelease_schema_fn",files,file.path(package_root,release_config$version),schemas)))
  compute_parquet(as_duckdb_tibble(tibble(corrupt=TRUE)),schemas)
  fail(use("srelease_schema_fn",files,file.path(package_root,release_config$version),schemas))
  changed <- resources;changed[[1L]]$source<-new_path
  fail(use("srelease_contract_fn",changed,shape,release_config,package_root))
  compute_parquet(as_duckdb_tibble(tibble(corrupt=TRUE)),files[[1L]])
  fail(use("srelease_verify_fn",resources[[1L]],files[[1L]],contract,release_config))
  # Parse the emitted graph and every new page without evaluating target constructors.
  parse("_targets.R")
  stopifnot(all(vapply(c("26_page-prep-2025-2026.qmd","27_page-clean-2025-2026.qmd","28_page-geospatial-aggregate-2025-2026.qmd"),
    function(p) any(grepl(p,readLines("_targets.R"),fixed=TRUE)),TRUE)))
  message("New-wave R fixtures passed: Stagecoach inventory and R parameters, matching/conflicts/reviews, parsing/maxima, identities, clinical missingness, final keys, seam evidence and immutable packaging. No targets ran.")
})
