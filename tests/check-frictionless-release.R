# Synthetic fixtures only: no research data, target store, or graph execution.
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-frictionless-release.R
suppressPackageStartupMessages({library(dplyr);library(duckplyr);library(sf)})
local({
  scratch <- tempfile("frictionless-fixtures-");dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  env <- new.env(parent=environment())
  pages <- c("pipeline/22_page-clean-clinic-diagnosis.qmd",
    "pipeline/23_page-construct-healthsheds.qmd","pipeline/30_page-validate-migration.qmd",
    "releases/19_page-package-release.qmd","releases/21_page-verify-release.qmd",
    "releases/22_page-export-frictionless.qmd")
  for(page in pages) {
    output <- knitr::purl(file.path("notebooks",page),output=file.path(scratch,paste0(basename(page),".R")),documentation=0L,quiet=TRUE)
    for(expr in parse(output)) if(is.call(expr) && identical(expr[[1L]],as.name("<-"))) {
      definition <- is.call(expr[[3L]]) && identical(expr[[3L]][[1L]],as.name("function"))
      if(definition || identical(expr[[2L]],as.name("srelease_parameters_list"))) eval(expr,env)
    }
  }
  use <- function(name,...) get(name,envir=env)(...)
  fail <- function(expr) stopifnot(inherits(tryCatch({force(expr);NULL},error=function(e)e),"error"))
  config <- env$srelease_parameters_list
  use("srelease_parameters_fn",config)
  for(version in c("01.0.0","1.0","1.0.0-internal","v1.0.0","../1.0.0")) {
    bad <- config;bad$version <- version;fail(use("srelease_parameters_fn",bad))
  }
  for(change in c("major","minor","patch")) {
    next_config <- config;next_config$previous_version <- "1.2.3";next_config$change <- change
    next_config$version <- c(major="2.0.0",minor="1.3.0",patch="1.2.4")[[change]]
    use("srelease_parameters_fn",next_config)
    next_config$version <- "1.2.3";fail(use("srelease_parameters_fn",next_config))
  }
  write <- function(sql,name) use("s4a_diagnosis_write_fn",sql,file.path(scratch,paste0(name,".parquet")))
  diagnoses <- write("SELECT 'A' AS clinic_ID, 2025 AS year, 0::DOUBLE AS measurement, [1,2] AS candidates","diagnosis")
  seam <- write("SELECT 'A' AS clinic_ID, NULL::DOUBLE AS measurement","seam")
  geom <- st_as_sfc(c("POLYGON ((0 0,1 0,1 1,0 1,0 0))","POLYGON EMPTY"),crs=4326)
  geometry <- use("srelease_geometry_fn",st_sf(fs_uid=c("A","B"),geometry=geom),2022L,file.path(scratch,"geometry.parquet"))

  # First try the same direct file-path API as HeatStress, on unchanged Parquet.
  direct <- tryCatch(frictionless::add_resource(frictionless::create_package(),"healthsheds",geometry),error=function(e)e)
  stopifnot(inherits(direct,"error"))
  message("Direct Parquet attempt (frictionless ",packageVersion("frictionless"),"): ",conditionMessage(direct))

  resources <- list(list(name="by_diagnosis/file1.parquet",source=diagnoses),
    list(name="audits/seams/file1.parquet",source=seam),
    list(name="healthsheds2022.parquet",source=geometry))
  root <- file.path(scratch,"releases")
  shape <- tibble(ok=TRUE)
  contract <- use("srelease_contract_fn",resources,shape,config,root)
  files <- vapply(resources,function(r)use("srelease_copy_fn",r,contract,config,root),"")
  checks <- bind_rows(lapply(seq_along(resources),function(i)
    use("srelease_verify_fn",resources[[i]],files[[i]],contract,config)))
  manifest <- use("srelease_manifest_fn",checks,resources,config,file.path(dirname(contract),"manifest.parquet"))
  schemas <- use("srelease_schema_fn",files,dirname(contract),file.path(dirname(contract),"resource_schemas.parquet"))
  inventory <- use("srelease_inventory_fn",resources,files,contract,manifest,schemas,config)
  stopifnot(length(inventory)==6L,
    identical(vapply(inventory[1:3],`[[`,"","path"),vapply(resources,`[[`,"","name")),
    identical(inventory[[1]]$parquetSchema[[4]]$type,"INTEGER[]"),
    inventory[[1]]$rowCount==1,inventory[[3]]$spatial$referenceYear==2022L,
    !inventory[[3]]$spatial$geoparquet)
  before <- tools::md5sum(files)
  descriptor <- use("srelease_datapackage_fn",inventory,config,root)
  stopifnot(identical(before,tools::md5sum(files)),
    identical(descriptor,use("srelease_datapackage_fn",inventory,config,root)))
  package <- frictionless::read_package(descriptor)
  stopifnot(identical(package$name,config$name),identical(package$version,config$version),
    identical(frictionless::resource_names(package),vapply(inventory,`[[`,"","name")))
  # Check that the whole release can move without changing the descriptor.
  relocated <- file.path(scratch,"relocated");dir.create(relocated)
  stopifnot(all(file.copy(list.files(dirname(descriptor),full.names=TRUE,all.files=TRUE,no..=TRUE),relocated,recursive=TRUE)))
  moved <- frictionless::read_package(file.path(relocated,"datapackage.json"))
  stopifnot(all(vapply(moved$resources,function(r)file.exists(file.path(relocated,r$path)),TRUE)))
  restored <- read_parquet_duckdb(file.path(relocated,moved$resources[[3]]$path),prudence="stingy") |> collect()
  restored_geometry <- st_as_sfc(restored$geometry_wkt,crs=restored$crs_wkt[[1]])
  stopifnot(st_crs(restored_geometry)==st_crs(geom),
    all(st_is_empty(restored_geometry)==restored$is_empty),
    st_equals(geom[1],restored_geometry[1],sparse=FALSE)[1,1])

  changed <- config;changed$notes <- "Changed metadata"
  fail(use("srelease_datapackage_fn",inventory,changed,root))
  fail(use("srelease_contract_fn",resources,shape,changed,root))
  fail(use("srelease_contract_fn",resources,tibble(ok=FALSE),config,root))
  fail(use("srelease_inventory_fn",resources,files[-1],contract,manifest,schemas,config))
  duplicate <- resources;duplicate[[2]]$name <- duplicate[[1]]$name
  fail(use("srelease_inventory_fn",duplicate,files,contract,manifest,schemas,config))
  # A changed source must fail even if the packaged bytes are still intact.
  original <- readBin(diagnoses,"raw",n=file.info(diagnoses)$size)
  writeBin(as.raw(0),diagnoses)
  fail(use("srelease_inventory_fn",resources,files,contract,manifest,schemas,config))
  writeBin(original,diagnoses)
  # A corrupt package is rejected on inventory and on descriptor retry.
  writeBin(as.raw(0),files[[1]])
  fail(use("srelease_inventory_fn",resources,files,contract,manifest,schemas,config))
  fail(use("srelease_datapackage_fn",inventory,config,root))
  message("Frictionless release fixtures passed: semantic versioning, native Parquet types, spatial round trip, relative paths, checksums and immutable retries. No research release created.")
})
