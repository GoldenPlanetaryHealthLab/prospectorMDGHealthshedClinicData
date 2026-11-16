# Synthetic fixtures and graph declarations only; never calls tar_make().
library(dplyr)
library(tidyr)
library(magrittr)
library(sf)
library(duckplyr)

local({
  scratch <- tempfile("clinic-metadata-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  env <- new.env(parent = environment())
  assignments <- list()
  for (page in c("23_page-construct-healthsheds", "24_page-locate-clinics", "25_page-combine-clinic-metadata")) {
    notebook <- file.path("notebooks/pipeline", paste0(page, ".qmd"))
    parsed <- parsermd::parse_qmd(notebook)
    for (node in parsed@nodes) if (inherits(node,"rmd_chunk") && identical(as.character(node@engine),"r")) parse(text=node@code)
    path <- file.path(scratch,paste0(page,".R"))
    knitr::purl(notebook,output=path,documentation=0L,quiet=TRUE)
    for (expr in parse(path)) {
      if (!is.call(expr) || !identical(expr[[1]],as.name("<-"))) next
      name <- as.character(expr[[2]])
      assignments[[name]] <- expr[[3]]
      if (is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name("function"))) eval(expr,env)
    }
  }
  run <- function(name) {
    message("Checking ",name)
    result <- eval(assignments[[name]],env)
    assign(name,result,env)
    result
  }
  square <- function(x=0) st_polygon(list(matrix(c(x,0,x+2,0,x+2,2,x,2,x,0),ncol=2,byrow=TRUE)))
  geom <- st_sfc(square(),crs=3857)
  empty <- st_sfc(st_geometrycollection(),crs=3857)
  tracker <- tibble(clinic_ID=c("a","b","c","d","e","z"),
    y2019_m12=c(1L,2L,0L,0L,0L,0L),y2020_m01=c(0L,0L,3L,0L,0L,0L),y2022_m01=c(4L,0L,0L,5L,6L,0L))
  ids <- tibble(clinic_ID=c("a","a","b","c","e"),cAnnee=c(2019,2022,2019,2020,2022),
    cPeriode=c(12,1,12,1,1),fname=c(1L,4L,2L,3L,6L),cCodeStru=c("0007","A2022","0090",NA,NA),
    cNom=c("Historical A","Latest A","Historical B","Historical C","Historical E"),
    type_map=c("FSP_","FSP_","CSB1","FSP_",NA),ID_checked=c(FALSE,FALSE,FALSE,TRUE,NA))
  sect <- st_sf(region_uid="r",region_name="Region",district_uid="d",district_name="District",
    fs_uid=c("a","b"),fs_type=c("CSB2",NA),fs_name=c("Sector A",NA),f_or_nf=c("F",NA),geometry=rep(geom,2))
  sheds <- st_sf(fs_uid=c("a","b","c","e"),geometry=c(geom,empty,st_sfc(square(10),square(20),crs=3857)))
  locations <- tibble(fs_uid=letters[1:5],which_use=c("who2","unicef2","fkt_centroid","shed_centroid","none"))
  for (source in c("who","mdss","sect","gps","unicef")) {
    locations[[paste0(source,"_x")]]<-1:5
    locations[[paste0(source,"_y")]]<-6:10
  }
  locations$fkt_geometry<-st_sfc(lapply(0:4,square),crs=3857)
  pairs <- tibble(clinic_ID=c("b","d","orphan","e","c"),neighbor_to_merge=c("a",NA,"a","","c"))
  env$s7_clinic_reporting_track_df<-tracker
  env$s2_clinic_ids_df<-ids
  env$s5_sectorization_2022_clean_sf<-sect
  env$s5_healthshed_clinic_final_sf<-sheds
  env$s6_clinic_locations_selected_sf<-locations
  env$s5_spatial_runtime_list<-list(s2=sf_use_s2())
  env$s7_clinic_neighbor_pairings_df<-pairs
  policy <- run("s7_clinic_coordinate_policy_list")
  long <- run("s7_clinic_reporting_long_df")
  stopifnot(nrow(long)==18L,sum(long$fname==0)==12L,
    identical(unique(long$year_month),names(tracker)[-1]))
  reporting <- run("s7_clinic_reporting_metadata_df")
  stopifnot(nrow(reporting)==18L,reporting$clinic_database_code[reporting$clinic_ID=="a" & reporting$year==2019]=="0007",
    reporting$clinic_database_code[reporting$clinic_ID=="c" & reporting$year==2020]=="c",
    all(is.na(reporting$clinic_database_code[reporting$fname==0])))
  run("s7_clinic_reporting_summary_df")
  run("s7_clinic_sectorization_attributes_df")
  run("s7_clinic_latest_historical_attributes_df")
  attributes <- run("s7_clinic_metadata_attributes_df")
  stopifnot(!"z" %in% attributes$clinic_ID,attributes$n_month[attributes$clinic_ID=="a"]==2L,
    attributes$fs_name[attributes$clinic_ID=="a"]=="Sector A",
    attributes$fs_name_origin[attributes$clinic_ID=="a"]=="sectorization_2022",
    attributes$fs_type[attributes$clinic_ID=="b"]=="CSB1",attributes$pub_priv[attributes$clinic_ID=="b"]=="private",
    attributes$fs_type[attributes$clinic_ID=="c"]=="FSP",attributes$pub_priv[attributes$clinic_ID=="c"]=="public")
  run("s7_clinic_selected_coordinates_df")
  run("s7_clinic_metadata_located_df")
  located <- run("s7_clinic_metadata_centroid_filled_df")
  stopifnot(located$coordinate_status[located$clinic_ID=="b"]=="legacy_unicef_missing",
    is.na(located$clinic_x[located$clinic_ID=="d"]))
  classified <- run("s7_clinic_metadata_classified_df")
  stopifnot(all(classified$pub_priv[classified$clinic_ID %in% c("d","e")]=="private"),
    all(classified$pub_priv_origin[classified$clinic_ID %in% c("d","e")]=="recent_report_absent_sectorization_heuristic"))
  spatial <- run("s7_clinic_spatial_metadata_df")
  stopifnot(nrow(spatial)==5L,!any(vapply(spatial,inherits,logical(1),what="sfc")),
    spatial$gps_source[spatial$clinic_ID=="a"]=="who",spatial$gps_source_raw[spatial$clinic_ID=="a"]=="who2",
    spatial$neighbor_to_merge[spatial$clinic_ID=="b"]=="a",!spatial$has_healthshed[spatial$clinic_ID=="d"])

  # Execute only actual legacy transformations on the same synthetic inputs.
  # Skip every top-level source read, working-directory change, and writer.
  legacy <- new.env(parent=environment())
  legacy$clinic_track<-tracker;legacy$clinicID<-ids;legacy$sect2022<-st_drop_geometry(sect)
  legacy$healthsheds<-sheds;legacy$clinic_LL<-locations;legacy$merge_clinics<-pairs
  source <- parse("sandbox/madagascar_clinic_data_cleaning/scripts/7_combine_clinic_metadata.R")
  for (expr in source) {
    if (!is.call(expr)) next
    operation <- as.character(expr[[1]])
    if (operation == "<-" && is.symbol(expr[[2]]) &&
        as.character(expr[[2]]) %in% c("unique_nonNA","clinic_time","clinic_cross")) {
      eval(expr,legacy)
    } else {
      # Assignment pipes can be nested inside the outermost ordinary pipe.
      left <- expr
      assigns <- FALSE
      while (is.call(left) && as.character(left[[1]]) %in% c("%>%","%<>%")) {
        assigns <- assigns || identical(left[[1]],as.name("%<>%"))
        left <- left[[2]]
      }
      if (assigns && is.symbol(left) && as.character(left) %in% c("clinic_time","clinic_cross")) eval(expr,legacy)
    }
  }
  stopifnot(isTRUE(all.equal(as.data.frame(select(reporting,all_of(names(legacy$clinic_time)))),as.data.frame(legacy$clinic_time))))
  reference <- legacy$clinic_cross %>% select(-healthshed_geometry)
  # Joins may return different row orders; this fixture has unique clinic keys.
  stopifnot(!anyDuplicated(spatial$clinic_ID),!anyDuplicated(reference$clinic_ID))
  comparison <- all.equal(as.data.frame(arrange(select(spatial,all_of(names(reference))),clinic_ID)),
    as.data.frame(arrange(reference,clinic_ID)))
  if (!isTRUE(comparison)) stop("Legacy spatial reference: ",paste(comparison,collapse="; "))

  # Every extraction label, including both UNICEF forms, preserves policy.
  labels <- c("who","who2","mdss","mdss2","sect","sect2","gps","gps2",
    "unicef","unicef2","fkt_centroid","shed_centroid","none",NA)
  choices <- locations[rep(1,length(labels)),];choices$which_use<-labels
  xy <- env$s7_clinic_selected_coordinates_fn(choices,policy)
  stopifnot(all(xy$clinic_x[1:8]==1),all(xy$clinic_y[1:8]==6),
    all(is.na(xy$clinic_x[9:10])),xy$clinic_x[[11]]==1,
    all(is.na(xy$clinic_x[12:14])),identical(xy$gps_source_raw,labels))
  centroids <- env$s7_clinic_centroid_xy_fn(c(empty,geom,empty))
  stopifnot(nrow(centroids)==3L,all(is.na(centroids[c(1,3),])),all(centroids[2,]==c(1,1)))
  saved_located <- env$s7_clinic_metadata_located_df
  env$s7_clinic_metadata_located_df$which_use[env$s7_clinic_metadata_located_df$clinic_ID=="e"]<-"shed_centroid"
  filled <- run("s7_clinic_metadata_centroid_filled_df")
  stopifnot(filled$clinic_x[filled$clinic_ID=="e"]==21,filled$clinic_y[filled$clinic_ID=="e"]==1,
    filled$coordinate_status[filled$clinic_ID=="e"]=="available_pair")
  env$s7_clinic_metadata_located_df<-saved_located
  env$s7_clinic_metadata_centroid_filled_df<-located
  env$s7_clinic_metadata_centroid_filled_df$last_report[env$s7_clinic_metadata_centroid_filled_df$clinic_ID=="d"]<-"y2019_m12"
  older_classification <- run("s7_clinic_metadata_classified_df")
  stopifnot(is.na(older_classification$pub_priv[older_classification$clinic_ID=="d"]))
  env$s7_clinic_metadata_centroid_filled_df<-located;env$s7_clinic_metadata_classified_df<-classified
  older <- tracker;older$y2019_m12[[2]]<-9L
  env$s7_clinic_reporting_long_df<-env$s7_clinic_reporting_long_fn(older)
  unmatched <- run("s7_clinic_reporting_metadata_df")
  stopifnot(is.na(unmatched$clinic_database_code[unmatched$clinic_ID=="b" & unmatched$year==2019]))
  env$s7_clinic_reporting_long_df<-long;env$s7_clinic_reporting_metadata_df<-reporting

  # Distinct tied descriptions and code-join expansions remain auditable.
  env$s2_clinic_ids_df<-bind_rows(ids,mutate(filter(ids,clinic_ID=="b"),cNom="Tied B",cCodeStru="0091"))
  expanded <- run("s7_clinic_reporting_metadata_df")
  run("s7_clinic_latest_historical_attributes_df")
  tied <- run("s7_clinic_historical_attribute_audit_df")
  repeated <- run("s7_clinic_reporting_key_audit_df")
  stopifnot(nrow(expanded)==19L,tied$latest_descriptions[tied$clinic_ID=="b"]==2L,
    repeated$rows[repeated$clinic_ID=="b"]==2L,repeated$codes[repeated$clinic_ID=="b"]==2L)
  calendar <- run("s7_clinic_reporting_calendar_audit_df")
  stopifnot(all(calendar$valid_calendar))
  env$s7_clinic_reporting_long_df<-bind_rows(long,tibble(clinic_ID="bad",year_month="y2020_m13",year=2020,month=13,fname=0L))
  invalid_calendar <- run("s7_clinic_reporting_calendar_audit_df")
  stopifnot(!invalid_calendar$valid_calendar[invalid_calendar$year_month=="y2020_m13"])
  env$s5_sectorization_2022_clean_sf<-bind_rows(sect,mutate(sect[1,],fs_name="Conflicting A"))
  conflicting <- run("s7_clinic_sectorization_attribute_audit_df")
  stopifnot(nrow(conflicting)==1L,conflicting$fs_name_n_values==2L)
  pairing_audit <- run("s7_clinic_neighbor_audit_df")
  stopifnot(!pairing_audit$source_in_metadata[pairing_audit$clinic_ID=="orphan"],
    pairing_audit$neighbor_blank[pairing_audit$clinic_ID=="e"],pairing_audit$self_pair[pairing_audit$clinic_ID=="c"])
  env$s7_clinic_neighbor_pairings_df<-bind_rows(pairs,filter(pairs,clinic_ID=="b"))
  run("s7_clinic_spatial_metadata_df")
  stopifnot(run("s7_clinic_spatial_key_audit_df")$rows==2L)

  # Exact typed Parquet round trips, and reject accidentally retained geometry.
  for (name in c("reporting","spatial")) {
    table <- get(name)
    path <- env$s7_clinic_metadata_write_fn(table,file.path(scratch,paste0(name,".parquet")))
    actual <- duckplyr::read_parquet_duckdb(path,prudence="stingy") %>% collect()
    stopifnot(isTRUE(all.equal(as.data.frame(actual),as.data.frame(table))))
  }
  stopifnot(inherits(try(env$s7_clinic_metadata_write_fn(located,file.path(scratch,"invalid.parquet")),silent=TRUE),"try-error"))
  # Read the real pairing command from a tiny archive containing only fixtures.
  raw_path <- file.path(scratch,"pairings.csv")
  writeLines(c("fs_uid,clinic uid to be paired with","0001,0002"),raw_path)
  archive <- file.path(scratch,"fixture.zip")
  stopifnot(system2("python3",c("-c",shQuote("import sys,zipfile; z=zipfile.ZipFile(sys.argv[1],'w'); z.write(sys.argv[2],'data/pairings.csv'); z.close()"),shQuote(archive),shQuote(raw_path)))==0L)
  env$s2_clinic_months_raw_zip<-archive;env$s7_clinic_neighbor_member_chr<-"data/pairings.csv"
  # The archive reader is the page-21 helper; no scientific inputs are read.
  path <- file.path(scratch,"archive-helpers.R")
  knitr::purl("notebooks/pipeline/21_page-construct-diagnosis-dictionary.qmd",output=path,documentation=0L,quiet=TRUE)
  for (expr in parse(path)) if (is.call(expr) && identical(expr[[1]],as.name("<-")) &&
    identical(expr[[2]],as.name("s3_dictionary_archive_read_fn"))) eval(expr,env)
  paired <- run("s7_clinic_neighbor_pairings_df")
  stopifnot(paired$clinic_ID=="0001",paired$neighbor_to_merge=="0002")
  # Constructing graph declarations never reads or builds scientific data.
  declared <- tarchetypes::tar_tangle("notebooks/pipeline/25_page-combine-clinic-metadata.qmd")
  names <- vapply(declared,function(x) x$name,character(1))
  graph <- targets::tar_manifest(fields=c(name,format))
  stopifnot(all(names %in% graph$name),!anyDuplicated(graph$name),
    all(graph$format[grepl("^s7_.*_parquet$",graph$name)]=="file"),
    all(graph$format[grepl("^s7_.*_df$",graph$name)]=="rds"))
  targets::tar_validate()
  message("S7 synthetic checks passed: legacy reference parity, monthly codes, attribute precedence, coordinate policy, centroids, ownership, ambiguity audits, reviewed pairings, Parquet, and graph declarations.")
})
