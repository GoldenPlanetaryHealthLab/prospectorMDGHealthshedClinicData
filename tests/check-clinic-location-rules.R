# Synthetic fixtures and graph declarations only. Never calls tar_make().
library(dplyr)
library(tidyr)
library(magrittr)
library(sf)
library(duckplyr)

local({
  scratch <- tempfile("clinic-location-rules-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  env <- new.env(parent = environment())
  assignments <- list()
  for (page in c("21_page-construct-diagnosis-dictionary", "24_page-locate-clinics")) {
    notebook <- file.path("notebooks/pipeline", paste0(page, ".qmd"))
    parsed <- parsermd::parse_qmd(notebook)
    for (node in parsed@nodes) if (inherits(node, "rmd_chunk") && identical(as.character(node@engine), "r")) parse(text = node@code)
    path <- file.path(scratch, paste0(page, ".R"))
    knitr::purl(notebook, output = path, documentation = 0L, quiet = TRUE)
    for (expr in parse(path)) {
      if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
      name <- as.character(expr[[2]])
      assignments[[name]] <- expr[[3]]
      if (is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, env)
    }
  }
  run <- function(name) {
    message("Checking ", name)
    result <- eval(assignments[[name]], env)
    assign(name, result, env)
    result
  }
  square <- function(x = 0) st_polygon(list(matrix(c(x,0,x+2,0,x+2,2,x,2,x,0), ncol=2, byrow=TRUE)))
  empty <- st_sfc(st_geometrycollection(), crs=4326)
  nonempty <- st_sfc(square(), crs=4326)
  stopifnot(identical(env$s6_clinic_location_unique_nonNA_fn(c(NA,NA)), c(NA,NA)[1]),
    setequal(env$s6_clinic_location_unique_nonNA_fn(c("a",NA,"b","a")),c("a","b")))

  # Only the MDDSS zero/zero pair is excluded. The raw table remains unchanged.
  env$s6_clinic_location_raw_sources_list <- list(mdss=data.frame(
    FacilityID=c("0001","0002","0003","0004"), Latitude=c("0","0","1",NA), Longitude=c("0","1","0","0")))
  mdss <- run("s6_clinic_location_mdss_source_df")
  stopifnot(identical(mdss$FacilityID,c("0002","0003")),
    env$s6_clinic_location_raw_sources_list$mdss$FacilityID[[1]]=="0001")

  # Sectorization keeps the reported WKT alongside parsed longitude/latitude.
  sect <- st_sf(region_uid="r",region_name="Region",district_uid="d",district_name="District",
    commune_uid="c",commune_name="Commune",fs_uid=c("a","b"),fs_type="CSB2",f_or_nf="F",
    fs_ll=c("POINT (47 -18)","POINT ( )"),fs_name=c("CSB2 Alpha","CSB2 Beta"),geometry=rep(nonempty,2))
  env$s5_sectorization_2022_clean_sf <- sect
  sectorization <- run("s6_clinic_location_sectorization_df")
  stopifnot(sectorization$sect_x[[1]]==47,sectorization$sect_y[[1]]== -18,
    is.na(sectorization$sect_x[[2]]),sectorization$fs_ll_raw[[1]]==sect$fs_ll[[1]])

  # Historical code ambiguity preserves equal name minima and unlinked codes.
  env$s2_clinic_ids_df <- data.frame(clinic_ID=c("a","a2","b"),cCodeStru=c("001","001","002"),cNom=c("Alphá","Alpha","Beta"))
  env$s6_clinic_location_mdss_source_df <- data.frame(FacilityID=c("001","002","999"),
    GeSISNom=c("Alpha","Beta","Unknown"),Longitude=1,Latitude=1)
  identity <- run("s6_clinic_location_mdss_identity_candidates_df")
  stopifnot(nrow(identity)==4L,all(identity$name_dist[identity$FacilityID=="001"]==0),
    is.na(identity$clinic_ID[identity$FacilityID=="999"]))
  env$s6_clinic_location_sectorization_df <- data.frame(fs_uid=c("a","a2","b","none"),fs_name=c("Alpha","Alpha","Beta","No source"))
  mdss_joined <- run("s6_clinic_location_mdss_joined_df")
  stopifnot(nrow(mdss_joined)==4L,all(c("a","a2") %in% mdss_joined$fs_uid),
    is.na(mdss_joined$mdss_id[mdss_joined$fs_uid=="none"]))

  # Test the exact legacy WHO transformation on reviewed and ambiguous rows.
  who <- data.frame(Facility_UID=c("","","#N/A","#N/A","p8ldnmLbC0v",rep("WmBYR1JepA7",2),"duplicate","duplicate","normal"),
    Nom_complet_FS=c("SOATANANA","MANGATANY","SERANAMBE","UNKNOWN","Removed","CSB2 AMBODIMANGAVALO","Wrong",rep("Duplicate",2),"Normal"),
    Nom_Fokontany_par_CSB=c(rep("FKT",8),"Other","FKT"),longitude=c("47 -",rep("47",9)),latitude=18)
  env$s6_clinic_location_other_sources_list <- list(who=who)
  clean <- run("s6_clinic_location_who_clean_df")
  stopifnot(clean$who_y[clean$fs_uid=="VCf9Tra2OM4"]== -18,
    clean$who_x[clean$fs_uid=="VCf9Tra2OM4"]==47,
    all(c("GqEjMC845Qj","YQ9XN4vE4T6") %in% clean$fs_uid),
    !any(clean$fs_uid %in% c("#N/A","p8ldnmLbC0v")),
    sum(clean$fs_uid=="WmBYR1JepA7")==1L,is.na(clean$who_fkt[clean$fs_uid=="duplicate"]))
  legacy <- new.env(parent=environment())
  source <- readLines("sandbox/madagascar_clinic_data_cleaning/scripts/6_locate_clinics.R")
  begin <- which(trimws(source)=="who_ll %<>%")[[1]]
  end <- which(seq_along(source)>begin & trimws(source)=="clinic_ll %<>%")[[1]] - 1L
  legacy$who_ll <- who
  eval(parse(text=source[begin:end]),legacy)
  stopifnot(isTRUE(all.equal(clean,legacy$who_ll)))
  full <- run("s6_clinic_location_who_joined_df")
  stopifnot("normal" %in% full$fs_uid)

  # Bracket pairs and two independent GPS exclusions; ties survive the join.
  env$s6_clinic_location_other_sources_list$unicef <- data.frame(Name="UNICEF only",UID="u",Parent.UID="p",Coordinates="[47.1,-18.2]")
  unicef <- run("s6_clinic_location_unicef_joined_df")
  stopifnot(unicef$unicef_x[unicef$fs_uid=="u"]=="47.1",unicef$unicef_y[unicef$fs_uid=="u"]=="-18.2",
    unicef$Coordinates_raw[unicef$fs_uid=="u"]=="[47.1,-18.2]")
  env$s6_clinic_location_other_sources_list$gps <- data.frame(clinic_cod=c("001","110443043","002","999"),
    clinic_nam=c("Alpha","Alpha","AMBALABONGO","Unknown"),Longitude=1,Latitude=1)
  gps <- run("s6_clinic_location_gps_identity_candidates_df")
  stopifnot(nrow(gps)==3L,!any(gps$clinic_cod %in% c("002","110443043")))
  joined <- run("s6_clinic_location_gps_joined_df")
  stopifnot(all(c("a","a2","u") %in% joined$fs_uid))

  # Unique FKT, equal minima, missing distance, containment and comma units.
  cells <- st_sf(fs_uid=c("single",rep("tie",2),rep("missing",2),rep("contain",2)),
    fs_name=c("CSB2 Other",rep("CSB2 Alpha",2),rep("CSB2 Alpha",2),rep("CSB2 Alpha Centre",2)),
    fkt_uid=paste0("f",1:7),fkt_name=c("Different",rep("Alpha",5),"Beta"),fkt_instat_clean=as.character(1:7),
    distance_csb_fkt=c(NA,"0 km","0 km",NA,"1","1,5Km","2"),geometry=rep(nonempty,7))
  ranked <- env$s6_clinic_location_fkt_candidates_fn(cells)
  stopifnot(ranked$n_match[ranked$fs_uid=="single"]==1L,
    all(ranked$n_match[ranked$fs_uid=="tie"]==2L),
    all(is.na(ranked$n_match[ranked$fs_uid=="missing"])),
    ranked$match_name_dist[ranked$fs_uid=="contain"][[1]],ranked$dist[[6]]==1.5)

  # A missing clinic name is also a missing reverse-regex pattern. Keep NA
  # evidence, keep a single FKT eligible, and do not pick from ambiguous groups.
  unnamed <- cells[c(1,2,3,4,5,6,7),]
  unnamed$fs_uid <- c("unnamed_single",rep("unnamed_multiple",2),
    rep("partly_unnamed",2),"missing_fkt_name","blank_fkt_name")
  unnamed$fs_name <- c(rep(NA_character_,3),"CSB2 Alpha",NA,"CSB2 Alpha","CSB2 Alpha")
  unnamed$fkt_name <- c(rep("Alpha",5),NA,"")
  unnamed$distance_csb_fkt <- "0 km"
  unknown <- env$s6_clinic_location_fkt_candidates_fn(unnamed)
  stopifnot(nrow(unknown)==5L,
    all(is.na(unknown$name_contain[is.na(unknown$fs_name)])),
    unknown$n_match[unknown$fs_uid=="unnamed_single"]==1L,
    unknown$match_name_dist[unknown$fs_uid=="unnamed_single"],
    all(is.na(unknown$n_match[unknown$fs_uid %in% c("unnamed_multiple","partly_unnamed")])))
  # Complete-name matches retain the original stringr regex results.
  stopifnot(identical(ranked$name_contain,
    stringr::str_detect(tolower(ranked$name_noPre),tolower(ranked$fkt_name)) |
      stringr::str_detect(tolower(ranked$fkt_name),tolower(ranked$name_noPre))))
  env$s6_clinic_location_fkt_candidates_sf <- ranked
  matched <- run("s6_clinic_location_fkt_matched_df")
  stopifnot(all(c("single","contain") %in% matched$fs_uid),!any(c("tie","missing") %in% matched$fs_uid))

  # Sparse row pairing equals the reference dense diagonal, including crossing
  # matches to other rows, missing points, empty polygons, overlaps, boundaries.
  points <- st_sfc(st_point(c(0,1)),st_point(c(5,1)),st_point(),st_point(c(1,1)),crs=4326)
  polygons <- c(nonempty,nonempty,nonempty,empty)
  expected <- diag(st_intersects(points,polygons,sparse=FALSE))*ifelse(st_is_empty(points),NA,1)
  stopifnot(identical(env$s6_clinic_location_intersects_fn(points,polygons),expected))
  coordinates <- data.frame(fs_uid=c("a","b","c"))
  for (s in c("sect","mdss","who","unicef","gps")) {
    coordinates[[paste0(s,"_x")]] <- c("1",NA,"bad")
    coordinates[[paste0(s,"_y")]] <- c("1",NA,"2")
  }
  coordinates$fkt_geometry <- rep(nonempty,3)
  converted <- suppressWarnings(env$s6_clinic_location_points_fn(coordinates))
  stopifnot(st_crs(converted)==st_crs(nonempty),converted$sect_x[[3]]=="bad",
    identical(st_is_empty(converted$sect_ll),c(FALSE,TRUE,TRUE)))
  env$s5_spatial_runtime_list <- list(s2=sf_use_s2())
  env$s6_clinic_location_fkt_matched_df <- coordinates
  converted <- suppressWarnings(run("s6_clinic_location_points_sf"))
  env$s5_healthshed_clinic_final_sf <- st_sf(fs_uid=c("a","b","c"),geometry=rep(nonempty,3))
  evidence <- run("s6_clinic_location_containment_sf")
  stopifnot(evidence$who_ll_fktContains[[1]]==1,is.na(evidence$who_ll_fktContains[[2]]),
    identical(st_is_empty(evidence$who_ll),c(FALSE,TRUE,TRUE)))

  # All reachable labels and competing sources. GPS wins before WHO/shed;
  # gps2 is unreachable because the earlier GPS predicate is identical.
  labels <- c("who","sect","unicef","mdss","gps","who2","sect2","unicef2","mdss2","fkt_centroid","shed_centroid","none")
  choice <- data.frame(fs_uid=labels)
  for (s in c("sect","mdss","who","unicef","gps")) for (p in c("fkt","shed"))
    choice[[paste0(s,"_ll_",p,"Contains")]] <- rep(NA_real_,length(labels))
  for (i in 1:9) {
    s <- sub("2$","",labels[[i]])
    p <- if (i<=4) "fkt" else "shed"
    choice[[paste0(s,"_ll_",p,"Contains")]][[i]] <- 1
  }
  choice$fkt_geometry <- c(rep(empty,9),nonempty,empty,empty)
  choice$shed_geometry <- c(rep(empty,10),nonempty,empty)
  choice$sect_ll_fktContains[[1]]<-1
  choice$who_ll_shedContains[[5]]<-1
  stopifnot(identical(env$s6_clinic_location_select_fn(choice)$which_use,labels))
  env$s6_clinic_location_containment_sf <- evidence
  selected <- run("s6_clinic_locations_selected_sf")
  audit <- run("s6_clinic_location_parse_audit_df")
  stopifnot(nrow(audit)==15L,sum(audit$invalid_x)==5L,all(audit$missing_pair[audit$fs_uid=="c"]))
  summary <- run("s6_clinic_location_intersection_audit_df")
  stopifnot(nrow(summary)==10L,all(summary$n==1L),all(summary$success==1L),all(summary$fraction==1))

  # A synthetic CSV verifies leading-zero IDs and exact coordinate text.
  path <- file.path(scratch,"raw.csv")
  writeLines(c("FacilityID,Longitude,Latitude","0001,47.0000,18"),path)
  # Use the real extraction callback with an archive created from only fixtures.
  archive <- file.path(scratch,"fixture.zip")
  stopifnot(system2("python3",c("-c",shQuote("import sys,zipfile; z=zipfile.ZipFile(sys.argv[1],'w'); z.write(sys.argv[2],'data/raw.csv'); z.close()"),shQuote(archive),shQuote(path)))==0L)
  raw <- env$s6_clinic_location_read_fn(archive,"data/raw.csv")
  stopifnot(raw$FacilityID=="0001",raw$Longitude=="47.0000")
  declared <- tarchetypes::tar_tangle("notebooks/pipeline/24_page-locate-clinics.qmd")
  names <- vapply(declared,function(x) x$name,character(1))
  stopifnot(!anyDuplicated(names),"s6_clinic_locations_selected_sf" %in% names)
  # Full graph checks construct declarations only, and do not read/build data.
  graph <- targets::tar_manifest(fields=c(name,format,command))
  stopifnot(all(names %in% graph$name),!anyDuplicated(graph$name),
    all(graph$format[match(names,graph$name)]=="rds"))
  targets::tar_validate()
  message("S6 synthetic checks passed: source parsing, WHO reference parity, historical identities, FKT ties, sparse intersections, source precedence, audits, and graph declarations.")
})
