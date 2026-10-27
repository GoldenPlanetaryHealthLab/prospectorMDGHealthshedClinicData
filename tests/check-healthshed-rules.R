# Synthetic fixtures only; no staged research inputs or tar_make().
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(magrittr)
library(sf)
library(igraph)

local({
  scratch <- tempfile("healthshed-rules-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  env <- new.env(parent = environment())
  assignments <- list()
  for (page in c("21_page-construct-diagnosis-dictionary", "23_page-construct-healthsheds")) {
    notebook <- file.path("notebooks/pipeline", paste0(page, ".qmd"))
    parsed <- parsermd::parse_qmd(notebook)
    for (node in parsed@nodes) if (inherits(node,"rmd_chunk") && identical(as.character(node@engine),"r")) parse(text=node@code)
    path <- file.path(scratch, paste0(page,".R"))
    knitr::purl(notebook, output=path, documentation=0L, quiet=TRUE)
    for (expr in parse(path)) {
      if (!is.call(expr) || !identical(expr[[1]], as.name("<-"))) next
      name <- as.character(expr[[2]])
      assignments[[name]] <- expr[[3]]
      if (is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) eval(expr, env)
    }
  }
  run <- function(name) {
    message("Checking ", name)
    value <- eval(assignments[[name]], env)
    assign(name, value, env)
    value
  }
  must_error <- function(expr) stopifnot(inherits(try(force(expr), silent=TRUE), "try-error"))
  square <- function(x, y=0, size=1) st_polygon(list(matrix(c(x,y,x+size,y,x+size,y+size,x,y+size,x,y), ncol=2, byrow=TRUE)))
  geometries <- st_sfc(square(0), square(1), square(4), square(1,1), crs=3857)
  adjacency <- env$s5_healthshed_adjacency_fn(geometries)
  stopifnot(adjacency[1,2], !adjacency[1,3], !adjacency[1,4], all(diag(adjacency)),
    env$s5_healthshed_components_fn(geometries[1:3])==2L,
    env$s5_healthshed_components_fn(geometries[c(1,4)])==2L,
    env$s5_healthshed_components_fn(geometries[integer()])==0L)
  hole <- st_as_sfc("POLYGON ((0 0,4 0,4 4,0 4,0 0),(1 1,1 3,3 3,3 1,1 1))", crs=3857)
  inside <- st_sfc(square(1.5,1.5,0.5),crs=3857)
  stopifnot(env$s5_healthshed_components_fn(c(hole,inside))==2L)

  # Compare the real legacy aggregation function, not a second implementation.
  legacy <- new.env(parent=environment())
  old <- parse("sandbox/madagascar_clinic_data_cleaning/scripts/5_clean_sect2022_construct_healthsheds.R")
  helper <- Filter(function(x) is.call(x) && identical(x[[1]],as.name("<-")) &&
    identical(x[[2]],as.name("sect_to_shed")),as.list(old))[[1]]
  eval(helper,legacy)
  legacy$empty_geom <- st_sfc(st_geometrycollection(),crs=3857)
  cells <- st_sf(region_uid="r",region_name="Region",district_uid="d",district_name="District",
    fs_uid=c(rep("a",4),"b","c"),fs_type="CSB2",fs_name=c(rep("A",4),"B","C"),fs_ll="POINT EMPTY",
    fkt_uid=c("u1","u1","u2","u3","u1","u4"),fkt_name=c("one","one","two","three","one","none"),
    fkt_instat_clean=c("1","1","2","3","1",NA),fkt_pop_2022=c(10,10,20,30,10,5),
    n_fkt_instat=c(3,3,3,3,1,0),geometry=c(geometries[c(1,1,2,3,1)],st_sfc(st_geometrycollection(),crs=3857)))
  actual <- env$s5_healthshed_aggregate_fn(cells)
  expected <- legacy$sect_to_shed(cells)
  message("Checking reference aggregation")
  nonempty <- which(!st_is_empty(actual))
  stopifnot(isTRUE(all.equal(st_drop_geometry(actual),st_drop_geometry(expected))),
    identical(st_is_empty(actual),st_is_empty(expected)),
    all(st_equals(actual,expected,sparse=FALSE)[cbind(nonempty,nonempty)]),
    actual$fs_pop[actual$fs_uid=="a"]==60,
    actual$fs_pop[actual$fs_uid=="b"]==10,
    actual$n_components[actual$fs_uid=="a"]==2,
    st_is_empty(actual[actual$fs_uid=="c",]),st_crs(actual)==st_crs(cells))

  # The duplicate review merges populations without deleting approved sharing.
  env$s5_sectorization_2022_source_df <- data.frame(fkt_uid=c("x","y","z","shared","shared"),
    fkt_instat=c("0001","0001","0002","0003","0003"),fkt_pop_2022=c(4,6,9,2,2),fs_uid=c("a","a","a","a","b"))
  env$s5_sectorization_duplicate_reviews_df <- data.frame(fkt_uid=c("x","y"),fkt_instat="0001",NOTE=c("keep that row","delete"))
  population <- run("s5_sectorization_population_reconciled_df")
  stopifnot(nrow(population)==4L,population$fkt_pop_2022[population$fkt_uid=="x"]==10,
    sum(population$fkt_uid=="shared")==2,sum(population$fkt_pop_2022)==sum(env$s5_sectorization_2022_source_df$fkt_pop_2022))

  # Grouped fuzzy matching retains ties and does not compare across groups.
  first <- data.frame(group=c("a","b","unshared"),name=c("Ambato","Ambato","Ambato"))
  second <- data.frame(group=c("a","a","b","other"),label=c("Ambato","Ambato","Different","Ambato"))
  joined <- env$s5_grouped_stringdist_fulljoin_fn(first,second,"group","group","name","label",
    select_match=function(x) filter(group_by(x,df1_id),dist==min(dist)))
  stopifnot(sum(joined$group=="a")==2L,!any(joined$group %in% c("unshared","other")),
    all(joined$dist[joined$group=="a"]==0),all(joined$df1_id[joined$group=="a"]==1L))
  empty_join <- env$s5_grouped_stringdist_fulljoin_fn(first[first$group=="unshared",],second,
    "group","group","name","label")
  stopifnot(nrow(empty_join)==0L)

  # Approved matching preserves original precedence and one-to-many rows.
  env$s5_sectorization_population_reconciled_df <- data.frame(fkt_uid=c("original","fuzzy","manual"),
    fkt_instat=c("100",NA,NA),geometry="POINT EMPTY",fs_uid="a",fkt_name=c("one","two","three"))
  env$s5_fkt_code_lookup_initial_df <- data.frame(ADM4C_BNGRC=as.character(100:104))
  env$s5_sectorization_fkt_match_reviews_list <- list(
    match_check=data.frame(fkt_uid=c("original","fuzzy","fuzzy"),fkt_code_instat=c("101","101","102"),NOTE="yes"),
    manual_match=data.frame(fkt_uid="manual",fkt_code_instat="103"),
    manual2=data.frame(match_type="fkt",fkt_instat_code="104",match_uid="fuzzy",fkt_ocha_name="unused"),
    match_check_v2=data.frame(fkt_uid=character(),fkt_code_instat=character(),NOTE=character()),
    match_check_v3=data.frame(fkt_uid=character(),fkt_code_instat=character(),note=character()))
  matched <- run("s5_sectorization_fkt_matches_df")
  stopifnot(nrow(matched)==4L,matched$fkt_instat_clean[matched$fkt_uid=="original"]=="100",
    setequal(matched$fkt_instat_clean[matched$fkt_uid=="fuzzy"],c("101","102")),
    matched$fkt_instat_clean[matched$fkt_uid=="manual"]=="103")

  # Preserve the source's order-sensitive Nosy Mitsio split and lookup update.
  multi <- st_sfc(st_multipolygon(list(unclass(square(0,0,3)),unclass(square(10)),unclass(square(12)))),crs=3857)
  env$s5_fkt_shapes_valid_sf <- st_sf(ADM4_PCODE="MG71717150005",ADM4_EN="Mainland",geometry=multi)
  split <- run("s5_fkt_shapes_initial_sf")
  stopifnot(setequal(split$ADM4_PCODE,c("MG71717150005A","MG71717150005B")),
    as.numeric(st_area(split[split$ADM4_PCODE=="MG71717150005A",]))==9,
    as.numeric(st_area(split[split$ADM4_PCODE=="MG71717150005B",]))==2,
    isTRUE(st_equals(st_union(split),st_union(env$s5_fkt_shapes_valid_sf),sparse=FALSE)[1,1]))
  env$s5_fkt_code_lookup_source_df <- data.frame(ADM4_PCODE="MG71717150005",ADM4C_BNGRC="717150005",ADM4_EN="Mainland",ADM4E_BNGRC="MAINLAND")
  lookup <- run("s5_fkt_code_lookup_initial_df")
  stopifnot(setequal(lookup$ADM4C_BNGRC,c("717150005A","717150005B")),
    identical(lookup$ADM4_PCODE,split$ADM4_PCODE))

  # Empty or missing matches keep their report rows and the source CRS.
  assignment <- data.frame(fkt_instat_clean=c("717150005A",NA,"unknown"),fs_uid=c("a","b","c"))
  refreshed <- env$s5_healthshed_refresh_fn(assignment,lookup,split)
  stopifnot(nrow(refreshed)==3L,identical(st_is_empty(refreshed),c(FALSE,TRUE,TRUE)),st_crs(refreshed)==st_crs(split))
  fallback <- assignment; fallback$ADM4_PCODE<-"MG71717150005B"
  refreshed <- env$s5_healthshed_refresh_fn(fallback,lookup,split,fallback=TRUE)
  stopifnot(!any(st_is_empty(refreshed)),refreshed$ADM4_PCODE[[1]]=="MG71717150005A")

  # Line-contact contiguity filter, including the protected manual assignment.
  topology <- cells[c(1,3,4),]
  topology$match_type <- c("original","original","fuzzy1")
  topology$ADM4_PCODE <- c("p1","p2","p3")
  env$s5_sectorization_initial_geometry_sf <- topology
  topology <- run("s5_sectorization_initial_topology_sf")
  stopifnot(all(topology$drop_solo),identical(topology$n_adj_fkt,c(1,1,0)))
  env$s5_fkt_shapes_initial_sf <- st_sf(ADM4_PCODE=c("p1","p2","p3"),geometry=geometries[1:3])
  topology$ADM4_PCODE<-c("p1","p2","p3")
  env$s5_sectorization_initial_topology_sf <- topology
  filtered <- run("s5_sectorization_contiguity_filtered_sf")
  stopifnot(is.na(filtered$fkt_instat_clean[[3]]),st_is_empty(filtered[3,]))
  topology$match_type[[3]]<-"manual2";env$s5_sectorization_initial_topology_sf<-topology
  protected <- run("s5_sectorization_contiguity_filtered_sf")
  stopifnot(!is.na(protected$fkt_instat_clean[[3]]),!st_is_empty(protected[3,]))

  # Coastal rules: largest mainland; special code separates the smallest part.
  env$s5_fkt_shapes_initial_sf <- st_sf(ADM4_PCODE=c("p","MG71713253004"),ADM4_EN=c("ordinary","special"),geometry=c(multi,multi))
  env$s5_sectorization_island_assignments_df <- data.frame(ADM4_PCODE=c("p","MG71713253004"),fkt_instat=c("001","002"))
  coastal <- run("s5_fkt_shapes_final_sf")
  stopifnot(nrow(coastal)==4L,as.numeric(st_area(coastal[coastal$ADM4_PCODE=="p",]))==9,
    as.numeric(st_area(coastal[coastal$ADM4_PCODE=="MG71713253004",]))==9,
    sum(as.numeric(st_area(coastal)))==2*sum(as.numeric(st_area(multi))))

  # Temporary .by groups do not persist into the special-code max(). Keep
  # the global rank rule even when a replacement feature has more parts.
  four <- st_sfc(st_multipolygon(list(unclass(square(20,0,4)),unclass(square(30,0,3)),
    unclass(square(40,0,2)),unclass(square(50)))),crs=3857)
  special <- st_sfc(st_multipolygon(list(unclass(square(0,0,3)),unclass(square(10,0,2)),
    unclass(square(15)))),crs=3857)
  env$s5_fkt_shapes_initial_sf <- st_sf(ADM4_PCODE=c("p","MG71713253004"),
    ADM4_EN=c("ordinary","special"),geometry=c(four,special))
  unequal <- run("s5_fkt_shapes_final_sf")
  stopifnot(!"MG71713253004_A" %in% unequal$ADM4_PCODE,
    as.numeric(st_area(unequal[unequal$ADM4_PCODE=="MG71713253004",]))==14,
    sum(as.numeric(st_area(unequal)))==44)

  # Review diagnostics and nonspatial exports use the same declared helpers.
  reviews <- env$s5_healthshed_review_plots_fn(actual,cells)
  stopifnot(identical(names(reviews),"a"),inherits(reviews[[1]],"ggplot"))
  invisible(ggplot2::ggplotGrob(reviews[[1]]))
  # Attaching duckplyr in the real graph must not change population semantics.
  suppressPackageStartupMessages(library(duckplyr))
  with_duckplyr <- env$s5_healthshed_aggregate_fn(cells)
  stopifnot(isTRUE(all.equal(st_drop_geometry(with_duckplyr),st_drop_geometry(expected))))
  exported <- env$s5_healthshed_write_table_fn(st_drop_geometry(with_duckplyr),file.path(scratch,"sheds.parquet"))
  roundtrip <- duckplyr::read_parquet_duckdb(exported) %>% collect()
  stopifnot(isTRUE(all.equal(as.data.frame(roundtrip),as.data.frame(st_drop_geometry(with_duckplyr)))))

  # Commune additions are evaluated with their existing clinic healthshed.
  env$s5_sectorization_commune_candidates_sf <- st_sf(ADM4_PCODE=c("add_a","add_b"),fs_uid=c("a","b"),
    geometry=st_sfc(square(1),square(14),crs=3857))
  env$s5_healthshed_clinic_v2_sf <- st_sf(fs_uid=c("a","b"),n_components=1L,
    geometry=st_sfc(square(0),square(10),crs=3857))
  commune <- run("s5_sectorization_commune_topology_sf")
  stopifnot(all(commune$n_comp_new[commune$fs_uid=="a"]==1L),
    all(commune$n_comp_new[commune$fs_uid=="b"]==2L),sum(commune$type=="bad")==1L)

  # Final reassignments clear only the specified old clinic; a shared
  # assignment survives, and the replacement never invents population.
  final <- cells[c(1,5,3),]
  final$fs_uid<-c("a","b","c"); final$fs_name<-c("A","B","C")
  final$fkt_instat_clean<-c("111","111","222"); final$fkt_instat_orig<-final$fkt_instat_clean
  final$ADM4_PCODE<-c("p1","p1","p2"); final$match_type<-"original"
  final$distance_csb_fkt<-1; final$transit<-"walk"
  env$s5_sectorization_islands_fixed_sf<-final
  env$s5_fkt_code_lookup_final_df<-data.frame(ADM4C_BNGRC=c("111","222"),ADM4_PCODE=c("p1","p2"),ADM4_EN=c("One","Two"))
  env$s5_fkt_shapes_final_sf<-st_sf(ADM4_PCODE=c("p1","p2"),geometry=geometries[1:2])
  env$s5_sectorization_final_reassignment_reviews_df<-data.frame(ADM4_PCODE="p1",old_fs_uid="a",new_fs_uid="c",new_fs_name="C")
  unlinked<-run("s5_sectorization_final_unlinked_sf")
  stopifnot(is.na(unlinked$fkt_instat_clean[unlinked$fs_uid=="a"]),
    st_is_empty(unlinked[unlinked$fs_uid=="a",]),unlinked$fkt_instat_clean[unlinked$fs_uid=="b"]=="111")
  replacements<-run("s5_sectorization_final_reassignment_rows_df")
  stopifnot(nrow(replacements)==1L,is.na(replacements$fkt_pop_2022),replacements$fs_uid=="c")
  clean<-run("s5_sectorization_2022_clean_sf")
  stopifnot(nrow(clean)==4L,sum(clean$fkt_pop_2022,na.rm=TRUE)==sum(final$fkt_pop_2022),
    sum(clean$fkt_instat_clean=="111",na.rm=TRUE)==2L)

  # A real shapefile bundle in a synthetic archive tests extraction cleanup
  # and exact member selection, including misleading duplicate basenames.
  archive_root <- file.path(scratch,"archive")
  dir.create(file.path(archive_root,"data/raw/shapes"),recursive=TRUE)
  shape_path <- file.path(archive_root,"data/raw/shapes/source.shp")
  st_write(st_sf(id=1L,geometry=geometries[1]),shape_path,quiet=TRUE)
  csv <- file.path(archive_root,"data/raw/reviews.csv")
  writeLines(c("fkt_uid,fkt_instat,NOTE","u1,000123,keep"),csv)
  archive <- file.path(scratch,"fixture.zip")
  command <- "import sys,pathlib,zipfile; root=pathlib.Path(sys.argv[2]); z=zipfile.ZipFile(sys.argv[1],'w'); [z.write(p,p.relative_to(root)) for p in root.rglob('*') if p.is_file()]; z.close()"
  stopifnot(system2("python3",c("-c",shQuote(command),shQuote(archive),shQuote(archive_root)))==0L)
  members <- utils::unzip(archive,list=TRUE)$Name
  read_shapes <- env$s5_healthshed_shapes_read_fn(archive,members[grepl("shapes/",members)])
  stopifnot(nrow(read_shapes)==1L,st_crs(read_shapes)==st_crs(geometries))
  must_error(env$s5_healthshed_shapes_read_fn(archive,members[endsWith(members,".shp")]))
  read_review <- env$s5_healthshed_read_fn(archive,c(review="data/raw/reviews.csv"),"review")
  stopifnot(identical(read_review$fkt_instat,"000123"))

  graph <- targets::tar_manifest(fields=c(name,format,command))
  declared <- tarchetypes::tar_tangle("notebooks/pipeline/23_page-construct-healthsheds.qmd")
  names <- vapply(declared,function(x) x$name,character(1))
  stopifnot(all(names %in% graph$name),!anyDuplicated(graph$name),
    all(graph$format[grepl("^s5_.*_parquet$",graph$name)]=="file"),
    all(graph$format[grepl("^s5_.*_sf$",graph$name)]=="rds"),
    "s5_spatial_runtime_list" %in% graph$name)
  targets::tar_validate()
  message("S5 synthetic checks passed: reference aggregation, topology, population, grouped ties, splits, geometry refresh, archive readers, and graph contracts.")
})
