# Synthetic replication fixtures only; never run tar_make() or read staged data.
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(magrittr)
library(rlang)
library(duckplyr)
library(data.table)

local({
  scratch <- tempfile("check-clinic-diagnosis-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  env <- new.env(parent = environment())
  assignments <- list()
  for (page in c("21_page-construct-diagnosis-dictionary", "22_page-clean-clinic-diagnosis")) {
    notebook <- file.path("notebooks/pipeline", paste0(page, ".qmd"))
    parsed <- parsermd::parse_qmd(notebook)
    for (node in parsed@nodes) if (inherits(node,"rmd_chunk") && identical(as.character(node@engine),"r")) parse(text=node@code)
    tangled <- file.path(scratch, paste0(page,".R"))
    knitr::purl(notebook, output=tangled, documentation=0L, quiet=TRUE)
    for (expr in parse(tangled)) {
      if (!is.call(expr) || !identical(expr[[1]],as.name("<-"))) next
      assignments[[as.character(expr[[2]])]] <- expr[[3]]
      if (is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name("function"))) eval(expr,env)
    }
  }
  f <- function(name, ...) env[[paste0("s4a_diagnosis_",name,"_fn")]](...)
  write <- function(data, name) {
    path <- file.path(scratch,paste0(name,".parquet"))
    compute_parquet(as_duckdb_tibble(as.data.frame(data)), path)
    path
  }
  read <- function(path) read_parquet_duckdb(path,prudence="stingy") |> collect() |> as.data.frame()
  sql <- function(query) f("query",query) |> collect() |> as.data.frame()
  must_error <- function(expr) stopifnot(inherits(try(force(expr),silent=TRUE),"try-error"))
  same <- function(actual, expected, keys=names(expected)) {
    actual <- as.data.frame(actual)[,names(expected),drop=FALSE]
    expected <- as.data.frame(expected)
    # R infers logical for entirely absent columns; Parquet measurements use
    # an explicit DOUBLE schema. Compare their missing values as numeric.
    for (name in grep("^V[0-9]+$",names(expected),value=TRUE)) {
      if (is.logical(expected[[name]])) expected[[name]] <- as.double(expected[[name]])
    }
    canonical <- function(x) { x <- x[do.call(order,c(x[keys],list(na.last=TRUE))),,drop=FALSE]; rownames(x)<-NULL; x }
    a <- canonical(actual); b <- canonical(expected)
    if (!isTRUE(all.equal(a,b,check.attributes=FALSE))) {
      print(a); print(b); stop(paste(all.equal(a,b,check.attributes=FALSE),collapse="\n"))
    }
  }
  stopifnot(identical(f("drop_na_if",numeric()),NA_integer_),
    identical(f("drop_na_if",c(NA,1,2,1)),c(1,2)))
  # Expressions: explicit -1 differs from NA, both in direct terms and sums.
  ex <- function(terms,wave) sql(paste0("SELECT ", f("expression",terms,wave), " v FROM (SELECT -1::DOUBLE A,NULL::DOUBLE B)"))$v
  stopifnot(ex("A","wave1")==-1, is.na(ex(c("A","B"),"wave1")),
    ex(c("A","B"),"wave3A")==-1, is.na(ex(character(),"wave1")))
  must_error(f("rules", tibble(var="V1",filename="file1",wave1_colname="unknown",wave2_colname=NA_character_,
    wave3A_colname=NA_character_,wave3B_colname=NA_character_),tibble(w3_colname="X1"),tibble(Column.Name="A")))

  # Monthly data covers every decision, missing sentinel, no-positive reports,
  # inventory multiplicity, unmapped identities, and the excluded clinic.
  tall <- data.frame(clinic_ID=c(rep("a",20),"cancel","cancel","zero","excluded","unknown"),
    cCode=c(rep("U",2),rep("N",3),rep("S",4),rep("S2",3),rep("E",4),rep("M",4),"U","U","U","U","U"),
    Variable=c("A","A","B","B","B","C","C","D","D","E","E","F","G","G","H","H","I","J","K","K","A","B","A","A","A"),
    Value=c(2,2,NA,3,3,1,2,NA,4,2,3,9,1,2,5,5,NA,10,NA,NA,5,-7,0,2,3))
  tall$cAnnee <- 2014L; tall$cPeriode <- 1L; tall$fname <- 1L
  tall$cCodeNiv <- "1"; tall$cCodeStru <- "2"; tall$cNom <- tall$clinic_ID
  ids <- unique(tall[c("clinic_ID","cAnnee","cPeriode","fname","cCodeNiv","cCodeStru","cNom")])
  ids$clinic_ID[ids$clinic_ID=="excluded"] <- "qCHtCHkwqtQ"
  ids$clinic_ID[ids$clinic_ID=="unknown"] <- NA_character_
  ids <- rbind(ids, transform(ids[1,],cNom="unmatched",clinic_ID="unmatched"))
  raw <- tall[,setdiff(names(tall),"clinic_ID")]
  input <- write(raw,"month"); identities <- write(ids,"ids")
  reduced_paths <- f("reduce", input,identities,file.path(scratch,"reduced.parquet"))
  reduced <- read(reduced_paths[[1]])
  audit <- read(reduced_paths[[2]])
  stopifnot(setequal(audit$decision,c("unique","unique_nonNA","sum","sum2","error")), all(reduced$clinic_ID=="a"))
  expected_values <- c(A=2,B=3,C=3,D=4,E=5,F=9,G=-99,H=-99,I=NA,J=10,K=NA)
  same(reduced[c("Variable","Value")], data.frame(Variable=names(expected_values),Value=unname(expected_values)))

  # Run the actual monthly anonymous function parsed from pinned 4A. Its
  # top-level readers, futures, and file writers are never evaluated.
  legacy_source <- parse("sandbox/madagascar_clinic_data_cleaning/scripts/4A_clean_clinic_diagnosis.R")
  legacy <- new.env(parent=environment())
  for (expr in legacy_source) {
    if (is.call(expr) && identical(expr[[1]],as.name("<-")) && identical(expr[[2]],as.name("drop_na_if"))) eval(expr,legacy)
  }
  monthly_assignment <- Filter(function(e) is.call(e) && identical(e[[1]],as.name("<-")) &&
    identical(e[[2]],as.name("w1_2_dt")),as.list(legacy_source))[[1]]
  legacy_month <- eval(monthly_assignment[[3]][[3]],legacy)
  dictionary <- tibble(var=paste0("V",1:7),filename="file1",wave1_colname=c("A","B","I","A + I","A + Missing","Missing","K"),
    wave2_colname=NA_character_,wave3A_colname=NA_character_,wave3B_colname=NA_character_)
  rules <- f("rules",dictionary,tibble(w3_colname="X1"),tibble(Column.Name=c(names(expected_values),"Missing")))
  env$s3_diagnosis_dictionary_df <- dictionary
  env$s3_w3_column_dictionary_df <- tibble(w3_colname="X1")
  env$s3_diagnosis_wave12_dictionary_df <- tibble(Column.Name=c(names(expected_values),"Missing"),ID.=seq_len(length(expected_values)+1L))
  env$s3_diagnosis_supplementary_dictionary_df <- tibble(Column.Name="Extra",ID.="text-ID")
  stopifnot(nrow(eval(assignments$s4a_diagnosis_evaluation_rules_df,env))==4*nrow(dictionary))
  legacy$clinicID <- ids
  legacy$addtl_diag_w1_2 <- as.data.table(raw[FALSE,])
  legacy$diag_map <- dictionary
  for (wave in c("wave1","wave2")) {
    legacy$diag_map[[paste0(wave,"_colname_replaced")]] <- vapply(dictionary[[paste0(wave,"_colname")]],function(x) {
      if (is.na(x)) return(NA_character_)
      terms <- strsplit(x," + ",fixed=TRUE)[[1]]
      if(length(terms)==1) x else paste0("ifelse(",terms," == -1, 0, ",terms,")",collapse=" + ")
    },character(1))
  }
  legacy$wave1_2_dict <- tibble(wave="wave1",col_name=c(names(expected_values),"Missing"))
  csv <- file.path(scratch,"2014_01.csv")
  fwrite(raw,csv,na="NA")
  reference <- legacy_month(csv)
  mapped_path <- f("month_map",reduced_paths,rules,file.path(scratch,"mapped.parquet"))
  mapped <- read(mapped_path)
  same(mapped,reference)
  stopifnot(mapped$V3==-1,mapped$V4==2,is.na(mapped$V5),is.na(mapped$V6),mapped$V7==-1)
  # Apply the same fixture through wave2's dictionary and fname selector.
  dictionary2 <- dictionary
  dictionary2$wave2_colname <- dictionary$wave1_colname
  legacy$diag_map$wave2_colname_replaced <- legacy$diag_map$wave1_colname_replaced
  legacy$wave1_2_dict <- bind_rows(legacy$wave1_2_dict, mutate(legacy$wave1_2_dict,wave="wave2"))
  raw2 <- raw;raw2$fname<-2L;ids2<-ids;ids2$fname<-2L
  legacy$clinicID<-ids2;legacy$addtl_diag_w1_2<-as.data.table(raw2[FALSE,])
  fwrite(raw2,csv,na="NA")
  rules2 <- f("rules",dictionary2,tibble(w3_colname="X1"),tibble(Column.Name=c(names(expected_values),"Missing")))
  reduced2 <- f("reduce",write(raw2,"month2"),write(ids2,"ids2"),file.path(scratch,"reduced2.parquet"))
  same(read(f("month_map",reduced2,rules2,file.path(scratch,"mapped2.parquet"))),legacy_month(csv))
  # One-row subsets are skipped. Absent cells differ from explicit missing.
  one <- write(reduced[1,],"one")
  stopifnot(nrow(read(f("month_map",one,rules,file.path(scratch,"one_mapped.parquet"))))==0)
  absent <- reduced
  absent$clinic_ID <- "second"
  absent <- absent[absent$Variable!="I",]
  missing_cells <- write(rbind(reduced,absent),"missing_cells")
  missing_result <- read(f("month_map",missing_cells,rules,file.path(scratch,"missing_mapped.parquet")))
  stopifnot(is.na(missing_result$V3[missing_result$clinic_ID=="second"]))
  stopifnot(f("mapping_audit",one)$skipped_one_row,
    !f("mapping_audit",reduced_paths)$skipped_one_row)
  # Duplicate surviving cells trigger the actual dcast length fallback.
  duplicate_cells <- write(rbind(reduced,reduced[1,]),"duplicate_cells")
  stopifnot(f("mapping_audit",duplicate_cells)$dcast_length_fallback)
  legacy$clinicID<-ids;legacy$addtl_diag_w1_2<-as.data.table(raw[FALSE,])
  # A synthetic reduced relation compares directly to dcast's fallback.
  dcast_input <- as.data.table(rbind(reduced,reduced[1,]))
  dcast_input[is.na(Value),Value:=-1]
  wide_counts <- suppressWarnings(dcast(dcast_input,clinic_ID+cAnnee+cPeriode+fname~Variable,value.var="Value"))
  mapped_counts <- read(f("month_map",duplicate_cells,rules,file.path(scratch,"counts_mapped.parquet")))
  stopifnot(mapped_counts$V1==wide_counts$A,is.na(mapped_counts$V6))

  # Regression: the real wave2 dictionary has 1,237 unique source terms.
  # A scalar EXISTS repeated in every pivot column exhausts DuckDB's binder
  # depth even though individual formulas are short. Use tiny rows at real
  # dictionary width, with both waves, absent cells, and an unmapped duplicate
  # that must trigger length fallback for the entire wave2 subset only.
  width <- 1237L
  source_terms <- paste0("term",seq_len(width))
  wide_dictionary <- tibble(var=paste0("V",seq_len(width)),filename="file1",
    wave1_colname=source_terms,wave2_colname=source_terms,
    wave3A_colname=NA_character_,wave3B_colname=NA_character_)
  wide_rules <- f("rules",wide_dictionary,tibble(w3_colname="X1"),tibble(Column.Name=source_terms))
  wide_cells <- tibble(clinic_ID="dense",cAnnee=2014L,cPeriode=1L,fname=2L,
    Variable=source_terms[-width],Value=c(-1,NA_real_,seq.int(3,width-1L)))
  sparse <- wide_cells[1,]; sparse$clinic_ID<-"sparse"; sparse$Value<-7
  other_wave <- wide_cells[1:2,]; other_wave$clinic_ID<-"wave1"; other_wave$fname<-1L
  unmapped_report <- sparse; unmapped_report$clinic_ID<-"unmapped_only"; unmapped_report$Variable<-"not_in_dictionary"
  wide_input <- bind_rows(wide_cells,sparse,other_wave,unmapped_report)
  wide_mapped <- read(f("month_map",write(wide_input,"wide_cells"),wide_rules,file.path(scratch,"wide_mapped.parquet")))
  stopifnot(nrow(wide_mapped)==4L,ncol(wide_mapped)==width+4L,
    wide_mapped$V1[wide_mapped$clinic_ID=="dense"]==-1,
    wide_mapped$V2[wide_mapped$clinic_ID=="dense"]==-1,
    wide_mapped$V3[wide_mapped$clinic_ID=="dense"]==3,
    wide_mapped$V1[wide_mapped$clinic_ID=="sparse"]==7,
    is.na(wide_mapped$V2[wide_mapped$clinic_ID=="sparse"]),
    all(is.na(wide_mapped[wide_mapped$clinic_ID=="unmapped_only",wide_dictionary$var])),
    all(is.na(wide_mapped[[paste0("V",width)]])))
  unmapped <- wide_cells[rep(1L,2L),]; unmapped$Variable<-"not_in_dictionary"
  wide_counts <- read(f("month_map",write(bind_rows(wide_input,unmapped),"wide_duplicates"),
    wide_rules,file.path(scratch,"wide_counts.parquet")))
  stopifnot(wide_counts$V1[wide_counts$clinic_ID=="dense"]==1,
    wide_counts$V2[wide_counts$clinic_ID=="dense"]==1,
    wide_counts$V1[wide_counts$clinic_ID=="sparse"]==1,
    wide_counts$V2[wide_counts$clinic_ID=="sparse"]==0,
    wide_counts$V1[wide_counts$clinic_ID=="wave1"]==-1,
    wide_counts$V1[wide_counts$clinic_ID=="unmapped_only"]==0,
    wide_counts$V2[wide_counts$clinic_ID=="unmapped_only"]==0,
    all(is.na(wide_counts[[paste0("V",width)]])))
  # Repeated inventory keys must multiply numeric-disagreement sums.
  repeated <- f("reduce",input,write(rbind(ids,ids[1,]),"repeated_ids"),file.path(scratch,"repeated.parquet"))
  stopifnot(read(repeated[[1]])$Value[read(repeated[[1]])$Variable=="C"]==6)

  # Source-selection boundaries, nearest ties, discarded fallbacks and legacy
  # wrong_fname classification. Preserve repeated rows at one source.
  candidates <- tibble(clinic_ID=c("march","march","april","april","tie","tie","far","far","far","new","repeat","repeat"),
    cAnnee=c(2015,2015,2015,2015,2017,2017,2018,2018,2018,2020,2019,2019),
    cPeriode=c(3,3,4,4,1,1,1,1,1,1,1,1),fname=c(1,2,1,2,2,4,1,2,3,6,5,5))
  selection <- sql(f("selection_sql",f("read",write(candidates,"candidates"))))
  stopifnot(identical(selection$selection[selection$clinic_ID=="tie"],rep("nearest_fallback",2)),
    selection$selection[selection$clinic_ID=="march" & selection$fname==1]=="expected",
    selection$selection[selection$clinic_ID=="april" & selection$fname==1]=="wrong_fname",
    sum(selection$selection[selection$clinic_ID=="far"]=="discarded_fallback")==2)
  legacy_selected <- candidates |>
    mutate(fname_correct=case_when(cAnnee<2015 | cAnnee==2015 & cPeriode<=3 ~ 1,
      cAnnee %in% c(2015,2016)~2,cAnnee==2017~3,cAnnee==2018~4,cAnnee==2019~5,cAnnee>2019~6,TRUE~0)) |>
    mutate(any_fname_correct=any(fname==fname_correct),.by=c(clinic_ID,cAnnee,cPeriode)) |>
    filter(fname==fname_correct | !any_fname_correct) |>
    mutate(fname_dist=abs(fname-fname_correct)) |>
    filter(fname_dist==min(fname_dist),.by=c(clinic_ID,cAnnee,cPeriode)) |>
    select(all_of(names(candidates)))
  same(selection[selection$selection %in% c("expected","nearest_fallback"),names(candidates)],legacy_selected)
  selection$report_order <- match(paste(selection$clinic_ID,selection$fname),paste(candidates$clinic_ID,candidates$fname))
  selection_path <- write(selection,"selection")
  must_error(f("tracker",selection_path,file.path(scratch,"bad_tracker.parquet")))
  clean_selection <- selection[!selection$clinic_ID %in% c("tie","repeat"),]
  tracker <- read(f("tracker",write(clean_selection,"clean_selection"),file.path(scratch,"tracker.parquet")))
  stopifnot(setequal(names(tracker),c("clinic_ID","y2015_m03","y2015_m04","y2018_m01","y2020_m01")),
    tracker$y2015_m03[tracker$clinic_ID=="march"]==1,tracker$y2015_m04[tracker$clinic_ID=="march"]==0)
  na_candidate <- write(tibble(clinic_ID="missing_period",cAnnee=NA_integer_,cPeriode=NA_integer_,fname=6L,report_order=1L),"missing_period")
  na_selection <- write(sql(f("selection_sql",f("read",na_candidate))),"missing_period_selection")
  na_tracker <- read(f("tracker",na_selection,file.path(scratch,"missing_period_tracker.parquet")))
  stopifnot(na_tracker$yNA_mNA==6)
  output_data <- candidates |> mutate(V1=seq_len(n()))
  outputs <- f("output","file1",dictionary[1,],write(output_data,"output_data"),write(output_data[FALSE,],"empty"),selection_path,file.path(scratch,"outputs"))
  same(read(outputs[[1]]),inner_join(output_data,distinct(legacy_selected),by=names(candidates)))
  stopifnot(nrow(read(outputs[[2]]))==2,setequal(read(outputs[[2]])$fname,c(2,1)))
  no_wrong <- write(clean_selection[clean_selection$selection=="expected",],"no_wrong")
  empty_output <- f("output","file1",dictionary[1,],write(output_data,"output_data2"),write(output_data[FALSE,],"empty2"),no_wrong,file.path(scratch,"outputs_empty"))
  stopifnot(nrow(read(empty_output[[2]]))==0,identical(names(read(empty_output[[1]])),names(read(empty_output[[2]]))))

  ordered_candidates <- f("candidates",write(output_data,"ordered_w12"),write(output_data[FALSE,],"ordered_w3"),file.path(scratch,"ordered_candidates.parquet"))
  stopifnot(isTRUE(all.equal(read(ordered_candidates)$report_order,as.double(seq_len(nrow(output_data))))))
  # Wave 3 full join and schema order; fixture has the real X positions but
  # only a few rows, so accidental renumbering is observable.
  descriptions <- c("organisationunitid","periodcode","V1","TYPE","FS","code_fs",paste0("measure",7:1736))
  schema <- tibble(w3_description=c(descriptions,"new_anemia"),orig=c(rep(TRUE,length(descriptions)),FALSE),w3_colname=paste0("X",seq_len(length(descriptions)+1L)))
  w3 <- as.data.frame(setNames(rep(list(rep(NA_real_,5)),length(descriptions)),descriptions))
  w3$organisationunitid <- c("old","old","cold","repair","later")
  w3$periodcode <- c("202001","202001","202001","202001","202101")
  w3$V1 <- seq_len(5); w3$TYPE<-"T";w3$FS<-"F";w3$code_fs<-"C"
  w3$measure10 <- c(1,1,NA,NA,2)
  w3$measure1067 <- c(-1,-1,NA,-4,-2)
  w3$measure1068 <- -3; w3$measure1736 <- -2
  w3$measure1708 <- c(0,0,10,0,0);w3$measure1709<-1
  # Use measure20 as an existing anemia field that must be replaced in 2020.
  w3$measure20 <- 99
  anemia <- tibble(organisationunitid=c("old","anemia_only"),periodcode="202001",measure20=c(4,7),new_anemia=c(5,6))
  corrected_path <- f("anemia_correct",write(w3,"w3"),write(anemia,"anemia"),schema,file.path(scratch,"corrected.parquet"))
  corrected <- read(corrected_path)
  stopifnot(nrow(corrected)==6,identical(names(corrected),schema$w3_description),
    corrected$measure20[corrected$organisationunitid=="later"]==99,
    all(corrected$measure20[corrected$organisationunitid=="old"]==4),
    is.na(corrected$measure20[corrected$organisationunitid=="cold"]))
  must_error(f("anemia_correct",write(w3,"w3_reorder"),write(anemia,"anemia_reorder"),schema[nrow(schema):1,],file.path(scratch,"bad.parquet")))
  dedup_path <- f("w3_deduplicate",corrected_path,schema,file.path(scratch,"w3_dedup.parquet"))
  dedup <- read(dedup_path)
  stopifnot(nrow(dedup)==5,all(is.na(dedup$X1067)),all(is.na(dedup$X1068)),all(is.na(dedup$X1736)))
  collapsed <- corrected[corrected$organisationunitid=="old",]
  collapsed$measure1067<-c(-1,-2)
  post_repair <- read(f("w3_deduplicate",write(collapsed,"post_repair_source"),schema,file.path(scratch,"post_repair.parquet")))
  stopifnot(nrow(post_repair)==2,all(is.na(post_repair$X1067)))
  reporting_path <- f("w3_reporting",dedup_path,file.path(scratch,"reporting.parquet"))
  reporting <- read(reporting_path)
  stopifnot(setequal(reporting$organisationunitid,c("old","anemia_only","later")))
  w3_dictionary <- tibble(var=c("V1","V2"),filename="file1",wave1_colname=NA_character_,wave2_colname=NA_character_,
    wave3A_colname=c("X20","X20 + X1737"),wave3B_colname=c("X10","X20 + X1737"))
  w3_rules <- f("rules",w3_dictionary,schema,tibble(Column.Name=character()))
  w3_mapped <- read(f("w3_map",reporting_path,w3_rules,file.path(scratch,"w3_mapped.parquet")))
  stopifnot(w3_mapped$V1[w3_mapped$clinic_ID=="old"]==4,w3_mapped$V2[w3_mapped$clinic_ID=="old"]==9,
    w3_mapped$V1[w3_mapped$clinic_ID=="later"]==2,all(w3_mapped$fname==6))

  # Regression: S0 actually stores DOUBLE periods, while the anemia reader
  # stores VARCHAR. Text-only fixtures previously missed both the starts_with
  # binder error and the ensuing join/coalesce type mismatch. Check INTEGER
  # as well as DOUBLE, including full-join rows and downstream substr mapping.
  for (period_type in c("DOUBLE","INTEGER")) {
    numeric_w3 <- w3
    numeric_w3$periodcode <- if (period_type=="DOUBLE") as.double(w3$periodcode) else as.integer(w3$periodcode)
    numeric_corrected_path <- f("anemia_correct",write(numeric_w3,paste0("numeric_w3_",period_type)),
      write(anemia,paste0("text_anemia_",period_type)),schema,file.path(scratch,paste0("numeric_corrected_",period_type,".parquet")))
    actual_type <- sql(paste("DESCRIBE SELECT periodcode FROM",f("read",numeric_corrected_path)))$column_type
    stopifnot(identical(actual_type,period_type))
    numeric_corrected <- read(numeric_corrected_path)
    text_expected <- corrected
    text_expected$periodcode <- as.numeric(text_expected$periodcode)
    same(numeric_corrected,text_expected,keys=c("organisationunitid","periodcode","V1"))
    numeric_dedup <- f("w3_deduplicate",numeric_corrected_path,schema,file.path(scratch,paste0("numeric_dedup_",period_type,".parquet")))
    numeric_reporting <- f("w3_reporting",numeric_dedup,file.path(scratch,paste0("numeric_reporting_",period_type,".parquet")))
    numeric_mapped <- read(f("w3_map",numeric_reporting,w3_rules,file.path(scratch,paste0("numeric_mapped_",period_type,".parquet"))))
    same(numeric_mapped,w3_mapped,keys=c("clinic_ID","cAnnee","cPeriode","fname"))
    stopifnot(setequal(numeric_mapped$cAnnee,c(2020L,2021L)),all(numeric_mapped$cPeriode==1L))
  }

  # Actual encoded CSV parsing, Variable repair, and archive extraction.
  source_dir <- file.path(scratch,"archive/data/raw/from_Kevin/mdg_tall_split")
  dir.create(source_dir,recursive=TRUE)
  encoded <- raw[1:2,]; encoded$cNom <- "CSB Sœurs Ampitafà"; encoded$Variable<-c("c$_A","SOMME ACCTS_*")
  csv_path <- file.path(source_dir,"2014_01.csv")
  con <- file(csv_path,"wt",encoding="CP1252"); write.csv(encoded,con,row.names=FALSE,na="NA"); close(con)
  AVC <- encoded[1,,drop=FALSE];AVC$cCode<-"AVC";AVC$Variable<-"AVC_A"
  diar <- encoded[1,,drop=FALSE];diar$cCode<-"Di";diar$Variable<-"old_prefix_count"
  for (entry in c("AVC","diar_pneu_fix")) {
    con <- file(file.path(source_dir,paste0(entry,".csv")),"wt",encoding="CP1252")
    write.csv(if(entry=="AVC") AVC else diar,con,row.names=FALSE,na="NA");close(con)
  }
  anemia_csv <- file.path(scratch,"archive/data/raw/anemie_2020.csv")
  anemia_fixture <- data.frame(organisationunitid="a",periodcode="202001",organisationunitcode="code",organisationunitdescription="desc")
  anemia_fixture[["Anémie ?"]]<-4
  write.csv(anemia_fixture,anemia_csv,row.names=FALSE,fileEncoding="UTF-8")
  archive <- file.path(scratch,"fixture.zip")
  zip_code <- "import sys,zipfile,pathlib; root=pathlib.Path(sys.argv[2]); z=zipfile.ZipFile(sys.argv[1],'w'); [z.write(p,p.relative_to(root)) for p in root.rglob('*.csv')]; z.close()"
  stopifnot(system2("python3",c("-c",shQuote(zip_code),shQuote(archive),shQuote(file.path(scratch,"archive"))))==0)
  members <- c("data/raw/from_Kevin/mdg_tall_split/AVC.csv","data/raw/from_Kevin/mdg_tall_split/diar_pneu_fix.csv")
  parsed_anemia <- read(f("anemia",archive,"data/raw/anemie_2020.csv",file.path(scratch,"parsed_anemia.parquet")))
  stopifnot(identical(names(parsed_anemia),c("organisationunitid","periodcode","Anémie ≥")),parsed_anemia[["Anémie ≥"]]==4)
  supplements <- f("supplements",archive,members,file.path(scratch,"supplements.parquet"))
  stopifnot(identical(read(supplements)$Variable,c("AVC_A","Di_count")))
  month_input <- f("month_input",archive,"data/raw/from_Kevin/mdg_tall_split/2014_01.csv",supplements,file.path(scratch,"decoded.parquet"))
  stopifnot(nrow(read(month_input))==4,all(read(month_input)$cNom=="CSB Sœurs Ampitafà"))
  encoded_ids <- ids[1,,drop=FALSE];encoded_ids$cNom<-encoded$cNom[1]
  joined <- sql(f("join",month_input,write(encoded_ids,"encoded_ids")))
  stopifnot(all(c("c__A","SOMME_ACCTS__") %in% joined$Variable))

  graph <- targets::tar_manifest(fields=c(name,format,pattern,command))
  s4 <- graph[grepl("^s4a_",graph$name),]
  stopifnot(nrow(s4)>30,!anyDuplicated(graph$name),
    all(s4$format[grepl("_parquet$",s4$name)]=="file"),
    all(s4$format[grepl("_fn$",s4$name)]=="rds"))
  declared <- tarchetypes::tar_tangle("notebooks/pipeline/22_page-clean-clinic-diagnosis.qmd")
  stopifnot(all(vapply(declared,function(x)x$name,character(1)) %in% graph$name))
  targets::tar_validate()
  message("S4 synthetic replication checks passed: actual legacy monthly function, duplicate decisions, expression semantics, reporting boundaries, source selection, W3 correction/schema, encoding, empty partitions, and graph contracts.")
})
