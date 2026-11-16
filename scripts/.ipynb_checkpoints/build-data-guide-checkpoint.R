# National aggregate evidence for notebooks/00_data-guide.qmd; no tar_make().
# Run the prepare, pinned archive-reader, and plot commands in the guide.
# This diagnostic reads saved products and never changes source/pipeline data.
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(duckplyr);library(ggplot2)})
local({
  args <- commandArgs(trailingOnly=TRUE)
  if(!length(args) || !args[[1]] %in% c("prepare","plot") || length(args)>3L) stop("Expected prepare|plot [work-directory] [assets-directory].")
  work <- if(length(args)>1L) args[[2]] else "data/intermediates/data-guide"
  assets <- if(length(args)>2L) args[[3]] else "notebooks/assets/data-guide"
  dir.create(work,recursive=TRUE,showWarnings=FALSE)
  sha <- function(path) {
    value <- system2("sha256sum",shQuote(path),stdout=TRUE)
    if(!is.null(attr(value,"status")) || length(value)!=1L || !grepl("^[a-f0-9]{64}",value)) stop("SHA-256 failed.")
    substr(value,1L,64L)
  }
  literal <- function(x) paste0("'",gsub("'","''",x,fixed=TRUE),"'")
  read <- function(p) paste0("read_parquet(",literal(p),")")
  query <- function(sql) read_sql_duckdb(sql,prudence="stingy") |> collect()
  save_table <- function(x,name) compute_parquet(as_duckdb_tibble(as.data.frame(x)),file.path(work,paste0(name,".parquet")))
  variables <- c("V114","V121","V135")
  labels <- c(V114="Simple malaria, ages 1-4",V121="Diarrhea, ages 1-4",V135="Pneumonia, ages 1-4")
  fields <- c("description_en","wave1_colname","wave2_colname","wave3A_colname","wave3B_colname","wave3_description")
  request_path <- file.path(work,"request.json")
  if(args[[1]]=="prepare") {
    baseline <- dget("src/archived-comparison-baseline.R")
    if(!identical(sha(baseline$archive_zip),baseline$archive_sha256)) stop("Unreviewed archive.")
    paths <- unname(targets::tar_read(s10_final_diagnosis_parquet))
    paths <- paths[basename(paths)=="file2.parquet"]
    stopifnot(length(paths)==1L,file.exists(paths))
    dictionary <- targets::tar_read(s3_diagnosis_dictionary_df)
    selected <- dictionary[match(variables,dictionary$var),c("var","filename",fields)]
    stopifnot(identical(selected$var,variables),all(selected$filename=="file2"))
    temporary <- tempfile("guide-dictionary-");dir.create(temporary)
    on.exit(unlink(temporary,recursive=TRUE),add=TRUE)
    old_path <- unzip(baseline$archive_zip,files=baseline$dictionary_member,exdir=temporary)
    stopifnot(identical(sha(old_path),baseline$dictionary_sha256))
    old <- read.csv(old_path,check.names=FALSE)
    old <- old[match(variables,old$var),c("var","filename",fields)]
    # X numbering can shift with prepared source schemas. Compare descriptions
    # as well as expressions, and retain every expression difference explicitly.
    stopifnot(identical(old$var,selected$var),identical(old$description_en,selected$description_en),
      identical(old$wave3_description,selected$wave3_description))
    semantics <- bind_rows(lapply(fields,function(field) {
      archived <- as.character(old[[field]]);present <- as.character(selected[[field]])
      tibble(var=variables,field=field,archive=archived,current=present)
    }))
    semantics$same <- (is.na(semantics$archive) & is.na(semantics$current)) |
      (!is.na(semantics$archive) & !is.na(semantics$current) & semantics$archive==semantics$current)
    save_table(semantics,"dictionary-comparison")
    request <- list(baseline=baseline,variables=variables,
      current_path=paths[[1]],current_sha256=sha(paths[[1]]),
      dictionary_path=targets::tar_read(s3_diagnosis_dictionary_parquet),
      reporting_path=targets::tar_read(s10_final_reporting_metadata_parquet),
      captured_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
    jsonlite::write_json(request,request_path,pretty=TRUE,auto_unbox=TRUE,null="null",na="null")
    message("Prepared a saved-product snapshot and dictionary comparison. No targets built.")
  } else {
    request <- jsonlite::read_json(request_path,simplifyVector=TRUE)
    receipt <- jsonlite::read_json(file.path(work,"archive-receipt.json"),simplifyVector=TRUE)
    old_path <- file.path(work,"archive-selected.parquet")
    stopifnot(identical(request$current_sha256,sha(request$current_path)),
      identical(receipt$request_sha256,sha(request_path)),
      identical(receipt$projection_sha256,sha(old_path)),
      identical(receipt$archive_sha256,request$baseline$archive_sha256))
    # Unique nonmissing clinic-month keys are required before full joins.
    for(path in c(old_path,request$current_path)) {
      keys <- query(paste0("SELECT count(*) AS rows,count(DISTINCT (clinic_ID,cAnnee,cPeriode)) AS keys,",
        "count(*) FILTER (WHERE clinic_ID IS NULL OR cAnnee IS NULL OR cPeriode IS NULL) AS missing FROM ",read(path)))
      stopifnot(keys$rows==keys$keys,keys$missing==0)
    }
    monthly <- function(path,source) {
      long <- paste0("SELECT cAnnee AS year,cPeriode AS month,var,value FROM ",read(path),
        " UNPIVOT INCLUDE NULLS (value FOR var IN (",paste(variables,collapse=","),"))")
      query(paste0("SELECT year,month,var,sum(CASE WHEN value>=0 THEN value END) AS total,",
        "count(*) FILTER (WHERE value>=0) AS measured_clinics,",
        "count(*) FILTER (WHERE value<0) AS negative_cells,count(*) FILTER (WHERE value IS NULL) AS missing_cells,",
        "count(*) AS reporting_clinics FROM (",long,") GROUP BY ALL ORDER BY var,year,month")) |> mutate(source=source)
    }
    totals <- bind_rows(monthly(old_path,"Frozen archive"),monthly(request$current_path,"Current pipeline")) |>
      mutate(date=as.Date(sprintf("%04d-%02d-01",year,month)),diagnosis=unname(labels[var]))
    stopifnot(max(totals$year[totals$source=="Frozen archive"])==2024L)
    # Missing calendar months must break a line rather than being interpolated.
    totals <- bind_rows(lapply(split(totals,interaction(totals$source,totals$var)),function(x) {
      calendar <- tibble(date=seq(min(x$date),max(x$date),by="month"))
      left_join(calendar,x,by="date") |> mutate(source=x$source[[1]],var=x$var[[1]],diagnosis=x$diagnosis[[1]])
    }))
    totals$source <- factor(totals$source,levels=c("Frozen archive","Current pipeline"))
    save_table(totals,"monthly-aggregates")
    evidence <- bind_rows(lapply(variables,function(v) {
      sql <- paste0("WITH a AS (SELECT *,true AS in_old FROM ",read(old_path)," WHERE cAnnee<=2024),",
        "b AS (SELECT *,true AS in_new FROM ",read(request$current_path)," WHERE cAnnee<=2024),",
        "p AS (SELECT a.in_old,b.in_new,a.fname AS old_fname,b.fname AS new_fname,a.",v," AS old_value,b.",v,
        " AS new_value FROM a FULL OUTER JOIN b USING(clinic_ID,cAnnee,cPeriode)) SELECT ",
        "count(*) FILTER (WHERE in_old AND in_new) AS shared_keys,",
        "count(*) FILTER (WHERE in_old AND in_new IS NULL) AS archive_only,",
        "count(*) FILTER (WHERE in_new AND in_old IS NULL) AS current_only,",
        "count(*) FILTER (WHERE in_old AND in_new AND old_fname IS DISTINCT FROM new_fname) AS source_differences,",
        "count(*) FILTER (WHERE in_old AND in_new AND old_value IS NOT DISTINCT FROM new_value) AS equal_cells,",
        "count(*) FILTER (WHERE in_old AND in_new AND old_value IS DISTINCT FROM new_value) AS different_cells FROM p")
      result <- query(sql);result$var <- v;result$diagnosis <- labels[[v]];result
    }))
    save_table(evidence,"keyed-comparison")
    dir.create(assets,recursive=TRUE,showWarnings=FALSE)
    theme <- theme_minimal(base_size=12)+theme(legend.position="bottom",panel.grid.minor=element_blank())
    colors <- c("Frozen archive"="#d55e00","Current pipeline"="#0072b2")
    historical <- totals |> filter(date<as.Date("2025-01-01"))
    overlay <- ggplot(historical,aes(date,total,color=source,linetype=source))+
      geom_line(linewidth=0.6,na.rm=TRUE)+facet_wrap(~diagnosis,ncol=1,scales="free_y")+
      scale_color_manual(values=colors)+scale_linetype_manual(values=c("Frozen archive"="solid","Current pipeline"="dashed"))+
      labs(x=NULL,y="Monthly recorded cases",color=NULL,linetype=NULL,
        title="Frozen archive and current pipeline: shared history through 2024",
        subtitle="Three individual dictionary variables; all retained clinics in each source",
        caption="Negative sentinels excluded; all-missing months stay missing.\nRecorded case totals are not population rates.")+theme
    ggsave(file.path(assets,"archive-comparison.png"),overlay,width=11,height=9,dpi=160)
    continuation <- totals |> filter(source=="Current pipeline")
    extension <- ggplot(continuation,aes(date,total))+
      geom_vline(xintercept=as.Date(c("2015-04-01","2020-01-01","2021-01-01")),color="grey70",linetype="dotted")+
      geom_vline(xintercept=as.Date("2025-01-01"),color="#d55e00",linetype="dashed")+
      geom_line(color="#0072b2",linewidth=0.6,na.rm=TRUE)+facet_wrap(~diagnosis,ncol=1,scales="free_y")+
      labs(x=NULL,y="Monthly recorded cases",title="Current pipeline: historical series and observed extension",
        subtitle="Orange boundary: January 2025 deliveries; grey boundaries: historical collection changes",
        caption="Available observations only; no forecast, smoothing, or claim of a stable case definition.")+theme
    ggsave(file.path(assets,"trend-extension.png"),extension,width=11,height=9,dpi=160)
    coverage <- ggplot(totals,aes(date,measured_clinics,color=source,linetype=source))+
      geom_line(linewidth=0.6,na.rm=TRUE)+facet_wrap(~diagnosis,ncol=1,scales="free_y")+
      scale_color_manual(values=colors)+scale_linetype_manual(values=c("Frozen archive"="solid","Current pipeline"="dashed"))+
      geom_vline(xintercept=as.Date("2025-01-01"),color="grey50",linetype="dotted")+
      labs(x=NULL,y="Clinics with nonnegative measurements",color=NULL,linetype=NULL,
        title="Coverage behind the diagnosis totals",subtitle="Entered zero counts as measured; negative sentinels and missing cells do not")+theme
    ggsave(file.path(assets,"reporting-coverage.png"),coverage,width=11,height=9,dpi=160)
    # The versioned public evidence contains only national aggregates and hashes.
    jsonlite::write_json(list(snapshot_utc=request$captured_utc,baseline=request$baseline$label,
      archive_sha256=receipt$archive_sha256,archive_member=receipt$member,
      archive_member_sha256=receipt$member_sha256,current_sha256=request$current_sha256,
      current_period=c(min(totals$date[totals$source=="Current pipeline"]),max(totals$date[totals$source=="Current pipeline"])),
      keyed_comparison=as.data.frame(evidence),
      variables=variables,R=as.character(getRversion()),pyarrow=receipt$pyarrow,
      code_sha256=as.list(setNames(vapply(c("scripts/build-data-guide.R","scripts/read-guide-archive.py"),sha,""),
        c("scripts/build-data-guide.R","scripts/read-guide-archive.py")))),
      file.path(assets,"provenance.json"),pretty=TRUE,auto_unbox=TRUE,na="null",digits=NA)
    print(evidence)
    print(totals |> filter(source=="Current pipeline") |> summarise(first=min(date),last=max(date)))
    message("Figures generated from saved data. Protected projections and aggregates remain in ",work)
  }
})
