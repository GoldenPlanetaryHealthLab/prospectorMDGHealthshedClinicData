# Run from the repository root:
# scripts/mdg-healthshed-clinic-data exec uvr run tests/check-clinic-month-encoding.R
# Optionally append -- /path/to/madagascar_clinic_data_cleaning.zip to check real months.
library(duckplyr)

local({
  scratch <- tempfile("check-clinic-encoding-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  # Load notebook helpers without evaluating targets or touching pipeline state.
  tangled <- file.path(scratch, "page20.R")
  knitr::purl(
    "notebooks/pipeline/20_page-construct-clinic-ids.qmd",
    output = tangled, documentation = 0L, quiet = TRUE
  )
  for (expr in parse(tangled)) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
      eval(expr)
    }
  }

  check_member <- function(archive, member) {
    csv <- utils::unzip(archive, files = member, exdir = file.path(scratch, "reference"))
    # An independent CSV parser supplies the reference, including decoding,
    # missing values, integer inference, deduplication, and source order.
    reference <- utils::read.csv(csv, fileEncoding = "CP1252", stringsAsFactors = FALSE)
    reference <- reference[s2_clinic_months_identity_columns_fn()]
    keep <- !duplicated(reference)
    reference <- reference[keep, , drop = FALSE]

    output <- file.path(scratch, paste0(basename(member), ".parquet"))
    s2_clinic_months_write_member_fn(archive, member, output)
    actual <- read_parquet_duckdb(output, prudence = "stingy") |>
      arrange(.inventory_row) |>
      collect() |>
      as.data.frame()
    stopifnot(
      identical(names(actual), c(s2_clinic_months_identity_columns_fn(), ".inventory_row", ".inventory_file")),
      isTRUE(all.equal(actual[s2_clinic_months_identity_columns_fn()], reference, check.attributes = FALSE)),
      identical(as.numeric(actual$.inventory_row), as.numeric(which(keep))),
      all(actual$.inventory_file == basename(member)),
      all(validUTF8(actual$cNom[!is.na(actual$cNom)]))
    )
    message(basename(member), ": ", sum(keep), " identities match the CP1252 reference")
    actual
  }

  # Place accents beyond DuckDB's default sniffing sample. Include Windows-1252
  # 0x9C (oe ligature), quoted commas/quotes, empty names, NA, and duplicates.
  member <- "mdg_tall_split/2010_01.csv"
  dir.create(file.path(scratch, "mdg_tall_split"))
  rows <- c(
    "cNom,cAnnee,cPeriode,cCodeNiv,cCodeStru,cCode,fname,Variable,Value",
    rep("CSB1 Plain,2010,01,330421,330452044,ACTRAUM,1,Trauma_CasAge0,", 21000L),
    '"CSB1 Ampitaf\u00e0, Sant\u00e9 \"\"A\"\"",2010,01,330421,330452044,ACTRAUM,1,Trauma_CasAge0,',
    "CSB2 S\u0153urs Trinitaires,2010,01,330421,330452045,XBRTH,1,cACC_AV,",
    "CSB1 L\u00e8proserie No\u00ebl H\u00f4pital\u00a8,2010,01,330421,330452046,XBRTH,1,cACC_AV,",
    ",2010,01,330421,330452047,XBRTH,1,cACC_AV,",
    "NA,2010,01,330421,NA,XBRTH,1,cACC_AV,",
    "CSB1 Plain,2010,01,330421,330452044,XBRTH,1,cACC_AV,2"
  )
  writeBin(
    charToRaw(iconv(paste0(paste(rows, collapse = "\r\n"), "\r\n"), from = "UTF-8", to = "CP1252")),
    file.path(scratch, member)
  )
  archive <- file.path(scratch, "fixture.zip")
  # Python is supplied by the runtime; the external `zip` command is not.
  stopifnot(nzchar(Sys.which("python3")))
  zip_status <- system2("python3", c(
    "-c", shQuote(paste(
      "import sys, zipfile;",
      "z = zipfile.ZipFile(sys.argv[1], 'w', zipfile.ZIP_DEFLATED);",
      "z.write(sys.argv[2], sys.argv[3]); z.close()"
    )),
    shQuote(archive), shQuote(file.path(scratch, member)), shQuote(member)
  ))
  stopifnot(zip_status == 0L)
  fixture <- check_member(archive, member)
  stopifnot(nrow(fixture) == 6L, "CSB2 S\u0153urs Trinitaires" %in% fixture$cNom)

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) {
    stopifnot(length(args) == 1L, file.exists(args[[1L]]))
    members <- s2_clinic_months_list_members_fn(args[[1L]])
    for (month in c("2010_01.csv", "2015_01.csv")) {
      invisible(check_member(args[[1L]], members[basename(members) == month]))
    }
  }
})
