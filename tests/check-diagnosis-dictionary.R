# Run from the repository root:
# scripts/mdg-healthshed-clinic-data exec Rscript tests/check-diagnosis-dictionary.R
# Verification only: synthetic fixtures and parsed source; never build targets.
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(duckplyr)
library(magrittr)

local({
  scratch <- tempfile("check-dictionary-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  tangled <- file.path(scratch, "page21.R")
  knitr::purl("notebooks/pipeline/21_page-construct-diagnosis-dictionary.qmd",
              output = tangled, documentation = 0L, quiet = TRUE)
  assignments <- list()
  env <- new.env(parent = environment())
  # The tangler parses each chunk separately; a combined purl parse alone can
  # hide a dangling pipe that accidentally captures the next chunk's assignment.
  notebook <- parsermd::parse_qmd("notebooks/pipeline/21_page-construct-diagnosis-dictionary.qmd")
  for (node in notebook@nodes) {
    if (inherits(node, "rmd_chunk") && identical(as.character(node@engine), "r")) {
      parse(text = node@code)
    }
  }
  for (expr in parse(tangled)) {
    if (!is.call(expr) || !identical(expr[[1L]], as.name("<-"))) next
    assignments[[as.character(expr[[2L]])]] <- expr[[3L]]
    if (is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
      eval(expr, env)
    }
  }
  declared <- tarchetypes::tar_tangle(
    "notebooks/pipeline/21_page-construct-diagnosis-dictionary.qmd"
  )
  declared_names <- vapply(declared, function(target) target$name, character(1))
  stopifnot(all(names(assignments) %in% declared_names))
  run <- function(name) {
    value <- eval(assignments[[name]], env)
    assign(name, value, env)
    value
  }
  must_error <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))

  # Only evaluate this function definition from the legacy script, not its readers.
  original <- parse("sandbox/madagascar_clinic_data_cleaning/scripts/3_construct_dictionary.R")
  aggregate_definition <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("aggregate_to_target_wave"))
  }, as.list(original))[[1L]]
  legacy_env <- new.env(parent = environment())
  eval(aggregate_definition, legacy_env)
  fixture <- tibble(
    wave1_colname = c("A", "A", "B"), wave1_description = c("a", "a", "b"),
    wave2_colname = c("C", "D", NA), wave2_description = c("c", "d", NA),
    wave3A_colname = c("X1", "X2", "X3"), wave3A_description = c("x1", "x2", "x3"),
    wave3B_colname = c("X4", "X5", "X3"), wave3B_description = c("x4", "x5", "x3")
  )
  for (wave in c("wave1", "wave2", "wave3A", "wave3B")) {
    stopifnot(isTRUE(all.equal(
      env$s3_diagnosis_aggregate_to_wave_fn(fixture, wave),
      legacy_env$aggregate_to_target_wave(fixture, wave)
    )))
  }
  aggregated <- env$s3_diagnosis_aggregate_to_wave_fn(fixture, "wave1")
  stopifnot(identical(aggregated$wave2_colname, c("C + D", NA_character_)))
  fixture$wave2_colname[[2L]] <- NA_character_
  stopifnot(is.na(env$s3_diagnosis_aggregate_to_wave_fn(fixture, "wave1")$wave2_colname[[1L]]))

  # Range/slash/semicolon ordering and missing/ignored matches are hand-specified.
  env$s3_diagnosis_mapping_source_df <- tibble(
    wave3_colname_specialchar = paste0("T4: definition ", 1:5),
    Wave_1_Match = c("1-3", "4/5", "?", "Ignore", "7;8"),
    Wave_2_Match = c("9", "10", "?", "Ignore", "11")
  )
  env$s3_w3_column_dictionary_df <- tibble(
    w3_description = c(paste0("T4: definition ", 1:5), "organisationunitid"),
    orig = TRUE, w3_colname = paste0("X", 1:6)
  )
  expanded <- run("s3_diagnosis_matches_expanded_df")
  stopifnot(
    identical(expanded$Wave_1_Match, c("4", "5", NA, NA, "7", "8", "1", "2", "3")),
    !any(expanded$wave3_description == "organisationunitid"),
    length(unique(expanded$row_id[expanded$Wave_1_Match %in% c("1", "2", "3")])) == 1L
  )

  # Archived translation pairs are exact keys, including punctuation.
  translations <- tibble(fr = c("A, B + C", "A, B + C", "Other"),
                         en = c("Archived phrase", "Archived phrase", NA))
  lookup <- env$s3_diagnosis_translation_lookup_fn(translations, "fr", "en")
  stopifnot(nrow(lookup) == 1L, identical(lookup$english, "Archived phrase"))
  must_error(env$s3_diagnosis_translation_lookup_fn(
    tibble(fr = c("A", "A"), en = c("First", "Second")), "fr", "en"
  ))
  stopifnot(identical(env$s3_diagnosis_multiple_gsub_fn(c("A", "B"), c("B", "C"), "A"), "C"))
  must_error(env$s3_diagnosis_multiple_gsub_fn(1:2, 1:3, "x"))

  # Alias retention includes the legacy repeated-key first-match rule.
  env$s3_diagnosis_code_dictionary_source_df <- tibble(
    cCode = c("DiSSDH", "AFNEURO"), cDesc = c("French A", "French B"), file = "dir1"
  )
  aliases <- run("s3_diagnosis_code_aliases_df")
  codes <- run("s3_diagnosis_code_dictionary_df")
  stopifnot(identical(aliases$new_cCode, c("DiSS", "Neurological")),
            identical(codes$cCode, c("DiSSDH", "AFNEURO", "DiSS", "Neurological")))

  # Match actual codes rather than arbitrary substrings; retain grouped French keys.
  env$s3_diagnosis_variable_map_df <- tibble(
    var = paste0("V", 1:3), wave1_colname = c("A_1 + B_2", "BA_1", NA),
    wave2_colname = c("C_3", "B_4", NA)
  )
  env$s3_diagnosis_code_dictionary_df <- tibble(
    cCode = c("A", "B", "C", "B"), cDesc = c("Fr A", "Fr B", "Fr C", "Fr B"),
    file = c("dir1", "dir1", "dir2", "dir2")
  )
  matches <- run("s3_diagnosis_code_matches_df")
  stopifnot(!any(matches$diag_var == "V3"), !any(matches$diag_var == "V2" & matches$cDesc == "Fr A"))
  french <- run("s3_diagnosis_variable_code_french_df")
  expected <- tibble(diag_var = c("V1", "V2"),
                    cCode_description_fr = c("Fr A + Fr B (wave1), Fr C (wave2)", "Fr B"))
  stopifnot(isTRUE(all.equal(arrange(french, diag_var), expected)))
  env$s3_diagnosis_code_translation_lookup_df <- tibble(
    french = expected$cCode_description_fr, english = c("Archived combined text", "Archived B")
  )
  translated <- run("s3_diagnosis_variable_code_descriptions_df")
  stopifnot(setequal(translated$cCode_description_en, c("Archived combined text", "Archived B")))

  # Coverage gaps are inspectable; the public dictionary target refuses missing pairs.
  env$s3_diagnosis_dictionary_final_df <- tibble(
    var = "V1", cCode_description_fr = "Unknown", cCode_description_en = NA_character_,
    wave3_table_desc_fr = NA_character_, wave3_table_desc_en = NA_character_
  )
  gaps <- run("s3_diagnosis_translation_gaps_df")
  stopifnot(nrow(gaps) == 1L, gaps$field == "diagnostic_code")
  must_error(run("s3_diagnosis_dictionary_df"))
  env$s3_diagnosis_dictionary_final_df$cCode_description_en <- "Known"
  run("s3_diagnosis_translation_gaps_df")
  stopifnot(nrow(run("s3_diagnosis_dictionary_df")) == 1L)

  # Availability follows expression missingness and grouping remains the legacy cut.
  env$s3_diagnosis_dictionary_age_corrected_df <- tibble(
    var = paste0("V", 1:4), description_fr = "French", description_en = "English",
    cCode_description_en = NA_character_,
    wave1_colname = c("A", NA, "B", NA), wave2_colname = c(NA, "C", "D", NA),
    wave3A_colname = c(NA, "X1", "X2", NA), wave3B_colname = c(NA, NA, "X3", NA)
  )
  described <- run("s3_diagnosis_dictionary_described_df")
  stopifnot(identical(described$waves_available, c("1", "2, 3A", "1, 2, 3A, 3B", "")),
            identical(described$filename, paste0("file", as.numeric(as.factor(cut(1:4, 40))))))

  # Schema validation reports both reuse and repeated summands instead of repairing them.
  definitions <- tibble(var = c("V1", "V2"), wave1_colname = c("A + A", "A + A"),
                        wave2_colname = NA_character_, wave3A_colname = c("X1", "X999"),
                        wave3B_colname = NA_character_)
  issues <- env$s3_diagnosis_validate_fn(
    definitions, tibble(w3_colname = "X1"), tibble(Column.Name = "A")
  )
  stopifnot(setequal(issues$issue, c("reused_expression", "repeated_summand", "unknown_source_column")))

  # Exercise the real Parquet write command with a temporary destination.
  env$s3_diagnosis_dictionary_df <- tibble(var = c("V1", "V2"), description_fr = c("Santé", NA))
  command <- assignments$s3_diagnosis_dictionary_parquet
  output <- file.path(scratch, "dictionary.parquet")
  command[[2L]] <- substitute(path <- destination, list(destination = output))
  result <- eval(command, env)
  stats <- env$s3_diagnosis_parquet_stats_fn(result)
  stopifnot(stats$rows == 2, stats$columns == 2L, stats$bytes > 0,
            isTRUE(all.equal(read_parquet_duckdb(result, prudence = "stingy") |> collect(),
                             env$s3_diagnosis_dictionary_df)))

  # Verify bounded archive readers and positional schema numbering with tiny files.
  csv <- file.path(scratch, "diar_pneu_fix.csv")
  writeLines(c(
    "cNom,cCode,Variable,Value",
    '"Santé, clinic",Diarrhea,old_under5,1',
    '"Santé, clinic",Diarrhea,old_under5,2',
    '"Sœurs",Pneumonia,new_over5,not_numeric'
  ), csv, useBytes = TRUE)
  # The selected identity fields are ASCII; exercise CP1252 decoding of the
  # ignored name column and do not let mixed measurement types drive inference.
  bytes <- readBin(csv, "raw", n = file.info(csv)$size)
  writeBin(charToRaw(iconv(rawToChar(bytes), from = "UTF-8", to = "CP1252")), csv)
  anemia <- file.path(scratch, "anemia.csv")
  writeLines(c("organisationunitcode,organisationunitdescription,existing,anemie?",
               "id,clinic,1,2"), anemia)
  archive <- file.path(scratch, "inputs.zip")
  status <- system2("python3", c("-c", shQuote(paste(
    "import sys, zipfile; z=zipfile.ZipFile(sys.argv[1], 'w');",
    "z.write(sys.argv[2], 'bundle/data/raw/diar_pneu_fix.csv');",
    "z.write(sys.argv[3], 'bundle/data/raw/anemia.csv'); z.close()"
  )), shQuote(archive), shQuote(csv), shQuote(anemia)))
  stopifnot(status == 0L)
  env$s2_clinic_months_raw_zip <- archive
  env$s3_dictionary_archive_members_chr <- c(
    diar_pneu = env$s3_dictionary_archive_member_fn(archive, "data/raw/diar_pneu_fix.csv"),
    anemia = env$s3_dictionary_archive_member_fn(archive, "data/raw/anemia.csv")
  )
  definitions <- run("s3_diagnosis_diar_pneu_variables_df")
  stopifnot(isTRUE(all.equal(definitions, tibble(
    cCode = c("Diarrhea", "Pneumonia"), Variable = c("old_under5", "new_over5")
  ))))
  columns <- run("s3_w3_anemia_columns_chr")
  stopifnot(identical(columns, c("existing", "anemie≥")))
  upstream <- file.path(scratch, "upstream.parquet")
  compute_parquet(as_duckdb_tibble(tibble(existing = 1, other = 2)), upstream)
  env$s0_clinic_RMA_W3_clean_parquet <- upstream
  schema <- run("s3_w3_column_dictionary_df")
  stopifnot(identical(schema$w3_description, c("existing", "other", "anemie≥")),
            identical(schema$w3_colname, c("X1", "X2", "X3")),
            identical(schema$orig, c(TRUE, TRUE, FALSE)))
  must_error(env$s3_dictionary_archive_read_fn(archive, "../outside.csv", readLines))
  must_error(env$s3_dictionary_archive_member_fn(archive, "missing.csv"))

  message("Dictionary logic verified: legacy aggregation, notation, aliases, archived translation coverage, schema validation, and Parquet metadata/round trip.")
})
