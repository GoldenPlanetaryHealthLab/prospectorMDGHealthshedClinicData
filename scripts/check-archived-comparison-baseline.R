# Read-only ZIP baseline verification; run from the repository root through the runner.
# Extracts only W3, dictionary, and tracker into session scratch (~2 GB).
local({
  baseline <- dget("src/archived-comparison-baseline.R")
  sha256 <- function(path) {
    stopifnot(file.exists(path), nzchar(Sys.which("sha256sum")))
    result <- system2("sha256sum", shQuote(path), stdout = TRUE)
    stopifnot(is.null(attr(result, "status")), length(result) == 1L)
    substr(result, 1L, 64L)
  }
  stopifnot(identical(sha256(baseline$archive_zip), baseline$archive_sha256))
  listing <- utils::unzip(baseline$archive_zip, list = TRUE)
  fields <- grep("_member$", names(baseline), value = TRUE)
  members <- unlist(baseline[setdiff(fields, "diagnosis_directory_member")], use.names = FALSE)
  stopifnot(all(members %in% listing$Name),
            !any(grepl("gdrive|rclone", unlist(baseline), ignore.case = TRUE)))
  scratch <- tempfile("archived-baseline-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  selected <- c(baseline$wave3_archive_member, baseline$dictionary_member,
                baseline$reporting_tracker_member)
  paths <- utils::unzip(baseline$archive_zip, files = selected, exdir = scratch)
  stopifnot(identical(vapply(paths, sha256, "", USE.NAMES = FALSE),
                      c(baseline$wave3_sha256, baseline$dictionary_sha256,
                        baseline$reporting_tracker_sha256)))
  dictionary <- read.csv(paths[[2L]], check.names = FALSE)
  groups <- paste0("file", 1:40)
  stopifnot(nrow(dictionary) == 3595L, setequal(dictionary$filename, groups))
  expected <- paste0(baseline$diagnosis_directory_member,
                    c(paste0(groups, ".feather"), paste0(groups, "_wrong_fname.feather"),
                      "clinic_reporting_track.feather"))
  actual <- listing$Name[startsWith(listing$Name, baseline$diagnosis_directory_member) &
                          endsWith(listing$Name, ".feather")]
  stopifnot(setequal(expected, actual))
  w3_versions <- listing[grepl("/wave3_with_keys[^/]*[.]csv$", listing$Name) &
                           startsWith(listing$Name, "madagascar_clinic_data_cleaning/"), ]
  stopifnot(listing$Date[match(baseline$wave3_archive_member, listing$Name)] ==
              max(w3_versions$Date))
  print(listing[match(members, listing$Name), c("Name", "Length", "Date")], row.names = FALSE)
  message("Verified the hand-delivered May 2025 ZIP SHA-256, W3/dictionary/tracker hashes, ",
          "and its 81-file May 4 diagnosis family. All references are members of one ZIP. ",
          "This does not establish measurement equality or one synchronized run.")
})
