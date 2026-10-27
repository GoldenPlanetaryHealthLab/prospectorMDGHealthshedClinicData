# Read with dget() from the repository root. Selection metadata only.
# Evidence and limits: notebooks/pipeline/mapping/legacy-pipeline-map.qmd,
# section "Most recent archived data comparison baseline" (ZIP-only decision, 2026-11-06).
list(
  label = "most recent archived data within the hand-delivered May 2025 ZIP",
  reviewed_on = "2026-11-06",
  archive_zip = "data/inputs/01_gold_mine/gdn_datafreeze_may2025/madagascar_clinic_data_cleaning.zip",
  archive_sha256 = "cdc9e5534b947551cdaee3fe0ef38f0e46b26c7a7bba539d9c5ff518023061cb",
  wave3_archive_member = "madagascar_clinic_data_cleaning/data/raw/from_Kevin/wave3_with_keys.csv",
  wave3_sha256 = "8e27bed87f8177f0d463910f7fb0fb5cc87ae185eae0433a9ac92166a48b1df0",
  facility_mapping_member = "madagascar_clinic_data_cleaning/data/intermediate/mapping_gesis_dhis2_clean.csv",
  clinic_ids_member = "madagascar_clinic_data_cleaning/data/intermediate/w2010_2019_clinicID.rds",
  dictionary_member = "madagascar_clinic_data_cleaning/data/clean/diagnosis_dictionary_translated.csv",
  dictionary_sha256 = "121048c18ae0a04183fb0c04a42d724ccb04b3ea3966a6e3d7b5c5cfb8682921",
  diagnosis_directory_member = "madagascar_clinic_data_cleaning/data/clean/by_diagnosis_2024/",
  reporting_tracker_member = "madagascar_clinic_data_cleaning/data/clean/by_diagnosis_2024/clinic_reporting_track.feather",
  reporting_tracker_sha256 = "fed6df6bf7219f97d1b9889dc630c83f8eea6878896c938aa4a76c903e932d1d",
  sectorization_member = "madagascar_clinic_data_cleaning/data/intermediate/sectorization2022_cleaned.feather",
  healthsheds_member = "madagascar_clinic_data_cleaning/data/clean/healthsheds2022.rds",
  clinic_locations_member = "madagascar_clinic_data_cleaning/data/intermediate/clinic_LL.rds",
  spatial_metadata_member = "madagascar_clinic_data_cleaning/data/clean/clinic_spatial_metadata.csv",
  reporting_metadata_member = "madagascar_clinic_data_cleaning/data/clean/clinic_reporting_metadata.csv"
)
