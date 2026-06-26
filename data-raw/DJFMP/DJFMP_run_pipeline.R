# ============================================================================
# DATA PIPELINE EXECUTION INTERFACE: DJFMP
# ============================================================================
# Load utilities and core helper engines
devtools::load_all()

# 1. Establish Environment Context Path
SURVEY_NAME <- "DJFMP"
CONFIG_PATH <- file.path(
  "data-raw",
  SURVEY_NAME,
  paste0(SURVEY_NAME, "_config.yml")
)

# ----------------------------------------------------------------------------
# STEP 1: Pre-Flight Metadata Summary (Review)
# ----------------------------------------------------------------------------
# Run this first to print the custom metadata summary dashboard
run_pipeline_preflight(CONFIG_PATH)

# 🔍 CHECKPOINT PAUSE:
# Read the console printout. If the timezones, data paths, or units look wrong, or if any
# parameters need to be removed or added, stop here, fix the 'DJFMP_config.yml' file in
# this folder, and re-run Step 1.

# ----------------------------------------------------------------------------
# STEP 2: Import, Standardize, & Clean (Execute)
# ----------------------------------------------------------------------------
# Run this only when you are certain the configuration rules are correct.
workbench <- run_survey_pipeline(CONFIG_PATH)

# 📊 DATA QUALITY AUDIT PAUSE:
# Open and review the final data set:
View(workbench$final_data)

# If anything looks incorrect or suspicious:
# 1. Start by looking at the custom cleaning script 'DJFMP_custom_cleaning.R' in this folder
# 2. Step through it using 'workbench$ls_standardized' to locate the what's causing the issue
# 3. Fix the the custom cleaning script and re-run Step 2.

# NOTE: workbench$ls_standardized contains the standardized data inputs before combining and
# custom cleaning. Run this to view them:
workbench$ls_standardized

# ----------------------------------------------------------------------------
# STEP 3: Publish, Document, & Update (Publish)
# ----------------------------------------------------------------------------
# Run this final step manually when the final_data within the workbench is ready to be published.
publish_survey_data(
  data_final = workbench$final_data,
  dataset_name = SURVEY_NAME,
  config_path = CONFIG_PATH,
  edi_info = workbench$ls_edi_info
)
