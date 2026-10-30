# ============================================================================
# DATA PIPELINE EXECUTION INTERFACE: DJFMP
# ============================================================================
# Load utilities and core helper engines
devtools::load_all()

# 1. Establish Environment Context Path
SURVEY <- "DJFMP"
CONFIG_PATH <- file.path("data-raw", SURVEY, paste0(SURVEY, "_config.yml"))

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
# STEP 2: Import, Standardize, & Clean (Execute / Debug)
# ----------------------------------------------------------------------------
# Choose your pipeline execution stop boundary via `interactive_until`:
#  * "raw"         -> Halts post-import and returns `ls_raw` (list of raw datasets)
#  * "standardize" -> Halts post-standardization and returns `ls_standardized` (list of standardized datasets)
#  * "custom"      -> Halts after running `cleaner_fn` and returns `df_custom_data` (single tibble)
#  * "full"        -> Executes completely and returns list(final_data, ls_edi_info)

# --- WORKFLOW A: INTERACTIVE CLEANER DEVELOPMENT / DEBUGGING ---
# Step 2a: Pull standardized data inputs to step through `DJFMP_custom_cleaning.R` line-by-line
ls_std <- run_survey_pipeline(CONFIG_PATH, interactive_until = "standardize")

# View individual standardized data tables in the console / viewer:
# View(ls_std[[1]])

# Step 2b: Test the output of your custom cleaner function directly before final general cleaning
# steps:
df_custom <- run_survey_pipeline(CONFIG_PATH, interactive_until = "custom")
# View(df_custom)

# --- WORKFLOW B: FULL PIPELINE COMPILATION ---
# Execute the full pipeline once configuration and custom cleaner logic are finalized:
workbench <- run_survey_pipeline(CONFIG_PATH, interactive_until = "full")

# 📊 DATA QUALITY AUDIT PAUSE:
# Open and review the compiled dataset:
View(workbench$final_data)

# If anything in `workbench$final_data` looks incorrect or suspicious:
# 1. Re-run `interactive_until = "standardize"` to inspect raw standardized inputs (`ls_std`)
# 2. Fix 'DJFMP_custom_cleaning.R' or 'DJFMP_config.yml'
# 3. Re-run Step 2

# ----------------------------------------------------------------------------
# STEP 3: Publish, Document, & Update (Publish)
# ----------------------------------------------------------------------------
# Run this final step manually when `workbench$final_data` is validated and ready for publication:
publish_survey_data(
  data_final = workbench$final_data,
  dataset_name = SURVEY,
  config_path = CONFIG_PATH,
  edi_info = workbench$ls_edi_info
)
