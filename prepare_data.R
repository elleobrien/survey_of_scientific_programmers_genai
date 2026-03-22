# prepare_data.R
# Reads the deidentified survey CSV and applies all transformations needed by the
# report_* and productivity_factors scripts. After sourcing this file, a single 
# data frame `survey_df` is available containing all raw + derived variables.

library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# 1. Read deidentified data
# ---------------------------------------------------------------------------
survey_df <- read.csv("data/processed/survey_deidentified.csv",
                      stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# 2. Ordered factor levels (from transform_demo_vars, transform_dev_vars,
#    transform_ai_vars)
# ---------------------------------------------------------------------------

# -- Demographics --
survey_df$program_freq <- factor(
  survey_df$program_freq,
  levels  = c("Less than once a month", "Monthly", "Weekly", "Daily"),
  ordered = TRUE
)

# Unordered demographic factors
for (var in c("organization_type_choice", "country", "gender", "research_area")) {
  survey_df[[var]] <- as.factor(survey_df[[var]])
}

# -- Development practices (9 vars) --
practice_levels <- c("Never", "Sometimes", "About half the time",
                     "Most of the time", "Always")

practice_vars <- c("code_publishing", "code_reuse_inner", "code_reuse_outer",
                   "devel_practices_freq_version", "devel_practices_freq_review",
                   "devel_practices_freq_ci", "devel_practices_freq_unit",
                   "devel_practices_freq_regression", "devel_practices_freq_system")

for (var in practice_vars) {
  survey_df[[var]] <- factor(survey_df[[var]],
                             levels  = practice_levels,
                             ordered = TRUE)
}

# Unordered dev factor
survey_df$code_reviewer_choice <- as.factor(survey_df$code_reviewer_choice)

# -- GenAI variables --
usage_levels <- c("I've never tried", "Tried but gave up",
                  "Sometimes", "Most of the time", "Always")
survey_df$genai_tool_freq <- factor(survey_df$genai_tool_freq,
                                    levels  = usage_levels,
                                    ordered = TRUE)

lines_accepted_levels <- c(
  "I don't use genAI to produce code suggestions directly",
  "<1 line", "1-5 lines", "5-10 lines",
  "10-50 lines", "50-100 lines", ">100 lines"
)
survey_df$genai_lines_accepted <- factor(survey_df$genai_lines_accepted,
                                         levels  = lines_accepted_levels,
                                         ordered = TRUE)

space_levels <- c("Strongly disagree", "Somewhat disagree",
                  "Neither agree nor disagree", "Somewhat agree",
                  "Strongly agree")
space_vars <- grep("^space_", names(survey_df), value = TRUE)
for (var in space_vars) {
  survey_df[[var]] <- factor(survey_df[[var]],
                             levels  = space_levels,
                             ordered = TRUE)
}

survey_df$genai_primary_tool_choice <- as.factor(survey_df$genai_primary_tool_choice)

# ---------------------------------------------------------------------------
# 3. Derived demographic variables (from transform_demo_vars)
# ---------------------------------------------------------------------------

# Numeric coercion
survey_df$years_research_exp <- as.numeric(survey_df$years_research_exp)
survey_df$years_program_exp  <- as.numeric(survey_df$years_program_exp)

# Log-transformed experience
survey_df$logyears_research_exp <- log10(survey_df$years_research_exp + 1)
survey_df$logyears_program_exp  <- log10(survey_df$years_program_exp + 1)

# Consolidate small-n research areas to "Other"
small_n_areas <- survey_df %>%
  group_by(research_area) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count <= 20) %>%
  pull(research_area)

survey_df$research_area_major <- as.character(survey_df$research_area)
survey_df$research_area_major[survey_df$research_area %in% small_n_areas] <- "Other"

# Position recode: the deidentified data does not include free-text position
# responses, so we use current_position_choice directly. This matches the
# original recode for all respondents who selected a standard category;
# "Other (self-describe)" entries that were manually recoded in the original
# pipeline will remain as "Other (self-describe)" here.
survey_df$current_position_recode <- survey_df$current_position_choice

# ---------------------------------------------------------------------------
# 4. Derived development-practice variables (from transform_dev_vars)
# ---------------------------------------------------------------------------

# Familiarity booleans (from the multi-select devel_practices field)
survey_df$version_familiar <- ifelse(is.na(survey_df$devel_practices), NA,
                                     grepl("Version control", survey_df$devel_practices))
survey_df$testing_familiar  <- ifelse(is.na(survey_df$devel_practices), NA,
                                     grepl("Code testing", survey_df$devel_practices))
survey_df$review_familiar   <- ifelse(is.na(survey_df$devel_practices), NA,
                                     grepl("Code review", survey_df$devel_practices))
survey_df$ci_familiar       <- ifelse(is.na(survey_df$devel_practices), NA,
                                     grepl("Continuous integration", survey_df$devel_practices))

# Usage scores (0-5): 0 = unfamiliar, 1-5 = frequency levels
survey_df <- survey_df %>%
  mutate(
    version_score    = ifelse(version_familiar == FALSE, 0,
                              as.numeric(factor(devel_practices_freq_version, levels = practice_levels))),
    review_score     = ifelse(review_familiar == FALSE, 0,
                              as.numeric(factor(devel_practices_freq_review, levels = practice_levels))),
    ci_score         = ifelse(ci_familiar == FALSE, 0,
                              as.numeric(factor(devel_practices_freq_ci, levels = practice_levels))),
    unit_score       = ifelse(testing_familiar == FALSE, 0,
                              as.numeric(factor(devel_practices_freq_unit, levels = practice_levels))),
    regression_score = ifelse(testing_familiar == FALSE, 0,
                              as.numeric(factor(devel_practices_freq_regression, levels = practice_levels))),
    system_score     = ifelse(testing_familiar == FALSE, 0,
                              as.numeric(factor(devel_practices_freq_system, levels = practice_levels)))
  ) %>%
  mutate(across(ends_with("_score"), as.ordered))

# Composite development-practice score
survey_df$dev_score <- survey_df %>%
  select(ends_with("_score")) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  rowMeans(na.rm = TRUE)
survey_df$dev_score <- survey_df$dev_score - 1

# ---------------------------------------------------------------------------
# 5. Derived GenAI variables (from transform_ai_vars)
# ---------------------------------------------------------------------------

# Adoption flags
survey_df <- survey_df %>%
  mutate(
    reports_no_adoption = genai_tool_freq %in% c("I've never tried", "Tried but gave up"),
    lists_no_tools_tried = is.na(genai_tool_exp_choice)
  )

# SPACE composite average
survey_df$space_avg <- survey_df %>%
  select(all_of(space_vars)) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  rowMeans(na.rm = TRUE)

# Robustness check: SPACE average without the A dimension
survey_df$spce_avg <- survey_df %>%
  select(all_of(space_vars)) %>%
  select(-space_a1) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>%
  rowMeans(na.rm = TRUE)

# ---------------------------------------------------------------------------
# 6. Parse genai_tool_exp_choice into binary tried_* columns
#    (replaces loading genai_tools.rds)
# ---------------------------------------------------------------------------
tool_patterns <- list(
  tried_chatgpt          = "ChatGPT",
  tried_org_tool         = "A custom tool provided by organization",
  tried_github_copilot   = "GitHub Copilot",
  tried_google_gemini    = "Google Gemini",
  tried_cursor           = "Cursor",
  tried_tabnine          = "TabNine",
  tried_claude           = "Claude",
  tried_claude_code      = "Claude Code",
  tried_perplexity       = "Perplexity",
  tried_microsoft_copilot = "Microsoft Copilot"
)

for (col_name in names(tool_patterns)) {
  pattern <- tool_patterns[[col_name]]
  # Use exact word-boundary matching to avoid e.g. "Claude" matching "Claude Code"
  if (pattern == "Claude") {
    # Match "Claude" but not "Claude Code"
    survey_df[[col_name]] <- grepl("\\bClaude\\b", survey_df$genai_tool_exp_choice) &
                             !grepl("Claude Code", survey_df$genai_tool_exp_choice)
    # But also TRUE if BOTH Claude and Claude Code are listed
    has_claude_code <- grepl("Claude Code", survey_df$genai_tool_exp_choice)
    # Check if "Claude" appears as a separate entry (comma-separated)
    survey_df[[col_name]] <- grepl("(^|,)Claude(,|$)", survey_df$genai_tool_exp_choice)
  } else {
    survey_df[[col_name]] <- grepl(pattern, survey_df$genai_tool_exp_choice, fixed = TRUE)
  }
  # NAs in genai_tool_exp_choice → FALSE
  survey_df[[col_name]][is.na(survey_df$genai_tool_exp_choice)] <- FALSE
}

# ---------------------------------------------------------------------------
# 7. Programming language binary columns (from transform_dev_vars)
# ---------------------------------------------------------------------------
language_list <- c("Python", "R", "C", "C\\+\\+", "MATLAB", "Stata", "FORTRAN",
                   "Rust", "Julia", "JavaScript", "Java", "Bash", "Other")

for (lang in language_list) {
  lang_col <- paste0("uses_", tolower(lang))
  if (lang == "C\\+\\+") lang_col <- "uses_cpp"

  if (lang == "Java") {
    survey_df[[lang_col]] <- grepl("Java,", survey_df$program_lang_choice) |
                             grepl("Java$", survey_df$program_lang_choice)
  } else if (lang == "C") {
    survey_df[[lang_col]] <- grepl("C,", survey_df$program_lang_choice) |
                             grepl("C$", survey_df$program_lang_choice)
  } else {
    survey_df[[lang_col]] <- grepl(lang, survey_df$program_lang_choice)
  }
}
