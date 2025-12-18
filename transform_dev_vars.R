library(dplyr)

# clear the environment
rm(list = ls())

# read in the anonymized, cleaned dataset
df <- read.csv("./data/interim/survey_cleaned.csv")

# extract variables related to development practices- starting from "program_lang_choice" to
# "code_reviewer_text"
# select column names in the ranges "program_lang_choice" to "code_reviewer_text"
dev_df <- df %>%
  select(ResponseId, program_lang_choice:code_reviewer_text)

# Make sure unordered categorical variables are factors
categorical_vars <- c("code_reviewer_choice")
dev_df[categorical_vars] <- lapply(dev_df[categorical_vars], as.factor)

# Make sure ordered categorical variables are factors with levels
ordered_vars <- c("code_publishing", "code_reuse_inner", "code_reuse_outer",
                  "devel_practices_freq_version","devel_practices_freq_review",
                  "devel_practices_freq_ci", "devel_practices_freq_unit", 
                  "devel_practices_freq_regression", "devel_practices_freq_system")

practice_levels <- c("Never", "Sometimes", "About half the time", "Most of the time", "Always")

for (var in ordered_vars) {
  dev_df[[var]] <- factor(dev_df[[var]], 
                           levels = practice_levels, 
                           ordered = TRUE)
}


#######################
## Familiarity with development practices
######################
# Create a boolean "familiar" variable for the development practice concepts.
# If a participant entered nothing (NA) for a question, leave as NA instead of assigning boolean. 
dev_df$version_familiar <- ifelse(is.na(dev_df$devel_practices), NA, grepl("Version control",dev_df$devel_practices))
dev_df$testing_familiar <- ifelse(is.na(dev_df$devel_practices), NA, grepl("Code testing",dev_df$devel_practices))
dev_df$review_familiar <- ifelse(is.na(dev_df$devel_practices), NA, grepl("Code review",dev_df$devel_practices))
dev_df$ci_familiar <- ifelse(is.na(dev_df$devel_practices),NA, grepl("Continuous integration",dev_df$devel_practices))

######### Create usage scores. ##############
# Convert ordered factors to numeric (1-5) and then add a new 0 value 0 is assigned
# if the user was not familiar with the practice. Finally, convert BACK to an ordered factor.
# If there was a more graceful way to do this I couldn't find it. 

dev_df <- dev_df %>%
  mutate(across(starts_with("devel_practices_freq"), ~ as.numeric(factor(., 
                                                                         levels = practice_levels,
  )))) %>%
  mutate(version_score = ifelse(version_familiar == FALSE, 0, devel_practices_freq_version)) %>%
  mutate(review_score = ifelse(review_familiar == FALSE, 0, devel_practices_freq_review)) %>%
  mutate(ci_score = ifelse(ci_familiar == FALSE, 0, devel_practices_freq_ci)) %>%
  mutate(unit_score = ifelse(testing_familiar == FALSE, 0, devel_practices_freq_unit),
         regression_score= ifelse(testing_familiar == FALSE, 0, devel_practices_freq_regression),
         system_score= ifelse(testing_familiar == FALSE, 0, devel_practices_freq_system)) %>%
  mutate(across(ends_with("_score"), as.ordered))


# Make one composite score for development practices. 
# There are not an overwhelming number of NAs in the "How often do you use X practice" questions,
# but they still need to be handled to make a composite score. Using the rowMeans with na.rm = TRUE
# reflects an assumption that there's no systematic bias in who skips a question w.r.t. practice usage.
dev_df$dev_score <- dev_df %>%
  select(ends_with("_score")) %>%
  mutate(across(everything(), ~ as.numeric(.))) %>% # note that this introduces shift- 0 factor becomes 1, etc. 
  rowMeans(na.rm = TRUE) 
# shift down by 1 so that a score of 0 is unfamiliar with anything
dev_df$dev_score <- dev_df$dev_score - 1

# write to file
write.csv(dev_df, "./data/interim/dev_variables.csv", row.names = FALSE)
# also write an RDS
saveRDS(dev_df, "./data/interim/dev_variables.rds")

# get correlations between all the _scores
library(polycor)
cor_df <- dev_df %>%
  select(ends_with("_score"))

cor_matrix <- hetcor(cor_df, ML = TRUE, use = "pairwise.complete.obs")
min(cor_matrix$correlations)
max(cor_matrix$correlations) 
# use cormat to visualize
library(corrplot)

png(file = "figures/dev_practices_correlation_matrix.png",
    width = 6, height = 6, units = "in", res = 300)
corrplot(cor_matrix$correlations, 
         type = "lower",
         method = "color",
         tl.col = "black", 
         # include correlation coefficients
         addCoef.col = "white",
         tl.srt = 45, 
         number.cex = 0.7)
dev.off()


#####################
# Convert programming language useage to a wide format
#####################
language_list <- c("Python", "R", "C", "C\\+\\+", "MATLAB", "Stata", "FORTRAN", "Rust",
                   "Julia", "JavaScript", "Java", "Bash", "Other")
lang_df <- df %>%
  select(ResponseId, program_lang_choice)

# for each language in language list, use grepl() to make a new variable just for that language
for (lang in language_list) {
  lang_col <- paste0("uses_", tolower(lang))
  if (lang == "C\\+\\+") {
    lang_col <- "uses_cpp"  
  }
  if (lang == "Java"){
    lang_df[[lang_col]] <- grepl("Java,", lang_df$program_lang_choice) | grepl("Java$", lang_df$program_lang_choice)
  } else if (lang == "C"){
    lang_df[[lang_col]] <- grepl("C,", lang_df$program_lang_choice) | grepl("C$", lang_df$program_lang_choice)
  }
  else {
  lang_df[[lang_col]] <- grepl(lang, lang_df$program_lang_choice)
}}


# save language dataframe to own file
write.csv(lang_df, "./data/interim/programming_languages.csv", row.names = FALSE)
# also write as RDS
saveRDS(lang_df, "./data/interim/programming_languages.rds")

