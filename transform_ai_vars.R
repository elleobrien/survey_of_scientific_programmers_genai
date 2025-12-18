library(dplyr)
library(polycor)
library(corrplot)

# clear the environment
rm(list = ls()) 

# read in the anonymized, cleaned dataset
df <- read.csv("./data/interim/survey_cleaned.csv")

# extract variables related to AI usage questions
genai_df <- df %>%
  select(ResponseId, genai_tool_exp_choice:use_case_conf_eval)


# Make sure unordered categorical variables are factors
categorical_vars <- c("genai_primary_tool_choice")
genai_df[categorical_vars] <- lapply(genai_df[categorical_vars], as.factor)

# Strip extra text from confidence columns, leaving just numerical rating
conf_vars <- genai_df %>% select(contains("conf_")) %>% names()

genai_df[conf_vars] <- genai_df[conf_vars] %>% 
  mutate(across(everything(), ~ gsub("[^0-9.]", "", .)))

# Make sure ordered categorical variables are factors with levels
space_vars <- genai_df %>% select(starts_with("space_")) %>% names()
lines_vars <- c("genai_lines_accepted")
usage_freq <- c("genai_tool_freq")

all_categorical <- c(categorical_vars, space_vars, lines_vars, usage_freq, conf_vars)
genai_df[all_categorical] <- lapply(genai_df[all_categorical], as.factor)

usage_levels <- c("I've never tried", "Tried but gave up", "Sometimes", "Most of the time", "Always")
space_levels <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
lines_accepted_levels <- c("I don't use genAI to produce code suggestions directly", 
                           "<1 line", "1-5 lines", "5-10 lines", "10-50 lines","50-100 lines",">100 lines")
conf_levels <- c("1", "2", "3", "4", "5")

# Set ordered factors 
genai_df$genai_tool_freq <- factor(genai_df$genai_tool_freq, levels = usage_levels, ordered = TRUE)
genai_df$genai_lines_accepted <- factor(genai_df$genai_lines_accepted, levels = lines_accepted_levels, ordered = TRUE)
genai_df[space_vars] <- lapply(genai_df[space_vars], 
                                function(x) factor(x, levels = space_levels, ordered = TRUE))
genai_df[conf_vars] <- lapply(genai_df[conf_vars], 
                               function(x) factor(x, levels = conf_levels, ordered = TRUE))

# Set adoption-related logical variable
genai_df <- genai_df %>%
  mutate(reports_no_adoption = ifelse(genai_tool_freq %in% c("I've never tried", "Tried but gave up"), TRUE, FALSE)) %>%
  mutate(lists_no_tools_tried = ifelse(is.na(genai_tool_exp_choice), TRUE, FALSE))


# Create a composite "average" space score
genai_df$space_avg <- genai_df %>%
  select(starts_with("space_")) %>% # convert to numeric and then calculate rowmeans
  mutate(across(everything(), ~ as.numeric(.))) %>%
  rowMeans(na.rm = TRUE) 

# For robustness check- remove the "A" and recompute the average
genai_df$spce_avg <- genai_df %>%
  select(starts_with("space_")) %>%
  select(-c(space_a1,space_avg)) %>% #janky, I'm sorry
  mutate(across(everything(), ~ as.numeric(.))) %>%
  rowMeans(na.rm = TRUE)


cor_df <- genai_df %>%
  select(starts_with("space_"))

cor_matrix <- hetcor(cor_df, ML = TRUE, use = "pairwise.complete.obs")

# use cormat to visualize

png(filename = "./figures/genai_space_correlation_matrix.png", 
    width = 8, height = 6, units = "in", res = 300)
corrplot(cor_matrix$correlations, 
         type = "lower",
         method = "color",
         tl.col = "black", 
         # include correlation coefficients
         addCoef.col = "white",
         tl.srt = 45, 
         number.cex = 0.7)
dev.off()

# Write to file
write.csv(genai_df, "./data/interim/genai_variables.csv", row.names = FALSE)
# also write to rds
saveRDS(genai_df, "./data/interim/genai_variables.rds")


