library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)
library(stargazer)


rm(list = ls())


df <- readRDS("data/interim/demo_variables.rds")

# What countries are most common?
table(df$country)

# Research area
non_na_count_area <- sum(!is.na(df$research_area))

### Table for manuscript
df %>%
  group_by(research_area) %>%
  summarise(count = n()) %>%
  na.omit(research_area) %>%
  arrange(-count) %>%
  kable(col.names = c("Research Area", "Count"),
        format = "latex", booktabs = TRUE, linesep= "")

################################################################
# Job title
# how many non-na answers did we get? 
non_na_count_job <- sum(!is.na(df$current_position_recode))

### Turn this into a quality figure for appendix
df %>%
  group_by(current_position_recode) %>%
  summarise(count = n()) %>%
  na.omit(current_position_recode) %>%
  ggplot(., aes(x = reorder(current_position_recode, count), y = count)) +
  geom_col() +
  coord_flip() +
  ggtitle("Current position", 
          subtitle = paste0("n = ", non_na_count_job)) +
  xlab("") + 
  ylab("Number of Respondents")

# table version - this is in manuscript appendix
df %>%
  group_by(current_position_recode) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  kable(col.names = c("Current position", "Count"), 
        caption = "Current position of respondents",
        label = "current_position",
        format = "latex", booktabs = TRUE, linesep= "")



################# ORRGANIZATION TYPE ######################
# Organization type table
# how many non-na answers did we get?
non_na_count_org <- sum(!is.na(df$organization_type_choice))

# for manuscript appendix
df %>%
  group_by(organization_type_choice) %>% 
  summarise(count = n())  %>%
  kable(col.names = c("Organization type", "Count"),
        caption = "Organizations where respondents work",
        label = "organization_type",
        format = "latex", booktabs = TRUE, linesep= "")

# gender table
# how many non-na responses to gender question
non_na_count_gender <- sum(!is.na(df$gender))

df %>%
  group_by(gender) %>% 
  summarise(count = n()) %>%
  arrange(-count) %>%
  kable(caption = paste0("n = ", non_na_count_gender, " responses"),
        col.names = c("Gender", "Count")) %>% 
  kable_styling("striped")


### Make nice version for manuscript
# Histogram of years of research experience
px_program_exp <- ggplot(df, aes(x = years_program_exp)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Years of Programming Experience", y = "Count")+
  geom_vline(aes(xintercept = median(years_program_exp, na.rm = TRUE)), 
             color = "red", linetype = "dashed")

ggsave(px_program_exp, 
       filename = "figures/years_program_exp_histogram.png", 
       width = 6, height =4, dpi = 300)


# summary statistics of research years of experience
summary(df$years_research_exp)
summary(df$years_program_exp)

sd(df$years_research_exp, na.rm = TRUE)
sd(df$years_program_exp, na.rm = TRUE)

# What is the correlation between years of programming and years of research experience?
cor.test(df$logyears_program_exp, df$logyears_research_exp, use = "complete.obs", method = "pearson")

# Programming frequency 
non_na_count_program_freq <- sum(!is.na(df$program_freq))
df %>%
  group_by(program_freq) %>%
  summarise(count = n()) %>%
  ggplot(., aes(x = reorder(program_freq, count), y = count)) +
  geom_col() +
  coord_flip() +
  xlab("") + 
  ylab("Number of Respondents") +
  ggtitle("How often do you program as part of your research?",
          subtitle = paste0("n = ", non_na_count_program_freq))

# get percentages of each
df %>%
  group_by(program_freq) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(percentage)) %>%
  kable(caption = paste0("n = ", non_na_count_program_freq, " responses"),
        col.names = c("Programming Frequency", "Count", "Percentage")) %>%
  kable_styling("striped")



# Programming language counts
langs <- readRDS("data/interim/programming_languages.rds")

# Make a chart of languages in descending order
lang_freq <- langs %>%
  select(starts_with("uses_")) %>%
  colSums(.)

# remove the "uses_" prefix for reporting in a table
names(lang_freq) <- gsub("uses_", "", names(lang_freq))

# Table for manuscript
lang_freq %>%
  sort(decreasing = TRUE) %>%
  kable(col.names = c("Language", "Count"), 
        format = "latex", booktabs = TRUE, linesep = "")

# How many lanugages are used per person?
langs_per_person <- langs %>%
  select(starts_with("uses_")) %>%
  rowSums(.)
median(langs_per_person)
