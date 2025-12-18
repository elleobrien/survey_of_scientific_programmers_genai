# report_adoption
library(dplyr)
library(ggplot2)
library(kableExtra)
library(polycor)
library(stargazer)

rm(list =ls())

# Reporting on adoption
adopt_df <- readRDS("data/interim/genai_variables.rds")

df <- readRDS("data/interim/demo_variables.rds")


# How many respondents answered at all about genai usage?
# This should be made nice for the manuscript
px_adopt<- adopt_df %>%
  filter(!is.na(genai_tool_freq)) %>%
  ggplot(., aes(x = genai_tool_freq)) +
  geom_bar()+
  xlab("") +
  ylab("Count")+
  ggtitle("How often do you use a genAI tool in your research-related\nprogramming?",
          subtitle = paste0("n = ", sum(!is.na(adopt_df$genai_tool_freq))))
px_adopt
ggsave(px_adopt, filename = "figures/genai_adoption_barchart.png",
       dpi = 300,
       width = 6, height =4)

table(adopt_df$genai_tool_freq)

# Does adoption % differ by field? 
adopt_df <- adopt_df %>%
  left_join(., df %>% select(ResponseId, research_area_major, years_program_exp, logyears_program_exp, current_position_recode), by = "ResponseId")


# Add line breaks to very long levels in "research_area_major" before plotting
adopt_df$research_area_major <- gsub("Geosciences, atmospheric sciences & ocean sciences", "Geosciences, atmospheric sciences\n& ocean sciences", adopt_df$research_area_major)

# make a stacked bar chart of adoption rate by research area
# make this nice for the manuscript
px_adopt_by_field <- adopt_df %>%
  filter(!is.na(genai_tool_freq)) %>%
  group_by(research_area_major, genai_tool_freq) %>%
  summarise(count = n()) %>%
  group_by(research_area_major) %>%
  mutate(prop = count / sum(count)) %>%
  ggplot(., aes(x = research_area_major, y = prop, fill = genai_tool_freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Proportion of respondents") +
  ggtitle("How often do you use a genAI tool in your\nresearch-related programming?") +
  # make all text size larger for readability
  theme(text = element_text(size = 16)) +
  scale_fill_brewer(palette = "PRGn", name = "")+
  geom_text(aes(label = count), position=position_fill(vjust=0.5), size=4)

px_adopt_by_field

ggsave(px_adopt_by_field, filename = "figures/genai_adoption_by_field.png",
       dpi = 300,
       height =8, width = 10)


# Is there a significant difference by research area? omnibus test and report
# print out standard reporting of F and p value
anova_res <- anova(lm(as.numeric(genai_tool_freq) ~ research_area_major, data = adopt_df))
paste("ANOVA results: F(", anova_res$Df[1], ",", anova_res$Df[2], ") = ", round(anova_res$`F value`[1], 2), ", p = ", signif(anova_res$`Pr(>F)`[1], 3), "\n", sep = "")

###### Research role effects
px_adopt_by_role <- adopt_df %>%
  filter(!is.na(genai_tool_freq)) %>%
  filter(!(current_position_recode == "Other (self-describe)")) %>% # category small for stable estimation, so drop it for visual
  group_by(current_position_recode, genai_tool_freq) %>%
  summarise(count = n()) %>%
  group_by(current_position_recode) %>%
  mutate(prop = count / sum(count)) %>%
  ggplot(., aes(x = current_position_recode, y = prop, fill = genai_tool_freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(text = element_text(size = 16)) +
  xlab("") +
  ylab("Proportion of respondents") +
  ggtitle("How often do you use a genAI tool in your research-related
programming?") +
  scale_fill_brewer(palette = "PRGn", name = "")+
  geom_text(aes(label = count), position=position_fill(vjust=0.5), size=4) 

px_adopt_by_role
ggsave(px_adopt_by_role, filename = "figures/genai_adoption_by_role.png",
       dpi = 300,
       height =8, width = 10)


# print out standard reporting of F and p value
anova_res <- anova(lm(as.numeric(genai_tool_freq) ~ current_position_recode, data = adopt_df))
paste("ANOVA results: F(", anova_res$Df[1], ",", anova_res$Df[2], ") = ", round(anova_res$`F value`[1], 2), ", p = ", signif(anova_res$`Pr(>F)`[1], 3), "\n", sep = "")
lm(as.numeric(genai_tool_freq) ~ current_position_recode, data = adopt_df) %>%
  summary()

########### Years of programming experience
# Does adoption differ by years of programming experience?
px_adopt_by_exp <- adopt_df %>%
  subset(!is.na(genai_tool_freq))%>% 
  ggplot(., aes(x = genai_tool_freq, y = logyears_program_exp)) +
  geom_boxplot() +
  xlab("How often do you use a genAI tool in your\nresearch-related programming?") +
  ylab("Log-years of programming experience")
px_adopt_by_exp
ggsave(px_adopt_by_exp, filename = "figures/genai_adoption_by_programming_exp.png",
       dpi = 300,
       height =5, width = 7)

# test significance
poly_tool_freq <- polyserial(adopt_df$logyears_program_exp, adopt_df$genai_tool_freq, ML = TRUE, std.err = TRUE)

# should put this in a util file somewhere because I use it somewhere else. sorry, technical debt. 
poly_to_pvalue <- function(polyserial_output) {
std.err <- sqrt(polyserial_output$var[1, 1]) 
p_value <- 2 * pnorm(-abs(polyserial_output$rho / std.err))
return(p_value)
}

poly_to_pvalue(poly_tool_freq)

########## Gender difference
# merge in gender info from demo_df
demo_df <- readRDS("data/interim/demo_variables.rds")
adopt_df <- adopt_df %>%
  left_join(demo_df %>% select(ResponseId, gender), by = "ResponseId")

gender_lm <- lm(as.numeric(genai_tool_freq) ~ gender, data = subset(adopt_df, gender != "Prefer not to say")) 
gender_anova_res <- anova(gender_lm)
paste("ANOVA results: F(", gender_anova_res$Df[1], ",", gender_anova_res$Df[2], ") = ", round(gender_anova_res$`F value`[1], 2), ", p = ", signif(gender_anova_res$`Pr(>F)`[1], 3), "\n", sep = "")

# Report main effect associated with gender == woman
gender_lm %>% summary()

stargazer(gender_lm, title = "Adoption and gender", align = TRUE)

 ################### gender################### TOOL CHOICE #########################
# What tools are being used?
genai_tools <- readRDS("data/interim/genai_tools.rds")

# Median number of tools tried? 
tools_per_person <- genai_tools %>%
  select(starts_with("tried_")) %>%
  rowSums(.)
median(tools_per_person)

# What about actual tool CHOSEN? 
genai_tool_choice <- adopt_df %>%
  select(ResponseId, genai_primary_tool_choice) %>%
  filter(!is.na(genai_primary_tool_choice)) %>%
  group_by(genai_primary_tool_choice) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Make nice for manuscript 
genai_tool_choice %>%
  kable(caption = "Primary tool (for adopters)", 
        col.names = c("Tool", "Count"), 
        format = "latex", booktabs = TRUE, linesep = "",
        label = "primary_tool_choice")


##### Developer tool vs. general purpose tool #####
is_dev_tool <- c("Claude Code", "Cursor", "GitHub Copilot")
is_general_tool <- c("ChatGPT", "Google Gemini", "Claude", "Microsoft Copilot", "Perplexity")
adopt_df$is_dev_tool <- adopt_df$genai_primary_tool_choice %in% is_dev_tool
adopt_df$is_general_tool <- adopt_df$genai_primary_tool_choice %in% is_general_tool
table(adopt_df$is_dev_tool)
table(adopt_df$is_general_tool)


# As a validity check- we should expect that in-ide tools are most likely to be developer tools by
# the classification scheme. so counts should be close to similar (if not entirely identical; someone
# could reasonably say Cursor or Claude Code is a desktop app rather than a plugin for their IDE).
adopt_df$accessed_web <- grepl("browser", adopt_df$genai_access_mode_choice)
adopt_df$accessed_in_dev_env <- grepl("VS Code", adopt_df$genai_access_mode_choice)
table(adopt_df$accessed_web)
table(adopt_df$accessed_in_dev_env)

# mutate to add a variable that classifies tool type- if genai_primary_tool_choice is in the list is_dev_tool, label dev_tool,
# if in is_general_tool, label general_tool, else label other/unknown
adopt_df <- adopt_df %>%
  mutate(tool_type = ifelse(is_dev_tool == TRUE, "Developer tool", 
                            ifelse(is_general_tool == TRUE, "General purpose tool", "Other/Unknown")))

# subset for tool choice analysis
tool_choice_analysis <- adopt_df %>%
  subset(tool_type != "Other/Unknown")

tool_choice_analysis %>%
  group_by(research_area_major, tool_type)%>%
  summarise(count = n()) %>%
  group_by(research_area_major) %>%
  mutate(prop = count / sum(count)) %>%
  ggplot(., aes(x = research_area_major, y = prop, fill = tool_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") +
  ylab("Proportion of respondents") +
  ggtitle("Tool choice by research area")+
  scale_fill_brewer(palette = "Set2", name = "Tool type")+
  geom_text(aes(label = count), position=position_fill(vjust=0.5), size=4)
# save this picture
ggsave("figures/tool_choice_by_field.png", dpi = 300, width = 6, height =8)

# linear model test for this 
tool_choice_lm <- glm(as.factor(tool_type) ~ research_area_major, data = tool_choice_analysis, family = "binomial")
summary(tool_choice_lm)
# anova of the binomial model
tool_choice_anova <- anova(tool_choice_lm)
tool_choice_anova
# report analysis of deviance in standard form
paste("analysis of deviance results: Chi-sq(", tool_choice_anova$Df[2], ") = ", round(tool_choice_anova$`Deviance`[2], 3), ", p = ", signif(tool_choice_anova$`Pr(>Chi)`[2], 3), "\n", sep = "")

# tool choice by years of experience
tool_choice_analysis%>%
  ggplot(., aes(x = tool_type, y = logyears_program_exp, fill = tool_type)) +
  geom_boxplot() +
  xlab("Primary genAI tool choice") +
  ylab("Log-years of programming experience") +
  ggtitle("Primary genAI tool choice by years of programming experience")+
  scale_fill_brewer(palette = "Set2", name = "Tool type")

# t-test of significance with log-transformed years (will be same as if this had been done by linear model)
tool_choice_ttest <- t.test(logyears_program_exp ~ tool_type, data = tool_choice_analysis)
tool_choice_ttest
# print out t-test for manuscript
paste0("t(", round(tool_choice_ttest$parameter, 3), ") = ", round(tool_choice_ttest$statistic, 3), ", p = ", signif(tool_choice_ttest$p.value, 3), "\n")

# In terms of median years of programming experience for interpetability
tool_choice_analysis %>%
  group_by(tool_type) %>%
  summarise(median_years_exp = median(years_program_exp, na.rm = TRUE))


# tool choice by gender
tool_choice_analysis %>%
  #left_join(demo_df %>% select(ResponseId, gender), by = "ResponseId") %>%
  filter(gender != "Prefer not to say") %>%
  group_by(tool_type, gender)%>%
  summarise(count = n()) %>%
  group_by(tool_type) %>%
  mutate(prop = count / sum(count))

# is there a significant difference by gender?
tool_choice_gender_lm <- glm(as.factor(tool_type) ~ gender, data = subset(tool_choice_analysis, gender != "Prefer not to say"), family = "binomial")
summary(tool_choice_gender_lm)
# anova of the binomial model
tool_choice_gender_anova <- anova(tool_choice_gender_lm)
tool_choice_gender_anova
# report analysis of deviance in standard form
paste("analysis of deviance results: Chi-sq(", tool_choice_gender_anova$Df[2], ") = ", round(tool_choice_gender_anova$`Deviance`[2], 3), ", p = ",... = signif(tool_choice_gender_anova$`Pr(>Chi)`[2], 3), "\n", sep = "")  

stargazer(tool_choice_gender_lm)


# Chunk size accepted differs by tool type, presumably.
adopt_df %>%
  filter(!is.na(genai_lines_accepted)) %>%
  ggplot(., aes(x = genai_lines_accepted)) +
  geom_bar() +
  facet_wrap(~is_code_tool) + 
  coord_flip() +
  xlab("") +
  ggtitle("How many lines of generated code suggestions do you\ntypically accept at one time when working with this tool?", 
          subtitle = paste0("n = ", sum(!is.na(adopt_df$genai_lines_accepted))))



