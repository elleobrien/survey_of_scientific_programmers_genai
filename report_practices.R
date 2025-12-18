library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)
library(stargazer)


rm(list = ls())

############# Report research group practices ##################################
dev_df <- readRDS("data/interim/dev_variables.rds")

# count and proportion of responses to code_publishing question
dev_df %>%
  filter(!is.na(code_publishing)) %>%
  group_by(code_publishing) %>%
  summarise(count = n(), 
            proportion = n()/nrow(.))

# Code reuse inside group
dev_df %>%
  filter(!is.na(code_reuse_inner)) %>%
  group_by(code_reuse_inner) %>%
  summarise(count = n(), 
            proportion = n()/nrow(.)) 

# Code reuse outside group
dev_df %>%
  filter(!is.na(code_reuse_outer)) %>%
  group_by(code_reuse_outer) %>%
  summarise(count = n(), 
            proportion = n()/nrow(.)) 


reuse <- dev_df %>%
  select(code_reuse_inner, code_reuse_outer, code_publishing) %>%
  # Melt into longform
  pivot_longer(cols = everything(), names_to = "practice", values_to = "frequency")


# Rename "practice" levels in reuse for plotting
reuse$practice <- factor(reuse$practice, 
                         levels = c("code_reuse_inner", "code_reuse_outer", "code_publishing"),
                         labels = c("How often do you write code that is reused INSIDE your research group?", 
                                    "How often do you write code that is reused OUTSIDE your research group?", 
                                    "When you publish scientific research that involved programming to complete,\n do you share any related code in a public repository?"))

px_reuse <- ggplot(na.omit(reuse), aes(x = frequency)) + 
  geom_bar() +
  facet_wrap(~practice, nrow = 3) +
  labs(x = "", y = "Count")
px_reuse
ggsave(px_reuse, 
       filename = "figures/code_reuse.png", 
       width = 6, height =5, dpi = 300)



# Familiarity with development practices
practice_awareness <- dev_df %>%
  select(ends_with("_familiar")) %>%
  pivot_longer(cols = everything(), names_to = "practice", values_to = "familiarity")
practice_awareness$practice <- factor(practice_awareness$practice, 
                                      levels = c("version_familiar", "testing_familiar", "review_familiar", "ci_familiar"),
                                      labels = c("Version control", "Code testing", "Code review", "Continuous integration"))
ggplot(na.omit(practice_awareness), aes(x = familiarity)) +
  geom_bar() +
  facet_wrap(~practice, nrow = 2) +
  labs(x = "Familiar with this practice", y = "Count")

# In table form, what percent are familiar with each? 
practice_awareness %>%
  group_by(practice) %>%
  filter(!is.na(familiarity)) %>%
  summarise(proportion = sum(familiarity)/n(),
            n = sum(familiarity),
            responses = n())

# How many are unfamiliar with ANY practices?
n_practices_familiar <- dev_df %>%
  select(ends_with("_familiar")) %>%
  rowSums(na.rm = TRUE)

# median number of practices familiar with
median(n_practices_familiar)
# proportion familiar with NO practices
sum(n_practices_familiar == 0)
sum(n_practices_familiar == 0) / length(n_practices_familiar)


# What about actual reported usage? 
practice_use <- dev_df %>%
  select(contains("_score")) %>%
  select(-dev_score)%>%
  pivot_longer(cols = everything(), names_to = "practice", values_to = "frequency")

# For plotting, make human-readable labels for the practices
practice_use$practice <- factor(practice_use$practice, 
                                levels = c("version_score", "review_score", 
                                           "ci_score", "unit_score", "regression_score",
                                           "system_score"),
                                labels = c("Version control", "Code review", "Continuous integration",
                                           "Unit testing", "Regression testing", "System testing"))

px_practice_adoption <- ggplot(na.omit(practice_use), aes(x = frequency)) +
  geom_bar() +
  facet_wrap(~practice, nrow = 6) +
  labs(x = "", y = "Count")+
  scale_x_discrete(breaks = 0:5,
                   labels = c("Unfamiliar\nwith practice", 
                              "Familiar but\nnever use", 
                              "Sometimes", 
                              "About half\nthe time", 
                              "Most of the time", 
                              "Always")) 
px_practice_adoption
ggsave(px_practice_adoption, 
       filename = "figures/practice_adoption.png", 
       width = 6, height =8, dpi = 300)

# reset the factor order of "practice" 
practice_use$practice <- factor(practice_use$practice, 
                                levels = rev(c("Version control", "Code review", 
                                           "Unit testing", "Regression testing", "System testing", "Continuous integration")))


px_practice_with_counts <- ggplot(na.omit(practice_use), aes(x = practice, fill = as.factor(frequency))) +
  geom_bar(position = "fill") +
  labs(x = "Development Practice", y = "Proportion of respondents", fill = "Usage Frequency") +
  scale_fill_brewer(palette = "BrBG", breaks = 0:5,
                      labels = c("Unfamiliar\nwith practice", 
                                 "Familiar but\nnever use", 
                                 "Sometimes", 
                                 "About half\nthe time", 
                                 "Most of the time", 
                                "Always")) +
  coord_flip() + 
  # add text labels with count of each filled bar
  geom_text(stat = 'count', aes(label=..count..), position=position_fill(vjust=0.5), size=3)
px_practice_with_counts

ggsave(px_practice_with_counts, 
       filename = "figures/practice_adoption_counts.png", 
       width = 8, height =5, dpi = 300)

# How does usage of development practices vary with years of programming experience?
df <- readRDS("data/interim/demo_variables.rds")

dev_df <- dev_df %>%
  left_join(., df %>% select(ResponseId, logyears_program_exp), by = "ResponseId")

cor.test(dev_df$logyears_program_exp, dev_df$dev_score, use = "complete.obs", method = "pearson")


dev_df <- dev_df %>%
  left_join(., df %>% select(ResponseId, research_area_major), by = "ResponseId")

ggplot(dev_df, aes(x = research_area_major, y = dev_score)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Research Area", y = "Development Practice Score")

# ANOVA to see if there are significant differences in dev_score by research area
dev_score_by_field <- lm(dev_score ~ research_area_major, data = dev_df)
stargazer(dev_score_by_field, type = "text")

#omnibus anova for dev_score_by_field
anova(dev_score_by_field)
# print out standard reporting of F and p value
anova_res <- anova(dev_score_by_field)
paste("ANOVA results: F(", anova_res$Df[1], ",", anova_res$Df[2], ") = ", round(anova_res$`F value`[1], 2), ", p = ", signif(anova_res$`Pr(>F)`[1], 3), "\n", sep = "")

dev_score_by_field_years <- lm(dev_score ~ research_area_major + logyears_program_exp, data = dev_df)
stargazer(dev_score_by_field, dev_score_by_field_years, title = "My Table", align = TRUE)
