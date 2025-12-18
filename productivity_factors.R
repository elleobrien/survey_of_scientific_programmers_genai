# Lets just look at EVERYTHING that is correlated with the SPACE composite score.
library(dplyr)
library(ggplot2)
library(polycor)
library(corrplot)
library(stargazer)

rm(list = ls())
# read rds from data/interim
dev_df <- readRDS("data/interim/dev_variables.rds")

# read rds from demographic
demo_df <- readRDS("data/interim/demo_variables.rds")

# read rds from genai_variables
genai_df <- readRDS("data/interim/genai_variables.rds")

big_df <- dev_df %>%
  select(ResponseId,ends_with("_score"), code_reuse_inner, code_reuse_outer,code_publishing) %>%
  left_join(demo_df, by = "ResponseId") %>%
  left_join(genai_df, by = "ResponseId") %>%
  # filter out anyone who indicated they don't use a genAI tool
  filter(reports_no_adoption == FALSE) %>%
  filter(lists_no_tools_tried == FALSE)

#if someone wasn't presented the SPACE questions, don't include in this analysis.
          # this would be anyone who indicated they never use genAI/tried but gave up,
          # or lists NO genAI tools tried. 

### make a correlation matrix of numerical/ordered variables 
cor_vars <- c("space_avg", "code_reuse_inner", "code_reuse_outer",
              "dev_score", "version_score", "review_score", "ci_score", 
              "unit_score", "regression_score", "system_score",
              "logyears_program_exp",
              "genai_lines_accepted","program_freq")

# Make a hetcor matrix with polychor
cormat <- big_df %>%
  select(cor_vars) %>%
  hetcor(., ML = TRUE, use = "pairwise.complete.obs") 

# human readable labels 
feature_labels <- c("Perceived Productivity Score", "Code Reuse (inner)", "Code Reuse (outer)",
                    "Development Practices Score", "Version Control Score", "Code Review Score", "Continuous Integration Score",
                    "Unit Testing Score", "Regression Testing Score", "System Testing Score",
                    "Log-years Programming Experience",
                    "Typical Lines of  Code Accepted","Programming Frequency")
rownames(cormat$correlations) <- feature_labels
colnames(cormat$correlations) <- feature_labels

# png
png(filename = "figures/big_correlation_matrix.png", width = 10, height = 8, units = "in", res = 300)
corrplot(cormat$correlations, 
         method = "color",
         # use cormat labels
         order = "hclust",
         tl.col = "black", 
         tl.srt = 45, 
         number.cex = 0.7)
dev.off()

cor.test(big_df$logyears_program_exp, big_df$space_avg, use = "pairwise.complete.obs", method = "pearson")



# really important pic! LOOK at the  relationship between lines accepted and perceived
# productivity

# reset this level just for plotting niceness
levels(big_df$genai_lines_accepted)[1] <- "I don't use genAI\n to produce code suggestions\ndirectly"

big_df %>%
  filter(!is.na(genai_lines_accepted)) %>%
  ggplot(., aes(x = genai_lines_accepted, y = spce_avg))+
  geom_boxplot() +
  ylab("Perceived Productivity Score") +
  xlab("")+
  ggtitle("How many lines of generated code suggestions do you typically accept at\none time when working with this tool?")

ggsave("figures/space_by_lines_accepted.png", width = 7, height = 5, dpi = 300)

# statistical model
lm(space_avg ~ as.numeric(genai_lines_accepted), data = big_df)  %>%
  summary()


# Based on implementation suggested by https://stackoverflow.com/questions/16281667/p-value-for-polyserial-correlation
poly_to_pvalue <- function(polyserial_output) {
  std.err <- sqrt(polyserial_output$var[1, 1]) 
  p_value <- 2 * pnorm(-abs(polyserial_output$rho / std.err))
  return(p_value)
}

poly_space <- polyserial(big_df$space_avg, big_df$genai_lines_accepted, ML = TRUE, std.err = TRUE)
poly_to_pvalue(poly_space)

# robustness check with SPCE (no A! )
ply_spce <- polyserial(big_df$spce_avg, big_df$genai_lines_accepted, ML = TRUE, std.err = TRUE)
poly_to_pvalue(ply_spce)
# also check code reuse inne

# Now, test interaction between programming experience and development practices score
big_df$dev_median_split <- ifelse(big_df$dev_score > median(big_df$dev_score), "High use of\ndevelopment practices", "Low use of\ndevelopment practices")
table(big_df$dev_median_split)

m1 <- lm(space_avg ~ dev_score*logyears_program_exp, data = big_df)
summary(m1)

stargazer(m1, title = "My Table", align = TRUE)


m2 <- lm(space_avg ~ dev_score + logyears_program_exp, data = big_df)
anova_table <- anova(m2, m1)

ggplot(big_df, aes(x = logyears_program_exp, y = space_avg)) +
  geom_point()+
  geom_smooth(method = "lm") + 
  xlab(expression(Log[10]~"Years of Programming Experience"))+
  ylab("Perceived producivity score")+
  facet_wrap(~dev_median_split)
ggsave("figures/space_by_programming_exp_dev_split.png", width = 7, height = 5, dpi = 300)


# Testing lines_accepted as a covariate
m_lines_accepted <- lm(space_avg ~ dev_score*logyears_program_exp + as.numeric(genai_lines_accepted), data = big_df)
summary(m_lines_accepted)

stargazer(m1, m_lines_accepted, title = "Covariate Analysis of SPACE Score", align = TRUE)

# differenceas in productivity estimates by field?
lm(space_avg ~ research_area_major, data = big_df) %>%
  summary()
anova(lm(space_avg ~ research_area_major, data = big_df))

# Lines accepted by tool type
dev_tool_list <- c("GitHub Copilot", "Cursor", "Claude Code")
general_tool_list <- c("ChatGPT", "Claude", "Microsoft Copilot", "Perplexity")

big_df <- big_df %>%
  filter(!is.na(genai_lines_accepted)) %>%
  mutate(tool_type = case_when(
    genai_primary_tool_choice %in% dev_tool_list ~ "Development-focused tool",
    genai_primary_tool_choice %in% general_tool_list ~ "General-purpose tool",
    TRUE ~ "Unknown"
  ))

table(big_df$tool_type)
lm(as.numeric(genai_lines_accepted) ~ tool_type, data = subset(big_df, tool_type != "Unknown")) %>%
  summary()

anova(lm(as.numeric(genai_lines_accepted) ~ tool_type, data = subset(big_df, tool_type != "Unknown")))

ggplot(big_df %>% filter(tool_type != "Unknown"), aes(x = genai_lines_accepted)) +
  geom_bar() +
  xlab("Type of Generative AI Tool Used")+
  ylab("Typical Lines of Code Accepted")+ 
  facet_wrap(~tool_type, nrow = 2)+
  ggtitle("Developers using general-purpose genAI tools report accepting more lines of code at once")
