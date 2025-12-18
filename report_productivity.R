# Understand- relationship between programming experience, quality control,
# and perceived productivity of AI tools.
library(dplyr)
library(ggplot2)
library(polycor)
library(corrplot)

# read rds from data/interim
dev_df <- readRDS("data/interim/dev_variables.rds")

# read rds from demographic
demo_df <- readRDS("data/interim/demo_variables.rds")

# read rds from genai_variables
genai_df <- readRDS("data/interim/genai_variables.rds")

# look at correlations of the space variables using polychor::hetcor
space <- genai_df %>%
  select(starts_with("space_"))
space_cor <- hetcor(space, use = "pairwise.complete.obs")
# plot the correlation matrix with corplot

corrplot::corrplot(space_cor$correlations, method = "color", 
                   tl.col = "black", tl.srt = 45, addCoef.col = "black",
                   number.cex = 0.7, diag = FALSE)

# this seems to justify just averaging over all of them, like in the Ziegler paper. 
genai_df$space_avg <- genai_df %>%
  select(starts_with("space_")) %>% # convert to numeric and then calculate rowmeans
  mutate(across(everything(), ~ as.numeric(.))) %>%
  rowMeans(na.rm = TRUE)

ggplot(genai_df, aes(x = space_avg) )+
  geom_histogram(bins = 30) +
  labs( x = "Perceived Productivity Score",
       y = "Count") + 
  # vertical line for median
  geom_vline(aes(xintercept = median(space_avg, na.rm = TRUE)), 
             color = "red", linetype = "dashed") 

# summary stats for space_avg 
summary(genai_df$space_avg)

# how does this relate to years of programming experience?
space_factors <- genai_df %>%
  select(ResponseId, space_avg) %>%
  merge(., demo_df, by = "ResponseId") %>% # 
  filter(!is.na(space_avg))

ggplot(space_factors, aes(x= logyears_program_exp, y = space_avg)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Years of programming experience",
       y = "Perceived Productivity Score")
  

# Is there a signific correlation between years of programming exp and space avg?
summary(lm(space_avg ~ logyears_program_exp, data = space_factors))
# yes, but it's not very big. r2 around 1! r around 0.11!

cor(space_factors$logyears_program_exp, space_factors$space_avg, use = "pairwise.complete.obs")

