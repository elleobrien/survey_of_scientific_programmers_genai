# Understand- relationship between programming experience, quality control,
# and perceived productivity of AI tools.
library(dplyr)
library(ggplot2)
library(polycor)
library(corrplot)

source("prepare_data.R")

# look at correlations of the space variables using polychor::hetcor
space <- survey_df %>%
  select(space_s1, space_p1, space_p2, space_a1, space_c1, space_c2, space_e1, space_e2, space_global)
space_cor <- hetcor(space, use = "pairwise.complete.obs")
# plot the correlation matrix with corplot

corrplot::corrplot(space_cor$correlations, method = "color",
                   tl.col = "black", tl.srt = 45, addCoef.col = "black",
                   number.cex = 0.7, diag = FALSE)

# this seems to justify just averaging over all of them, like in the Ziegler paper.
# (space_avg already computed in prepare_data.R)

ggplot(survey_df, aes(x = space_avg) )+
  geom_histogram(bins = 30) +
  labs( x = "Perceived Productivity Score",
       y = "Count") +
  # vertical line for median
  geom_vline(aes(xintercept = median(space_avg, na.rm = TRUE)),
             color = "red", linetype = "dashed")

# summary stats for space_avg
summary(survey_df$space_avg)

# how does this relate to years of programming experience?
space_factors <- survey_df %>%
  select(space_avg, logyears_program_exp) %>%
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
