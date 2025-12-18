# Load packages
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)

rm(list = ls())
# ----------------------------------------------------------
# 1) Read data
# ----------------------------------------------------------
axial <- read_csv("data/qualitative/annotated/non_adoption_coded.csv")

# ----------------------------------------------------------
# 2) Normalize + split
# ----------------------------------------------------------
normalize_label <- function(lbl) {
  if (is.na(lbl) || lbl == "") return(lbl)
  lbl <- str_trim(lbl)
  
  repl <- c(
    "Existing Tools"   = "Existing Tool",
    "Ineffiency"       = "Inefficiency",
    "Ineffficiency"    = "Inefficiency",
    "Ineffiiciency"    = "Inefficiency"
  )
  if (lbl %in% names(repl)) lbl <- repl[[lbl]]
  lbl
}

clean_axial <- function(x) {
  if (is.na(x) | x == "") return(NA_character_)
  parts <- str_split(x, "/", simplify = TRUE) %>% as.vector()
  parts <- str_trim(str_remove_all(parts, "[()]+"))
  parts <- parts[parts != ""]
  
  out <- character(0)
  for (p in parts) {
    p <- normalize_label(p)
    if (str_starts(tolower(p), "ethics") ||
        p %in% c("Environment", "Professionalism", "Deskilling",
                 "Degrading Systems", "Mistrust")) {
      out <- c(out, "Ethics")
    } else {
      out <- c(out, p)
    }
  }
  unique(out)
}

# ----------------------------------------------------------
# 3) Apply cleaning
# ----------------------------------------------------------
axial$Cleaned <- lapply(axial$axial_code ,clean_axial)

# ----------------------------------------------------------
# 4) Count labels
# ----------------------------------------------------------
theme_counts <- axial %>%
  unnest(Cleaned) %>%
  group_by(Cleaned) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(desc(Frequency))

print(theme_counts)

# ----------------------------------------------------------
# 5) Make bar graph
# ----------------------------------------------------------

ggplot(na.omit(theme_counts),
            aes(x = reorder(Cleaned, Frequency), y = Frequency)) +
  geom_col(fill = "#25445C") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of Responses") +
  ggtitle(label = "Thematic analysis of reasons for non-adoption",
          subtitle = "n = 210")+
  theme(
    axis.text.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.ticks.length = grid::unit(5, "pt")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
ggsave("figures/nonadoption_themes.png", width = 6, height = 4, dpi = 300)
