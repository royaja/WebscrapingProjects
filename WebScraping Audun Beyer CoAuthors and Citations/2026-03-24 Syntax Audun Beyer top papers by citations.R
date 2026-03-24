
# This project scrape top publications from Professor Audun Beyer. 
# Top publication is defined as publications with most citations. 
# All data are gathered from the Google Scholar profil of Audun Beyer 
# https://scholar.google.com/citations?user=jE3DAo8AAAAJ&hl=no 
# Code developed 24.03.2026 

# Load library 
library(tidyverse)
library(rvest)
library(scales)

# Scholar profile
google_scholar <- "https://scholar.google.com/citations?user=jE3DAo8AAAAJ&hl=no&oi=ao"

page <- 
  read_html(google_scholar)

titles <- 
  page %>%
  html_elements(".gsc_a_at") %>%
  html_text2()


citations <- 
  page %>%
  html_elements(".gsc_a_ac") %>%
  html_text2() %>%
  na_if("") %>%
  parse_number()

# Scrape publication years
years <- 
  page %>%
  html_elements(".gsc_a_y span") %>%
  html_text2() %>%
  na_if("") %>%
  parse_number()

# Make sure lengths match
n <- 
  min(length(titles), length(citations), length(years))

papers_df <- 
  tibble(
  title = titles[1:n],
  citations = citations[1:n],
  year = years[1:n]
) %>%
  arrange(desc(citations)) %>%
  slice_head(n = 10)

# Plot
papers_df %>%
  ggplot(aes(x = reorder(title, citations), y = citations)) +
  geom_col(width = 0.7, fill = "steelblue") +
  geom_text(
    aes(label = citations),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x = NULL,
    y = "Number of citations",
    title = "Most cited papers by Professor Audun Beyer found on Google Scholar",
    subtitle = "Top publications based on citation counts from the public Google Scholar profile",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.margin = margin(10, 30, 10, 10)
  )

