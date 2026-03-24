
# This project scrape top publications from Professor Audun Beyer. 
# Top publication is defined as publications with most citations. 
# All data are gathered from the Google Scholar profil of Audun Beyer 
# https://scholar.google.com/citations?user=jE3DAo8AAAAJ&hl=no 
# Code developed 24.03.2026 

# Load library
library(tidyverse)
library(rvest)
library(scales)

# Google Scholar profile
google_scholar <- "https://scholar.google.com/citations?user=jE3DAo8AAAAJ&hl=no&oi=ao"

page <- 
  read_html(google_scholar)

coauthors <- 
  page %>%
  html_elements("#gsc_rsb_co a") %>%
  html_text2()

citations <- 
  page %>%
  html_elements(".gsc_a_ac") %>%
  html_text2() %>%
  na_if("") %>%
  parse_number()

affiliations <- 
  page %>%
  html_elements(".gsc_rsb_a_ext") %>%
  html_text2()

affiliations <- 
  affiliations[!str_detect(affiliations, "^Verifisert e-postadresse")]

n <- 
  min(length(coauthors), length(citations), length(affiliations))

scholar_df <- tibble(
  coauthor = coauthors[1:n],
  citations = citations[1:n],
  affiliation = affiliations[1:n]
) %>%
  mutate(
    affiliation = str_squish(affiliation)
  ) %>%
  arrange(desc(citations))

# Plot
scholar_df %>%
  ggplot(aes(x = reorder(coauthor, citations), y = citations, fill = affiliation)) +
  geom_col(width = 0.7, color = "black") +
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
    title = "Audun Beyer's co-authors on Google Scholar",
    subtitle = "A plot that displays Audun Beyer's co-authors, their affiliations, and citation counts, based on data from Google Scholar",
    fill = "Affiliation",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    plot.margin = margin(10, 30, 10, 10)
  )


