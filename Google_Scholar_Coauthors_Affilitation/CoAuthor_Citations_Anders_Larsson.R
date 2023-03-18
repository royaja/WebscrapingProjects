
# Scraping co-authors, citations, and affiliations from Google Scholar 
# Visualizing co-authors and citations in a barchart using the ggchicklet package

# Load library
library(tidyverse)
library(rvest)
library(ggchicklet)
library(viridis)

# Page 
google_scholar <- "https://scholar.google.com/citations?user=MPGY79kAAAAJ&hl=no&oi=ao"

# Scraping and wrangle data 
coauthors <- google_scholar %>% 
  read_html() %>% 
  html_nodes(css = "#gsc_rsb_co a") %>% 
  html_text() 

coauthors=as.data.frame(coauthors)

citations <- google_scholar %>% 
  read_html() %>% 
  html_nodes(css = ".gsc_a_ac") %>% 
  html_text() %>% 
  as.numeric()

citations=as.data.frame(citations)

affiliations <- google_scholar %>% 
  read_html() %>% 
  html_nodes(css = ".gsc_rsb_a_ext") %>% 
  html_text()

affiliations=as.data.frame(affiliations)

affiliations <- affiliations %>% 
  filter(!str_starts(affiliations, "Verifisert e-postadresse")) %>% 
  mutate(
    affiliations = case_when(
      str_detect(affiliations, "University of Bergen") ~ "University of Bergen",
      str_detect(affiliations, "Kristiania University College") ~ "Kristiania University College",
      str_detect(affiliations, "University of Oslo") ~ "University of Oslo",
      str_detect(affiliations, "Oslo Metropolitan University") ~ "Oslo Metropolitan University",
      str_detect(affiliations, "Johannes Gutenberg-University Mainz") ~ "Johannes Gutenberg-University Mainz",
      str_detect(affiliations, "U of Oslo") ~ "University of Oslo",
      str_detect(affiliations, "Professor of Journalism Studies") ~ "Stockholm University",
      str_detect(affiliations, "Drexel University") ~ "Drexel University",
      str_detect(affiliations, "Uppsala University") ~ "Uppsala University",
      str_detect(affiliations, "Mid Sweden University") ~ "Mid Sweden University",
      str_detect(affiliations, "	
ESPOL Universit?? Catholique de Lille") ~ "	
ESPOL Universit?? Catholique de Lille",
      str_detect(affiliations, "KTH Royal Institute of Technology") ~ "KTH Royal Institute of Technology",
      str_detect(affiliations, "IT University of Copenhagen") ~ "IT University of Copenhagen",
      str_detect(affiliations, "University of Innsbruck") ~ "University of Innsbruck",
      str_detect(affiliations, "Technische Universit??t Dresden") ~ "Technische Universit??t Dresden",
      TRUE ~ affiliations
    ))

coauthors$row_num <- 1:nrow(coauthors)
citations$row_num <- 1:nrow(citations)
affiliations$row_num <- 1:nrow(affiliations)

merged_df <- merge(coauthors, citations, by = "row_num", all.x = TRUE)
merged_df <- merge(merged_df, affiliations, by = "row_num", all.x = TRUE)

merged_df$row_num <- NULL


# Plot 
merged_df %>%
  ggplot(aes(x = reorder(coauthors, citations), y = citations)) +
  geom_chicklet(aes(fill = `affiliations`), color = "black", size = 0.5) +
  coord_flip() +
  labs(
    x = "Co-Authors", 
    y = "Number of citations", 
    title = "Mapping the Scholarly Impact of Prof. Dr. Anders Olof Larsson at Kristiania University College",
    subtitle = "Scraping number of citations, university affilitation and name of coauthors from Google Scholar to explore who Prof. Dr. Anders Olof Larsson \ncommonly work together with",
    fill = "Affiliation"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, 50)) +
  scale_fill_viridis_d(option = "plasma") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 14),
    plot.title = element_text(size = 16),
    legend.position = "right"
  )

