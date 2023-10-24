
# Examining most frequent words used in covid-19 measure reports from the Norwegian government 
# Data is scraped from https://www.regjeringen.no/no/tema/Koronasituasjonen/tidslinje-koronaviruset/id2692402/
# Scraping done using the rvest package
# Code created 24.10.2023 

# Load libraries  
library(tidyverse)
library(rvest)
library(tidytext)

# Scrape timeline and events 
url <- "https://www.regjeringen.no/no/tema/Koronasituasjonen/tidslinje-koronaviruset/id2692402/"
page <- read_html(url)

collect_timeline <- 
  page |> 
  html_nodes(".timeline-startdate") |> 
  html_text()

timeline <- data.frame(timeline = collect_timeline, stringsAsFactors = FALSE)

timeline <- 
  timeline |> 
  mutate(line = row_number(), 
         timeline = str_replace(timeline, "^\\w+\\s", ""))

event <- 
  page |> 
  html_nodes(".timeline-headline span") |> 
  html_text()

event_df <- data.frame(event = event, stringsAsFactors = FALSE)

events <- 
  event_df |> 
  mutate(line = row_number()) |> 
  filter(line %% 2 != 0) |> 
  mutate(line = row_number())

covid_timeline <- left_join(timeline, events, by = "line")

Sys.setlocale("LC_TIME", "Norwegian")

covid_timeline <- 
  covid_timeline |> 
  mutate(timeline = dmy(timeline))


# Process text 
text <- 
  covid_timeline |> 
  select(event) |> 
  unnest_tokens(word, event) |>    
  filter(nchar(as.character(word)) > 3) |> 
  anti_join(get_stopwords(language = "norwegian", source = "snowball"))

# Plot 
p <- text |> 
  count(word, sort = TRUE) |> 
  filter(n > 10) |> 
  mutate(word = reorder(word, n),
         top_four = rank(-n) <= 4) |> 
  ggplot(aes(n, word, fill = top_four)) +
  geom_col(show.legend = FALSE) + 
  labs(y = NULL)

p <- p + theme_classic()

p <- p + geom_text(aes(label = n), hjust = -1)

p <- p + scale_x_continuous(breaks = NULL)

title <- "Norge, Regjeringen, tiltak og flere er de mest brukte ordene knyttet til COVID-19 tiltak"
subtitle <- "De mest brukte ordene i COVID-19 meldinger fra regjeringen"
caption <- "R.A. Jacobsen | @AulieRoy | Source: www.regjeringen.no"

p <- p + labs(
  title = title,
  subtitle = subtitle,
  caption = caption)

p <- p + theme(
  axis.title.x = element_blank(), 
  axis.line.x = element_blank(), 
  axis.ticks.x = element_blank(), 
  plot.title = element_text(
    size = 16, 
    face = "bold"), 
  plot.subtitle = element_text(
    size = 12,
    face = "bold"), 
  plot.caption = element_text(
    size = 7, 
    hjust = 0)
)

p
