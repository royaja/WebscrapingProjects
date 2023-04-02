
# Married at First Sight Australia Season 8
# Data gathered from Wikipedia 01.04.2023

# Load library
library(tidyverse)

# Load data - Data was scraped from Wikipedia and saved to a csv-file
mafs <- read_csv2("mafs.csv")

# Text 
title <- "Relationship between Age and Final Decision"
subtitle <- "Younger couples tend to stay together more often than older couples"
x <- "Age"
y <- ""
color <- "Final Decision: Stay Together vs. Break Up"

# Plot 
ggplot(mafs,aes(x = age, y = couple, color = final_decision)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(age)), color = "red", linetype = "dashed", size = 0.5) + # Add a vertical line at mean age
  annotate("text", x = mean(mafs_couple_profile$age) + 1, y = max(mafs_couple_profile$couple),
           label = "Mean Age", color = "red", hjust = 0) +
  labs(title = title,
       subtitle = subtitle,
       x = x,
       y = y,
       color = color) +
  scale_x_continuous(limits = c(20, 50), breaks = seq(20, 45, by = 5)) +   
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(1.5, "lines"),
        legend.position = ("top"))
