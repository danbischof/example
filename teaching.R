# packages 
library(haven)
library(dplyr)
library(ggplot2)

# read in data: 
study1 <- read_dta("Parliamentary Representation Data/study1.dta")

# drop all but two variables we want to plot: 
study1_subset <- study1 %>%
  select(performance, parl)

# Filter out parties with performance > 10
filtered_data <- study1_subset %>%
  filter(performance <= 10)

# figure: 
plot <- ggplot(data = filtered_data, aes(x = performance, y = parl)) +
  # vertical line x = 0
  geom_vline(xintercept = 0, color = "black", size = 1) +
  # adapt style of points to parliament status y/n
  geom_point(
    aes(color = ifelse(performance < 0, "Below Zero", "Above Zero"), shape = ifelse(performance < 0 & parl == 1, "X", "Bubble")),
    size = 3, alpha = 0.5
  ) +
  # colors 
  scale_color_manual(values = c("Below Zero" = "red", "Above Zero" = "blue"), name = "Performance") +
  # shapes
  scale_shape_manual(values = c("X" = 4, "Bubble" = 16), guide = "none") +  # Remove legend for shapes
  # labels
  labs(
    title = "Party Performance vs. Parliament Status",
    x = "% of Votes a Party Received",
    y = "Parliament Status (0 = Not in Parliament, 1 = In Parliament)"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# save as pdf: 
ggsave("party_performance_plot.pdf", plot = plot, width = 8, height = 6)
