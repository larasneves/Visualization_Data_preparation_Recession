library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(gridExtra)
library(ggthemes)

hikes1 <- read.csv('ffr_99_06.csv')
hikes2 <- read.csv('ffr_15_23.csv')

# Grouping by month
hikes1$DATE <- as.Date(hikes1$DATE)
hikes1 <- hikes1 %>%
  group_by(year = format(DATE, "%Y"), month = format(DATE, "%m")) %>%
  summarise(DFEDTAR = max(DFEDTAR, na.rm = TRUE))

hikes1$DATE <- as.Date(paste(hikes1$year, hikes1$month, "01", sep = "-"))

hikes1 <- hikes1 %>% select(DATE, DFEDTAR)
colnames(hikes1) <- c('year', 'date', 'rate')

hikes2$DATE <- as.Date(hikes2$DATE)
hikes2 <- hikes2 %>%
  group_by(year = format(DATE, "%Y"), month = format(DATE, "%m")) %>%
  summarise(DFEDTARU = max(DFEDTARU, na.rm = TRUE))

hikes2$DATE <- as.Date(paste(hikes2$year, hikes2$month, "01", sep = "-"))

hikes2 <- hikes2 %>% select(DATE, DFEDTARU)
colnames(hikes2) <- c('year', 'date', 'rate')

#Creating a table for each cycle
cycle1 <- hikes1 %>%
          filter(date >= '1999-05-01' & date <= '2000-05-01') %>%
          select(rate) %>%
          mutate(rate = rate - 4.75)
cycle1$month <- seq_along(cycle1$rate) - 1

cycle2 <- hikes1 %>%
          filter(date >= '2004-05-01' & date <= '2006-06-01') %>%
          select(rate) %>%
          mutate(rate = rate - 1)
cycle2$month <- seq_along(cycle2$rate) - 1

cycle3 <- hikes2 %>%
          filter(date >= '2015-11-01' & date <= '2018-12-01') %>%
          select(rate) %>%
          mutate(rate = rate - 0.25)
cycle3$month <- seq_along(cycle3$rate) - 1

cycle4 <- hikes2 %>%
  filter(date >= '2022-02-01' & date <= '2023-07-01') %>%
  select(rate) %>%
  mutate(rate = rate - 0.25)
cycle4$month <- seq_along(cycle4$rate) - 1


# Plotting
hikes <- ggplot(data = cycle1, aes(x = month, y = rate)) +
  geom_line(aes(color = "1999"), size = 1.5) +
  geom_line(data = cycle2, aes(x = month, y = rate, color = "2004"), size = 1.5) +
  geom_line(data = cycle3, aes(x = month, y = rate, color = "2015"), size = 1.5) +
  geom_line(data = cycle4, aes(x = month, y = rate, color = "2022"), size = 1.5) +
  theme_wsj() +
  labs(x = 'Months since first rate hike',
       title = "Federal Funds Rate changes",
       subtitle = "Changes in percentage points in different hiking cycles",
       color = NULL) +
  theme(axis.title.x = element_text(family = "Tw Cen MT", face = 'italic', size = 12),
        plot.title = element_text(family = "Tw Cen MT", face = 'bold', color = "black", size = 18),
        plot.subtitle = element_text(family = "Tw Cen MT", face = 'italic', color = "black", size = 12),
        legend.position = "top") +
  scale_color_manual(values = c("1999" = "#d7191c", "2004" = "#abd9e9", "2015" = "#fdae61", "2022" = "#2c7bb6"))
  
hikes

ggsave(plot = hikes, 
       file = 'plot2.png',
       height = 15,
       width = 15*13/10,
       units = 'cm',
       dpi = 600)
