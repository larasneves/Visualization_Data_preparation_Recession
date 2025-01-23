library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(gridExtra)
library(ggthemes)

spread <- read.csv("10Y-3M.csv")

# Converting dates
spread$DATE <- as.Date(spread$DATE)

# Changing date for day 1
spread <- spread %>%
  group_by(month = format(DATE, "%Y-%m-01")) %>%
  filter(DATE == min(DATE)) %>%
  ungroup()

# Renaming columns
spread <- spread %>% select('month', 'T10Y3M')
colnames(spread) <- c('date', '10y-3m')
spread$date <- as.Date(spread$date)
spread$`10y-3m` <- as.numeric(spread$`10y-3m`)

# Remove missing values
final_spread <- na.omit(spread)

# Create points with zero so that we can color differently the areas
intercept <- 0
r <- ifelse(final_spread$`10y-3m` < intercept, 0, 1)
ind <- na.exclude((1:length(r))[r != c(r[-1], NA)])
npx <- sapply(ind, function(i) approx(final_spread$`10y-3m`[i:(i+1)], final_spread$date[i:(i+1)], intercept)$y)
# Create a new data frame with the points
newdata <- data.frame(date = as.Date(npx), value = intercept)
colnames(newdata)[2] <- "10y-3m"

# Adjust values in newdata
newdata$`10y-3m` <- ifelse(newdata$`10y-3m` >= intercept, newdata$`10y-3m`, intercept)
newdata$`10y-3m` <- ifelse(newdata$`10y-3m` <= intercept, newdata$`10y-3m`, intercept)

# Combine the two data sets
final_spread2 <- rbind(final_spread, newdata)
colnames(final_spread2)[2] <- "value"

final_spread2 <- final_spread2 %>%
  filter(date >= as.Date("1987-01-01"))

df_subset <- subset(final_spread2, date < '1999-06-01')
df_subset2 <- subset(final_spread2, date >= '1999-05-01' & date < '2000-07-01')
df_subset3 <- subset(final_spread2, date >= '2000-06-01' & date < '2004-06-01')
df_subset4 <- subset(final_spread2, date >= '2004-05-01' & date < '2006-07-01')
df_subset5 <- subset(final_spread2, date >= '2006-06-01' & date < '2015-12-01')
df_subset6 <- subset(final_spread2, date >= '2015-11-01' & date < '2019-08-01')
df_subset7 <- subset(final_spread2, date >= '2019-07-01' & date < '2022-03-01')
df_subset8 <- subset(final_spread2, date >= '2022-02-01')

# First chart
p <- ggplot(final_spread2, aes(x = date, y = value)) + 
  geom_rect(aes(xmin = as.Date("1989-10-01"), xmax = as.Date("1991-03-01"), ymin = -Inf, ymax = Inf, fill = "Recession")) +
  geom_rect(aes(xmin = as.Date("2001-01-01"), xmax = as.Date("2001-09-01"), ymin = -Inf, ymax = Inf, fill = "Recession")) +
  geom_rect(aes(xmin = as.Date("2007-10-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf, fill = "Recession")) +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf, fill = "Recession")) +
  geom_ribbon(data = subset(final_spread2, value <= intercept), aes(ymin = value, ymax = intercept), fill = "red", alpha = 0.6) +
  geom_line(data = df_subset, aes(group = 1), color = 'black') +
  geom_line(data = df_subset2, aes(group = 1), color = '#d7191c', size = 0.8) +
  geom_line(data = df_subset3, aes(group = 1), color = 'black') +
  geom_line(data = df_subset4, aes(group = 1), color = '#abd9e9', size = 0.8) +
  geom_line(data = df_subset5, aes(group = 1), color = 'black') +
  geom_line(data = df_subset6, aes(group = 1), color = '#fdae61', size = 0.8) +
  geom_line(data = df_subset7, aes(group = 1), color = 'black') +
  geom_line(data = df_subset8, aes(group = 1), color = '#2c7bb6', size = 0.8) +
  theme_wsj() + 
  labs(
    x = NULL,
    y = "10-Year Minus 3-Month Spread",
    subtitle = "10-Year minus 3-Month Treasury yield spread in percentage points",
    fill = NULL) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_x_date(breaks = years, labels = scales::date_format("%Y"), date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), position = "right") +
  theme(plot.subtitle = element_text(family = "Tw Cen MT", face = 'bold', color = "black", size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt"),
        legend.position = 'top',
        legend.justification = 'right') +
  scale_fill_manual(values = c(Recession = "#D3D3D3"))

# Data for the second chart
unemployment <- read.csv("unemployment.csv")
unemployment$DATE <- as.Date(unemployment$DATE)
colnames(unemployment) <- c('date', 'rate')
unemployment <- unemployment %>%
  filter(date >= as.Date("1987-01-01"))

years <- seq(min(final_spread2$date), max(final_spread2$date), by = '5 years')

# Second chart
p2 <- ggplot(unemployment, aes(x = date, y = rate)) +
  geom_rect(aes(xmin = as.Date("1989-10-01"), xmax = as.Date("1991-03-01"), ymin = -Inf, ymax = Inf), fill = "#D3D3D3") +
  geom_rect(aes(xmin = as.Date("2001-01-01"), xmax = as.Date("2001-09-01"), ymin = -Inf, ymax = Inf), fill = "#D3D3D3") +
  geom_rect(aes(xmin = as.Date("2007-10-01"), xmax = as.Date("2009-06-01"), ymin = -Inf, ymax = Inf), fill = "#D3D3D3") +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-06-01"), ymin = -Inf, ymax = Inf), fill = "#D3D3D3") +
  theme_wsj()+
  geom_line(color = "black", alpha = 1) +
  geom_hline(yintercept = 0, color = "#92959e", linetype = "solid") +
  scale_x_date(breaks = years, labels = scales::date_format("%Y"), date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), position = "right")+
  labs(x = "Unemployment Rate") +
  theme(
    axis.text.x = element_blank(), 
    axis.title.y = element_blank(),  # Remove o título do eixo x
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.line.x = element_blank(),
    plot.title.position = "plot",
    axis.title.x = element_text(family = "Tw Cen MT", face = 'bold', size = 14, hjust = 0),
    plot.margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt"))

# Time axis
time <- ggplot(unemployment, aes(x = date)) +
  theme_wsj()+
  theme(axis.text.x = element_text(size = 10),
        plot.margin = margin(t = 5, r = 0, b = 10, l = 0, unit = "pt")) + 
  scale_x_date(breaks = years, labels = scales::date_format("%Y"), date_labels = "%Y")
 
# Combining the plots
combined_plot <- (p / time) + p2 +
  plot_layout(ncol = 1, heights = c(10,0.5,6)) +
  theme(plot.margin = unit(c(0,0,0,0), "null"))

print(combined_plot)

ggsave(plot = combined_plot, 
       file = 'plot1.png',
       height = 15,
       width = 15*16/8,
       units = 'cm',
       dpi = 600)
