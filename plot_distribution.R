# replace this with your directory
setwd("/home/xuyuan/Desktop/2024 fall/BIOSTAT625-Project")

# we first directly merge the data together:
library(dplyr)
library(data.table)
library(zoo)
library(bit64)
library(ggplot2)

final_output_for_ml <- "cleaned_data_final.csv"
data <- read.csv(final_output_for_ml)

filtered_data <- data %>% filter(`Cost.to.Revenue.Ratio` < 1)
plot <- ggplot(filtered_data, aes(x = `Cost.to.Revenue.Ratio`)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "lightblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Cost to Revenue Ratio",
       x = "Cost to Revenue Ratio",
       y = "Density") +
  theme_minimal()
ggsave(filename = "Cost_to_Revenue_Ratio_Distribution.pdf",
       plot = plot,
       device = "pdf",
       dpi = 150,
       width = 8,
       height = 6)


plot2 <- ggplot(data, aes(x = `Revenue.per.Bed`)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "lightblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Revenue per Bed",
       x = "Revenue per Bed",
       y = "Density") +
  theme_minimal()

plot2

ggsave(filename = "Revenue_per_Bed_Distribution.pdf",
       plot = plot2,
       device = "pdf",
       dpi = 150,
       width = 8,
       height = 6)
