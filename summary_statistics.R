library(dplyr)
library(tidyr)

final_output_for_ml <- "cleaned_data_final.csv"
data <- read.csv(final_output_for_ml)

summary_stats <- data %>%
  select(all_of(selected_variables)) %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )

write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)
