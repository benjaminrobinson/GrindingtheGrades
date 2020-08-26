library(tidyverse)
library(rvest)
library(janitor)

'https://overthecap.com/draft-trade-value-chart/' %>%
  read_html %>%
  html_table %>%
  lapply(clean_names) %>%
  bind_rows %>%
  select_if(~ all(!is.na(.))) %>%
  select(-contains("pick")) %>%
  rename(value_1 = value) %>%
  pivot_longer(
    cols = starts_with("value_"),
    names_to = "pick",
    values_to = "fs_draft_value",
    values_drop_na = TRUE
  ) %>%
  arrange(desc(fs_draft_value)) %>%
  mutate(
    pick = 1:nrow(.)
  ) -> otc
