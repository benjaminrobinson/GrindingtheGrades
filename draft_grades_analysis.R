library(tidyverse)
library(googlesheets4)
library(ggthemes)
library(ggridges)
library(teamcolors)
library(bestNormalize)
library(ggpubr)
library(patchwork)
library(showtext)
font_add_google("Roboto", "roboto")

brightness <- function(hex) {
  result <- rep(0, length(hex))
  for (i in 2:7)
  {
    ch <- substr(hex, i, i)
    result <- result + ifelse(i %% 2 == 0, 16, 1) * case_when(
      ch == "0" ~ 0,
      ch == "1" ~ 1,
      ch == "2" ~ 2,
      ch == "3" ~ 3,
      ch == "4" ~ 4,
      ch == "5" ~ 5,
      ch == "6" ~ 6,
      ch == "7" ~ 7,
      ch == "8" ~ 8,
      ch == "9" ~ 9,
      ch == "a" | ch == "A" ~ 10,
      ch == "b" | ch == "B" ~ 11,
      ch == "c" | ch == "C" ~ 12,
      ch == "d" | ch == "D" ~ 13,
      ch == "e" | ch == "E" ~ 14,
      ch == "f" | ch == "F" ~ 15,
      TRUE ~ 0
    )
  }
  return(result)
}

read_sheet("1x8FTY7yaAPdWeJIIvQNREl76-VuY-HyC1avISkikzZQ") %>%
  mutate(
    text = gsub("â€™", "'", text),
    norm = bestNormalize(number)$x.t,
    letter = factor(
      letter,
      levels = c(
        "F-",
        "F",
        "F+",
        "D-",
        "D",
        "D+",
        "C-",
        "C",
        "C+",
        "B-",
        "B",
        "B+",
        "A-",
        "A",
        "A+"
      )
    )
  ) %>%
  left_join(
    teamcolors %>%
      filter(league == 'nfl') %>%
      select(-league, team = name) %>%
      mutate(
        use_color1 = ifelse(brightness(primary) > 140, primary, secondary),
        use_color2 = ifelse(brightness(primary) <= 140, secondary, primary)
      ) %>%
      select(-primary,-secondary) %>%
      rename(primary = use_color1, secondary = use_color2) %>%
      select(team, primary, secondary, everything())
  ) %>%
  group_by(team) %>%
  mutate(med = median(number, na.rm = TRUE),
         med_norm = median(norm, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(
    team = ifelse(team == 'Oakland Raiders', 'Las Vegas Raiders', team),
    team = ifelse(team == 'Washington Redskins', 'Washington Football Team', team),
    location = ifelse(location == 'Oakland', 'Las Vegas', location),
    mascot = ifelse(mascot == 'Redskins', "Football Team", mascot)
  ) -> grades

showtext_auto()
grades %>%
  filter(!is.na(letter)) %>%
  ggplot(aes(x = letter)) +
  geom_bar(fill = '#F4911E',
           color = 'black',
           stat = 'count') +
  theme_pander() +
  labs(
    x = "Letter Grade",
    y = "Count of Letter Grades",
    title = "Draft Grades are Pretty Optimistic",
    caption = "Chart and Data by @benj_robinson, 2020."
  ) +
  theme(text = element_text(family = "roboto", size = 16),
        axis.text = element_text(size = 12))

ggsave(
  "Chart 1.png",
  height = 6,
  width = 6,
  units = 'in',
  dpi = 96
)


grades %>%
  ggplot() +
  geom_density(aes(x = norm),
               binwidth = .5,
               fill = 'dodgerblue',
               color = 'black') +
  theme_pander() +
  labs(
    x = "Normalized Letter Grades",
    y = "Density of Letter Grades",
    caption = "Chart and Data by @benj_robinson, 2020.",
    title = "Distribution of Normalized Draft Grades (2012-2017)"
  ) +
  stat_overlay_normal_density(aes(norm),
                              color = "black",
                              linetype = "dashed",
                              size = 1) +
  theme(text = element_text(family = "roboto", size = 16),
        axis.text = element_text(size = 12))

ggsave(
  "Chart 2.png",
  height = 6,
  width = 6,
  units = 'in',
  dpi = 96
)


grades %>%
  ggplot(aes(x = norm, fill = author)) +
  geom_histogram(bins = 10, show.legend = FALSE) +
  theme_pander() +
  labs(
    x = "Normalized Draft Grade",
    y = "Count of Normalized Draft Grade Bin",
    title = "Some Graders are Stricter than Others",
    caption = "Chart and Data by @benj_robinson, 2020."
  ) +
  facet_wrap( ~ paste(author, "-", site)) +
  scale_fill_manual(values = c(colorblind_pal()(8), "#808080")) +
  theme(text = element_text(family = "roboto", size = 16),
        axis.text = element_text(size = 12))

ggsave(
  "Chart 3.png",
  height = 8,
  width = 10,
  units = 'in',
  dpi = 96
)


grades %>%
  ggplot(aes(
    x = norm,
    y = factor(team, levels = unique(team[order(division, med_norm, team)]), ordered =
                 TRUE),
    fill = factor(team)
  )) +
  geom_density_ridges(show.legend = FALSE, alpha = .5) +
  theme_pander() +
  scale_fill_manual(values = grades %>%
                      distinct(team, primary) %>%
                      deframe) +
  labs(
    x = "Normalized Draft Grade",
    y = "",
    title = "Draft Grades Don't Vary Too Much from Team to Team",
    caption = "Chart and Data by @benj_robinson, 2020."
  ) +
  facet_wrap(division ~ ., nrow = 4, scales = 'free_y') +
  theme(text = element_text(family = "roboto", size = 16),
        axis.text = element_text(size = 12))

ggsave(
  "Chart 4.png",
  height = 8,
  width = 10,
  units = 'in',
  dpi = 96
)

rm(brightness)
