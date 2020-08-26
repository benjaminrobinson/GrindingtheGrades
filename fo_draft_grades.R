options(stringsAsFactors = FALSE)
library(googlesheets4)
library(tidyverse)
library(scales)
library(janitor)
library(pins)
library(ggbeeswarm)

data.frame(
  team = c(
    "2TM",
    "3TM",
    "4TM",
    "ARI",
    "ATL",
    "BAL",
    "BUF",
    "CAR",
    "CHI",
    "CIN",
    "CLE",
    "DAL",
    "DEN",
    "DET",
    "GB",
    "HOU",
    "IND",
    "JAX",
    "KC",
    "LAC",
    "LAR",
    "MIA",
    "MIN",
    "NE",
    "NO",
    "NONE",
    "NYG",
    "NYJ",
    "OAK",
    "PHI",
    "PIT",
    "SD",
    "SEA",
    "SF",
    "STL",
    "TB",
    "TEN",
    "WAS"
  ),
  team_name = c(
    "2 Teams",
    "3 Teams",
    "4 Teams",
    "Arizona Cardinals",
    "Atlanta Falcons",
    "Baltimore Ravens",
    "Buffalo Bills",
    "Carolina Panthers",
    "Chicago Bears",
    "Cincinnati Bengals",
    "Cleveland Browns",
    "Dallas Cowboys",
    "Denver Broncos",
    "Detroit Lions",
    "Green Bay Packers",
    "Houston Texans",
    "Indianapolis Colts",
    "Jacksonville Jaguars",
    "Kansas City Chiefs",
    "Los Angeles Chargers",
    "Los Angeles Rams",
    "Miami Dolphins",
    "Minnesota Vikings",
    "New England Patriots",
    "New Orleans Saints",
    "None",
    "New York Giants",
    "New York Jets",
    "Las Vegas Raiders",
    "Philadelphia Eagles",
    "Pittsburgh Steelers",
    "Los Angeles Chargers",
    "Seattle Seahawks",
    "San Francisco 49ers",
    "Los Angeles Rams",
    "Tampa Bay Buccaneers",
    "Tennessee Titans",
    "Washington Football Team"
  )
) -> team_xwalk

source("draft_grades_analysis.R")

map(
  paste(c("Offensive", "Defensive"), "Players"),
  ~ read_sheet(
    "1roQGmBY8vNzSoSQT5pwuoLnNdpfI-K3ozEf8PSgz7qk",
    sheet = .,
    col_types = c("ccncccccnnnncnccccnncnccccnnnc")
  )
) %>%
  bind_rows %>%
  clean_names %>%
  mutate_all(~ ifelse(. == 'NA', NA, .)) -> dat_19


map(
  paste(c("Offensive", "Defensive"), "Players"),
  ~ read_sheet(
    "12ZJS_imqvpMaePQsvr3NZyUEHkTKQ8yFXu43nhDTDYk",
    sheet = .,
    col_types = c("ccncccccnnnncnccccnncnccccnnn")
  )
) %>%
  bind_rows %>%
  clean_names %>%
  mutate_all( ~ ifelse(. == 'NA', NA, .)) -> dat

source("fo_snaps.R")

source("fs_otc_draft_value.R")

## https://pages.collegeboard.org/how-to-convert-gpa-4.0-scale

bind_rows(
  grades %>%
    group_by_at(vars(
      -site, -url, -author, -number, -norm, -text, -letter
    )) %>%
    summarize(
      number = median(number, na.rm = TRUE),
      norm = median(norm, na.rm = TRUE)
    ) %>%
    mutate(
      site = 'Grader',
      author = 'Median',
      url = '',
      text = NA,
      letter = case_when(
        between(round(number, 1), 0, .9) ~ 'F',
        between(round(number, 1), 1, 1.2) ~ 'D',
        between(round(number, 1), 1.3, 1.6) ~ 'D+',
        between(round(number, 1), 1.7, 1.9) ~ 'C-',
        between(round(number, 1), 2, 2.2) ~ 'C',
        between(round(number, 1), 2.3, 2.6) ~ 'C+',
        between(round(number, 1), 2.7, 2.9) ~ 'B-',
        between(round(number, 1), 3, 3.2) ~ 'B',
        between(round(number, 1), 3.3, 3.6) ~ 'B+',
        between(round(number, 1), 3.7, 3.9) ~ 'A-',
        TRUE ~ 'A'
      ),
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
    ),
  grades
) %>%
  ungroup -> grades

data.frame(av_weight = seq(0, 1, .05) %>% rev,
           av_rank = 1:(seq(0, 1, .05) %>% length)) -> av_xwalk

bind_rows(dat,
          dat_19) %>%
  filter(yr >= 2012 & yr <= 2017) %>%
  group_by(fo_master_id) %>%
  mutate(av_rank = dense_rank(-(av * year))) %>%
  left_join(av_xwalk) %>%
  mutate(
    drafted_by = case_when(
      grepl("Chargers", drafted_by) ~ "Los Angeles Chargers",
      grepl("Cowboys", drafted_by) ~ "Dallas Cowboys",
      grepl("Colts", drafted_by) ~ "Indianapolis Colts",
      grepl("Rams", drafted_by) ~ "Los Angeles Rams",
      grepl("Titans", drafted_by) ~ "Tennessee Titans",
      grepl("Raiders", drafted_by) ~ "Las Vegas Raiders",
      grepl("Redskins", drafted_by) ~ "Washington Football Team",
      TRUE ~ drafted_by
    ),
    car_av = sum(av * av_weight)
  ) %>%
  left_join(team_xwalk) %>%
  left_join(
    snaps %>% mutate(season = as.numeric(season)) %>%
      filter(position_type == snap_type &
               !is.na(season) & team == 'NFL') %>%
      select(-team, -player),
    by = c(
      "pbp_id" = "pbp_name",
      "year" = "season",
      "pos2" = "position"
    )
  ) %>%
  mutate(pick = unlist(pick) %>% as.numeric) %>%
  left_join(otc) -> tmp

tmp %>%
  group_by(drafted_by, yr) %>%
  summarize(
    n_year = n_distinct(year),
    n_games = sum(g, na.rm = TRUE),
    n_starts = sum(gs, na.rm = TRUE),
    n_picks = n_distinct(fo_master_id),
    av = sum(av, na.rm = TRUE),
    car_av = sum(car_av, na.rm = TRUE),
    snaps = sum(snaps, na.rm = TRUE),
    pb = sum(pb, na.rm = TRUE),
    ap1 = sum(ap1, na.rm = TRUE)
  ) %>%
  left_join(grades, by = c('yr' = 'year', 'drafted_by' = 'team')) %>%
  left_join(
    tmp %>%
      distinct(drafted_by, yr, pick, fs_draft_value) %>%
      group_by(drafted_by, yr) %>%
      summarize(
        pick_val = sum(fs_draft_value, na.rm = TRUE),
        pick_sum = sum(pick, na.rm = TRUE)
      )
  ) %>%
  filter(!is.na(number)) %>%
  ungroup -> fin

fin %>%
  ggplot(aes(
    x = norm,
    y = av / n_year / n_picks,
    fill = factor(drafted_by),
    color = factor(drafted_by),
    text = paste(
      "<b>",
      " Team:",
      "</b>",
      drafted_by,
      "<br>",
      "<b>",
      "Year:",
      "</b>",
      yr,
      "<br>",
      "<b>",
      "AV:",
      "</b>",
      paste0(av, " (", percent_rank(av) %>% percent %>% gsub("%", "", .), " %ile)"),
      "<br>",
      "<b>",
      "Snaps:",
      "</b>",
      paste0(
        snaps %>% comma,
        " (",
        percent_rank(snaps) %>% percent %>% gsub("%", "", .),
        " %ile)"
      ),
      "<br>",
      "<b>",
      "Draft Capital:",
      "</b>",
      paste0(pick_val %>% comma,
             " (",
             n_picks,
             " Draft Picks)"),
      "<br>",
      "<b>",
      "Author & Site:",
      "</b>",
      paste(author, "|", site),
      "<br>",
      "<b>",
      "Grade:",
      "</b>",
      paste0(letter, " (", round(number, 2), ", ", round(norm, 2), " Normalized)"),
      "<br>"
    )
  )) +
  geom_beeswarm(show.legend = FALSE,
                size = 2,
                alpha = .5) +
  geom_smooth(
    show.legend = FALSE,
    method = 'lm',
    se = FALSE,
    formula = y ~ x,
    size = 1.25,
    aes(group = 1)
  ) +
  theme_pander() +
  labs(
    x = "Normalized Draft Grades",
    y = "Approximate Value Per Year Per Draft Pick",
    title = "Draft Grades Don't Tell Us Much About Future Value",
    caption = "Chart by @benj_robinson | Data by @pfref & @fboutsiders, 2020."
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "roboto", size = 16),
    axis.text = element_text(size = 12)
  ) +
  scale_fill_manual(values = fin %>%
                      distinct(drafted_by, primary) %>%
                      deframe) +
  scale_color_manual(values = fin %>%
                       distinct(drafted_by, secondary) %>%
                       deframe) -> overall_plot

ggsave(
  plot = overall_plot,
  "Chart 5.png",
  height = 6,
  width = 8,
  units = 'in',
  dpi = 96
)

fin %>%
  ggplot(aes(
    x = norm,
    y = av / n_year / n_picks,
    color = factor(drafted_by),
    fill = factor(drafted_by),
    text = paste(
      "<b>",
      " Team:",
      "</b>",
      drafted_by,
      "<br>",
      "<b>",
      "Year:",
      "</b>",
      yr,
      "<br>",
      "<b>",
      "AV:",
      "</b>",
      paste0(av, " (", percent_rank(av) %>% percent %>% gsub("%", "", .), " %ile)"),
      "<br>",
      "<b>",
      "Snaps:",
      "</b>",
      paste0(
        snaps %>% comma,
        " (",
        percent_rank(snaps) %>% percent %>% gsub("%", "", .),
        " %ile)"
      ),
      "<br>",
      "<b>",
      "Draft Picks:",
      "</b>",
      paste0(
        n_picks,
        " (",
        n_picks %>% percent_rank %>% percent %>% gsub("%", "", .),
        " %ile Draft Capital)"
      ),
      "<br>",
      "<b>",
      "Author & Site:",
      "</b>",
      paste(author, "|", site),
      "<br>",
      "<b>",
      "Grade:",
      "</b>",
      paste0(letter, " (", round(number, 2), ", ", round(norm, 2), " Normalized)"),
      "<br>"
    )
  )) +
  geom_point(show.legend = FALSE,
             size = 2,
             alpha = .5) +
  geom_smooth(
    show.legend = FALSE,
    method = 'lm',
    formula = y ~ x,
    aes(group = 1)
  ) +
  theme_light() +
  scale_fill_manual(values = fin %>%
                      distinct(drafted_by, primary) %>%
                      deframe) +
  scale_color_manual(values = fin %>%
                       distinct(drafted_by, secondary) %>%
                       deframe) +
  labs(
    x = "Draft Grades (Normalized)",
    y = "Approximate Value Per Year Per Draft Pick",
    title = "Graders Get Future Value Right Just as Much as They Get it Wrong",
    caption = "Chart by @benj_robinson | Data by @pfref & @fboutsiders, 2020."
  ) +
  facet_wrap(~ paste(author, "|", site), nrow = 2) +
  theme(legend.position = "none") -> av_plot

ggsave(
  plot = av_plot + theme_pander() + theme(
    text = element_text(family = "roboto", size = 16),
    axis.text = element_text(size = 12)
  ),
  "Chart 6.png",
  height = 7.5,
  width = 15,
  units = 'in',
  dpi = 96
)

library(plotly)

ggplotly(av_plot,
         tooltip = 'text') %>%
  layout(
    margin = list(l = -5,
                  b = -5),
    annotations = list(
      x = 1,
      y = -0.16,
      text = 'Chart by @benj_robinson | Data by @pfref & @fboutsiders, 2020.',
      showarrow = FALSE,
      xref = 'paper',
      yref = 'paper',
      xanchor = 'right',
      yanchor = 'auto',
      yshift = 27.5,
      font = list(size = 12, color = "black")
    )
  )

library(GGally)
library(broom)
library(ggforce)
library(concaveman)

bind_rows(
  fin,
  fin %>%
    filter(author != 'Median') %>%
    group_by(
      drafted_by = 'Average Football Team',
      primary = '#000000',
      secondary = '#000000',
      author = "Average",
      site = "Grader",
      yr
    ) %>%
    summarize_at(
      vars(
        n_picks,
        av,
        car_av,
        snaps,
        pb,
        ap1,
        norm,
        number,
        pick_val,
        pick_sum,
        n_year,
        n_games,
        n_starts
      ),
      ~ mean(., na.rm = TRUE)
    )
) %>%
  mutate(
    drafted_by = factor(drafted_by),
    drafted_by = relevel(drafted_by, ref = "Average Football Team"),
    author = factor(author),
    author = relevel(author, ref = 'Average')
  ) -> fin

lm(av ~ norm + drafted_by + pick_val + n_year + author,
   data = subset(fin, author != 'Median' | is.na(author))) -> mod

mod %>% glance

mod %>%
  ggcoef(sort = 'ascending') +
  theme_pander() +
  theme(
    legend.position = "none",
    text = element_text(family = "roboto", size = 16),
    axis.text = element_text(size = 12)
  ) +
  labs(
    x = "Regression Coefficient",
    y = "Model Features",
    title = "Being Drafted by a Good Team Goes A Long Way",
    caption = "Chart by @benj_robinson | Data by @pfref & @Jason_OTC/@PFF_Brad, 2020."
  ) -> coef_plot

coef_plot$data %>%
  mutate(
    term = case_when(
      term == 'norm' ~ 'Normalized Draft Grades',
      term == 'pick_val' ~ 'Fitzgerald-Spielberger Draft Chart Value',
      term == 'n_year' ~ 'Number of Years Played Since Drafted',
      TRUE ~ gsub("drafted_by", "Drafted By: ", term)
    )
  ) %>%
  filter(!grepl("author", term)) %>%
  arrange(desc(estimate)) %>%
  mutate(term = factor(term %>% reorder(estimate))) -> coef_plot$data

ggsave(
  plot = coef_plot,
  "Chart 7.png",
  height = 8,
  width = 8,
  units = 'in',
  dpi = 96
)

mod %>%
  augment %>%
  left_join(fin %>% distinct(av, norm, drafted_by, yr, author)) %>%
  distinct -> mod_aug

mod_aug %>%
  ggplot(aes(
    x = .fitted,
    y = av,
    fill = factor(drafted_by),
    color = factor(drafted_by)
  )) +
  geom_jitter(aes(alpha = ifelse((drafted_by == 'Seattle Seahawks' &
                                    yr == 2012) |
                                   (drafted_by == 'Las Vegas Raiders' &
                                      yr == 2014) |
                                   (drafted_by == 'Houston Texans' &
                                      yr == 2014) |
                                   (drafted_by == 'Miami Dolphins' &
                                      yr == 2013),
                                 .75,
                                 0
  )),
  show.legend = FALSE,
  size = 3) +
  geom_smooth(
    show.legend = FALSE,
    method = 'lm',
    se = FALSE,
    size = 2,
    formula = y ~ x,
    aes(group = 1)
  ) +
  geom_mark_rect(
    data = mod_aug %>% filter(drafted_by == 'Seattle Seahawks' &
                                yr == 2012),
    aes(
      label = "Seattle Seahawks '12",
      description = "Russell Wilson and Bobby Wagner headline the #1 Class by Approximate Value.",
      fill = NA
    ),
    label.fill = NA
  ) +
  geom_mark_rect(
    data = mod_aug %>% filter(drafted_by == 'Las Vegas Raiders' &
                                yr == 2014),
    aes(
      label = "Las Vegas Raiders '14",
      description = "Khalil Mack and Derek Carr have also been highly productive in their time in the NFL.",
      fill = NA
    ),
    label.fill = NA
  ) +
  geom_mark_hull(
    data = mod_aug %>% filter((drafted_by == 'Houston Texans' &
                                 yr == 2014) |
                                (drafted_by == 'Miami Dolphins' &
                                   yr == 2013)
    ),
    aes(
      label = "Dolphins '13 & Texans '14",
      description = "High draft picks don't always mean high AV.",
      group = 1,
      fill = NA
    ),
    label.fill = NA,
    label.buffer = unit(-3, 'cm')
  ) +
  theme_pander() +
  theme(
    legend.position = "none",
    text = element_text(family = "roboto", size = 16),
    axis.text = element_text(size = 12)
  ) +
  scale_fill_manual(values = fin %>%
                      distinct(drafted_by, primary) %>%
                      deframe) +
  scale_color_manual(values = fin %>%
                       distinct(drafted_by, secondary) %>%
                       deframe) +
  labs(
    x = "Draft Class Predicted Approximate Value",
    y = "Draft Class Actual Approximate Value",
    title = "Predicting Draft Class Future Value",
    caption = "Chart by @benj_robinson | Data by @pfref & @Jason_OTC/@PFF_Brad, 2020."
  ) +
  scale_x_continuous(limits = c(0, 250))

ggsave(
  "Chart 8.png",
  height = 8,
  width = 8,
  units = 'in',
  dpi = 96
)
