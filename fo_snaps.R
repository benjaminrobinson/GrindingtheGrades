library(tidyverse)
library(rvest)
library(janitor)

map_df(2012:2019, function(x) {
  paste0(
    "https://www.footballoutsiders.com/stats/nfl/snap-counts?year=",
    x,
    "&team=ALL&week=ALL&position=ALL&op=Submit&form_build_id=form-WypiYkisD1PYE2NDPV3LGh-jIK1LTLLx_yiq2XSN9wQ&form_id=fo_stats_snap_counts_form"
  ) %>%
    read_html() %>%
    html_table %>%
    .[[2]] %>%
    mutate(season = as.character(x))
}) %>%
  bind_rows %>%
  clean_names %>%
  mutate(
    # player_number = gsub("-.*", "", player),
    pbp_name = player,
    player = gsub("[0-9][0-9][-]", "", player),
    player = gsub("[0-9][-]", "", player),
    player = sub("[.]", " ", player),
    position_type = case_when(
      position %in% c('DB', 'DL', 'LB') ~ 'def',
      position == 'ST' ~ 'st',
      TRUE ~ 'off'
    )
  ) %>%
  mutate_at(vars(contains("pct")), ~ (gsub("[%]", "", .) %>% as.numeric) / 100) %>%
  select(-total_snaps) %>%
  group_by(season,
           player,
           pbp_name,
           team,
           position,
           position_type) %>%
  mutate_at(vars(contains("pct")), ~ max(., na.rm = TRUE)) %>%
  distinct %>%
  group_by(team, season) %>%
  mutate(
    snaps_off = ifelse(off_snap_pct == max(off_snap_pct), off_snaps / off_snap_pct, NA),
    snaps_off = ifelse(
      is.nan(snaps_off) |
        is.infinite(snaps_off),
      max(off_snaps),
      snaps_off
    ),
    off_snap_pct = ifelse(
      off_snap_pct == 0 &
        off_snaps != 0,
      off_snaps / snaps_off,
      off_snap_pct
    ),
    snaps_def = ifelse(def_snap_pct == max(def_snap_pct), def_snaps / def_snap_pct, NA),
    snaps_def = ifelse(
      is.nan(snaps_def) |
        is.infinite(snaps_def),
      max(def_snaps),
      snaps_def
    ),
    def_snap_pct = ifelse(
      def_snap_pct == 0 &
        def_snaps != 0,
      def_snaps / snaps_def,
      def_snap_pct
    ),
    snaps_st = ifelse(st_snap_pct == max(st_snap_pct), st_snaps / st_snap_pct, NA),
    snaps_st = ifelse(
      is.nan(snaps_st) | is.infinite(snaps_st),
      max(st_snaps),
      snaps_st
    ),
    st_snap_pct = ifelse(st_snap_pct == 0 &
                           st_snaps != 0, st_snaps / snaps_st, st_snap_pct)
  ) %>%
  mutate_at(vars(contains("snaps_")), ~ max(., na.rm = TRUE) %>% round) %>%
  # rowwise %>%
  # mutate(
  #   snaps_total = snaps_off + snaps_def + snaps_st,
  #   total_snap_pct = total_snaps / snaps_total
  # ) %>%
  ungroup -> tmp


bind_rows(
  tmp %>%
    group_by(player,
             pbp_name,
             position,
             position_type,
             season,
             team = 'NFL') %>%
    summarize_at(vars(-contains('pct')), ~ sum(., na.rm = TRUE)),
  tmp %>%
    group_by(
      pbp_name,
      player,
      position,
      position_type,
      season = 'All',
      team = 'NFL'
    ) %>%
    summarize_at(vars(-contains('pct')), ~ sum(., na.rm = TRUE)),
  tmp
) %>%
  group_by(player, pbp_name, position, position_type, season) %>%
  mutate(
    off_snap_pct = ifelse(n() == 2, max(off_snap_pct, na.rm = TRUE), NA),
    off_snap_pct = coalesce(off_snap_pct, off_snaps / snaps_off),
    def_snap_pct = ifelse(n() == 2, max(def_snap_pct, na.rm = TRUE), NA),
    def_snap_pct = coalesce(def_snap_pct, def_snaps / snaps_def),
    st_snap_pct = ifelse(n() == 2, max(st_snap_pct, na.rm = TRUE), NA),
    st_snap_pct = coalesce(st_snap_pct, st_snaps / snaps_off)
    # , total_snap_pct = ifelse(n() == 2, max(total_snap_pct, na.rm = TRUE), NA)
    # , total_snap_pct = coalesce(total_snap_pct, total_snaps / snaps_total)
  ) %>%
  select(-contains("snaps_")) %>%
  gather(key,
         value,
         -player,
         -pbp_name,
         -team,
         -position,
         -position_type,
         -season) %>%
  mutate(
    snap_type = case_when(
      grepl("def", key) ~ 'def',
      grepl("off", key) ~ 'off',
      grepl("st", key) ~ 'st',
      grepl("total", key) ~ 'total'
    ),
    metric_type = gsub(".*_", "", key),
    key = NULL
  ) %>%
  distinct %>%
  filter(value > 0) %>%
  group_by_at(vars(-value)) %>%
  filter(value == max(value)) %>%
  spread(metric_type, value) %>%
  ungroup -> snaps

rm(tmp)
