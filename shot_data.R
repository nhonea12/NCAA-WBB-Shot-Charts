library(tidyverse)
library(wehoop)

# load NCAA women's basketball play-by-play data
wbb_pbp <- wehoop::load_wbb_pbp()

# load player box score data
player_box_scores <- load_wbb_player_box()

# add player names and headshots to play-by-play data
wbb_pbp <- wbb_pbp |> 
  left_join(player_box_scores, by = c("athlete_id_1" = "athlete_id", 
                                      "team_id" = "team_id",
                                      "season" = "season",
                                      "season_type" = "season_type",
                                      "game_id" = "game_id",
                                      "game_date" = "game_date",
                                      "game_date_time" = "game_date_time"))


# get just shoooting plays from the play-by-play data and adjust their coordinates
wbb_shots <- wbb_pbp |> 
  filter(
    shooting_play == TRUE, 
    !(type_text %in% c("MadeFreeThrow", "MissedFreeThrow"))
  ) |> 
  mutate(
    loc_x = -1*(coordinate_x_raw - 25),
    loc_y = coordinate_y_raw + 5
  )

# add in shot distances and areas that can be used for the hexagonal charts
wbb_shots <- wbb_shots |>
  mutate(
    shot_distance = sqrt(loc_x^2 + loc_y^2) - 5.25,
    shot_zone_range = case_when(
      shot_distance <= 4 ~ "Restricted Area",
      shot_distance <= 8 ~ "In The Paint (Non-RA)",
      shot_distance <= 16 ~ "Mid-Range",
      shot_distance <= 22.1458 ~ "Mid-Range",
      TRUE ~ "3PT"
    ),
    shot_zone_area = case_when(
      loc_x < -8 ~ "Left Side",
      loc_x >  8 ~ "Right Side",
      TRUE       ~ "Center"
    ),
    shot_zone_area = case_when(
      shot_zone_range == "3PT" & abs(loc_x) > 22 ~ "Corner",
      TRUE ~ shot_zone_area
    ),
    shot_made_numeric = as.integer(scoring_play),
    shot_value = case_when(
      grepl("three", text) ~ 3L,
      grepl("Three", text) ~ 3L,
      TRUE                 ~ 2L
    )
  )

saveRDS(wbb_shots, "wbb_shots.rds")
