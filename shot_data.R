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

saveRDS(wbb_shots, "wbb_shots.rds")