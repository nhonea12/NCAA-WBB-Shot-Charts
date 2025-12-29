library(wehoop)
library(tidyverse)
library(hexbin)

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
   ) |> 
  dplyr::select(
    team_location,
    athlete_display_name,
    text,
    loc_x,
    loc_y,
    scoring_play,
    team_logo,
    athlete_jersey,
    athlete_headshot_href,
    athlete_position_name,
    athlete_position_abbreviation,
    athlete_id_1,
    team_id,
    team_name,
    team_abbreviation,
    team_slug,
    team_color
  )

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
      TRUE                 ~ 2L
    )
  )


wbb_shots |> names()

wbb_shots$text |> head()
wbb_shots$shot_zone_area |> head()
wbb_shots$shot_zone_range |> head()
typeof(wbb_shots$shot_value)


averages <- wbb_shots |> 
  group_by(
    shot_zone_range, 
    shot_zone_area
    ) |> 
  summarize(
    fgm = sum(shot_made_numeric),
    fga = n(),
    league_pct = fgm/fga,
    .groups = "drop"
  )

averages

# create the court points
##Draw court (from https://raw.githubusercontent.com/Henryjean/NBA-Court/refs/heads/main/CourtDimensions.R) (dimensions changed from NBA to college)
width = 50
height = 94/2
key_height = 19
inner_key_width = 12
outer_key_width = 12 #Edited to same as inner key, as no outer key in college
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 22.1458333 #Edited to college 3 point line
three_point_side_radius = 21.65625 #Edited to college 3 point line
three_point_side_height = 9.86458333 #Edited to college 3 point line

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

court_points = data.frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2, 
        outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2,
        -backboard_width / 2, backboard_width / 2, 
        0, 0),
  y = c(height, 0, 0, height, height, 0, key_height, key_height, 0,
        backboard_offset, backboard_offset, 
        backboard_offset, backboard_offset + neck_length),
  desc = c(rep("perimeter", 5), rep("outer_key", 4), rep("backboard", 2),
           rep("neck", 2))
)

# define foul circle
foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom") #Not included in this chart as no bottom ft circle in college

# define hoop
hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop") 
restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted")

# define 3-point line
three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
three_point_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)

court_points = rbind(court_points , foul_circle_top, hoop, restricted, three_point_line)











hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots  |> 
    group_by(hexbin_id) |>
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  hexbin_ids_to_zones = shots |>
    dplyr::group_by(hexbin_id, shot_zone_range, shot_zone_area) |>
    summarize(attempts = n(), .groups = "drop") |>
    arrange(hexbin_id, desc(attempts)) |>
    group_by(hexbin_id) |>
    filter(row_number() == 1) |>
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    tibble(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}


calculate_hexbins_from_shots = function(shots, league_averages, binwidths = c(1, 1), min_radius_factor = 0.6, fg_diff_limits = c(-0.12, 0.12), fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) {
  shots <- tibble::as_tibble(as.data.frame(shots))
  league_averages <- tibble::as_tibble(as.data.frame(league_averages))
  
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = shots |> 
    group_by(shot_zone_range, shot_zone_area)
  
  zone_stats = grouped_shots |>
    summarize(
      zone_attempts = n(),
      zone_pct = mean(shot_made_numeric),
      zone_points_scored = sum(shot_made_numeric * shot_value),
      zone_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  league_zone_stats = league_averages |>
    group_by(shot_zone_range, shot_zone_area) |>
    summarize(league_pct = sum(fgm) / sum(fga), .groups = "drop")
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("shot_zone_area", "shot_zone_range")
  
  hex_data = hex_data |>
    inner_join(zone_stats, by = join_keys) |>
    inner_join(league_zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * (hex_attempts / max_hex_attempts)^0.35,
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
}


percent_formatter = function(x) {
  scales::percent(x, accuracy = 1)
}

points_formatter = function(x) {
  scales::comma(x, accuracy = 0.01)
}

# altered ggplot theme from the F5
theme_f5 <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "roboto") %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin=margin(2.5,0,10,0), size = 9), 
      plot.caption = element_text(color = 'gray65', margin=margin(-2,0,0,0), hjust = 1, size = 6)
    )
}

generate_hex_chart = function(hex_data, base_court, metric = "bounded_fg_diff", alpha_range = c(0.85, 0.98)) {
  if (length(hex_data) == 0) {
    return(base_court)
  }
  
  if (metric == "bounded_fg_diff") {
    fill_limit = hex_data$fg_diff_limits
    fill_label = "FG% vs. League Avg"
    label_formatter = percent_formatter
  } else if (metric == "bounded_fg_pct") {
    fill_limit = hex_data$fg_pct_limits
    fill_label = "FG%"
    label_formatter = percent_formatter
  } else if (metric == "bounded_points_per_shot") {
    fill_limit = hex_data$pps_limits
    fill_label = "Points Per Shot"
    label_formatter = points_formatter
  } else {
    stop("invalid metric")
  }
  
  ggplot() + 
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc),
            color = "black", linewidth = .25,
            , inherit.aes = FALSE) +
    geom_polygon(
      data = hex_data$hex_data,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id,
        fill = !!sym(metric),
        alpha = hex_attempts
      ),
      # size = court_theme$hex_border_size,
      # color = court_theme$hex_border_color
    ) +
    # custom theme
    theme_f5()  +
    # set opacity limits
    #scale_alpha_continuous(range = c(0.4, 1)) +
    # set y-axis limits
    scale_y_continuous(limits = c(-2.5, 45), oob = scales::oob_squish) +
    # set x-axis limits
    scale_x_continuous(limits = c(-30, 30), oob = scales::oob_squish) + 
    geom_jitter(size = 2, 
                show.legend = FALSE, 
                width = 0.4, 
                height = 0.4 #Change amount of variation in jitter (default width and height is 0.4)
    ) + #add shots
    scale_shape_manual(values = c(4,19)) + #X's and O's
    scale_color_manual(values = c("#000000", "#CC0000")) +
    # theme tweaks
    theme(legend.position = 'none',
          line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          plot.margin = margin(.25, 0, 0.25, 0, "lines"),
          plot.title = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 25),
          plot.subtitle = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 15, lineheight = 0.25),
          plot.caption = element_text(size = 5)) + 
    scale_fill_viridis_c(
      paste0(fill_label, "   "),
      limit = fill_limit,
      labels = label_formatter,
      guide = guide_colorbar(barwidth = 15)
    ) +
    scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    theme(legend.text = element_text(size = rel(0.6)))
}

nrow(tilda_shots)
tilda_shots <- wbb_shots |> 
  filter(
    athlete_display_name == "Tilda Trygger"
  )

tilda_hexbin_data <- calculate_hexbins_from_shots(shots = tilda_shots, league_averages = averages)

generate_hex_chart(
  hex_data = tilda_hexbin_data,
  base_court = court_points
)

tilda_hexbin_data$hex_data$hex_attempts |> sum()
