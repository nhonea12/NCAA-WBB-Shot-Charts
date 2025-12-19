library(tidyverse)
library(shiny)
library(wehoop) # for WBB data
library(showtext) # for adding fonts
library(ggimage) # for incorporating images into the shot charts
library(shinycssloaders)

# Add Google font "Roboto"
font_add_google("Roboto", "roboto")

# Enable showtext for plots
showtext_auto()

# include WBB data in the server so it automatically updates


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





# user interface for the Shiny app
ui <- fluidPage(
  h1("NCAA Women's Basketball Player Shot Charts"),
  # sidebar panel of the user interface
  sidebarLayout(
    sidebarPanel(
      h2("Select Team and Player"),
      h5("Team and player names must match what is seen on ESPN.com"),
      # allow user to set the team name
      textInput(
        inputId = "team_name",
        label = "Team:",
        value = "NC State"
      ),
      # allow the user to set the player name
      textInput(
        inputId = "player_name",
        label = "Player:",
        value = "Tilda Trygger"
      ),
      actionButton(
        inputId = "action_button",
        label = "Press to Plot"
      )
    ),
    # main panel of the user interface
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput(
          outputId = "shot_chart",
          width = "100%",
          height = "650px"
          )
        )
    )
  )
)




# server for the Shiny app
server <- function(input, output, session) {
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
      x_coord = -1*(coordinate_x_raw - 25),
      y_coord = coordinate_y_raw + 5
    )
  
  
  # subset the data to only the specified player's shots, only update when the action button is pressed
  player_shots <- eventReactive(input$action_button, {
    wbb_shots |> 
    filter(team_short_display_name == input$team_name, 
           athlete_display_name == input$player_name)
    })
  
  # make the player and team name's update only when the action button is pressed
  selected_player <- eventReactive(input$action_button, {
    list(
      name = input$player_name,
      team = input$team_name
    )
  })
  
  # reactive plot of player shots
  output$shot_chart <- renderPlot({
    # download player headshot if available
    if(!is.na(player_shots()$athlete_headshot_href[1])){
      headshot_png <- paste0(str_replace_all(selected_player()$name, " ", ""), "Headshot.png")
      download.file(player_shots()$athlete_headshot_href[1], 
                    destfile = headshot_png)
    }
    # create a new variable that stores the player name in a way that it will still show the headshot if the player has a ' in their name
    html_safe_player_name <- selected_player()$name |> 
      str_replace_all(" ", "") |> 
      str_replace_all("'", "&#39;")
    # now create the shot chart
    ggplot(data = player_shots(),
           aes(x = x_coord,
               y = y_coord,
               shape = scoring_play,
               color = scoring_play,
               fill = scoring_play))  +
      # plot court
      geom_path(data = court_points,
                aes(x = x, y = y, group = desc),
                color = "black", linewidth = .25,
                , inherit.aes = FALSE)  +
      coord_fixed(clip = 'off') +
      # custom theme
      theme_f5()  +
      # set opacity limits
      scale_alpha_continuous(range = c(0.4, 1)) +
      # set y-axis limits
      scale_y_continuous(limits = c(-2.5, 45), oob = scales::oob_squish) +
      # set x-axis limits
      scale_x_continuous(limits = c(-30, 30), oob = scales::oob_squish) + 
      geom_jitter(size = 4, 
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
            plot.title = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 40),
            plot.subtitle = element_text(face = 'bold', hjust= .5, vjust = 0, family = "roboto", size = 25, lineheight = 0.25),
            plot.caption = element_text(size = 5)) + 
      # add labels 
      labs(
        #title = paste0(player, " Shot Chart"),
        title = ifelse(!is.na(player_shots()$athlete_headshot_href[1]),
                       paste0("<img src = '", html_safe_player_name, "Headshot.png'", " height = 50>",
                              "<span style='font-size: 40pt'>",
                              selected_player()$name,
                              " Shot Chart</span>"),
                       paste0(selected_player()$name, " Shot Chart")
        ),
        subtitle = paste0("#", player_shots()$athlete_jersey[1], ", ", player_shots()$athlete_position_name[1])) +
      annotate("label", 
               x = 20, y = 42, 
               label = paste0("FG: ", nrow(player_shots() %>% filter(grepl("made", text))), "/", nrow(player_shots()), " \n 3 FG: ", nrow(player_shots() %>% filter(grepl("Three", text), grepl("made", text))), "/", nrow(player_shots() %>% filter(grepl("Three", text)))),
               size = 5, 
               color = "black", 
               fill = "floralwhite", 
               fontface = "bold",
               lineheight = 1, # Controls line spacing (default is 1)
               label.padding = unit(0.1, "lines"),  # Reduces padding inside the label (less space between text and label border)
               
      ) +
      theme(
        plot.title = ggtext::element_markdown()
      )
  })
}
#run the application
shinyApp(ui = ui, server = server)