library(tidyverse)
library(shiny)
library(bslib)
library(wehoop) # for WBB data
library(showtext) # for adding fonts
library(ggimage) # for incorporating images into the shot charts
library(shinycssloaders) # to create a loading graphic while charts are being created
library(ggtext)
library(ggnewscale) # for the density charts

# Add Google font "Roboto"
font_add_google("Roboto", "roboto")

# Enable showtext for plots
showtext_auto()

# read in shot data from rds file (rds file created in shot_data.R)
wbb_shots <- readRDS("wbb_shots.rds")

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
      # # allow user to select a traditional shot chart or density chart
      # radioButtons(
      #   inputId = "chart_type",
      #   label = "Chart Type:",
      #   choices = c("Shot Chart", "Density Chart"),
      #   selected = "Shot Chart"
      # ),
      # require action button to plot a new plot
      actionButton(
        inputId = "action_button",
        label = "Press to Plot"
      )
    ),
    # main panel of the user interface
    mainPanel(
      card(
        shinycssloaders::withSpinner(
          plotOutput(
            outputId = "shot_chart",
            width = "100%",
            height = "650px"
            )
          ),
        card_footer("Data from ESPN. Pulled using the wehoop package.")
        )
      )
    )
  )



# server for the Shiny app
server <- function(input, output, session) {
  
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
  
  # # create a reactive value for the selected chart type, so it will only update when the action button is pressed
  # selected_chart_type <- eventReactive(input$action_button, {
  #   input$chart_type
  # })
  # 
  # # calculate density data outside of the render plot function
  # density_data <- eventReactive(input$action_button, {
  #   
  #   # how detailed we want our contours to be
  #   n = 300
  #   
  #   # filter data for every player other than our player of interest, assign it to p2
  #   p2 <- wbb_shots %>% 
  #     dplyr::select(loc_x, loc_y, athlete_display_name, team_short_display_name) %>% 
  #     filter(athlete_display_name != selected_player()$name & team_short_display_name != selected_player()$team)
  #   
  #   # get x/y coords as vectors
  #   p1_x <- pull(player_shots(), loc_x)
  #   p1_y <- pull(player_shots(), loc_y)
  #   
  #   # get x/y coords as vectors
  #   p2_x <- pull(p2, loc_x)
  #   p2_y <- pull(p2, loc_y)
  #   
  #   # get x and y range to compute comparisons across
  #   x_rng = range(c(-27.5, 27.5))
  #   y_rng = range(c(0, 52))
  #   
  #   # Explicitly calculate bandwidth for future use
  #   bandwidth_x <- MASS::bandwidth.nrd(c(p1_x, p2_x))
  #   bandwidth_y <- MASS::bandwidth.nrd(c(p1_y, p2_y))
  #   
  #   bandwidth_calc <- c(bandwidth_x, bandwidth_y)
  #   
  #   # Calculate the density estimate over the specified x and y range
  #   d2_p1 = MASS::kde2d(p1_x, p1_y, h = c(7, 7), n=n, lims=c(x_rng, y_rng))
  #   d2_p2 = MASS::kde2d(p2_x, p2_y, h = c(7, 7), n=n, lims=c(x_rng, y_rng))
  #   
  #   # Create a new dataframe that contains the difference in shot density between our two dataframes 
  #   df_diff <- d2_p1
  #   
  #   # matrix subtraction density from p1 from league average
  #   df_diff$z <- d2_p1$z - d2_p2$z
  #   
  #   # add matrix col names
  #   colnames(df_diff$z) <- df_diff$y
  #   
  #   # Convert list to dataframe with relevant variables and columns
  #   df_diff <- df_diff$z %>% 
  #     as_tibble() %>% 
  #     mutate(loc_x = df_diff$x) %>% 
  #     pivot_longer(-loc_x, names_to = "loc_y", values_to = "z") %>% 
  #     mutate(loc_y = as.double(loc_y))
  #   
  #   # create a separate dataframe for values that are less than 0
  #   df_negative <- df_diff %>% filter(z < 0)
  #   
  #   # make positive 
  #   df_negative$z <- abs(df_negative$z)
  #   
  #   # if less than 0, make 0
  #   df_diff$z <- ifelse(df_diff$z < 0, 0, df_diff$z)
  #   
  #   # return these values as a list
  #   list(
  #     df_diff = df_diff,
  #     df_negative = df_negative
  #   )
  #   
  # })
  
  
  # reactive plot of player shots
  output$shot_chart <- renderPlot({
    # require the number of shots by a player to be greater than 0 for the plot to render
    req(
      player_shots(), 
      nrow(player_shots()) > 0
      )
    
    #if (selected_chart_type() == "Shot Chart"){
    # now create the shot chart
    ggplot(data = player_shots(),
           aes(x = loc_x,
               y = loc_y,
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
        #title = paste0(html_safe_player_name, " Shot Chart"),
        title = ifelse(!is.na(player_shots()$athlete_headshot_href[1]),
                       paste0("<img src = '", player_shots()$athlete_headshot_href[1], "' height = 50>",
                              "<span style='font-size: 40pt'>",
                              selected_player()$name,
                              " Shot Chart</span>"),
                       paste0(selected_player()$name, " Shot Chart")
        ),
        subtitle = paste0("#", player_shots()$athlete_jersey[1], ", ", player_shots()$athlete_position_name[1])) +
      annotate("label", 
               x = 20, y = 42, 
               label = paste0("FG: ", nrow(player_shots() %>% filter(grepl("made", text))), "/", nrow(player_shots()), " \n 3 FG: ", nrow(player_shots() %>% filter((grepl("Three", text) | grepl("three", text)), grepl("made", text))), "/", nrow(player_shots() %>% filter((grepl("Three", text) | grepl("three", text))))),
               size = 8, 
               color = "black", 
               fill = "floralwhite", 
               fontface = "bold",
               lineheight = 0.8, # Controls line spacing (default is 1)
               label.padding = unit(0.2, "lines"),  # Reduces padding inside the label (less space between text and label border)
               
      ) +
      theme(
        plot.title = ggtext::element_markdown()
      )
  # } else {
  # 
  # # plot of player shot densities
  #   ggplot()  +
  #     # plot court
  #     geom_path(data = court_points,
  #               aes(x = x, y = y, group = desc),
  #               color = "black", linewidth = .25)  +
  #     coord_fixed(clip = 'off') +
  #     # custom theme
  #     theme_f5()  +
  #     # set opacity limits
  #     scale_alpha_continuous(range = c(0.4, 1)) +
  #     # set y-axis limits
  #     scale_y_continuous(limits = c(-2.5, 45)) +
  #     # set x-axis limits
  #     scale_x_continuous(limits = c(-30, 30)) + 
  #     # first layer (high freqency spots)
  #     geom_raster(data = density_data()$df_diff %>% filter(z >= mean(z)), aes(x = loc_x, y = loc_y, alpha = sqrt(z), fill = sqrt(z)))  +
  #     stat_contour(data = density_data()$df_diff %>% filter(z >= mean(z)), aes(x = loc_x, y = loc_y, z = sqrt(z), color = ..level..), linewidth = .25, bins = 4) +
  #     scale_fill_gradient2(low = 'floralwhite', mid = 'floralwhite', high = "#cc0000",  trans = 'sqrt')  +
  #     scale_color_gradient2(low = "floralwhite", mid = 'floralwhite', high = "#cc0000",  trans = 'sqrt') +
  #     # second layer (low frequency spots)
  #     new_scale_fill() +
  #     new_scale_color() +
  #     geom_raster(data = density_data()$df_negative %>% filter(z >= mean(z)), aes(x = loc_x, y = loc_y, alpha = sqrt(z), fill = sqrt(z)))  +
  #     stat_contour(data = density_data()$df_negative %>% filter(z >= mean(z)), aes(x = loc_x, y = loc_y, z = sqrt(z), color = ..level..), linewidth = .25, bins = 4) +
  #     scale_fill_gradient2(low = "floralwhite", mid = "floralwhite", high = "#aaaaaa",  trans = 'sqrt') +
  #     scale_color_gradient2(low = "floralwhite", mid = "floralwhite", high = "#aaaaaa", trans = 'sqrt') +
  #     # theme tweaks
  #     theme(legend.position = 'none',
  #           line = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.title.y = element_blank(),
  #           axis.text.x = element_blank(),
  #           axis.text.y = element_blank(), 
  #           plot.margin = margin(.25, 0, 0.25, 0, "lines"),
  #           plot.title = element_text(face = 'bold', hjust= .5, vjust = -2.5, family = "roboto"),
  #           plot.subtitle = element_text(face = 'bold', hjust= .5, vjust = -3.5, family = "roboto", size = 25, lineheight = 0.25),
  #           plot.caption = element_text(family = "roboto", size = 15))  +
  #     labs(
  #       title = ifelse(!is.na(player_shots()$athlete_headshot_href[1]),
  #                         paste0("<img src = '", player_shots()$athlete_headshot_href[1], "' height = 50>",
  #                                "<span style='font-size: 40pt'>",
  #                                selected_player()$name,
  #                                " Density Chart</span>"),
  #                         paste0(selected_player()$name, " Density Chart")
  #     ),
  #       subtitle = paste0("#", player_shots()$athlete_jersey[1], ", ", player_shots()$athlete_position_name[1]),
  #     caption = "Densities are relative to national average") +
  #     theme(
  #       plot.title = ggtext::element_markdown()
  #     )
  #   } # end of else
  }) # end of renderPlot
} # end of server function

#run the application
shinyApp(ui = ui, server = server)