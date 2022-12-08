### Functionalizing Dan Morse's shot charts
### Using the nhlapi package
### Not my code

### Load packages
library(hockeyR)
library(nhlapi)
library(tidyverse)
library(sportyR)
library(glue)

### Write a function to load the game data
live_data <- function(game_id){
  ### @params: game_id: int for the game id code
  ### returns: shots_df: dataframe of all shots
  
  ### Pull in the live game data via nhlapi
  live_game_df <- nhl_games_feed(game_id)
  
  ### Set the date home team
  DATE = as.Date(live_game_df[[1]][["gameData"]][["datetime"]][["dateTime"]])
  HOME_TEAM = live_game_df[[1]][["gameData"]][["teams"]][["home"]][["name"]]
  AWAY_TEAM = live_game_df[[1]][["gameData"]][["teams"]][["away"]][["name"]]
  
  ### Pull just the shots
  shots_df <- live_game_df[[1]][["liveData"]][["plays"]][["allPlays"]] %>%
    filter(result.event %in% c("Blocked Shot", "Goal", "Missed Shot", "Shot")) %>%
    mutate(date = DATE,
           home_team = HOME_TEAM,
           away_team = AWAY_TEAM)
  
  return(shots_df)
}

### Function to plot the shot data
### Using Dan Morse's code from the hockeyR package
plot_shots <- function(game_id){
  ### @params: game_id: int for the game id code
  ### returns: shots.jpeg: a picture of the shot chart
  
  ### Pull in the live game data via nhlapi
  shots_df <- live_data(game_id) %>%
    ### Adjust the coordinates of the shot
    ### Flip home and away locations
    mutate(coordinates.x = ifelse((team.name == home_team & coordinates.x < 0) |
                                    (team.name == away_team & coordinates.x > 0),
                                  coordinates.x * -1,
                                  coordinates.x))
  
  ### Load in the logos (hockeyR)
  team_logos <- hockeyR::team_logos_colors %>%
    filter(full_team_name %in% unique(shots_df$team.name)) %>%
    ### Set the coordinates of the shot
    mutate(x = ifelse(full_team_name == unique(shots_df$home_team), 50, -50),
           y = 0)
  
  ### Make transparent (hockeyR)
  transparent <- function(img) {
    magick::image_fx(img, expression = "0.3*a", channel = "alpha")
  }
  
  ### Merge the dataframes (hockeyR)
  full_shots_df <- shots_df %>%
    ### Add team colors
    left_join(team_logos, by = c("team.triCode" = "team_abbr"))
  
  ### Plot the shot chart (sportyR and hockeyR)
  shots_plot <- geom_hockey("nhl") +
    ### Plot the rink (sportyR)
    ggimage::geom_image(
      data = team_logos,
      aes(x = x, y = y, image = team_logo_espn),
      image_fun = transparent, size = 0.22, asp = 2.35) +
    ### Plot the data points (hockeyR)
    geom_point(
      data = full_shots_df,
      aes(coordinates.x, coordinates.y, shape = result.event),
      size = 3,
      color = full_shots_df$team_color2) +
    ### Give titles (hockeyR)
    labs(
      title = glue("{unique(full_shots_df$away_team)} @ {unique(full_shots_df$home_team)}"),
      subtitle = glue("{unique(full_shots_df$date)}"),
      caption = "Data from nhlalpi\nPlot made with hockeyR and sportyR",
      shape = "Shot Type") +
    ### Adjust theme (hockeyR)
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = .9)
    )
  ggsave(glue("shots_plot_{unique(full_shots_df$away_team)}_{unique(full_shots_df$home_team)}_{unique(full_shots_df$away_team)}.jpeg"),
         width = 20, height = 12, units = "cm")
}
