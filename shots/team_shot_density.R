### Functionalizing Dan Morse's shot charts
### Using the nhlapi package
### Not my code

### Create a comparison of shots for and shots allowed

### Load packages
library(hockeyR)
library(nhlapi)
library(tidyverse)
library(sportyR)
library(glue)
library(ggtext)

### Pull in game_ids for a team
get_game_ids_team <- function(season, team_name){
  ### @params: season: int the season of interest for a team
  ### @params: team_name: str for the team of interest
  ### @returns: game_ids: list of game_ids
  
  ### Pull in the live game data via nhlapi
  game_ids <- get_game_ids(season) %>%
    filter(home_name == team_name |
             away_name == team_name) %>%
    filter(game_type == "REG") %>%
    select(game_id)
  
  ### Convert to list of ids
  game_ids = as.list(game_ids[['game_id']])
  return(game_ids)
}

### Get live game data
live_data <- function(game_id){
  ### @params: game_id: int for the game id code
  ### @returns: shots_df: dataframe of all shots
  
  ### Pull in the live game data via nhlapi
  live_game_df <- nhl_games_feed(game_id)
  
  ### Set the date home team
  DATE = as.Date(live_game_df[[1]][["gameData"]][["datetime"]][["dateTime"]])
  HOME_TEAM = live_game_df[[1]][["gameData"]][["teams"]][["home"]][["name"]]
  AWAY_TEAM = live_game_df[[1]][["gameData"]][["teams"]][["away"]][["name"]]
  
  ### Pull just the shots
  shots_df <- live_game_df[[1]][["liveData"]][["plays"]][["allPlays"]]# %>%
    #dplyr::mutate(date = DATE,
    #       home_team = HOME_TEAM,
    #       away_team = AWAY_TEAM)
  
  return(shots_df)
}

### Loop through each game of the season
full_season_data <- function(season, team_name){
  ### @params: season: int the season of interest for a team
  ### @params: team_name: str for the team of interest
  ### @returns: full_season_df: a df of the full season results
  
  ### Pull in the live game data via nhlapi
  
  ### Get each game id
  game_ids = get_game_ids_team(season, team_name)
  
  ### Create a blank dataframe and build the full seaosn df
  full_season_df = data.frame()
  for (i in game_ids){
    df_game <- live_data(i)
    df_year <- data.frame(df_game)
    full_season_df <- rbind(full_season_df, df_year)
  }
  
  return(full_season_df)
}

### Plot the density of each team's shots
plot_shot_chart <- function(season, team_name){
  ### @params: season: int the season of interest for a team
  ### @params: team_name: str for the team of interest
  ### @returns: shots_plot: a png of a team's shots
  
  ### Load in shots data
  shot_df <- full_season_data(season, team_name) %>%
    filter(result.event %in% c("Blocked Shot", "Goal", "Missed Shot", "Shot")) %>%
    ### Fix Coordinates
    mutate(coordinates.x = ifelse(team.name == team_name & coordinates.x < 0,
                                  coordinates.x * -1,
                                  coordinates.x),
           coordinates.y = ifelse(team.name == team_name & coordinates.x < 0,
                                  coordinates.y * -1,
                                  coordinates.y),
           coordinates.x = ifelse(team.name != team_name & coordinates.x > 0,
                                  coordinates.x * -1,
                                  coordinates.x),
           coordinates.y = ifelse(team.name != team_name & coordinates.x > 0,
                                  coordinates.y * -1,
                                  coordinates.y),)
  
  ### Load in team logo data
  team_logos <- hockeyR::team_logos_colors %>%
    filter(full_team_name == team_name) %>%
    ### Set the coordinates of the shot
    mutate(x = 0,
           y = 0) %>%
    select(team_logo_espn, team_color1, team_color2, x, y) %>%
    ### Convert white to silver
    mutate(team_color1 = ifelse(team_color1 == "#FFFFFF",
                                "#C0C0C0",
                                team_color1),
           team_color2 = ifelse(team_color2 == "#FFFFFF",
                                "#C0C0C0",
                                team_color2))
  
  ### Make transparent (hockeyR)
  transparent <- function(img) {
    magick::image_fx(img, expression = "0.5*a", channel = "alpha")
  }
  
  ### Merge the dataframes (hockeyR)
  full_shots_df <- cbind(shot_df, team_logos)
  
  ### Plot the rink (sportyR)
  geom_hockey("nhl") +
    ### Plot logo
    ggimage::geom_image(
      data = team_logos,
      aes(x = x, y = y, image = team_logo_espn),
      image_fun = transparent, size = 0.12, asp = 2.35) +
    ### Plot the density of shots for team
    stat_density_2d(
      data = filter(full_shots_df, team.name == team_name),
      geom = "polygon",
      aes(x = coordinates.x,
          y = coordinates.y,
          alpha = stat(nlevel)),
      fill = unique(full_shots_df$team_color1)) +
    ### Plot the density of shots allowed
    stat_density_2d(
      data = filter(full_shots_df, team.name != team_name),
      geom = "polygon",
      aes(x = coordinates.x,
          y = coordinates.y,
          alpha = stat(nlevel)),
      fill = unique(full_shots_df$team_color2)) +
    ### Give titles (hockeyR)
    labs(
      title = glue("**{team_name} Shot Chart**"),
      subtitle = glue("{season-1} - {season} Seasons<br>
                      <span style='color:{unique(full_shots_df$team_color2)};'>Shots Against</span> 
                      ||
                      <span style='color:{unique(full_shots_df$team_color1)};'>Shots For</span>"),
      caption = "Data from nhlalpi\nPlot made with hockeyR and sportyR",
      alpha = "Shot Density") +
      ### Adjust theme (hockeyR)
      theme(
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_text(hjust = .9)
      )
    
    ### Save the image
    ggsave(glue("shots_plot_{team_name}_{season}.jpeg"),
           width = 20, height = 12, units = "cm")
   
}
