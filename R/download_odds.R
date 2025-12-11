# -----------------------------
# Load Required Libraries
# -----------------------------
# httr: handles API requests (GET, POST, etc.)
# jsonlite: parses JSON returned from API
# dplyr: data wrangling + pipes
# readr: write CSV files
# lubridate: convert timestamps
# purrr: map_dfr for list → dataframe conversion


library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

# -----------------------------
# API Key and Request URL
# -----------------------------

api_key <- "5282f67c29680a6e2eb36b79b1305ac5"

# Construct full Odds API endpoint URL
# h2h = head-to-head (moneyline odds)
# regions=us ensures US sportsbooks
# oddsFormat=american returns +150 / -180 style odds

odds_url <- paste0(
  "https://api.the-odds-api.com/v4/sports/icehockey_nhl/odds/?",
  "apiKey=", api_key, "&",
  "regions=us&",
  "markets=h2h&",
  "oddsFormat=american"
)

## API request
odds_response <- GET(odds_url)

## Stops if API fails

if (status_code(odds_response) != 200) {
  stop("Odds API request failed.")
}

## Converts JSON to a list in R

odds_data <- content(odds_response, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyVector = FALSE)

## gets all the moneylines prices for each team

get_price_for_team <- function(outcomes, team_name) {
  if (length(outcomes) == 0) return(NA_real_)
  names_vec <- vapply(outcomes, function(x) x$name, character(1))
  idx <- which(names_vec == team_name)
  if (length(idx) == 0) return(NA_real_)
  as.numeric(outcomes[[idx[1]]]$price)
}

## Built a data frame for all the games with prices and teams
                      
games_df <- map_dfr(
  odds_data,
  function(g) {
    if (length(g$bookmakers) == 0) return(NULL)
    bm <- g$bookmakers[[1]]
    if (length(bm$markets) == 0) return(NULL)
    
    mkt  <- bm$markets[[1]]
    outs <- mkt$outcomes
    
    home_price <- get_price_for_team(outs, g$home_team)
    away_price <- get_price_for_team(outs, g$away_team)
    
    tibble(
      game_id       = g$id,
      commence_time = ymd_hms(g$commence_time),
      home_team     = g$home_team,
      away_team     = g$away_team,
      home_price    = home_price,
      away_price    = away_price
    )
  }
)

## converted odds into probabilities for the visualization                      
                      
implied_prob <- function(price) {
  ifelse(
    is.na(price),
    NA_real_,
    ifelse(price < 0,
           (-price) / ((-price) + 100),
           100 / (price + 100))
  )
}

## added probabilities to the data frame                      
games_df <- games_df %>%
  mutate(
    home_prob = implied_prob(home_price),
    away_prob = implied_prob(away_price)
  )

dir.create("data", showWarnings = FALSE)
write_csv(games_df, "data/games_today.csv")
