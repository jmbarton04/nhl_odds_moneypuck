library(readr)
library(dplyr)

## scraped the csv

mp_url <- "https://moneypuck.com/moneypuck/playerData/seasonSummary/2025/regular/teams.csv"

moneypuck_teams <- read_csv(mp_url, show_col_types = FALSE)

## created a performance data frame

perf_df <- moneypuck_teams %>%
  filter(situation == "all") %>%
  transmute(
    team = name,
    xGoalsPercentage,
    xGoalsFor,
    scoreVenueAdjustedxGoalsFor,
    goalsFor
  )

dir.create("data", showWarnings = FALSE)
write_csv(perf_df, "data/moneypuck_today.csv")
