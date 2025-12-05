library(dplyr)
library(readr)
library(ggplot2)

# -------------------------
# Load today's data
# -------------------------
games_df <- read_csv("data/games_today.csv", show_col_types = FALSE)
perf_df  <- read_csv("data/moneypuck_today.csv", show_col_types = FALSE)

# Lookup table for team codes (matches MoneyPuck)
team_lookup <- tibble(
  home_team = c(
    "Anaheim Ducks","Arizona Coyotes","Boston Bruins","Buffalo Sabres",
    "Calgary Flames","Carolina Hurricanes","Chicago Blackhawks","Colorado Avalanche",
    "Columbus Blue Jackets","Dallas Stars","Detroit Red Wings","Edmonton Oilers",
    "Florida Panthers","Los Angeles Kings","Minnesota Wild","Montréal Canadiens",
    "Nashville Predators","New Jersey Devils","New York Islanders","New York Rangers",
    "Ottawa Senators","Philadelphia Flyers","Pittsburgh Penguins","San Jose Sharks",
    "Seattle Kraken","St Louis Blues","Tampa Bay Lightning","Toronto Maple Leafs",
    "Utah Mammoth","Vancouver Canucks","Vegas Golden Knights","Washington Capitals",
    "Winnipeg Jets"
  ),
  team_code = c(
    "ANA","ARI","BOS","BUF","CGY","CAR","CHI","COL","CBJ","DAL","DET",
    "EDM","FLA","LAK","MIN","MTL","NSH","NJD","NYI","NYR","OTT","PHI",
    "PIT","SJS","SEA","STL","TBL","TOR","UTA","VAN","VGK","WSH","WPG"
  )
)

# -------------------------
# Merge today's dataset
# -------------------------
merged_today <- games_df %>%
  left_join(team_lookup, by = "home_team") %>%
  left_join(perf_df, by = c("team_code" = "team"))

# -------------------------
# Plot of Market vs MoneyPuck
# -------------------------
p <- merged_today %>%
  ggplot(aes(x = home_prob,
             y = xGoalsPercentage,
             label = home_team)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_text(size = 3, vjust = -0.7) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Market-Implied Win Prob vs MoneyPuck xG%",
    subtitle = "Home teams today",
    x = "Market-implied probability",
    y = "MoneyPuck xGoalsPercentage"
  )

dir.create("plots", showWarnings = FALSE)
ggsave("plots/today_plot.png", p, width = 10, height = 6)

# -------------------------
# Historical logging
# -------------------------
if (!dir.exists("history")) dir.create("history")

history_file <- "history/games_history.csv"

merged_today <- merged_today %>%
  mutate(run_date = Sys.Date())

if (!file.exists(history_file)) {
  write_csv(merged_today, history_file)
} else {
  write_csv(merged_today, history_file, append = TRUE)
}

