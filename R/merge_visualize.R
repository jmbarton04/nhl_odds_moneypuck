library(dplyr)
library(readr)
library(ggplot2)
library(ggimage)

# -------------------------
# Load today's data
# -------------------------
games_df <- read_csv("data/games_today.csv", show_col_types = FALSE)
perf_df  <- read_csv("data/moneypuck_today.csv", show_col_types = FALSE)

# -------------------------
# TEAM lookup (home team → team_code)
# -------------------------
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
# TEAM LOGO lookup (via ESPN)
# -------------------------
logo_lookup <- tibble(
  team_code = team_lookup$team_code,
  logo = paste0(
    "https://a.espncdn.com/i/teamlogos/nhl/500/",
    tolower(team_lookup$team_code),
    ".png"
  )
)

# -------------------------
# Merge all data
# -------------------------
merged_today <- games_df %>%
  left_join(team_lookup, by = "home_team") %>%
  left_join(perf_df,     by = c("team_code" = "team")) %>%
  left_join(logo_lookup, by = "team_code") %>%
  mutate(edge = home_prob - xGoalsPercentage)

# ----------------------------------------
# PLOT 1: Market vs MoneyPuck with LOGOS
# ----------------------------------------
p1 <- ggplot(merged_today, aes(x = home_prob, y = xGoalsPercentage)) +
  geom_image(aes(image = logo), size = 0.06) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Market-Implied Win Prob vs MoneyPuck xG%",
    subtitle = "Home teams today",
    x = "Market-implied probability",
    y = "MoneyPuck xGoalsPercentage"
  )

# ----------------------------------------
# PLOT 2: Edge Plot (Market – MoneyPuck)
# ----------------------------------------
p2 <- ggplot(merged_today,
             aes(x = edge, y = reorder(home_team, edge))) +
  geom_point(size = 5, color = "red") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Today's Edges: Market – MoneyPuck",
    subtitle = "Positive values = Market overestimates team strength",
    x = "Betting Edge",
    y = "Team"
  )

# -------------------------
# Save both plots
# -------------------------
dir.create("plots", showWarnings = FALSE)

ggsave("plots/today_plot_logos.png",
       p1, width = 10, height = 6)

ggsave("plots/today_edge_plot.png",
       p2, width = 10, height = 6)

# -------------------------
# Historical logging
# -------------------------
if (!dir.exists("history")) dir.create("history")

history_file <- "history/games_history.csv"

merged_today <- merged_today %>% mutate(run_date = Sys.Date())

if (!file.exists(history_file)) {
  write_csv(merged_today, history_file)
} else {
  write_csv(merged_today, history_file, append = TRUE)
}
