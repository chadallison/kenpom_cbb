
### Data Acquisition

<details>
<summary>
View Code
</summary>

``` r
my_cols = c(
  "rk", "team", "conf", "w_l", "adj_em", "adj_o", "adj_o_rk", "adj_d", "adj_d_rk",
  "adj_t","adj_t_rk", "luck", "luck_rk", "sos_adj_em", "sos_adj_em_rk", "sos_opp_o",
  "sos_opp_o_rk", "sos_opp_d", "sos_opp_d_rk", "ncsos_adj_em", "ncsos_adj_em_rk"
)

get_kenpom = function() {
  url = "https://kenpom.com/"
  webpage = read_html(url)
  tbl = (webpage |> html_nodes(css = "#data-area") |> html_table())[[1]]
  
  df = tbl |>
    setNames(my_cols) |>
    filter(rk != "Rk" & rk != "") |>
    mutate(rk = as.integer(rk), adj_em = as.numeric(adj_em), adj_o = as.numeric(adj_o),
           adj_o_rk = as.integer(adj_o_rk), adj_d = as.numeric(adj_d), adj_d_rk = as.integer(adj_d_rk),
           adj_t = as.numeric(adj_t), adj_t_rk = as.integer(adj_t_rk), luck = as.numeric(luck),
           luck_rk = as.integer(luck_rk), sos_adj_em = as.numeric(sos_adj_em),
           sos_adj_em_rk = as.integer(sos_adj_em_rk), sos_opp_o = as.numeric(sos_opp_o),
           sos_opp_o_rk = as.integer(sos_opp_o_rk), sos_opp_d = as.numeric(sos_opp_d),
           sos_opp_d_rk = as.integer(sos_opp_d_rk), ncsos_adj_em = as.numeric(ncsos_adj_em),
           ncsos_adj_em_rk = as.integer(ncsos_adj_em_rk))
  
  return(df)
}

# kp_raw = get_kenpom() |>
#   mutate(team = trimws(str_remove_all(team, "\\d+")))

kpr = suppressMessages(read_excel("kenpom_raw.xlsx"))

kp_raw = kpr |>
    setNames(my_cols) |>
    filter(rk != "Rk" & rk != "") |>
    mutate(rk = as.integer(rk), adj_em = as.numeric(adj_em), adj_o = as.numeric(adj_o),
           adj_o_rk = as.integer(adj_o_rk), adj_d = as.numeric(adj_d), adj_d_rk = as.integer(adj_d_rk),
           adj_t = as.numeric(adj_t), adj_t_rk = as.integer(adj_t_rk), luck = as.numeric(luck),
           luck_rk = as.integer(luck_rk), sos_adj_em = as.numeric(sos_adj_em),
           sos_adj_em_rk = as.integer(sos_adj_em_rk), sos_opp_o = as.numeric(sos_opp_o),
           sos_opp_o_rk = as.integer(sos_opp_o_rk), sos_opp_d = as.numeric(sos_opp_d),
           sos_opp_d_rk = as.integer(sos_opp_d_rk), ncsos_adj_em = as.numeric(ncsos_adj_em),
           ncsos_adj_em_rk = as.integer(ncsos_adj_em_rk)) |>
  mutate(team = str_trim(str_remove_all(team, "\\d+")))

games_raw = cbd_torvik_game_box(year = 2025)
print("Data acquisition complete")
```

</details>

    ## [1] "Data acquisition complete"

------------------------------------------------------------------------

### Matching Team Names Between Data Sources

<details>
<summary>
View Code
</summary>

``` r
kp_teams = sort(unique(kp_raw$team))

torvik_teams = games_raw |>
  count(team) |>
  filter(n >= 20) |>
  pull(team)

matched_teams = intersect(kp_teams, torvik_teams)
unmatched_teams = kp_teams[!kp_teams %in% matched_teams]

kp = kp_raw |>
  mutate(team = case_when(team == "CSUN" ~ "Cal St. Northridge",
                          team == "Kansas City" ~ "UMKC",
                          team == "McNeese" ~ "McNeese St.",
                          team == "Nicholls" ~ "Nicholls St.",
                          team == "SIUE" ~ "SIU Edwardsville",
                          team == "Southeast Missouri" ~ "Southeast Missouri St.",
                          team == "East Texas A&M" ~ "Texas A&M Commerce",
                          T ~ team))

if (length(intersect(kp$team, torvik_teams)) == length(torvik_teams)) {
  print("Team names matched successfully")
} else {
  print("Team names not matched")
}
```

</details>

    ## [1] "Team names matched successfully"

------------------------------------------------------------------------

### Building Master Game Results Data Set

<details>
<summary>
View Code
</summary>

``` r
valid_game_ids = games_raw |>
  filter(team %in% torvik_teams) |>
  count(game_id) |>
  filter(n == 2) |>
  pull(game_id)

get_home_team = function(gid) {
  teams = games_raw |>
    filter(game_id == gid) |>
    pull(team)
  
  loc1 = str_locate(gid, teams[1])[1]
  loc2 = str_locate(gid, teams[2])[1]
  
  if (loc1 < loc2) {
    return(teams[2])
  } else {
    return(teams[1])
  }
}

get_away_team = function(gid) {
  teams = games_raw |>
    filter(game_id == gid) |>
    pull(team)
  
  loc1 = str_locate(gid, teams[1])[1]
  loc2 = str_locate(gid, teams[2])[1]
  
  if (loc1 < loc2) {
    return(teams[1])
  } else {
    return(teams[2])
  }
}

home_away_teams = data.frame(game_id = valid_game_ids) |>
  mutate(home_team = sapply(game_id, get_home_team),
         away_team = sapply(game_id, get_away_team))

team_game_pts_scored = games_raw |>
  distinct(game_id, team, pts)

game_results = home_away_teams |>
  inner_join(games_raw |>
  distinct(game_id, date), by = "game_id") |>
  select(game_id, date, home_team, away_team) |>
  inner_join(team_game_pts_scored, by = c("game_id", "home_team" = "team")) |>
  rename(home_score = pts) |>
  inner_join(team_game_pts_scored, by = c("game_id", "away_team" = "team")) |>
  rename(away_score = pts) |>
  mutate(win_team = ifelse(home_score > away_score, home_team, away_team),
         lose_team = ifelse(home_score > away_score, away_team, home_team),
         win_score = ifelse(home_score > away_score, home_score, away_score),
         lose_score = ifelse(home_score > away_score, away_score, home_score))

sprintf("%s game results retrieved successfully", nrow(game_results))
```

</details>

    ## [1] "5742 game results retrieved successfully"

------------------------------------------------------------------------

### KenPom Top 25

<details>
<summary>
View Code
</summary>

``` r
better_date = function(date) {
  return(paste0(month(date, label = T, abbr = F), " ", day(date), ", ", year(date)))
}

fig_data = kp |>
  slice_max(adj_em, n = 25, with_ties = F) |>
  mutate(team = paste0(team, " (", rk, ")"))

fig_data |>
  ggplot(aes(reorder(team, adj_em), adj_em)) +
  geom_col(fill = "#556d56") +
  geom_text(aes(label = adj_em), size = 3, hjust = -0.25) +
  coord_flip(ylim = c(0, max(fig_data$adj_em) * 1.05)) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  labs(x = NULL, y = "Adj. EM",
       title = "KenPom Top 25")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

------------------------------------------------------------------------

### Chad’s NPR Metric

<details>
<summary>
View Code
</summary>

``` r
get_team_off_ppg = function(tm) {
  home = game_results |> filter(home_team == tm) |> pull(home_score)
  away = game_results |> filter(away_team == tm) |> pull(away_score)
  return(round(mean(c(home, away)), 3))
}

get_team_def_ppg = function(tm) {
  home = game_results |> filter(home_team == tm) |> pull(away_score)
  away = game_results |> filter(away_team == tm) |> pull(home_score)
  return(round(mean(c(home, away)), 3))
}

all_teams = sort(unique(c(game_results$home_team, game_results$away_team)))

team_ppg = data.frame(team = all_teams) |>
  mutate(off_ppg = sapply(team, get_team_off_ppg),
         def_ppg = sapply(team, get_team_def_ppg))

npr_results = game_results |>
  inner_join(team_ppg, by = c("home_team" = "team")) |>
  rename(home_off_ppg = off_ppg, home_def_ppg = def_ppg) |>
  inner_join(team_ppg, by = c("away_team" = "team")) |>
  rename(away_off_ppg = off_ppg, away_def_ppg = def_ppg) |>
  mutate(home_exp = (home_off_ppg + away_def_ppg) / 2,
         away_exp = (away_off_ppg + home_def_ppg) / 2,
         home_off_npr = home_score - home_exp,
         home_def_npr = away_exp - away_score,
         away_off_npr = away_score - away_exp,
         away_def_npr = home_exp - home_score)

get_team_off_npr = function(tm) {
  home = npr_results |> filter(home_team == tm) |> pull(home_off_npr)
  away = npr_results |> filter(away_team == tm) |> pull(away_off_npr)
  return(round(mean(c(home, away)), 3))
}

get_team_def_npr = function(tm) {
  home = npr_results |> filter(home_team == tm) |> pull(home_def_npr)
  away = npr_results |> filter(away_team == tm) |> pull(away_def_npr)
  return(round(mean(c(home, away)), 3))
}

team_npr = data.frame(team = all_teams) |>
  mutate(off_npr = sapply(team, get_team_off_npr),
         def_npr = sapply(team, get_team_def_npr),
         ovr_npr = off_npr + def_npr) |>
  arrange(desc(ovr_npr))

inner_join(x = team_npr |> select(team, ovr_npr) |> mutate(npr_rk = rank(-ovr_npr)),
           y = kp |> select(team, kp_rank = rk, adj_em),
           by = "team") |>
  filter(kp_rank <= 25) |>
  ggplot(aes(ovr_npr, adj_em)) +
  geom_point(size = 2, col = "#556d56", shape = "square") +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 25) +
  scale_x_continuous(breaks = seq(0, 20, by = 0.5)) +
  scale_y_continuous(breaks = seq(0, 50, by = 2)) +
  labs(x = "Chad's Naive Performance Rating (NPR)", y = "KenPom's Net Adj. Efficiency Rating",
       title = "KenPom Top 25 + NPR", subtitle = "Teams above/below dashed line are better/worse than my NPR metric per KenPom")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
bubble_teams = c("North Carolina", "West Virginia", "Indiana", "Xavier", "Texas", "Ohio St.", "Boise St.", "San Diego St.")

team_npr |>
  mutate(npr_rank = rank(-ovr_npr)) |>
  distinct(team, ovr_npr, npr_rank) |>
  filter(team %in% bubble_teams)
```

    ##             team ovr_npr npr_rank
    ## 1 North Carolina   5.180       33
    ## 2      Boise St.   4.676       41
    ## 3       Ohio St.   4.537       46
    ## 4         Xavier   4.495       48
    ## 5          Texas   3.983       55
    ## 6  West Virginia   3.685       60
    ## 7  San Diego St.   3.616       62
    ## 8        Indiana   3.465       68

------------------------------------------------------------------------

### Building Data for Modeling

<details>
<summary>
View Code
</summary>

``` r
kp_refined = kp |>
  select(-contains("rk"), -conf)

kp_names = names(kp_refined)

home_kp = kp_refined |>
  setNames(paste0("home_", kp_names))

away_kp = kp_refined |>
  setNames(paste0("away_", kp_names))

modeling_results = game_results |>
  transmute(home_team, away_team,
            home_win = ifelse(win_team == home_team, 1, 0)) |>
  inner_join(home_kp, by = "home_team") |>
  inner_join(away_kp, by = "away_team") |>
  select(-c(home_team, away_team)) |>
  mutate(home_win = factor(home_win))

sprintf("Modeling data: %s games, %s variables", nrow(modeling_results), ncol(modeling_results) - 1)
```

</details>

    ## [1] "Modeling data: 5742 games, 20 variables"

------------------------------------------------------------------------

### Matchup Data Generation Function

<details>
<summary>
View Code
</summary>

``` r
# helper function to generate modeling data for a given matchup
generate_matchup_data = function(home_team, away_team) {
  return(data.frame(home_team = home_team, away_team = away_team) |>
    inner_join(home_kp, by = "home_team") |>
    inner_join(away_kp, by = "away_team") |>
    select(-c(home_team, away_team)))
}

# example output
generate_matchup_data(home_team = "North Carolina", away_team = "Duke")
```

</details>

    ##   home_w_l home_adj_em home_adj_o home_adj_d home_adj_t home_luck
    ## 1    22-13       19.55      118.9       99.3       70.4    -0.019
    ##   home_sos_adj_em home_sos_opp_o home_sos_opp_d home_ncsos_adj_em away_w_l
    ## 1           12.18          113.3          101.2             13.34     31-3
    ##   away_adj_em away_adj_o away_adj_d away_adj_t away_luck away_sos_adj_em
    ## 1       38.16        128       89.8       65.7    -0.019           10.03
    ##   away_sos_opp_o away_sos_opp_d away_ncsos_adj_em
    ## 1          112.4          102.4              9.31

------------------------------------------------------------------------

### Modeling

<details>
<summary>
View Code
</summary>

``` r
today_seed = as.numeric(str_remove_all(Sys.Date(), "-"))
set.seed(today_seed)
train_index = sample(nrow(modeling_results), 0.7 * nrow(modeling_results))
train_data = modeling_results[train_index, ]
test_data = modeling_results[-train_index, ]
rf_model = randomForest(home_win ~ ., data = train_data)
predictions = predict(rf_model, newdata = test_data)
accuracy = round(mean(predictions == test_data$home_win) * 100, 2)
sprintf("Random forest accuracy: %s%%", accuracy)
```

</details>

    ## [1] "Random forest accuracy: 72.78%"

<details>
<summary>
View Code
</summary>

``` r
conf_matrix = table(predictions, test_data$home_win)
precision = conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall = conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score = round(2 * precision * recall / (precision + recall) * 100, 2)
sprintf("Random forest F1: %s%%", f1_score)
```

</details>

    ## [1] "Random forest F1: 79.2%"

------------------------------------------------------------------------

### Generating Game Prediction Functions

<details>
<summary>
View Code
</summary>

``` r
make_game_prediction = function(home_team, away_team) {
  game_data = generate_matchup_data(home_team = home_team, away_team = away_team)
  home_win = predict(rf_model, game_data)[[1]]
  win_probs = predict(rf_model, game_data, type = "prob")
  if (home_win == 1) {
    return(sprintf("%s def. %s (%s%%)", home_team, away_team, win_probs[2] * 100))
  } else if (home_win == 0) {
    return(sprintf("%s def. %s (%s%%)", away_team, home_team, win_probs[1] * 100))
  }
}

neutral_predict = function(home_team, away_team) {
  if (home_team == "" & away_team == "") return("Select two teams to predict game outcome")
  if ((home_team == "" & away_team != "") | (home_team != "" & away_team == "")) {
    return("Select two teams to predcit game outcome")
  }
  if (home_team == away_team) return("Please select different teams")
  game_data = generate_matchup_data(home_team = home_team, away_team = away_team)
  other_data = generate_matchup_data(home_team = away_team, away_team = home_team)
  probs1 = predict(rf_model, game_data, type = "prob")[2]
  probs2 = predict(rf_model, other_data, type = "prob")[1]
  ovr = round(mean(c(probs1, probs2)) * 100, 1)
  if (ovr == 50) {
    return("Toss up")
  } else if (ovr > 50) {
    return(sprintf("%s def. %s (%s%%)", home_team, away_team, ovr))
  } else if (ovr < 50) {
    return(sprintf("%s def. %s (%s%%)", away_team, home_team, 100 - ovr))
  }
}
```

</details>

------------------------------------------------------------------------

### Predicting Final Four and National Championship

    ## [1] "Final Four"

    ## [1] "Auburn def. Florida (55.8%)"

    ## [1] "Houston def. Duke (51.2%)"

    ## [1] "----------------------------------------"

    ## [1] "National Championship"

    ## [1] "Auburn def. Houston (52.8%)"
