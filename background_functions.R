# This function takes two character vectors describing the home and away teams in specific games,
# and then gathers them into a data frame

extract_game_table <- function(home, away) {
  if (length(home) != length(away)) {
    stop("Home and away must be of equal length")
  }
  
  temp_home <- strsplit(home, split = "\n") %>%
    map(grep, pattern = "[^ ]", value = TRUE) %>%
    map(sub, pattern = "[ ]{2, }", replacement = "") %>%
    map(sub, pattern = " \\*", replacement = "")
  
  home_team <- map_chr(temp_home, function(x) x[[2]])
  home_score <- map_chr(temp_home, function(x) x[[3]]) %>%
    map_dbl(.f = function(x) {if (x == "None") return(NA_real_) else return(as.numeric(x))})
  
  temp_away <- strsplit(away, split = "\n") %>%
    map(grep, pattern = "[^ ]", value = TRUE) %>%  
    map(sub, pattern = "[ ]{2, }", replacement = "") %>%
    map(sub, pattern = " \\*", replacement = "")
  
  away_team <- map_chr(temp_away, function(x) x[[2]])
  away_score <- map_chr(temp_away, function(x) x[[1]]) %>%
    map_dbl(.f = function(x) {if (x == "None") return(NA_real_) else return(as.numeric(x))})
  
  played <- !is.na(home_score)
  test <- !is.na(away_score)
  
  if (!all(played == test)) stop("mismatch in home and away played")
  
  #
  df <- tibble(home_team = home_team,
               home_score = home_score,
               away_team = away_team,
               away_score = away_score,
               played = played)
  
  # remove duplicates
  df <- df %>% distinct(home_team, away_team, .keep_all = TRUE)
  
  # add winner
  
  df <- df %>% mutate(home_win = home_score > away_score)
  
  df
}



lookup_team_id <- function(team_name_lookup, mapping_table) {
  # finds team ID from team name and mapping table
  tmp_table <- filter(mapping_table, team_name == team_name_lookup)
  if (nrow(tmp_table) != 1) {
    stop(paste("lookup-rows should be equal to one but found", nrow(tmp_table), "instead"))
  }
  out <- tmp_table$team_id
  out
}

