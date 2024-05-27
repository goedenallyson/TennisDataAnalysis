# Data Cleaning

# Description: 
# This script processes two original datasets, 'match_data' and 'tour_data'. 
# The processing includes cleaning and preparing both datasets for integration. 
# The final output is a unified dataframe named 'tennis_data'
# Below sections use this 'tennis_data' output to restructure the data into various formats such as
# 'player_data' and 'region_data' 


# Set working directory and source libraries
source("Libraries.R")
source("Functions.R")

# Load CSV and Excel files
match_data <- read.csv("atp_matches_2022.csv")
tour_data <- read_excel("ATP_tour_2022.xlsx")

# Clean match_data: convert to snake case, remove unnecessary columns, and rename as needed
match_data <- match_data %>%  
  clean_names() %>% 
  dplyr::select(-c(tourney_id, tourney_date, 
                   loser_rank_points, winner_rank_points, loser_rank, 
                   winner_rank, tourney_level, match_num,
                   winner_id, loser_id)) %>%
  rename(tournament = tourney_name)

# Clean tour_data: convert to snake case and remove unnecessary columns
tour_data <- tour_data %>% 
  clean_names() %>% 
  dplyr::select(-c(b365w, b365l, psw, psl, 
                   max_w, max_l, avg_l, avg_w, atp, series, w_rank, l_rank,
                   w_pts, l_pts, location))

# Define mappings for each draw size
mapping_128 <- c("R128" = "1st Round", "R64" = "2nd Round", "R32" = "3rd Round", "R16" = "4th Round", "QF" = "QF", "SF" = "SF", "F" = "F")
mapping_64 <- c("R64" = "1st Round", "R32" = "2nd Round", "R16" = "3rd Round", "QF" = "QF", "SF" = "SF", "F" = "F")
mapping_32 <- c("R32" = "1st Round", "R16" = "2nd Round", "QF" = "QF", "SF" = "SF", "F" = "F")
mapping_rr <- c("RR" = "Round Robin")

# Apply the mapping to match_data based on the draw size
match_data <- match_data %>%
  mutate(round = case_when(
    draw_size == 128 ~ mapping_128[round],
    draw_size == 64 ~ mapping_64[round],
    draw_size == 32 ~ mapping_32[round],
    draw_size %in% c(16, 8, 4, 2) ~ mapping_rr[round],
    TRUE ~ round  # keep the original value if no condition is met
  ))

# Modify tour_data to match the round naming convention in match_data
tour_data <- tour_data %>%
  mutate(round = case_when(
    round == "Quarterfinals" ~ "QF",
    round == "Semifinals" ~ "SF",
    round == "The Final" ~ "F",
    TRUE ~ round  # keep the original value if no condition is met
  ))

# Format winner names

# Apply function to each winner_name
match_data$winner <- sapply(strsplit(as.character(match_data$winner_name), " "), function(x) {
  
  # Format name to "Last F." style
  formatted_name <- paste0(tail(x, 1), " ", paste(substr(head(x, -1), 1, 1), collapse = "."), ".")
  
  # Define special cases where the formatting is incorrect
  special_cases <- c("Agut R.B." = "Bautista Agut R.",
                     "Aliassime F.A." = "Auger-Aliassime F.",
                     "Almeida M.P.D." = "Pucinelli de Almeida M.",
                     "Baena R.C." = "Carballes Baena R.",
                     "Busta P.C." = "Carreno Busta P.",
                     "Carabelli C.U." = "Ugo Carabelli C.",
                     "Etcheverry T.M." = "Etcheverry T.",
                     "Fokina A.D." = "Davidovich Fokina A.",
                     "Minaur A.D." = "De Minaur A.",
                     "Oconnell C." = "O Connell C.",
                     "Ramos A." = "Ramos-Vinolas A.",
                     "Rijthoven T.V." = "Van Rijthoven T.",
                     "Tsonga J." = "Tsonga J.W.",
                     "Wild T.S." = "Seyboth Wild T.",
                     "Zandschulp B.V.D." = "Van De Zandschulp B.",
                     "Tseng C. H." = "Tseng C.H.",
                     "Miralles B.Z." = "Zapata Miralles B.",
                     "Zhang Z." = "Zhang Zh.",
                     "Varillas J. P." = "Varillas J.P.",
                     "Alvarez Varona N." = "Alvarez N.",
                     "Bailly G." = "Bailly G.A.",
                     "Del Potro J.M." = "Potro J.M.d.",
                     "Gimeno Valero C." = "Valero C.G.",
                     "Hong S." = "Hong S.C.",
                     "Kuznetsov An." = "Kuznetsov A.",
                     "Madaras D." = "Madaras D.N.",
                     "Rehberg M." = "Rehberg M.H.",
                     "Van Assche L." = "Assche L.V.")
  
  # Check if the formatted name is a special case
  if (formatted_name %in% names(special_cases)) {
    return(special_cases[[formatted_name]])
  } else {
    return(formatted_name)
  }
})

# Format loser names

# Apply function to each loser_name
match_data$loser <- sapply(strsplit(as.character(match_data$loser_name), " "), function(x) {
  
  # Format name to "Last F." style
  formatted_name <- paste0(tail(x, 1), " ", paste(substr(head(x, -1), 1, 1), collapse = "."), ".")
  
  # Define special cases where the formatting is incorrect
  special_cases <- c("Agut R.B." = "Bautista Agut R.",
                     "Aliassime F.A." = "Auger-Aliassime F.",
                     "Almeida M.P.D." = "Pucinelli de Almeida M.",
                     "Baena R.C." = "Carballes Baena R.",
                     "Busta P.C." = "Carreno Busta P.",
                     "Carabelli C.U." = "Ugo Carabelli C.",
                     "Etcheverry T.M." = "Etcheverry T.",
                     "Fokina A.D." = "Davidovich Fokina A.",
                     "Minaur A.D." = "De Minaur A.",
                     "Oconnell C." = "O Connell C.",
                     "Ramos A." = "Ramos-Vinolas A.",
                     "Rijthoven T.V." = "Van Rijthoven T.",
                     "Tsonga J." = "Tsonga J.W.",
                     "Wild T.S." = "Seyboth Wild T.",
                     "Zandschulp B.V.D." = "Van De Zandschulp B.",
                     "Tseng C. H." = "Tseng C.H.",
                     "Miralles B.Z." = "Zapata Miralles B.",
                     "Zhang Z." = "Zhang Zh.",
                     "Varillas J. P." = "Varillas J.P.",
                     "Alvarez N." = "Alvarez Varona N.",
                     "Bailly G.A." = "Bailly G.",
                     "Potro J.M.d." = "Del Potro J.M.",
                     "Hong S.C." = "Hong S.",
                     "Rehberg M.H" = "Rehberg M.",
                     "Assche L.V." = "Van Assche L.",
                     "Rehberg M.H." = "Rehberg M.",
                     "Madaras D.N." = "Madaras D.",
                     "Kuznetsov A." = "Kuznetsov An.",
                     "Valero C.G." = "Gimeno Valero C.")
  
  # Check if the formatted name is a special case
  if (formatted_name %in% names(special_cases)) {
    return(special_cases[[formatted_name]])
  } else {
    return(formatted_name)
  }
})

# Format Tournament Names

# Define the mapping for tournament names
tournament_mapping <- c("US Open" = "Us Open",
                        "Delray Beach Open" = "Delray Beach",
                        "Barcelona Open" = "Barcelona",
                        "Los Cabos Open" = "Los Cabos",
                        "San Diego Open" = "San Diego",
                        "Stockholm Open" = "Stockholm",
                        "Tel Aviv Open" = "Tel Aviv",
                        "Atlanta Open" = "Atlanta",
                        "Cordoba Open" = "Cordoba",
                        "Dallas Open" = "Dallas",
                        "Geneva Open" = "Geneva",
                        "Vienna Open" = "Vienna",
                        "Gijon Open" = "Gijon",
                        "Halle Open" = "Halle",
                        "Sofia Open" = "Sofia",
                        "Queen’s Club Championships" = "Queen’s Club",
                        "Melbourne Summer Set" = "Melbourne",
                        "Lyon Open Closest" = "Lyon",
                        "Eastbourne International" = "Eastbourne",
                        "BNP Paribas Masters" = "Paris Masters",
                        "Mallorca Championships" = "Mallorca",
                        "Winston-Salem Open at Wake Forest University" = "Winston-Salem",
                        "Sydney Tennis Classic" = "Sydney",
                        "Adelaide International 1" = "Adelaide 1",
                        "Adelaide International 2" = "Adelaide 2",
                        "Miami Open" = "Miami Masters",
                        "Dubai Tennis Championships" = "Dubai",
                        "Hall of Fame Championships" = "Halle",
                        "Canadian Open" = "Canada Masters",
                        "Mutua Madrid Open" = "Madrid Masters")

# Apply the mapping directly to the 'tournament' column in 'tour_data' dataframe
tour_data$tournament <- ifelse(tour_data$tournament %in% names(tournament_mapping), 
                               tournament_mapping[tour_data$tournament], 
                               tour_data$tournament)

# Doing the mapping in this way allows for additional observations
# takes us from 1025 to 1132

# Merge tour_data and match_data dataframes
tennis_data <- merge(tour_data, match_data, 
                     by = c("tournament", "winner", "loser", "surface", "best_of", "round"))

# Clean tennis_data and prep for analysis 
tennis_data <- tennis_data %>%
  select(-c(winner_seed, winner_entry, winner_name, loser_seed, loser_entry, loser_name))


# Add a column that determines if a match was played between Righties, Lefties or between handed players
tennis_data <- tennis_data %>%
  mutate(match_handedness = case_when(
    winner_hand == "R" & loser_hand == "R" ~ "RR",
    winner_hand == "L" & loser_hand == "L" ~ "LL",
    (winner_hand == "R" & loser_hand == "L") | (winner_hand == "L" & loser_hand == "R") ~ "RL",
    winner_hand == "U" | loser_hand == "U" ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Add a column that adds the winner's region of origin 
tennis_data <- tennis_data %>%
  mutate(w_region = case_when(
    winner_ioc %in% c("USA") ~ "USA",
    winner_ioc %in% c("FRA", "ESP", "GER", "GBR", "ITA", "SUI", "BEL", "CRO", "AUT", "SRB", "CZE", "RUS", "NED", "SWE", "BIH", "BUL", "DEN", "FIN", "GEO", "GRE", "HUN", "LAT", "LTU", "LUX", "MDA", "MKD", "MON", "MNE", "NOR", "POL", "POR", "ROU", "SVK", "SLO", "UKR") ~ "Europe",
    winner_ioc %in% c("ARG", "BOL", "BRA", "CHI", "COL", "ECU", "PER") ~ "South America",
    winner_ioc %in% c("CAN", "MEX") ~ "North America",
    winner_ioc %in% c("RSA", "TUN") ~ "Africa",
    winner_ioc %in% c("AUS") ~ "Australia",
    winner_ioc %in% c("CHN", "IND", "JPN", "KAZ", "KOR", "TPE") ~ "Asia",
    TRUE ~ "Other"
  ))

# Add a column that adds the loser's region of origin 
tennis_data <- tennis_data %>%
  mutate(l_region = case_when(
    loser_ioc %in% c("USA") ~ "USA",
    loser_ioc %in% c("FRA", "ESP", "GER", "GBR", "ITA", "SUI", "BEL", "CRO", "AUT", "SRB", "CZE", "RUS", "NED", "SWE", "BIH", "BUL", "DEN", "FIN", "GEO", "GRE", "HUN", "LAT", "LTU", "LUX", "MDA", "MKD", "MON", "MNE", "NOR", "POL", "POR", "ROU", "SVK", "SLO", "UKR") ~ "Europe",
    loser_ioc %in% c("ARG", "BOL", "BRA", "CHI", "COL", "ECU", "PER") ~ "South America",
    loser_ioc %in% c("CAN", "MEX") ~ "North America",
    loser_ioc %in% c("RSA", "TUN") ~ "Africa",
    loser_ioc %in% c("AUS") ~ "Australia",
    loser_ioc %in% c("CHN", "IND", "JPN", "KAZ", "KOR", "TPE") ~ "Asia",
    TRUE ~ "Other"
  ))
# _____________________________________________________________________________________

# Create a Player Data Dataframe 
# Description: 
# This section takes the 'tennis_data' dataframe and reformats it into observations 
# organized by player.

# Create a dataframe for player variables
player_data <- data.frame(
  name = c(tennis_data$winner, tennis_data$loser),
  handedness = c(tennis_data$winner_hand, tennis_data$loser_hand),
  height = c(tennis_data$winner_ht, tennis_data$loser_ht),
  age = c(tennis_data$winner_age, tennis_data$loser_age),
  nationality = c(tennis_data$winner_ioc, tennis_data$loser_ioc),
  minutes = tennis_data$minutes,
  best_of = tennis_data$best_of
) 

# Remove duplicate player names using unique()
player_data <- player_data[!duplicated(player_data$name), ]


# Count the number of times each player's name appears in the 'winner' and 'loser' column
matches_won <- tennis_data %>%
  count(winner, name = "matches_won")

matches_lost <- tennis_data %>%
  count(loser, name = "matches_lost")

# Merge the matches_won and matches_lost data with player_data
player_data <- player_data %>%
  left_join(matches_won, by = c("name" = "winner")) %>%
  left_join(matches_lost, by = c("name" = "loser")) %>%
  replace_na(list(matches_won = 0, matches_lost = 0)) 

# Calculate total matches played
player_data <- player_data %>%
  mutate(total_matches = matches_won + matches_lost)

# Calculate the percentage of matches won and lost
player_data <- player_data %>%
  mutate(percentage_won = round(matches_won / total_matches * 100, 2),
         percentage_lost = round(matches_lost / total_matches * 100, 2))

# Create a new column for region based on a players nationality 
player_data <- player_data %>%
  mutate(region = case_when(
    nationality %in% c("USA") ~ "USA",
    nationality %in% c("FRA", "ESP", "GER", "GBR", "ITA", "SUI", "BEL", "CRO", "AUT", "SRB", "CZE", "RUS", "NED", "SWE", "BIH", "BUL", "DEN", "FIN", "GEO", "GRE", "HUN", "LAT", "LTU", "LUX", "MDA", "MKD", "MON", "MNE", "NOR", "POL", "POR", "ROU", "SVK", "SLO", "UKR") ~ "Europe",
    nationality %in% c("ARG", "BOL", "BRA", "CHI", "COL", "ECU", "PER") ~ "South America",
    nationality %in% c("CAN", "MEX") ~ "North America",
    nationality %in% c("RSA", "TUN") ~ "Africa",
    nationality %in% c("AUS") ~ "Australia",
    nationality %in% c("CHN", "IND", "JPN", "KAZ", "KOR", "TPE") ~ "Asia",
    TRUE ~ "Other"
  ))
# ___________________________________________________________________

# Region Data Dataframe 
# This section takes the 'tennis_data' dataframe, and reformats it into observations by player, 
# that allow for repetition, unlike 'player_data' dataframe.

# Create a regional dataframe 
region_data <- data.frame(
  name = c(tennis_data$winner, tennis_data$loser),
  nationality = c(tennis_data$winner_ioc, tennis_data$loser_ioc),
  surface = tennis_data$surface)

# Count the number of times a player has played on each surface:
region_data <- region_data %>%
  group_by(name) %>%
  mutate(
    hard_played = sum(surface == "Hard"),
    clay_played = sum(surface == "Clay"),
    grass_played = sum(surface == "Grass")
  )

# Count the number of times a player has won on each surface: 
matches_surface <- tennis_data %>%
  group_by(winner, surface) %>%
  summarise(matches_won = n(), .groups = "drop") %>%
  spread(key = surface, value = matches_won, fill = 0) %>%
  rename(name = winner)

region_data <- region_data %>%
  left_join(matches_surface, by = "name") %>%
  rename(
    clay_won = Clay,
    hard_won = Hard,
    grass_won = Grass
  )

# Count the total number of times a player played a match:
region_data <- region_data %>% 
  mutate(total_played = hard_played + clay_played + grass_played)

# Count the total number of times a player won a match: 
region_data <- region_data %>% 
  mutate(total_won = hard_won + clay_won + grass_won)

# Find the win rate percentage for a player: 
region_data <- region_data %>% 
  mutate(total_won_per = total_won / total_played * 100)



# Find the win rate percentage for a given surface:
# Replace NA values with 0
region_data <- region_data %>%
  mutate(
    win_rate_per_hard = hard_won / hard_played * 100,
    win_rate_per_clay = clay_won / clay_played * 100,
    win_rate_per_grass = grass_won / grass_played * 100
  ) %>%
  replace_na(list(win_rate_per_hard = 0, win_rate_per_clay = 0, win_rate_per_grass = 0))

# Create a new column for region based on a players nationality 
region_data <- region_data %>%
  mutate(region = case_when(
    nationality %in% c("USA") ~ "USA",
    nationality %in% c("FRA", "ESP", "GER", "GBR", "ITA", "SUI", "BEL", "CRO", "AUT", "SRB", "CZE", "RUS", "NED", "SWE", "BIH", "BUL", "DEN", "FIN", "GEO", "GRE", "HUN", "LAT", "LTU", "LUX", "MDA", "MKD", "MON", "MNE", "NOR", "POL", "POR", "ROU", "SVK", "SLO", "UKR") ~ "Europe",
    nationality %in% c("ARG", "BOL", "BRA", "CHI", "COL", "ECU", "PER") ~ "South America",
    nationality %in% c("CAN", "MEX") ~ "North America",
    nationality %in% c("RSA", "TUN") ~ "Africa",
    nationality %in% c("AUS") ~ "Australia",
    nationality %in% c("CHN", "IND", "JPN", "KAZ", "KOR", "TPE") ~ "Asia",
    TRUE ~ "Other"
  ))

# ____________________________________________________

# Final Step: Remove unnecessary items from the environment for clean reading into other files 

rm(mapping_128, mapping_64, mapping_32, mapping_rr, tournament_mapping)
rm(matches_lost, matches_won)
rm(matches_surface)




