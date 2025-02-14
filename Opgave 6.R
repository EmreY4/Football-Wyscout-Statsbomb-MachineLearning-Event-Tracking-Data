#### -------------------------------------------------------- Tracking, Shots & Goals, FreezeFrames ------------------------------------------------------- ####  
# Emne: Analysering af spilleres skudstatistik og visualisering af dækning af boldens position  
# Dette script demonstrerer, hvordan man:  
#   1. Identificerer de top 10 spillere med flest mål, skud og skud på mål i kvindernes verdensmesterskab  
#   2. Bruger `dplyr` til at filtrere og gruppere data efter spiller og hold  
#   3. Visualiserer statistikkerne ved hjælp af `ggplot2`, herunder top scorer, skud i alt og skud på mål  
#   4. Henter og behandler trackingdata fra en MongoDB-database for at analysere spillernes bevægelser og interaktioner  
#   5. Opretter en visuel repræsentation af dækning af boldens position ved hjælp af trekantformede skygger  
#   6. Udfører datamanipulation og omdøber kolonner for at forenkle sammenføjningen af spillerdata  
#   7. Justerer spilleres koordinater til en ensartet skala for korrekt visualisering på fodboldbanen  

# Libraries  
library(dplyr)        # Data manipulation  
library(tidyverse)    # Datatransformation og visualisering  
library(mongolite)    # MongoDB database connection  
library(ggplot2)      # Visualisering af data  
library(ggsoccer)     # Visualisering af fodbolddata  
library(jsonlite)     # JSON håndtering  
library(unikn)        # Temaer og farveskemaer til ggplot2

# ----------------------------------------------------------------- Opgave 6.1 Skud & Mål ------------------------------------------------------------------- #
# Spiller - Alessia Russo (Shots) - Jennifer Hermoso Fuentes (passes) 
# Top 10 - Mål
top_scorer_women <- worldcup_women %>%
  filter(shot.outcome.name == "Goal") %>%
  group_by(player.name, team_name = team.name) %>%
  summarize(total_goals = n(), .groups = 'drop') %>%
  arrange(desc(total_goals)) %>%
  slice(1:10)

# Top 10 - Skud i alt
total_shots_women <- worldcup_women %>%
  filter(type.name == "Shot") %>%
  group_by(player.name, team_name = team.name) %>%
  summarize(total_shots = n(), .groups = 'drop') %>%
  arrange(desc(total_shots)) %>%
  slice(1:10)

# Top 10 - Skud på mål
total_shotsOnTarget_women <- worldcup_women %>%
  filter(type.name == "Shot" & shot.outcome.name %in% c("Saved", "Goal", "Saved to Post")) %>%
  group_by(player.name, team_name = team.name) %>%
  summarize(total_shots_on_target = n(), .groups = 'drop') %>%
  arrange(desc(total_shots_on_target)) %>%
  slice(1:10)

# Plot data
ggplot() +
  # Top 10 - Skud på mål
  geom_bar(data = total_shotsOnTarget_women, aes(x = player.name, y = total_shots_on_target, fill = "Skud på mål"), stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7, color = "black") +
  geom_text(data = total_shotsOnTarget_women, aes(x = player.name, y = total_shots_on_target, label = total_shots_on_target), vjust = -0.5, size = 3, color = "blue") +  # Tælling for skud på mål
  # Top 10 - Skud i alt
  geom_bar(data = total_shots_women, aes(x = player.name, y = total_shots, fill = "Skud i alt"), stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_text(data = total_shots_women, aes(x = player.name, y = total_shots, label = total_shots), vjust = -0.5, size = 3, color = "green") +  # Tælling for skud i alt
  # Top 10 - Mål
  geom_bar(data = top_scorer_women, aes(x = player.name, y = total_goals, fill = "Mål"), stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_text(data = top_scorer_women, aes(x = player.name, y = total_goals, label = total_goals), vjust = -0.5, size = 3, color = "red") +  # Tælling for mål
  labs(title = "Top 10 - Topscorer, flest skud & flest skud på mål", x = "Spillernavn", y = "", fill = "Statistik") +
  theme_unikn() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_blank(),  # Fjern lodrette baggrundslinjer
        panel.grid.minor.y = element_blank(),  # Fjern lodrette baggrundslinjer
        panel.grid.major.x = element_blank(),  # Fjern lodrette baggrundslinjer
        panel.grid.minor.x = element_blank(),  # Fjern lodrette baggrundslinjer
        plot.title = element_text(color = "black")) +  # Sæt titelfarven til sort
  coord_cartesian(ylim = c(0, 30))  # Indstil y-aksen grænser


# ----------------------------------------------------------------- Opgave 6.3 Trackingdata ----------------------------------------------------------------- #
# VBOB data
# Connecter til MongoDB 
connection_vbob <- mongo(
  collection = "vbob.data",
  db = "wys",
  url = "mongodb://localhost"
)
# Hent data fra MongoDB til en data frame i R
vbob_data <- connection_vbob$find()
# Konverterer data til en flad JSON-struktur
vbob.data <- fromJSON(toJSON(vbob_data), flatten = T)
# Gem vbob_data som en .rds fil
saveRDS(vbob.data, file = "vbob.data.rds")

#
# VBOB CSV
vbob_csv <- read_csv("bilag.vbob.csv")

#
# VBOB meta
vbob_meta <- fromJSON("bilag.vbob-meta.json")
# Konverter listen til en dataframe
vbob_meta <- as.data.frame(vbob_meta)

# --------------------------------------------------------------------- Datamanipulation ------------------------------------------------------------------- #
# Udtrækker test
ff <- vbob.data[36562, ]

# Spillere
# Laver ny dataframe til FF - Ny df med udvalgte spiller
ff_shot <- ff[1,] # skal ændres ved loop
# Hjemmehold
ff_hjemme <- ff$homePlayers[[1]]
ff_hjemme <- ff_hjemme %>% rowwise() %>% mutate(x=unlist(xyz[1]),
                                            y=unlist(xyz[2]),
                                            team = "Hjemme",
                                            xscale = x+52.5)
# Ude
ff_ude <- ff$awayPlayers[[1]]
ff_ude <- ff_ude %>% rowwise() %>% mutate(x=unlist(xyz[1]),
                                            y=unlist(xyz[2]),
                                            team = "Ude",
                                            xscale = x+52.5)
# Tester
ff_test <- rbind(ff_hjemme, ff_ude)

# Navne
# Hjemme
home_players <- vbob_meta %>%
  select(homePlayers.name, homePlayers.optaId)
# Ude
away_players <- vbob_meta %>%
  select(awayPlayers.name, awayPlayers.optaId)
# Omdøb kolonner for at lette sammenføjningen
home_players <- rename(home_players, optaId = homePlayers.optaId, name = homePlayers.name)
away_players <- rename(away_players, optaId = awayPlayers.optaId, name = awayPlayers.name)
# Sammenføj navne på ff_test baseret på optaId
ff_test <- ff_test %>%
  left_join(home_players, by = "optaId") %>%
  left_join(away_players, by = "optaId", suffix = c(".home", ".away"))
# Håndter potentielle dobbeltnavne fra både hjemme og udehold
ff_test <- ff_test %>%
  mutate(playerName = coalesce(name.home, name.away)) %>%
  select(-name.home, -name.away)  # Fjerner midlertidige kolonner

# Bold
# Extracting ball data from the 'ball.xyz' column
ball_xyz <- ff$ball.xyz[[1]]
# Defining `ff_shot` and using `ball.xyz` data
ball <- tibble(
  x = unlist(ball_xyz[1]),
  y = unlist(ball_xyz[2]),
  z = unlist(ball_xyz[3]),
  xscale = x + 52.5
)
# Tilføj boldens position som nye kolonner til ff_test
ff_test$ball_x <- ball$x[1]  # Antag at der kun er én observation i bolden
ff_test$ball_y <- ball$y[1]
ff_test$ball_xscale <- ball$xscale[1]

# Justér koordinater fra -52.5-52.5 til 0-100
ff_test <- ff_test %>%
  mutate(x = (x + 52.5) * (100 / 100),
         y = (y + 34) * (100 / 100),
         ball_x = (ball_x + 52.5) * (100 / 100),
         ball_y = (ball_y + 34) * (100 / 100))


# ---------------------------------------------------------------------- Plot opg. 3 & 4 -------------------------------------------------------------------- #
# Funktion til at oprette trekantformede dækningsskygger
create_covering_shadows_polygon <- function(ball_location_x, ball_location_y, player_locations, buffer_distance_player1, buffer_distance_player2, triangle_width_factors = c(1, 1), x_shift_factors = c(0, 0), y_shift_factors = c(0, 0)) {
  covering_shadows_polygon <- data.frame()
  
  for(i in 1:nrow(player_locations)) {
    player <- player_locations[i,]
    distance_to_player <- sqrt((player$x - ball_location_x)^2 + (player$y - ball_location_y)^2)
    
    if (distance_to_player > 0) {  # Sørg for, at spilleren ikke er bolden selv
      # Beregn vektoren fra bolden til spilleren
      direction_vector <- c(player$x - ball_location_x, player$y - ball_location_y)
      
      # Normaliser vektoren (gør dens længde til 1)
      normalized_direction <- direction_vector / sqrt(sum(direction_vector^2))
      
      # Bestem bufferafstanden for den aktuelle spiller
      buffer_distance <- ifelse(player$team == "Ude", buffer_distance_player1, buffer_distance_player2)
      
      # Beregn endepunkterne for trekanten
      triangle_end_x <- ball_location_x + normalized_direction[1] * buffer_distance
      triangle_end_y <- ball_location_y + normalized_direction[2] * buffer_distance
      
      # Beregn vinklen for trekanten
      angle <- atan2(player$y - ball_location_y, player$x - ball_location_x)
      
      # Hent den passende faktor for trekantens bredde for denne spiller
      triangle_width_factor <- triangle_width_factors[i]
      
      # Hent den passende faktor for forskydning i x-retning for denne spiller
      x_shift_factor <- x_shift_factors[i]
      
      # Hent den passende faktor for forskydning i y-retning for denne spiller
      y_shift_factor <- y_shift_factors[i]
      
      # Beregn punkterne for trekanten med den justerede bredde og position
      triangle_x <- c(ball_location_x, 
                      ball_location_x + buffer_distance * cos(angle + triangle_width_factor * pi/18) + x_shift_factor, 
                      ball_location_x + buffer_distance * cos(angle - triangle_width_factor * pi/18) + x_shift_factor, 
                      ball_location_x)
      triangle_y <- c(ball_location_y, 
                      ball_location_y + buffer_distance * sin(angle + triangle_width_factor * pi/18) + y_shift_factor, 
                      ball_location_y + buffer_distance * sin(angle - triangle_width_factor * pi/18) + y_shift_factor, 
                      ball_location_y)
      
      # Tilføj trekantens koordinater til dataframe
      new_row <- data.frame(x = triangle_x, y = triangle_y, team = player$team) # inkluder team kolonnen
      covering_shadows_polygon <- rbind(covering_shadows_polygon, new_row)
    }
  }
  
  return(covering_shadows_polygon)
}
# Definér spillernes numre
player1_number <- 11
# Find spillere med de angivne numre og hold "Ude"
player1 <- ff_test[ff_test$number == player1_number & ff_test$team == "Ude", c("x", "y", "team")]
# Definér spillernes positioner
player_locations <- rbind(player1)
# Definér boldens position
ball_location_x <- ff_test$ball_x[1]
ball_location_y <- ff_test$ball_y[1]
# Definér bufferafstand for hver spiller
buffer_distance_player1 <- 32.4
# Ændringsfaktor for trekantens bredde for hver spiller
triangle_width_factors <- c(0.2)  # Juster efter behov for hver spiller
# Faktorer for forskydning i x-retning for hver spiller
x_shift_factors <- c(1)  # Juster efter behov for hver spiller
# Faktorer for forskydning i y-retning for hver spiller
y_shift_factors <- c(2)  # Juster efter behov for hver spiller
# Generér trekantens koordinater baseret på boldens position og spillernes positioner
covering_shadows_polygon <- create_covering_shadows_polygon(ball_location_x, ball_location_y, player_locations, buffer_distance_player1, buffer_distance_player2, triangle_width_factors, x_shift_factors, y_shift_factors)

# Plot
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "darkgreen") +
  geom_point(data = ff_test, aes(x = x, y = y, color = team), size = 2.3) +
  geom_text(data = ff_test, aes(x = x, y = y, label = playerName), vjust = -1.5, color = "white", size = 2.5) +
  geom_polygon(data = covering_shadows_polygon, aes(x = x, y = y), fill = "white", alpha = 0.5) +  # Tilføj trekant
  geom_point(data = ff_test, aes(x = ball_x, y = ball_y), color = "black", size = 1.3, shape = 21, fill = "white") +
  geom_point(data = player_locations, aes(x = x, y = y, color = team), size = 3) +  # Markér spillere
  scale_color_manual(values = c("Hjemme" = "red", "Ude" = "navy"), labels = c("Vejle", "OB")) +
  scale_fill_manual(values = c("Hjemme" = "red", "Ude" = "navy")) +
  coord_flip(xlim = c(50, 0)) + 
  labs(title = "Covering Shadows - Første mål i kampen mellem Odense BK vs Vejle BK",
       subtitle = "T. Molgaard dækker for afleveringsmuligheden til E. Sabbi",
       color = "Hold") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.033, 0.917))


# ----------------------------------------------------------------------- Plot - Opg. 1 --------------------------------------------------------------------- #
# Udtrækker test
ff <- vbob.data[14232, ]

ff <- vbob.data[9532, ]

# Spillere
# Laver ny dataframe til FF - Ny df med udvalgte spiller
ff_shot <- ff[1,] # skal ændres ved loop
# Hjemmehold
ff_hjemme <- ff$homePlayers[[1]]
ff_hjemme <- ff_hjemme %>% rowwise() %>% mutate(x=unlist(xyz[1]),
                                                y=unlist(xyz[2]),
                                                team = "Hjemme",
                                                xscale = x+52.5)
# Ude
ff_ude <- ff$awayPlayers[[1]]
ff_ude <- ff_ude %>% rowwise() %>% mutate(x=unlist(xyz[1]),
                                          y=unlist(xyz[2]),
                                          team = "Ude",
                                          xscale = x+52.5)
# Tester
ff_test <- rbind(ff_hjemme, ff_ude)

# Navne
# Hjemme
home_players <- vbob_meta %>%
  select(homePlayers.name, homePlayers.optaId)
# Ude
away_players <- vbob_meta %>%
  select(awayPlayers.name, awayPlayers.optaId)
# Omdøb kolonner for at lette sammenføjningen
home_players <- rename(home_players, optaId = homePlayers.optaId, name = homePlayers.name)
away_players <- rename(away_players, optaId = awayPlayers.optaId, name = awayPlayers.name)
# Sammenføj navne på ff_test baseret på optaId
ff_test <- ff_test %>%
  left_join(home_players, by = "optaId") %>%
  left_join(away_players, by = "optaId", suffix = c(".home", ".away"))
# Håndter potentielle dobbeltnavne fra både hjemme og udehold
ff_test <- ff_test %>%
  mutate(playerName = coalesce(name.home, name.away)) %>%
  select(-name.home, -name.away)  # Fjerner midlertidige kolonner

# Bold
# Extracting ball data from the 'ball.xyz' column
ball_xyz <- ff$ball.xyz[[1]]
# Defining `ff_shot` and using `ball.xyz` data
ball <- tibble(
  x = unlist(ball_xyz[1]),
  y = unlist(ball_xyz[2]),
  z = unlist(ball_xyz[3]),
  xscale = x + 52.5
)

# Tilføj boldens position som nye kolonner til ff_test
ff_test$ball_x <- ball$x[1]  # Antag at der kun er én observation i bolden
ff_test$ball_y <- ball$y[1]
ff_test$ball_xscale <- ball$xscale[1]

# Justér koordinater fra -52.5-52.5 til 0-100
ff_test <- ff_test %>%
  mutate(x = (x + 52.5) * (100 / 100),
         y = (y + 34) * (100 / 100),
         ball_x = (ball_x + 52.5) * (100 / 100),
         ball_y = (ball_y + 34) * (100 / 100))

# Funktion til at oprette trekantformede dækningsskygger
create_covering_shadows_polygon <- function(ball_location_x, ball_location_y, player_locations, buffer_distance_player1, buffer_distance_player2, buffer_distance_player3, triangle_width_factors = c(1, 1, 1), x_shift_factors = c(0, 0, 0), y_shift_factors = c(0, 0, 0)) {
  covering_shadows_polygon <- data.frame()
  
  for(i in 1:nrow(player_locations)) {
    player <- player_locations[i,]
    distance_to_player <- sqrt((player$x - ball_location_x)^2 + (player$y - ball_location_y)^2)
    
    if (distance_to_player > 0) {  # Sørg for, at spilleren ikke er bolden selv
      # Beregn vektoren fra bolden til spilleren
      direction_vector <- c(player$x - ball_location_x, player$y - ball_location_y)
      
      # Normaliser vektoren (gør dens længde til 1)
      normalized_direction <- direction_vector / sqrt(sum(direction_vector^2))
      
      # Bestem bufferafstanden for den aktuelle spiller
      buffer_distance <- switch(i, buffer_distance_player1, buffer_distance_player2, buffer_distance_player3)
      
      # Beregn endepunkterne for trekanten
      triangle_end_x <- ball_location_x + normalized_direction[1] * buffer_distance
      triangle_end_y <- ball_location_y + normalized_direction[2] * buffer_distance
      
      # Beregn vinklen for trekanten
      angle <- atan2(player$y - ball_location_y, player$x - ball_location_x)
      
      # Hent den passende faktor for trekantens bredde for denne spiller
      triangle_width_factor <- triangle_width_factors[i]
      
      # Hent den passende faktor for forskydning i x-retning for denne spiller
      x_shift_factor <- x_shift_factors[i]
      
      # Hent den passende faktor for forskydning i y-retning for denne spiller
      y_shift_factor <- y_shift_factors[i]
      
      # Beregn punkterne for trekanten med den justerede bredde og position
      triangle_x <- c(ball_location_x, 
                      ball_location_x + buffer_distance * cos(angle + triangle_width_factor * pi/18) + x_shift_factor, 
                      ball_location_x + buffer_distance * cos(angle - triangle_width_factor * pi/18) + x_shift_factor, 
                      ball_location_x)
      triangle_y <- c(ball_location_y, 
                      ball_location_y + buffer_distance * sin(angle + triangle_width_factor * pi/18) + y_shift_factor, 
                      ball_location_y + buffer_distance * sin(angle - triangle_width_factor * pi/18) + y_shift_factor, 
                      ball_location_y)
      
      # Tilføj trekantens koordinater til dataframe
      new_row <- data.frame(x = triangle_x, y = triangle_y, team = player$team) # inkluder team kolonnen
      covering_shadows_polygon <- rbind(covering_shadows_polygon, new_row)
    }
  }
  
  return(covering_shadows_polygon)
}

# Definér spillernes numre
player1_number <- 7
player2_number <- 8
player3_number <- 11
player4_number <- 14  # Den fjerde spiller
player5_number <- 38  # Den femte spiller

# Find spillere med de angivne numre og hold "Ude"
player1 <- ff_test[ff_test$number == player1_number & ff_test$team == "Ude", c("x", "y", "team")]
player2 <- ff_test[ff_test$number == player2_number & ff_test$team == "Ude", c("x", "y", "team")]
player3 <- ff_test[ff_test$number == player3_number & ff_test$team == "Ude", c("x", "y", "team")]
player4 <- ff_test[ff_test$number == player4_number & ff_test$team == "Hjemme", c("x", "y", "team")]  # Den fjerde spiller
player5 <- ff_test[ff_test$number == player5_number & ff_test$team == "Hjemme", c("x", "y", "team")]  # Den femte spiller

# Definér spillernes positioner
player_locations <- rbind(player1, player2, player3)

# Definér boldens position
ball_location_x <- ff_test$ball_x[1]
ball_location_y <- ff_test$ball_y[1]

# Definér bufferafstand for hver spiller
buffer_distance_player1 <- 30
buffer_distance_player2 <- 48
buffer_distance_player3 <- 25

# Ændringsfaktor for trekantens bredde for hver spiller
triangle_width_factors <- c(0.5, 0.2, 0.22)  # Juster efter behov for hver spiller

# Faktorer for forskydning i x-retning for hver spiller
x_shift_factors <- c(0, 0, 0)  # Juster efter behov for hver spiller

# Faktorer for forskydning i y-retning for hver spiller
y_shift_factors <- c(0, 0, 0)  # Juster efter behov for hver spiller

# Generér trekantens koordinater baseret på boldens position og spillernes positioner
covering_shadows_polygon <- create_covering_shadows_polygon(ball_location_x, ball_location_y, player_locations, buffer_distance_player1, buffer_distance_player2, buffer_distance_player3, triangle_width_factors, x_shift_factors, y_shift_factors)

# Beregn midtpunktet af den stipplede linje for player 4
midpoint_x <- (ball_location_x + player4$x) / 2
midpoint_y <- (ball_location_y + player4$y) / 2

# Beregn midtpunktet af den stipplede linje for player 5
midpoint_x5 <- (ball_location_x + player5$x) / 2
midpoint_y5 <- (ball_location_y + player5$y) / 2

# Plot
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "darkgreen") +
  geom_point(data = ff_test, aes(x = x, y = y, color = team), size = 2.3) +
  geom_text(data = ff_test, aes(x = x, y = y, label = playerName), vjust = -1.5, color = "white", size = 2.5) +
  geom_polygon(data = covering_shadows_polygon, aes(x = x, y = y), fill = "white", alpha = 0.5) +  # Tilføj trekant
  geom_point(data = ff_test, aes(x = ball_x, y = ball_y), color = "black", size = 1.3, shape = 21, fill = "white") +
  geom_point(data = player_locations, aes(x = x, y = y, color = team), size = 3) +  # Markér spillere
  geom_segment(data = player4, aes(x = ball_location_x, y = ball_location_y, xend = x, yend = y), color = "yellow", linetype = "dotted") +  # Tilføj stiplet linje for den fjerde spiller
  geom_segment(data = player5, aes(x = ball_location_x, y = ball_location_y, xend = x, yend = y), color = "white", linetype = "dotted") +  # Tilføj stiplet linje for den femte spiller
  geom_text(aes(x = midpoint_x, y = midpoint_y, label = "Bedste aflevering", angle = -24), vjust = -1, hjust = 0.5, color = "yellow", size = 2.7) +  # Tilføj label over den stipplede linje med vinkel for player 4
  geom_text(aes(x = midpoint_x5, y = midpoint_y5, label = "Faktiske aflevering", angle = 11), vjust = -1, hjust = 0.5, color = "white", size = 2.7) +  # Tilføj label over den stipplede linje med vinkel for player 5
  scale_color_manual(values = c("Hjemme" = "red", "Ude" = "navy"), labels = c("Vejle", "OB")) +
  scale_fill_manual(values = c("Hjemme" = "red", "Ude" = "navy")) +
  coord_flip(xlim = c(0, 120)) + 
  labs(title = "Covering Shadows - Progression mod mål i kampen mellem Odense BK vs Vejle BK",
       subtitle = "Albentosa's afleveringsmulighed til tre medspillere bliver dækket af tre modstandere - Belyser den bedste aflevering ift. progressionplay, og faktiske aflevering",
       color = "Hold") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.033, 0.917))

# ------------------------------------------------------------------- LAV GIF AF OVENSTÅENDE ---------------------------------------------------------------- #
# GIF
# Brug pakke animate til at lave GIF


