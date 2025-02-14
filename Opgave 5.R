#### ----------------------------- Tracking data, event data, FreezeFrames, Covering Shadows  - Analyse af VM (Kvinder og Mænd) --------------------------- ####  
# Emne: Analyse af fodbolddata fra VM 2023 for både kvinder og mænd  
# Dette script demonstrerer, hvordan man:  
#   1. Indhenter og behandler data fra StatsBomb API for både kvinde- og herrelandshold  
#   2. Analyserer afleveringer, skud, fouls, tackles og kort for at sammenligne kvinders og mænds præstationer  
#   3. Visualiserer data for at identificere tendenser og forskelle mellem kønnene  
#   4. Udfører en dybdegående analyse af skudscenarier, herunder freeze frames og dækningsskygger  
#   5. Beregner og sammenligner nøglemål som præcisionsafleveringer, skud på mål, fouls og kort  

# Libraries  
library(dplyr)        # Data manipulation  
library(tidyverse)    # Datatransformation og visualisering  
library(devtools)     # Hjælpeværktøjer til pakkeudvikling  
library(SDMTools)     # Værktøjer til spatial data analyse  
library(StatsBombR)   # Indhentning af StatsBomb data  
library(ggplot2)      # Visualisering af data  
library(ggsoccer)     # Visualisering af fodbolddata  
library(jsonlite)     # Læsning og skrivning af JSON-data  
library(pitches)      # Værktøjer til at arbejde med fodboldbaner  
library(magrittr)     # Pipe-operatører til mere læsbar kode  

# Opgave 5.1
# ---------------------------------------------------------------------- Indhenter Data ------------------------------------------------------------------ #
# Indlæser data gennem StatsBomb API
competitions <- FreeCompetitions() # Henter alle turneringer
matches <- FreeMatches(competitions) # Henter alle kampe
#StatsBombData <- free_allevents(MatchesDF = matches, Parallel = T) # Henter alle events

# Kvinder
# Filtrer VM 2023 relateret til kvinder
worldcup_competition_women <- competitions %>% 
  filter(competition_name == "Women's World Cup" & season_name == "2023")
# Henter alle VM kampe for kvinder
worldcup_games_women <- FreeMatches(worldcup_competition_women)
# Afleveringer & Skud - Henter events som vi skal bruge for kvindernes VM kampe
worldcup_women <- free_allevents(MatchesDF = worldcup_games_women)


# Mænd
# Filtrer VM 2023 relateret til mænd
worldcup_competition_men <- competitions %>% 
  filter(competition_name == "FIFA World Cup" & season_name == "2022")
# Henter alle VM kampe for mænd
worldcup_games_men <- FreeMatches(worldcup_competition_men)
# Afleveringer & Skud - Henter events som vi skal bruge for mændenes VM kampe
worldcup_men <- free_allevents(MatchesDF = worldcup_games_men)


# ------------------------------------------------------------------- Afleveringer & Skud ---------------------------------------------------------------- #
# Afleveringsanalyse

# Kvinder
# Subsetter således at vi kun beholder afleveringer og deres udfald
afleveringer_women <- subset(worldcup_women, type.name == "Pass" & (is.na(pass.outcome.name) | pass.outcome.name != "Unknown"), select = c(type.name, pass.outcome.name))
# Summen af succesfulde afleveringer og fejlafleveringer
women_accurate_passes <- sum(is.na(afleveringer_women$pass.outcome.name))
women_inaccurate_passes <- sum(!is.na(afleveringer_women$pass.outcome.name))
# Udskriver resultaterne
print(paste("Antal Præcise Afleveringer for Kvinder:", women_accurate_passes))
print(paste("Antal Fejlafleveringer for Kvinder:", women_inaccurate_passes))


# Mænd
# Afleveringsanalyse
# Subsetter således at vi kun beholder afleveringer og deres udfald
afleveringer_men <- subset(worldcup_men, type.name == "Pass" & (is.na(pass.outcome.name) | pass.outcome.name != "Unknown"), select = c(type.name, pass.outcome.name))
# Summen af succesfulde afleveringer og fejlafleveringer
men_accurate_passes <- sum(is.na(afleveringer_men$pass.outcome.name))
men_inaccurate_passes <- sum(!is.na(afleveringer_men$pass.outcome.name))
# Udskriver resultaterne
print(paste("Antal Præcise Afleveringer for Mænd:", men_accurate_passes))
print(paste("Antal Fejlafleveringer for Mænd:", men_inaccurate_passes))


# Skudanalyse

# Kvinder
# Subsetter således at vi kun beholder skud og deres udfald
skud_women <- subset(worldcup_women, type.name == "Shot", select = c(type.name, shot.outcome.name))
# Opsummering af skuddenes udfald
summary(skud_women$shot.outcome.name)
# Tabel med antallet af udfald af hver kategori i kolonnen 'shot.outcome.name'
shot_outcome_table_women <- table(skud_women$shot.outcome.name)
# Udskriver resultaterne
print(shot_outcome_table_women)
# Plusser følgende med hinanden for at se antal missete skud: Off T, Post, Saved Off Target, Wayward, Blocked. Plusser resten for at se antal skud indenfor målrammen
missed_shots_women <- sum(shot_outcome_table_women[c("Off T", "Post", "Saved Off Target", "Wayward", "Blocked")])
ontarget_shots_women <- sum(shot_outcome_table_women[c("Goal", "Saved", "Saved to Post")])
print(paste("Antal missede skud for kvinder:", missed_shots_women))
print(paste("Antal skud på mål for kvinder:", ontarget_shots_women))


# Mænd
# Subsetter således at vi kun beholder skud og deres udfald
skud_men <- subset(worldcup_men, type.name == "Shot", select = c(type.name, shot.outcome.name))
# Opsummering af skuddenes udfald
summary(skud_men$shot.outcome.name)
# Tabel med antallet af udfald af hver kategori i kolonnen 'shot.outcome.name'
shot_outcome_table_men <- table(skud_men$shot.outcome.name)
# Udskriver resultaterne
print(shot_outcome_table_men)
# Plusser følgende med hinanden for at se antal missete skud: Off T, Post, Saved Off Target, Wayward, Blocked. Plusser resten for at se antal skud indenfor målrammen
missed_shots_men <- sum(shot_outcome_table_men[c("Off T", "Post", "Saved Off Target", "Wayward", "Blocked")])
ontarget_shots_men <- sum(shot_outcome_table_men[c("Goal", "Saved", "Saved to Post")])
print(paste("Antal missede skud for mænd:", missed_shots_men))
print(paste("Antal skud på mål for mænd:", ontarget_shots_men))

# --------------------------------------------------------------------- Visualiseringer ------------------------------------------------------------------ #
library(ggplot2)
# Opretter en dataframe for skuddata
skud_data <- data.frame(
  Køn = c(rep("Kvinder", 2), rep("Mænd", 2), "Kvinder", "Mænd"),
  Skudtype = c(rep(c("Missede skud", "Skud på mål"), 2), "Samlet antal skud", "Samlet antal skud"),
  Antal = c(missed_shots_women, ontarget_shots_women, missed_shots_men, ontarget_shots_men, sum(c(missed_shots_women, ontarget_shots_women)), sum(c(missed_shots_men, ontarget_shots_men)))
)

# Plot - Skud
ggplot(skud_data, aes(x = Køn, y = Antal, fill = Skudtype, label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  geom_text(position = position_dodge(width = 0.9), aes(label = paste(Antal)), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Sammenligning af skududfald mellem kvinder og mænd",
       x = "Køn", y = "Antal") +
  scale_fill_manual(values = c("Missede skud" = "#FF5733", "Skud på mål" = "#2E8B57", "Samlet antal skud" = "gray")) + # Definerer farven for den samlede antal skud søjle
  ylim(0, 1800) +  # Angiver y-aksegrænser
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_text(family = "Arial", size = 12, color = "black"),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        legend.position = "bottom")

# Afleveringer
# Opretter dataframe for afleveringsdata
afleveringer_data <- data.frame(
  Køn = c(rep("Kvinder", 2), rep("Mænd", 2), "Kvinder", "Mænd"),
  Afleveringstype = c(rep(c("Fejlafleveringer", "Præcise afleveringer"), 2), "Samlet antal afleveringer", "Samlet antal afleveringer"),
  Antal = c(women_inaccurate_passes, women_accurate_passes, men_inaccurate_passes, men_accurate_passes, women_accurate_passes + women_inaccurate_passes, men_accurate_passes + men_inaccurate_passes)
)

# Plot - Afleveringer
ggplot(afleveringer_data, aes(x = Køn, y = Antal, fill = Afleveringstype, label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  geom_text(position = position_dodge(width = 0.9), aes(label = paste(Antal)), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Sammenligning af afleveringsudfald mellem kvinder og mænd",
       x = "Køn", y = "Antal") +
  scale_fill_manual(values = c("Præcise afleveringer" = "#2E8B57", "Fejlafleveringer" = "#FF5733", "Samlet antal afleveringer" = "gray")) + # Bruger samme farver som i det forrige plot
  ylim(0, 70000) +  # Angiver y-aksegrænser
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_text(family = "Arial", size = 12, color = "black"),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        legend.position = "bottom")


###--- Gennemsnit ---###
# Beregn gennemsnitlige afleveringer pr. kamp
afleveringer_data$Gennemsnitlige_afleveringer <- afleveringer_data$Antal/64

# Plot - Afleveringer
ggplot(afleveringer_data, aes(x = Køn, y = Gennemsnitlige_afleveringer, fill = Afleveringstype, label = Gennemsnitlige_afleveringer)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "white") +
  geom_text(position = position_dodge(width = 0.9), aes(label = paste(round(Gennemsnitlige_afleveringer, 2))), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Sammenligning af gennemsnitlige afleveringer pr. kamp mellem kvinder og mænd",
       x = "Køn", y = "Gennemsnitlige afleveringer pr. kamp") +
  scale_fill_manual(values = c("Præcise afleveringer" = "#2E8B57", "Fejlafleveringer" = "#FF5733", "Samlet antal afleveringer" = "gray")) + # Bruger samme farver som i det forrige plot
  ylim(0, NA) +  # Justerer automatisk y-aksegrænser
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_text(family = "Arial", size = 12, color = "black"),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        legend.position = "bottom")



# Beregn gennemsnitlige skud pr. kamp
skud_data$skud.pr.kamp <- skud_data$Antal/64

# Plot - skud
ggplot(skud_data, aes(x = Køn, y = skud.pr.kamp, fill = Skudtype, label = skud.pr.kamp)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "white") +
  geom_text(position = position_dodge(width = 0.9), aes(label = paste(round(skud.pr.kamp, 2))), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Sammenligning af gennemsnitlige skud pr. kamp mellem kvinder og mænd",
       x = "Køn", y = "Gennemsnitlige skud pr. kamp") +
  scale_fill_manual(values = c("Skud på mål" = "#2E8B57", "Missede skud" = "#FF5733", "Samlet antal afleveringer" = "gray")) + # Bruger samme farver som i det forrige plot
  ylim(0, NA) +  # Justerer automatisk y-aksegrænser
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_text(family = "Arial", size = 12, color = "black"),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"),
        legend.position = "bottom")

# ----------------------------------------------------------------- Foul, Tacklinger, Kort --------------------------------------------------------------- #
library(ggplot2)
# Mænd - Fouls
fouls_men <- subset(worldcup_men, type.name == "Foul Committed", select = c(type.name))
fouls_count_men <- nrow(fouls_men)
# Kvinder - Fouls
fouls_women <- subset(worldcup_women, type.name == "Foul Committed", select = c(type.name))
fouls_count_women <- nrow(fouls_women)
# Mænd - Tackles
tackle_men <- subset(worldcup_men, duel.type.name == "Tackle", select = c(duel.type.name))
tackle_count_men <- nrow(tackle_men)
# Kvinder - Tackles
tackle_women <- subset(worldcup_women, duel.type.name == "Tackle", select = c(duel.type.name))
tackle_count_women <- nrow(tackle_women)
# Data for fouls og tackles
data <- data.frame(
  Køn = c("Kvinder", "Mænd", "Kvinder", "Mænd"),
  Type = c("Fouls", "Fouls", "Tackles", "Tackles"),
  Antal = c(fouls_count_women, fouls_count_men, tackle_count_women, tackle_count_men)
)

# Plot - Fouls og Tackles
ggplot(data, aes(x = Køn, y = Antal, fill = Type, label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Sammenligning af fouls og tackles mellem kvinder og mænd",
       x = "Køn", y = "Antal", fill = "Type") +
  ylim(0, max(data$Antal) * 1.2) +  # Angiver y-aksegrænser
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"))


###--- Sammenføjet ---###
# Mænd - Fouls
fouls_men <- subset(worldcup_men, type.name == "Foul Committed", select = c(type.name))
fouls_count_men <- nrow(fouls_men)
# Kvinder - Fouls
fouls_women <- subset(worldcup_women, type.name == "Foul Committed", select = c(type.name))
fouls_count_women <- nrow(fouls_women)
# Mænd - Tackles
tackle_men <- subset(worldcup_men, duel.type.name == "Tackle", select = c(duel.type.name))
tackle_count_men <- nrow(tackle_men)
# Kvinder - Tackles
tackle_women <- subset(worldcup_women, duel.type.name == "Tackle", select = c(duel.type.name))
tackle_count_women <- nrow(tackle_women)
# Mænd - Kort
kort_men <- subset(worldcup_men, !is.na(foul_committed.card.name), select = c(foul_committed.card.name))
kort_table_men <- table(kort_men$foul_committed.card.name)
# Kvinder - Kort
kort_women <- subset(worldcup_women, !is.na(foul_committed.card.name), select = c(foul_committed.card.name))
kort_table_women <- table(kort_women$foul_committed.card.name)
# Antal gule og røde kort for mænd
yellow_cards_men <- kort_table_men["Yellow Card"]
red_cards_men <- kort_table_men["Red Card"]
# Antal gule og røde kort for kvinder
yellow_cards_women <- kort_table_women["Yellow Card"]
red_cards_women <- kort_table_women["Red Card"]
# Data for fouls, tackles, gule og røde kort
data <- data.frame(
  Køn = c("Kvinder", "Mænd", "Kvinder", "Mænd", "Kvinder", "Mænd", "Kvinder", "Mænd"),
  Type = c("Tackles", "Tackles", "Fouls", "Fouls", "Gule kort", "Gule kort", "Røde kort", "Røde kort"),
  Antal = c(tackle_count_women, tackle_count_men, fouls_count_women, fouls_count_men, yellow_cards_women, yellow_cards_men, red_cards_women, red_cards_men)
)

# Plot - Fouls, Tackles, gule og røde kort
ggplot(data, aes(x = Køn, y = Antal, fill = factor(Type, levels = c("Tackles", "Fouls", "Gule kort", "Røde kort")), label = Antal)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  labs(title = "Sammenligning af fouls, tackles og kort begået mellem kvinder og mænd",
       x = "Køn", y = "Antal", fill = "Type") +
  ylim(0, max(data$Antal) * 1.2) +  # Angiver y-aksegrænser
  scale_fill_manual(values = c("Tackles" = "blue", "Fouls" = "green4", "Gule kort" = "yellow", "Røde kort" = "red")) + # Definerer farver for hver type
  theme_minimal() +
  theme(plot.title = element_text(family = "Arial Rounded MT Bold", size = 20),
        axis.title = element_text(family = "Arial", size = 14, color = "black"),
        axis.text = element_text(family = "Arial", size = 12, color = "black"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Arial", size = 10, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F4F4F4"),
        plot.background = element_rect(fill = "#F4F4F4"))






# Opgave 5.2
# ---------------------------------------------------------------- Kvinder - Datamanipulation ------------------------------------------------------------ #
# Udpakker Freezeframes
worldcupFF_women <- worldcup_women %>% filter(type.name=="Shot")

# Filtrer skud og afleveringer og fjern kolonner med kun NA-værdier
shots_women <- worldcupFF_women %>%
  filter(type.name %in% c("Shot")) %>%
  select(where(~ !all(is.na(.))))
# Split location og shot.end_location til to kolonner
shots_women <- shots_women %>%
  filter(!is.null(location)) %>%
  rowwise() %>%
  mutate(
    location_x = location[[1]],
    location_y = location[[2]],
    shot_end_location_x = if (!is.null(shot.end_location)) shot.end_location[[1]] else NA_real_,
    shot_end_location_y = if (!is.null(shot.end_location)) shot.end_location[[2]] else NA_real_
  )
# Beregn afstanden mellem location og shot.end_location
shots_women <- shots_women %>%
  mutate(afstand = sqrt((location_x - shot_end_location_x)^2 + (location_y - shot_end_location_y)^2))
# Behold samme kolonner som i dataframen "worldcupFF_women"
shots_women <- select(shots_women,
                      shot.end_location, shot.freeze_frame, player.id, player.name, 
                      position.id, position.name, team.id, team.name, location, 
                      location_x, location_y, shot_end_location_x, shot_end_location_y, afstand)

# ----------------------------------------------------------------------- FreezeFrame -------------------------------------------------------------------- #
# Freeze frame for kvinder
shot_frame_women <- shots_women[905,]
ff_women <- shot_frame_women$shot.freeze_frame[[1]] %>% 
  rowwise() %>% mutate(location.x = location[1], location.y = location[2])

# Funktion til at udtrække efternavne
extract_last_name_women <- function(full_name) {
  last_name <- tail(strsplit(full_name, " ")[[1]], 1)
  return(last_name)
}
# Tilføjer efternavne til ff_women
ff_women$last_name <- sapply(ff_women$player.name, extract_last_name_women)

# Opret dataframe til trekant for skudet for kvinder
create_shot_triangle_women <- function(shot_women) {
  g1 <- c(120, 36)
  g2 <- c(120, 44)
  x <- c(shot_women[1], g1[1], g2[1], shot_women[1])
  y <- c(shot_women[2], g1[2], g2[2], shot_women[2])
  res_shot <- data.frame(x, y)
  colnames(res_shot) <- c("sx", "sy")
  return(res_shot)
}
# Opret et trekantobjekt baseret på skudlokationerne for kvinder
triangle_for_shot_women <- create_shot_triangle_women(unlist(shot_frame_women$location))

# Funktion til at oprette trekantdata for en given spiller baseret på spillerens position
create_player_triangle_women <- function(player_location_x, player_location_y) {
  g1 <- c(120, 36)
  g2 <- c(120, 44)
  x <- c(player_location_x, g1[1], g2[1], player_location_x)
  y <- c(player_location_y, g1[2], g2[2], player_location_y)
  data.frame(sx = x, sy = y)
}

# Opret trekantdata for alle medspillere (teammates)
# Tilføj en unik identifikator til hver trekant baseret på spillerens navn eller en anden unik egenskab
teammate_triangles_women <- lapply(1:nrow(ff_women), function(i) {
  if (ff_women$teammate[i]) {
    triangle_women <- create_player_triangle_women(ff_women$location.x[i], ff_women$location.y[i])
    triangle_women$player_name <- ff_women$player.name[i] # Brug spillerens navn som identifikator
    return(triangle_women)
  } else {
    return(NULL)
  }
})
# Fjern NULL værdier fra listen og konverter til en data.frame, derefter fjern duplikater
teammate_triangles_women <- lapply(teammate_triangles_women, function(df) if (!is.null(df)) df[!duplicated(df), ] else NULL)
teammate_triangles_df_women <- do.call(rbind, teammate_triangles_women)
teammate_triangles_df_women$triangle_id <- rep(1:nrow(ff_women[ff_women$teammate, ]), each = 3)

calculate_triangle_area <- function(x1, y1, x2, y2, x3, y3) {
  # Calculate the area of the triangle using the coordinates
  area <- 0.5 * abs(x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2))
  return(area)
}

# Ny funktion til at tælle modstandere inde i trekanten
count_opponents_in_triangle_women <- function(ff_data_women, shooter_location) {
  # Beregn arealet af hele trekanten
  total_area_women <- calculate_triangle_area(120, 44, 120, 36, shooter_location[1], shooter_location[2])
  # Tæl hvor mange modstandere der er inden for trekanten
  opponents_inside_women <- sum(sapply(1:nrow(ff_data_women), function(i) {
    if (!ff_data_women$teammate[i]) {
      opponent_location_women <- c(ff_data_women$location.x[i], ff_data_women$location.y[i])
      area1 <- calculate_triangle_area(opponent_location_women[1], opponent_location_women[2], 120, 36, shooter_location[1], shooter_location[2])
      area2 <- calculate_triangle_area(120, 44, opponent_location_women[1], opponent_location_women[2], shooter_location[1], shooter_location[2])
      area3 <- calculate_triangle_area(120, 44, 120, 36, opponent_location_women[1], opponent_location_women[2])
      trisum <- area1 + area2 + area3
      return(abs(trisum - total_area_women) < 0.1)
    }
    return(FALSE)
  }))
  return(opponents_inside_women)
}

# Tilføjer antallet af modstandere inden for trekanten til hver spiller i ff_women
ff_women$opponents_in_triangle <- sapply(1:nrow(ff_women), function(i) {
  if (ff_women$teammate[i]) {
    count_opponents_in_triangle_women(ff_women, c(ff_women$location.x[i], ff_women$location.y[i]))
  } else {
    0
  }
})

# Beregner antallet af modstandere inden for trekanten for den skydende spiller
shooter_opponents_in_triangle_women <- count_opponents_in_triangle_women(ff_women, unlist(shot_frame_women$location))
# Find den laveste værdi af antallet af modspillere for alle medspillere
lowest_opponents_women <- min(ff_women$opponents_in_triangle[ff_women$teammate])

# -------------------------------------------------------------------------- Plots ----------------------------------------------------------------------- #
# Plot - Dot til medspiller med færrest modspiller i trekant
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "steelblue4") +
  geom_polygon(data = triangle_for_shot_women, aes(x = sx, y = sy), alpha = 0.2, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_polygon(data = teammate_triangles_df_women, aes(x = sx, y = sy, group = triangle_id), alpha = 0.1, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_point(data = ff_women, aes(x = location.x, y = location.y, color = teammate), size = 3) +
  geom_point(data = shot_frame_women, aes(x = location_x, y = location_y), size = 5, color = "green2") +
  theme_pitch() +
  labs(title = "FreezeFrame - Overblik over mest fordelagtige position", subtitle = "Medspillerens/medspillernes navn belyses KUN, hvis de har færre modspillere i egen trekantsareal") + # Titler og undertitler
  coord_flip(xlim = c(85, 130)) + # Læg # Foran for at gøre banen vandret
  geom_text(data = ff_women[(ff_women$teammate & ff_women$opponents_in_triangle < shooter_opponents_in_triangle_women) | ff_women$position.name == "Goalkeeper",], aes(x = location.x, y = location.y, label = last_name), size = 2.5, vjust = 2, angle = 0) + # Show last name if fewer than shooter opponents in triangle or if goalkeeper
  geom_text(data = shot_frame_women, aes(x = location_x, y = location_y, label = player.name), size = 4, vjust = 2, angle = 0) +
  scale_color_manual(values = c("red", "lightblue1"), labels = c("Modspiller", "Medspiller")) +
  guides(color = guide_legend(title = "")) +
  geom_text(data = ff_women[ff_women$teammate,], aes(x = location.x, y = location.y, label = opponents_in_triangle), size = 3, vjust = 0.5, angle = 0, color = "black") +
  geom_text(data = shot_frame_women, aes(x = location_x, y = location_y, label = shooter_opponents_in_triangle_women), size = 4, vjust = 0.5, angle = 0, color = "black") +
  geom_segment(data = ff_women[ff_women$teammate & ff_women$opponents_in_triangle == lowest_opponents_women,], 
               aes(xend = location.x, yend = location.y,
                   x = shot_frame_women$location_x, y = shot_frame_women$location_y), 
               arrow = arrow(type = "open", length = unit(0, "inches")), color = "yellow", linetype = "dotted", alpha = 1) +
  theme(legend.position = c(0.033, 0.98))

# Plot - Dot til færre spillere end skydende spiller
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "steelblue4") +
  geom_polygon(data = triangle_for_shot_women, aes(x = sx, y = sy), alpha = 0.2, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_polygon(data = teammate_triangles_df_women, aes(x = sx, y = sy, group = triangle_id), alpha = 0.1, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_point(data = ff_women, aes(x = location.x, y = location.y, color = teammate), size = 3) +
  geom_point(data = shot_frame_women, aes(x = location_x, y = location_y), size = 5, color = "green2") +
  theme_pitch() +
  labs(title = "FreezeFrame - Overblik over mest fordelagtige position", subtitle = "Medspillerens/medspillernes navn belyses KUN, hvis de har færre modspillere i egen trekantsareal") + # Titler og undertitler
  coord_flip(xlim = c(85, 130)) + # Læg # Foran for at gøre banen vandret
  geom_text(data = ff_women[(ff_women$teammate & ff_women$opponents_in_triangle < shooter_opponents_in_triangle_women) | ff_women$position.name == "Goalkeeper",], aes(x = location.x, y = location.y, label = last_name), size = 2.5, vjust = 2, angle = 0) + # Show last name if fewer than shooter opponents in triangle or if goalkeeper
  geom_text(data = shot_frame_women, aes(x = location_x, y = location_y, label = player.name), size = 4, vjust = 2, angle = 0) +
  scale_color_manual(values = c("red", "lightblue1"), labels = c("Modspiller", "Medspiller")) +
  guides(color = guide_legend(title = "")) +
  geom_text(data = ff_women[ff_women$teammate,], aes(x = location.x, y = location.y, label = opponents_in_triangle), size = 3, vjust = 0.5, angle = 0, color = "black") +
  geom_text(data = shot_frame_women, aes(x = location_x, y = location_y, label = shooter_opponents_in_triangle_women), size = 4, vjust = 0.5, angle = 0, color = "black") +
  geom_segment(data = ff_women[ff_women$teammate & ff_women$opponents_in_triangle < shooter_opponents_in_triangle_women,], 
               aes(xend = location.x, yend = location.y,
                   x = shot_frame_women$location_x, y = shot_frame_women$location_y), 
               arrow = arrow(type = "open", length = unit(0, "inches")), color = "yellow", linetype = "dotted", alpha = 1) +
  theme(legend.position = c(0.033, 0.98))

# -------------------------------------------- Analyse - Antal & Gns. af modspiller i skydende og medspillers trekant ------------------------------------ #
# Skydende spiller
# Funktion til at tælle antallet modspiller foran alle skydende spiller
count_opponents_for_all_shots <- function(shots_df) {
  totals <- numeric(nrow(shots_df)) # Opret en vektor til at holde totalen for hver skud
  for (i in 1:nrow(shots_df)) {
    # Tjek først, om 'shot.freeze_frame' er NULL
    if (!is.null(shots_df$shot.freeze_frame[[i]])) {
      # Hvis ikke NULL, fortsæt med at udtrække 'freeze_frame' og skyttens position
      current_ff <- shots_df$shot.freeze_frame[[i]] %>%
        rowwise() %>%
        mutate(location.x = location[1], location.y = location[2])
      shooter_location <- c(shots_df$location_x[i], shots_df$location_y[i])
      # Tæller antallet af modstandere i trekanten for det aktuelle skud
      totals[i] <- count_opponents_in_triangle_women(current_ff, shooter_location)
    } else {
      # Hvis 'shot.freeze_frame' er NULL, sæt totalen til 0, da der ikke er nogen data at analysere
      totals[i] <- 0
    }
  }
  return(totals)
}
# Anvender funktionen på 'shots_women' dataframet
opponents_count_women <- tryCatch({
  count_opponents_for_all_shots(shots_women)
}, error = function(e) {
  message("Der opstod en fejl for kvinderne: ", e$message)
  return(rep(NA, nrow(shots_women))) # Returnerer en vektor med NAs i tilfælde af fejl
})
# Tilføjer resultaterne til 'shots_women' som en ny kolonne, hvis der ikke er opstået fejl
if (!is.null(opponents_count_women)) {
  shots_women$opponents_in_triangle <- opponents_count_women
}

# Beregner gennemsnittet og totalen for kolonnen 'opponents_in_triangle' for kvinder
average_opponents_in_triangle_women <- mean(shots_women$opponents_in_triangle, na.rm = TRUE)
total_opponents_in_triangle_women <- sum(opponents_count_women, na.rm = TRUE)
# Udskriver gennemsnittet og totalen for kvinder
cat("Gennemsnit af antal modstandere i trekanten:", average_opponents_in_triangle_women, "\n")
cat("Total antal modstandere i trekanten:", total_opponents_in_triangle_women, "\n")

#
# Medspillere
# Funktion til at tælle antal medspillere med færre modspillere i deres trekant
count_teammates_in_better_positions <- function(shots_df) {
  total_better_positions <- 0
  for (i in 1:nrow(shots_df)) {
    # Tjek først, om 'shot.freeze_frame' er NULL
    if (!is.null(shots_df$shot.freeze_frame[[i]])) {
      # Hvis ikke NULL, fortsæt med at udtrække 'freeze_frame' og skyttens position
      current_ff <- shots_df$shot.freeze_frame[[i]] %>%
        rowwise() %>%
        mutate(location.x = location[1], location.y = location[2])
      shooter_location <- c(shots_df$location_x[i], shots_df$location_y[i])
      # Tæl antallet af medspillere med færre modspillere i deres trekant
      for (j in 1:nrow(current_ff)) {
        if (current_ff$teammate[j]) {
          opponents_in_teammate_triangle <- count_opponents_in_triangle_women(current_ff, c(current_ff$location.x[j], current_ff$location.y[j]))
          if (opponents_in_teammate_triangle < shots_df$opponents_in_triangle[i]) {
            total_better_positions <- total_better_positions + 1
          }
        }
      }
    }
  }
  return(total_better_positions)
}

# Total
# Resultat
total_better_positions_women <- count_teammates_in_better_positions(shots_women)
cat("Det samlede antal medspillere med færre modspillere i deres trekant for kvinder:", total_better_positions_women, "\n")

# Gennemsnit
# Resultat - Beregn gennemsnittet ved at dividere det samlede antal bedre positioner med antallet af skud
average_better_positions_women <- total_better_positions_women / nrow(shots_women)
cat("Gennemsnit af medspillere med færre modspillere i deres trekant for kvinder:", average_better_positions_women, "\n")



# Mænd
# ----------------------------------------------------------------- Mænd - Datamanipulation -------------------------------------------------------------- #
# Udpakker Freezeframes
worldcupFF_men <- worldcup_men %>% filter(type.name=="Shot")

# Filtrer skud og afleveringer og fjern kolonner med kun NA-værdier
shots_men <- worldcupFF_men %>%
  filter(type.name %in% c("Shot")) %>%
  select(where(~ !all(is.na(.))))
# Split location og shot.end_location til to kolonner
shots_men <- shots_men %>%
  filter(!is.null(location)) %>%
  rowwise() %>%
  mutate(
    location_x = location[[1]],
    location_y = location[[2]],
    shot_end_location_x = if (!is.null(shot.end_location)) shot.end_location[[1]] else NA_real_,
    shot_end_location_y = if (!is.null(shot.end_location)) shot.end_location[[2]] else NA_real_
  )
# Beregn afstanden mellem location og shot.end_location
shots_men <- shots_men %>%
  mutate(afstand = sqrt((location_x - shot_end_location_x)^2 + (location_y - shot_end_location_y)^2))
# Behold samme kolonner som i dataframen "worldcupFF_women"
shots_men <- select(shots_men,
                    shot.end_location, shot.freeze_frame, player.id, player.name, 
                    position.id, position.name, team.id, team.name, location, 
                    location_x, location_y, shot_end_location_x, shot_end_location_y, afstand)

# ----------------------------------------------------------------------- FreezeFrame -------------------------------------------------------------------- #
# Freeze frame for mænd
shot_frame_men <- shots_men[905,]
ff_men <- shot_frame_men$shot.freeze_frame[[1]] %>% 
  rowwise() %>% mutate(location.x = location[1], location.y = location[2])

# Funktion til at udtrække efternavne
extract_last_name <- function(full_name) {
  last_name <- tail(strsplit(full_name, " ")[[1]], 1)
  return(last_name)
}
# Tilføjer efternavne til ff_men
ff_men$last_name <- sapply(ff_men$player.name, extract_last_name)

# Opret dataframe til trekant for skudet for mænd
create_shot_triangle_men <- function(shot_men) {
  g1 <- c(120, 36)
  g2 <- c(120, 44)
  x <- c(shot_men[1], g1[1], g2[1], shot_men[1])
  y <- c(shot_men[2], g1[2], g2[2], shot_men[2])
  result_shot <- data.frame(x, y)
  colnames(result_shot) <- c("sx", "sy")
  return(result_shot)
}
# Opret et trekantobjekt baseret på skudlokationerne for mænd
triangle_for_shot_men <- create_shot_triangle_men(unlist(shot_frame_men$location))

# Funktion til at oprette trekantdata for en given spiller baseret på spillerens position
create_player_triangle <- function(player_location_x, player_location_y) {
  g1 <- c(120, 36)
  g2 <- c(120, 44)
  x <- c(player_location_x, g1[1], g2[1], player_location_x)
  y <- c(player_location_y, g1[2], g2[2], player_location_y)
  data.frame(sx = x, sy = y)
}

# Opret trekantdata for alle medspillere (teammates)
# Tilføj en unik identifikator til hver trekant baseret på spillerens navn eller en anden unik egenskab
teammate_triangles <- lapply(1:nrow(ff_men), function(i) {
  if (ff_men$teammate[i]) {
    triangle <- create_player_triangle(ff_men$location.x[i], ff_men$location.y[i])
    triangle$player_name <- ff_men$player.name[i] # Brug spillerens navn som identifikator
    return(triangle)
  } else {
    return(NULL)
  }
})
# Fjern NULL værdier fra listen og konverter til en data.frame, derefter fjern duplikater
teammate_triangles <- lapply(teammate_triangles, function(df) if (!is.null(df)) df[!duplicated(df), ] else NULL)
teammate_triangles_df <- do.call(rbind, teammate_triangles)
teammate_triangles_df$triangle_id <- rep(1:nrow(ff_men[ff_men$teammate, ]), each = 3)

# Ny funktion til at tælle modstandere inde i trekanten
count_opponents_in_triangle <- function(ff_data, shooter_location) {
  # Beregn arealet af hele trekanten
  total_area <- calculate_triangle_area(120, 44, 120, 36, shooter_location[1], shooter_location[2])
  # Tæl hvor mange modstandere der er inden for trekanten
  opponents_inside <- sum(sapply(1:nrow(ff_data), function(i) {
    if (!ff_data$teammate[i]) {
      opponent_location <- c(ff_data$location.x[i], ff_data$location.y[i])
      area1 <- calculate_triangle_area(opponent_location[1], opponent_location[2], 120, 36, shooter_location[1], shooter_location[2])
      area2 <- calculate_triangle_area(120, 44, opponent_location[1], opponent_location[2], shooter_location[1], shooter_location[2])
      area3 <- calculate_triangle_area(120, 44, 120, 36, opponent_location[1], opponent_location[2])
      trisum <- area1 + area2 + area3
      return(abs(trisum - total_area) < 0.1)
    }
    return(FALSE)
  }))
  return(opponents_inside)
}
# Tilføjer antallet af modstandere inden for trekanten til hver spiller i ff_men
ff_men$opponents_in_triangle <- sapply(1:nrow(ff_men), function(i) {
  if (ff_men$teammate[i]) {
    count_opponents_in_triangle(ff_men, c(ff_men$location.x[i], ff_men$location.y[i]))
  } else {
    0
  }
})
# Beregner antallet af modstandere inden for trekanten for den skydende spiller
shooter_opponents_in_triangle <- count_opponents_in_triangle(ff_men, unlist(shot_frame_men$location))
# Find den laveste værdi af antallet af modspillere for alle medspillere
lowest_opponents <- min(ff_men$opponents_in_triangle[ff_men$teammate])

# -------------------------------------------------------------------------- Plots ----------------------------------------------------------------------- #
# Plot - Dot til medspiller med færrest modspiller i trekant
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "steelblue4") +
  geom_polygon(data = triangle_for_shot_men, aes(x = sx, y = sy), alpha = 0.2, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_polygon(data = teammate_triangles_df, aes(x = sx, y = sy, group = triangle_id), alpha = 0.1, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_point(data = ff_men, aes(x = location.x, y = location.y, color = teammate), size = 3) +
  geom_point(data = shot_frame_men, aes(x = location_x, y = location_y), size = 5, color = "green2") +
  theme_pitch() +
  labs(title = "FreezeFrame - Overblik over mest fordelagtige position", subtitle = "Medspillerens/medspillernes navn belyses KUN, hvis de har færre modspillere i egen trekantsareal") + # Titler og undertitler
  coord_flip(xlim = c(85, 130)) + # Læg # Foran for at gøre banen vandret
  geom_text(data = ff_men[(ff_men$teammate & ff_men$opponents_in_triangle < shooter_opponents_in_triangle) | ff_men$position.name == "Goalkeeper",], aes(x = location.x, y = location.y, label = last_name), size = 2.5, vjust = 2, angle = 0) + # Show last name if fewer than shooter opponents in triangle or if goalkeeper
  geom_text(data = shot_frame_men, aes(x = location_x, y = location_y, label = player.name), size = 4, vjust = 2, angle = 0) +
  scale_color_manual(values = c("red", "lightblue1"), labels = c("Modspiller", "Medspiller")) +
  guides(color = guide_legend(title = "")) +
  geom_text(data = ff_men[ff_men$teammate,], aes(x = location.x, y = location.y, label = opponents_in_triangle), size = 3, vjust = 0.5, angle = 0, color = "black") +
  geom_text(data = shot_frame_men, aes(x = location_x, y = location_y, label = shooter_opponents_in_triangle), size = 4, vjust = 0.5, angle = 0, color = "black") +
  geom_segment(data = ff_men[ff_men$teammate & ff_men$opponents_in_triangle == lowest_opponents,], 
               aes(xend = location.x, yend = location.y,
                   x = shot_frame_men$location_x, y = shot_frame_men$location_y), 
               arrow = arrow(type = "open", length = unit(0, "inches")), color = "yellow", linetype = "dotted", alpha = 1) +
  theme(legend.position = c(0.033, 0.98))

# Plot - Dot til færre spillere end skydende spiller
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white", fill = "steelblue4") +
  geom_polygon(data = triangle_for_shot_men, aes(x = sx, y = sy), alpha = 0.2, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_polygon(data = teammate_triangles_df, aes(x = sx, y = sy, group = triangle_id), alpha = 0.1, fill = "white", color = alpha("lightblue", 0.35)) + # Slet color for at fjerne outlines
  geom_point(data = ff_men, aes(x = location.x, y = location.y, color = teammate), size = 3) +
  geom_point(data = shot_frame_men, aes(x = location_x, y = location_y), size = 5, color = "green2") +
  theme_pitch() +
  labs(title = "FreezeFrame - Overblik over mest fordelagtige position", subtitle = "Medspillerens/medspillernes navn belyses KUN, hvis de har færre modspillere i egen trekantsareal") + # Titler og undertitler
  coord_flip(xlim = c(85, 130)) + # Læg # Foran for at gøre banen vandret
  geom_text(data = ff_men[(ff_men$teammate & ff_men$opponents_in_triangle < shooter_opponents_in_triangle) | ff_men$position.name == "Goalkeeper",], aes(x = location.x, y = location.y, label = last_name), size = 2.5, vjust = 2, angle = 0) + # Show last name if fewer than shooter opponents in triangle or if goalkeeper
  geom_text(data = shot_frame_men, aes(x = location_x, y = location_y, label = player.name), size = 4, vjust = 2, angle = 0) +
  scale_color_manual(values = c("red", "lightblue1"), labels = c("Modspiller", "Medspiller")) +
  guides(color = guide_legend(title = "")) +
  geom_text(data = ff_men[ff_men$teammate,], aes(x = location.x, y = location.y, label = opponents_in_triangle), size = 3, vjust = 0.5, angle = 0, color = "black") +
  geom_text(data = shot_frame_men, aes(x = location_x, y = location_y, label = shooter_opponents_in_triangle), size = 4, vjust = 0.5, angle = 0, color = "black") +
  geom_segment(data = ff_men[ff_men$teammate & ff_men$opponents_in_triangle < shooter_opponents_in_triangle,], 
               aes(xend = location.x, yend = location.y,
                   x = shot_frame_men$location_x, y = shot_frame_men$location_y), 
               arrow = arrow(type = "open", length = unit(0, "inches")), color = "yellow", linetype = "dotted", alpha = 1) +
  theme(legend.position = c(0.033, 0.98))

# --------------------------------------------- Analyse - Antal & Gns. af modspillere i skydende og medspillers trekant ---------------------------------- #
# Skydende spiller
# Funktion til at tælle antallet modspillere foran alle skydende spillere
count_opponents_for_all_shots <- function(shots_men) {
  totals <- numeric(nrow(shots_men)) # Opret en vektor til at holde totalen for hver skud
  for (i in 1:nrow(shots_men)) {
    # Tjek først, om 'shot.freeze_frame' er NULL
    if (!is.null(shots_men$shot.freeze_frame[[i]])) {
      # Hvis ikke NULL, fortsæt med at udtrække 'freeze_frame' og skyttens position
      current_ff <- shots_men$shot.freeze_frame[[i]] %>%
        rowwise() %>%
        mutate(location.x = location[1], location.y = location[2])
      shooter_location <- c(shots_men$location_x[i], shots_men$location_y[i])
      # Tæller antallet af modstandere i trekanten for det aktuelle skud
      totals[i] <- count_opponents_in_triangle(current_ff, shooter_location)
    } else {
      # Hvis 'shot.freeze_frame' er NULL, sæt totalen til 0, da der ikke er nogen data at analysere
      totals[i] <- 0
    }
  }
  return(totals)
}

# Anvender funktionen på 'shots_men' dataframet
opponents_count_men <- tryCatch({
  count_opponents_for_all_shots(shots_men)
}, error = function(e) {
  message("Der opstod en fejl: ", e$message)
  return(rep(NA, nrow(shots_men))) # Returnerer en vektor med NAs i tilfælde af fejl
})

# Tilføjer resultaterne til 'shots_men' som en ny kolonne, hvis der ikke er opstået fejl
if (!is.null(opponents_count_men)) {
  shots_men$opponents_in_triangle <- opponents_count_men
}

# Beregner gennemsnittet og totalen for kolonnen 'opponents_in_triangle' for mænd
average_opponents_in_triangle_men <- mean(shots_men$opponents_in_triangle, na.rm = TRUE)
total_opponents_in_triangle_men <- sum(opponents_count_men, na.rm = TRUE)
# Udskriver gennemsnittet og totalen for mænd
cat("Gennemsnit af antal modspillere i trekanten:", average_opponents_in_triangle_men, "\n")
cat("Total antal modspillere i trekanten:", total_opponents_in_triangle_men, "\n")

#
# Medspillere
# Funktion til at tælle antal medspillere med færre modspillere i deres trekant
count_teammates_in_better_positions <- function(shots_men) {
  total_better_positions <- 0
  for (i in 1:nrow(shots_men)) {
    # Tjek først, om 'shot.freeze_frame' er NULL
    if (!is.null(shots_men$shot.freeze_frame[[i]])) {
      # Hvis ikke NULL, fortsæt med at udtrække 'freeze_frame' og skyttens position
      current_ff <- shots_men$shot.freeze_frame[[i]] %>%
        rowwise() %>%
        mutate(location.x = location[1], location.y = location[2])
      shooter_location <- c(shots_men$location_x[i], shots_men$location_y[i])
      # Tæl antallet af medspillere med færre modspillere i deres trekant
      for (j in 1:nrow(current_ff)) {
        if (current_ff$teammate[j]) {
          opponents_in_teammate_triangle <- count_opponents_in_triangle(current_ff, c(current_ff$location.x[j], current_ff$location.y[j]))
          if (opponents_in_teammate_triangle < shots_men$opponents_in_triangle[i]) {
            total_better_positions <- total_better_positions + 1
          }
        }
      }
    }
  }
  return(total_better_positions)
}

# Total
# Resultat
total_better_positions_men <- count_teammates_in_better_positions(shots_men)
cat("Det samlede antal medspillere med færre modspillere i deres trekant for mænd:", total_better_positions_men, "\n")

# Gennemsnit
# Resultat - Beregn gennemsnittet ved at dividere det samlede antal bedre positioner med antallet af skud
average_better_positions_men <- total_better_positions_men / nrow(shots_men)
cat("Gennemsnit af medspillere med færre modspillere i deres trekant for mænd:", average_better_positions_men, "\n")




