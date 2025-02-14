#### ---------------------------------------------------- Expected Points (xP) og analyse af skuddata ----------------------------------------------------- ####  
# Emne: Beregning af Expected Points (xP) baseret på Brøndbys xG for sæsonen 23/24  
# Dette script demonstrerer, hvordan man:  
#   1. Kombinerer flere datasæt ved hjælp af join-funktioner (left_join, inner_join)  
#   2. Beregner Expected Goals (xG) for både hjemme- og udehold for Brøndby  
#   3. Simulerer kampresultater baseret på xG og antal skud  
#   4. Beregner og visualiserer Brøndbys forventede point (xP) for sæsonen baseret på simulationsresultater  
#   5. Arbejder med specifikke kampdata og justerer for særlige hændelser som f.eks. ændrede xG-værdier og antal skud i en specifik kamp  
#   6. Anvender klassificerings- og regressionsmetoder som beslutningstræer til at analysere data  
#   7. Evaluere modelpræstation ved hjælp af ROC-kurver, præcision og andre metrikker  

# Libraries  
library(ggsoccer)      # Visualisering af fodbolddata på et fodboldbanekort  
library(dplyr)          # Data manipulation og transformation  
library(tidyr)          # Datatransformation og reshaping  
library(logr)           # Logning af handlinger, bruges til at spore scriptets eksekvering  
library(rpart)          # Beslutningstræ-algoritme til at bygge modeller baseret på data  
library(rpart.plot)     # Visualisering af beslutningstræer, gør det muligt at plotte træstrukturer  
library(ggplot2)        # Visualisering af data, primært til plots og diagrammer  
library(psych)          # Generelle funktioner til statistisk analyse  
library(caret)          # Udfører modeltræning og cross-validation (inkluderer andre modelbiblioteker)  
library(RColorBrewer)   # Farvevalg til pæne plots, fx trævisualiseringer  
library(party)          # Alternativ beslutningstræ-algoritme (Random Forest m.fl.)  
library(partykit)       # Konvertering af rpart objekt til BinaryTree for visualisering  
library(pROC)           # ROC-kurve og præstationsmåling af modeller  
library(ISLR)           # Inkluderer datasæt som Carseat til modellering og analyse  
library(jsonlite)       # Håndtering af JSON-data og API-integration  

# ------------------------------------------------------------------------ Opgave 2.1 ----------------------------------------------------------------------- #
#Expected point(Xp) baseret udfra Brøndbys Xg for sæsonen 23/24
# Left join - Joiner "allShots24", "wys_events_23" & "allMatches"
allMatches24 <- allShots24 %>%
  left_join(wys_events_23, by = c("WyEventId", "MatchId", "PrimaryType")) # %>%
# filter(CompetitionId == 335 & SeasonId == 188039) # NYT VIRKER IKKE KUN DENNE DEL AF KODEN RESTEN ER PERFEKT
# Tilføjer nu "allMatches" til ovenstående left join
allMatches24 <- allMatches24 %>%
  left_join(allMatches, by = c("MatchId"))
# Opretter et nyt dataframe for kampe med Brøndby
brondbyMatches <- allMatches24 %>%
  filter(grepl("Brøndby", MatchLabel))

# Antag at du har en dataframe 'brondbyMatches' med de nødvendige data
# Opretter nye kolonner for hjemme- og udeholdene
teams <- strsplit(brondbyMatches$MatchLabel, " - ")
brondbyMatches$HomeTeam <- sapply(teams, function(x) x[1])
brondbyMatches$AwayTeam <- sapply(teams, function(x) gsub(",.*", "", x[2]))
# Udskriver de første rækker af dataframen for at kontrollere ændringerne
head(brondbyMatches[, c("HomeTeam", "AwayTeam")])

#
# Vis alle unikke værdier i kolonnen PrimaryType
unique_values_in_PrimaryType <- unique(brondbyMatches$PrimaryType)
print(unique_values_in_PrimaryType)
# Definer en vektor med alle relevante skudtyper
skudtyper <- c("shot", "corner", "free_kick", "penalty")
# Tilføj kolonner med xg og antal skud for brøndby og modstander
brondbyXG <- brondbyMatches %>%
  group_by(GameWeek, MatchLabel, HomeTeam, AwayTeam) %>%
  summarize(
    HomeTeamXg = sum(ifelse(HomeTeam == "Brøndby", ifelse(TeamId == 7453, Shotxg, 0), ifelse(TeamId != 7453, Shotxg, 0))),
    AwayTeamXg = sum(ifelse(AwayTeam == "Brøndby", ifelse(TeamId == 7453, Shotxg, 0), ifelse(TeamId != 7453, Shotxg, 0))),
    HomeTeamShots = sum(ifelse(HomeTeam == "Brøndby" & TeamId == 7453, 1, ifelse(HomeTeam != "Brøndby" & TeamId != 7453, 1, 0))),
    AwayTeamShots = sum(ifelse(AwayTeam == "Brøndby" & TeamId == 7453, 1, ifelse(AwayTeam != "Brøndby" & TeamId != 7453, 1, 0))),
  ) %>%
  ungroup()

###
set.seed(42)  # Sæt seed for reproducerbarhed
simulate_game <- function(xg, shots) {
  # Juster xG opad baseret på antal skud
  adjusted_xg <- xg + (shots / 10) * 0.1
  goals <- rpois(1, adjusted_xg)
  return(goals)
}
xpoints_game <- function(home_xg, away_xg, home_shots, away_shots) {
  home_goals <- simulate_game(home_xg, home_shots)
  away_goals <- simulate_game(away_xg, away_shots)
  if (home_goals > away_goals) {
    home_points <- 3
    away_points <- 0
  } else if (home_goals == away_goals) {
    home_points <- 1
    away_points <- 1
  } else {
    home_points <- 0
    away_points <- 3
  }
  return(c(home_points, away_points))
}
xpoints_season <- function(brondbyXG, simulations) {
  brondbyXG$BrondbyXP <- NA  # Tilføj en ny kolonne til dataframe
  for (i in 1:nrow(brondbyXG)) {
    home_xg <- brondbyXG$HomeTeamXg[i]
    away_xg <- brondbyXG$AwayTeamXg[i]
    home_shots <- brondbyXG$HomeTeamShots[i]
    away_shots <- brondbyXG$AwayTeamShots[i]
    points_accumulated <- numeric(2)
    for (j in 1:simulations) {
      points <- xpoints_game(home_xg, away_xg, home_shots, away_shots)
      points_accumulated <- points_accumulated + points
    }
    # Bestem Brøndby's forventede point baseret på om de er hjemme- eller udehold
    if (brondbyXG$HomeTeam[i] == "Brøndby") {
      brondbyXG$BrondbyXP[i] <- points_accumulated[1] / simulations
    } else {
      brondbyXG$BrondbyXP[i] <- points_accumulated[2] / simulations
    }
  }
  return(brondbyXG)
}
# Antal simulationer
simulations <- 10000
# Beregn xP for Brøndby i hver kamp og opdater dataframe
brondbyXG <- xpoints_season(brondbyXG, simulations)
# Beregn og udskriv det samlede forventede antal point for Brøndby
total_xp_brondby <- sum(brondbyXG$BrondbyXP)
print(paste("Samlet forventet antal point for Brøndby:", total_xp_brondby))


# ------------------------------------------------------------------------ Opgave 2.2 ----------------------------------------------------------------------- #
# Sammensætter alle events dataframes til et stort
joined_df <- inner_join(events_main_info, events_typeSecondary, by = "eventId")
joined_df <- inner_join(joined_df, events_possessionTypes, by = "eventId")
joined_df <- left_join(joined_df, events_shots, by = "eventId")
joined_df <- left_join(joined_df, events_passes, by = "eventId")
joined_df <- left_join(joined_df, events_aerialDuel, by = "eventId")
joined_df <- left_join(joined_df, events_carry, by = "eventId")
joined_df <- left_join(joined_df, events_groundDuel, by = "eventId")
joined_df <- left_join(joined_df, events_infraction, by = "eventId")
joined_df <- left_join(joined_df, events_possession_cph, by = "eventId")

# Deler min joined data så jeg har for de 3 bif kampe jeg skal arbejde med
# Definér matchId'er for de tre kampe
match_ids <- c(5466032, 5466040, 5466044)
# Opret de tre nye dataframes ved at filtrere joined_df
bif_vejle <- subset(joined_df, matchId.x == match_ids[1])
bif_vff <- subset(joined_df, matchId.x == match_ids[2])
BrøndbyIF_SilkeborgIF <- subset(joined_df, matchId.x == match_ids[3])

# Antal 'Offside' i kolonnen 'typePrimary.x'
antal_offside <- sum(BrøndbyIF_SilkeborgIF$typePrimary.x == "offside", na.rm = TRUE)
# Print resultatet
print(antal_offside)

# Tilføj kolonner med xg og antal skud for brøndby og modstander
brondbyXG2 <- brondbyMatches %>%
  group_by(GameWeek, MatchLabel, HomeTeam, AwayTeam) %>%
  summarize(
    HomeTeamXg = sum(ifelse(HomeTeam == "Brøndby", ifelse(TeamId == 7453, Shotxg, 0), ifelse(TeamId != 7453, Shotxg, 0))),
    AwayTeamXg = sum(ifelse(AwayTeam == "Brøndby", ifelse(TeamId == 7453, Shotxg, 0), ifelse(TeamId != 7453, Shotxg, 0))),
    HomeTeamShots = sum(ifelse(HomeTeam == "Brøndby" & TeamId == 7453, 1, ifelse(HomeTeam != "Brøndby" & TeamId != 7453, 1, 0))),
    AwayTeamShots = sum(ifelse(AwayTeam == "Brøndby" & TeamId == 7453, 1, ifelse(AwayTeam != "Brøndby" & TeamId != 7453, 1, 0))),
  ) %>%
  ungroup()
# Opdater data for kampen i GameWeek 22, Brøndby mod Silkeborg
brondbyXG2 <- brondbyXG2 %>%
  mutate(
    HomeTeamXg = if_else(GameWeek == 22 & MatchLabel == "Brøndby - Silkeborg, 4-1" & HomeTeam == "Brøndby", HomeTeamXg - 0.04734, HomeTeamXg),
    HomeTeamShots = if_else(GameWeek == 22 & MatchLabel == "Brøndby - Silkeborg, 4-1" & HomeTeam == "Brøndby", HomeTeamShots - 1, HomeTeamShots),
    AwayTeamXg = if_else(GameWeek == 22 & MatchLabel == "Brøndby - Silkeborg, 4-1" & AwayTeam == "Brøndby", AwayTeamXg - 0.04734, AwayTeamXg),
    AwayTeamShots = if_else(GameWeek == 22 & MatchLabel == "Brøndby - Silkeborg, 4-1" & AwayTeam == "Brøndby", AwayTeamShots - 1, AwayTeamShots)
  )
# Kontroller de justerede værdier for den specifikke kamp
specific_game <- brondbyXG %>%
  filter(GameWeek == 22 & MatchLabel == "Brøndby - Silkeborg, 4-1")

set.seed(42)  # Sæt seed for reproducerbarhed
simulate_game <- function(xg, shots) {
  # Juster xG opad baseret på antal skud
  adjusted_xg <- xg + (shots / 10) * 0.1
  goals <- rpois(1, adjusted_xg)
  return(goals)
}
xpoints_game <- function(home_xg, away_xg, home_shots, away_shots) {
  home_goals <- simulate_game(home_xg, home_shots)
  away_goals <- simulate_game(away_xg, away_shots)
  if (home_goals > away_goals) {
    home_points <- 3
    away_points <- 0
  } else if (home_goals == away_goals) {
    home_points <- 1
    away_points <- 1
  } else {
    home_points <- 0
    away_points <- 3
  }
  return(c(home_points, away_points))
}

xpoints_season <- function(brondbyXG2, simulations) {
  brondbyXG$BrondbyXP <- NA  # Tilføj en ny kolonne til dataframe
  for (i in 1:nrow(brondbyXG)) {
    home_xg <- brondbyXG2$HomeTeamXg[i]
    away_xg <- brondbyXG2$AwayTeamXg[i]
    home_shots <- brondbyXG2$HomeTeamShots[i]
    away_shots <- brondbyXG2$AwayTeamShots[i]
    points_accumulated <- numeric(2)
    for (j in 1:simulations) {
      points <- xpoints_game(home_xg, away_xg, home_shots, away_shots)
      points_accumulated <- points_accumulated + points
    }
    # Bestem Brøndby's forventede point baseret på om de er hjemme- eller udehold
    if (brondbyXG2$HomeTeam[i] == "Brøndby") {
      brondbyXG2$BrondbyXP[i] <- points_accumulated[1] / simulations
    } else {
      brondbyXG2$BrondbyXP[i] <- points_accumulated[2] / simulations
    }
  }
  return(brondbyXG2)
}

# Antal simulationer
simulations <- 10000
# Beregn xP for Brøndby i hver kamp og opdater dataframe
brondbyXG2 <- xpoints_season(brondbyXG2, simulations)
# Beregn og udskriv det samlede forventede antal point for Brøndby
total_xp_brondby <- sum(brondbyXG2$BrondbyXP)
print(paste("Samlet forventet antal point for Brøndby:", total_xp_brondby))

