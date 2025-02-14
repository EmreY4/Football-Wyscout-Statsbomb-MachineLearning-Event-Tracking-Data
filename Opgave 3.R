#### ------------------------------------------------------ Clustering af event data - Holland & Polen ---------------------------------------------------- ####  
# Emne: Klyngedannelse af spillere baseret på afslutningsteknik og skudpræcision i den hollandske og polske liga for sæsonen 21/22 & 22/23
# Scriptet fokuserer på at identificere klynger af spillere baseret på deres afslutningsteknik og skudpræcision i både den hollandske og polske liga for sæsonerne 21/22 og 22/23. 
# Klyngerne beskrives yderligere med hensyn til spillerroller og præstationer, og visualiseringer bruges til at fremhæve de vigtigste forskelle i spilleradfærd på banen.
# Dette script fortsætter med at:
#   1. Beregne gennemsnitsvinkel og gennemsnitlig expected goal (xG) for spillernes skuddata i både den hollandske og polske liga
#   2. Udføre k-means clustering for at gruppere spillere baseret på deres afslutningsteknik og skudpræcision
#   3. Evaluere klyngernes kvalitet ved hjælp af inertia og visualisere klyngefordelingen i elbow plots
#   4. Udføre en detaljeret klyngeanalyse for at beskrive hver klynge baseret på spillerens rolle og skudpræstationer
#   5. Visualisere de enkelte klynger med scatter plots, hvor spillerens navn vises som tekstetiketter
#   6. Filtrere og analysere spillerdata for de enkelte klynger, herunder forsvarsspillere, angrebsspillere og midtbanespillere
#   7. Identificere mønstre og forskelle mellem spillere i de forskellige klynger for at få indsigt i spillernes skudvaner

# Libraries
library(dplyr)         # Data manipulation og transformation, bruges til at manipulere og summarise data
library(ggrepel)       # Visualisering af data med tekstetiketter, fjerner overlap og forbedrer synligheden
library(mongolite)     # MongoDB integration, bruges til at hente og gemme data i MongoDB-databaser
library(jsonlite)      # Håndtering af JSON-data, bruges til at parse og generere JSON-objekter
library(ggplot2)       # Visualisering af data, primært til at skabe plots og diagrammer som scatter plots og linjediagrammer
library(stats)         # Statistiske funktioner og modeller, bruges til at køre statistiske analyser som k-means clustering
library(cluster)       # Klyngeanalyse, indeholder funktioner som kmeans og andre clustering-algoritmer
library(tidyr)         # Datatransformation og reshaping, bruges til at formatere og opdele data på tværs af kolonner

# -------------------------------------------------------------- Dataindsamling & Transformering ------------------------------------------------------------ #
# Forbindelse med matches collection
con_matches <- mongo(
  collection = "matches",
  db = "wys",
  url = "mongodb://localhost"
)
# Forbindelse med games collection
con_games <- mongo(
  collection = "games",
  db = "wys",
  url = "mongodb://localhost"
)
# Forbindelse med players
con_players <- mongo(
  collection = 'players',
  db = 'wys',
  url = "mongodb://localhost"
)

# Find Hollandske og Polske liga
# Alle kampe - Henter alle kampe i Holland og Polen - Indeholder kun beskrivende data (Ingen skud, afleveringer osv data)
allmatches <- con_matches$find(
  query = '{}',
  fields = '{}',
)

# Alle spillere - Henter alle spillere (information omkring deres profil)
allplayers <- con_players$find(
  query = '{}',
  fields = '{}',
)
# Konverterer data til en flad JSON-struktur
allplayers <- fromJSON(toJSON(allplayers), flatten = T)

# Alle skud - Henter alle skud data i Holland og Polen
allshots <- con_games$find(
  query = '{"type.primary":"shot"}', # Syntaksen/koden/querien er fundet i mongoDB compass, hvor jeg kan se alle shots
  fields = '{}'
)
# Konverterer data til en flad JSON-struktur
allshots <- fromJSON(toJSON(allshots), flatten = T)

# Alle afleveringer - Henter alle skud data i Holland og Polen
allpasses <- con_games$find(
  query = '{"type.primary":"pass"}', # Syntaksen/koden/querien er fundet i mongoDB compass, hvor jeg kan se alle shots
  fields = '{}'
)
# Konverterer data til en flad JSON-struktur
allpasses <- fromJSON(toJSON(allpasses), flatten = T)

# Filtrering af allmatches for Holland og Polen
allmatches_nel <- allmatches %>%
  filter(competitionId == 635)
allmatches_pol <- allmatches %>%
  filter(competitionId == 692)

# Join allshot med allmatch_nel
allshot_nel <- inner_join(allshots, allmatches_nel, by = c("matchId" = "_id"))
# Joine allshot med allmatch_pol
allshot_pol <- inner_join(allshots, allmatches_pol, by = c("matchId" = "_id"))
# Joine allpasses med allmatch_nel
allpasses_nel <- inner_join(allpasses, allmatches_nel, by = c("matchId" = "_id"), relationship = "many-to-many")
# Joine allpasses med allmatch_pol
allpasses_pol <- inner_join(allpasses, allmatches_pol, by = c("matchId" = "_id"), relationship = "many-to-many")
# For allshot_combined_nel
allshot_nel <- inner_join(allshot_nel, allplayers, by = c("player.name" = "shortName"),relationship = "many-to-many")
# For allshot_combined_pol
allshot_pol <- inner_join(allshot_pol, allplayers, by = c("player.name" = "shortName"),relationship = "many-to-many")

# Nu har vi vores matchede data
# For allpasses_combined_nel
allpasses_nel <- inner_join(allpasses_nel, allplayers, by = c("player.name" = "shortName"), relationship = "many-to-many")
# For allpasses_combined_pol
allpasses_pol <- inner_join(allpasses_pol, allplayers, by = c("player.name" = "shortName"), relationship = "many-to-many")

# ----------------------------------------------------------------------- Sæsonopdeling --------------------------------------------------------------------- #
# Skud
# For den hollandske liga 21/22
holland_shot_2122 <- allshot_nel %>%
  filter(date >= "2021-08-13 20:00:00" & date <= "2022-05-15 14:30:00")
# For den hollandske liga 22/23
holland_shot_2223 <- allshot_nel %>%
  filter(date >= "2022-08-05" & date <= "2023-05-23")

# For den polske liga 21/22
poland_shot_2122 <- allshot_pol %>%
  filter(date >= "2021-07-23 18:00:00" & date <= "2022-05-21 17:30:00")
# For den polske liga 22/23
poland_shot_2223 <- allshot_pol %>%
  filter(date >= "2022-07-15" & date <= "2023-05-27")

# ------------------------------------------------------------------------ Afleveringer --------------------------------------------------------------------- #
# For den hollandske liga 21/22
holland_pass_2122 <- allpasses_nel %>%
  filter(date >= "2021-08-13 20:00:00" & date <= "2022-05-15 14:30:00")
# For den hollandske liga 22/23
holland_pass_2223 <- allpasses_nel %>%
  filter(date >= "2022-08-05" & date <= "2023-05-23")

# For den polske liga 21/22
poland_pass_2122 <- allpasses_pol %>%
  filter(date >= "2021-07-23 18:00:00" & date <= "2022-05-21 17:30:00")
# For den polske liga 22/23
poland_pass_2223 <- allpasses_pol %>%
  filter(date >= "2022-07-15" & date <= "2023-05-27")

# ------------------------------------------------------------- Lav en procent andel af accurate = T -------------------------------------------------------- #
# Grupper data efter spillernavn og summer 'pass.accurate' for at tælle antallet af præcise afleveringer
library(dplyr)
#holland_pass_2122_gruop <- holland_pass_2122 %>%
  #group_by(player.name) %>%
  #summarise(total_passes = n(),
    #        accurate_passes = sum(pass.accurate == TRUE),
   #         procent_accurate = round((accurate_passes / total_passes) * 100, 2))
#holland_pass_2223 <- holland_pass_2223 %>%
 # group_by(player.name) %>%
  #summarise(total_passes = n(), accurate_passes = sum(pass.accurate == TRUE)) %>%
  #mutate(pass_accuracy_percentage = (accurate_passes / total_passes) * 100)

# ----------------------------------------------------------------------- Find Variabler--------------------------------------------------------------------- #
# Udfør lineær regression
model <- lm(pass.accurate ~ pass.length + pass.angle + location.x + location.y + role.code3.x + pass.endLocation.x + pass.endLocation.y, data = holland_pass_2122)
#prøv at brug role.code3.x isteder for player.possition
#viser os at der er mange signifikante til om hvor vidt en aflevering bliver accurate eller ej,
#derfor når vi nu skal lave cluster vil vi tage de 3 mest betydningsfulde signifikanter med.
#player possiosion er for ustabil i signifikans
model2 <- lm(pass.accurate ~ pass.length + location.x + location.y + pass.endLocation.x + pass.endLocation.y, data = holland_pass_2122)

# --------------------------------------------------------------- Cluster model 1 Holland 21/22------------------------------------------------------------- #
# Laver cluster med pass.length, location.x, location.y, pass.endpossition.x og pass.possition.y
# Funktion til at beregne inerti
calculate_inertia <- function(data, k) {
  kmeans_model <- kmeans(data, centers = k)
  return(kmeans_model$tot.withinss)
}
# Funktion til at beregne gennemsnitlig afleveringslængde for hver spiller
calculate_avg_pass_length <- function(df) {
  avg_pass_length <- df %>%
    group_by(player.name) %>%
    summarise(avg_length = mean(pass.length, na.rm = TRUE))
  return(avg_pass_length)
}
gns_pass_length_hol_2122 <- calculate_avg_pass_length(holland_pass_2122)
# Gruppér data efter spillernavn og beregn gennemsnittet af location.x for hver spiller
player_avg_location <- holland_pass_2122 %>%
  group_by(player.name) %>%
  summarize(avg_location_x = mean(location.x, na.rm = TRUE))

# Slå sammen med avg_pass_length_holland
holland_merged <- left_join(player_avg_location, gns_pass_length_hol_2122, by = "player.name")

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(holland_merged[, -1], centers = 3)

# Tilknyt klyngeoplysninger til hver spiller
holland_merged$kmeans_cluster <- kmeans_result$cluster

# Opsummer centrale egenskaber for hver klynge
cluster_summary <- holland_merged %>%
  group_by(kmeans_cluster) %>%
  summarise(avg_pass_length = mean(avg_length), avg_location_x = mean(avg_location_x))

# Vis opsummering
print(cluster_summary)

# Laver klyngeanalyse for et varierende antal klynger og evaluer inertia
inertia <- numeric(8)  # Vi tester op til 10 klynger
for (i in 1:8) {
  kmeans_model <- kmeans(data1[, -1], centers = i)
  inertia[i] <- kmeans_model$tot.withinss
}

# Lav et plot for at visualisere inerti versus antallet af klynger
elbow_plot <- ggplot(data.frame(K = 1:8, Inertia = inertia), aes(x = K, y = Inertia)) +
  geom_line() +
  geom_point() +
  labs(x = "Antal klynger", y = "Inertia") +
  ggtitle("Elbow plot for klyngeanalyse Holland 21/22") +
  theme_minimal()

print(elbow_plot)

# Lav et scatterplot af dine data, farve efter klynger
ggplot(data = holland_merged, aes(x = avg_location_x  , y = avg_length, color = factor(kmeans_cluster))) +
  geom_point() +
  labs(x = "Gns Location on field", y = "Gns Pass Length" , color = "Cluster") +
  ggtitle("Clustre for hver spiller rolle") +
  theme_minimal()

# --------------------------------------------------------------------- Målmænd - Clustre ------------------------------------------------------------------- #
# Filtrer data for kun at inkludere klynge 1
cluster_df_data <- holland_merged %>%
  filter(kmeans_cluster == 1)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
p <- ggplot(data = cluster_df_data, aes(x = avg_location_x, y = avg_length, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Gns. Hollandske målmand 21/22 skyder omkring 35 meter op af banen") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster1_names <- unique(cluster_df_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_df <- holland_pass_2122 %>%
  filter(player.name %in% cluster3_names)

# ---------------------------------------------------------------- Midtbane spillere - Cluster -------------------------------------------------------------- #
# Filtrer data for kun at inkludere klynge 1
cluster_mb_data <- holland_merged %>%
  filter(kmeans_cluster == 3)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
m <- ggplot(data = cluster_mb_data, aes(x = avg_location_x, y = avg_length, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Cluster 2 - Midtbane og forsvar\nZ. Buurmeesster skiller sig ud som en defender med lange afleveringer") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster2_names <- unique(cluster_mb_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_mb <- holland_pass_2122 %>%
  filter(player.name %in% cluster2_names)
# ------------------------------------------------------..------------Cluster 1 - Angriber ----------------------------------------------------------------- #
cluster_ag_data <- holland_merged %>%
  filter(kmeans_cluster == 2)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
a <- ggplot(data = cluster_ag_data, aes(x = avg_location_x, y = avg_length, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Cluster 1 - Angriber og midtbane: Holland\nS. Van Duivenbooden ligger op til de andre angriber") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster1_names <- unique(cluster_ag_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_ag <- holland_pass_2122 %>%
  filter(player.name %in% cluster1_names)
# ----------------------------------------------------------------------- Polen 21/22 ----------------------------------------------------------------------- #
gns_pass_length_pol_2122 <- calculate_avg_pass_length(poland_pass_2122)
# Gruppér data efter spillernavn og beregn gennemsnittet af location.x for hver spiller
player_avg_location_polen <- poland_pass_2122 %>%
  group_by(player.name) %>%
  summarize(avg_location_x = mean(location.x, na.rm = TRUE))

# Slå sammen med avg_pass_length_holland
polen_merged2122 <- left_join(player_avg_location_polen, gns_pass_length_pol_2122, by = "player.name")

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(polen_merged2122[, -1], centers = 3)
# Tilknyt klyngeoplysninger til hver spiller
polen_merged2122$kmeans_cluster <- kmeans_result$cluster

# Opsummer centrale egenskaber for hver klynge
cluster_summary <- polen_merged2122 %>%
  group_by(kmeans_cluster) %>%
  summarise(avg_pass_length = mean(avg_length), avg_location_x = mean(avg_location_x))

# Vis opsummering
print(cluster_summary)

# Lav en klyngeanalyse for et varierende antal klynger og evaluer inerti
inertia <- numeric(10)  # Vi tester op til 10 klynger
for (i in 1:10) {
  kmeans_model <- kmeans(polen_merged2122[, -1], centers = i)
  inertia[i] <- kmeans_model$tot.withinss
}

# Lav et plot for at visualisere inerti versus antallet af klynger
elbow_plot <- ggplot(data.frame(K = 1:10, Inertia = inertia), aes(x = K, y = Inertia)) +
  geom_line() +
  geom_point() +
  labs(x = "Antal klynger", y = "Inertia") +
  ggtitle("Elbow plot for Klyngeanalyse Polen 2122") +
  theme_minimal()

print(elbow_plot)

# Lav et scatterplot af dine data, farve efter klynger
ggplot(data = polen_merged2122, aes(x = avg_location_x, y = avg_length, color = factor(kmeans_cluster))) +
  geom_point() +
  labs(x = "Gns Location on field", y = "Gns Pass Length" , color = "Cluster") +
  ggtitle("Clustre for hver spiller rolle Polen") +
  theme_minimal()
# --------------------------------------------------------------------- Cluster 2: målmænd ------------------------------------------------------------------ #
cluster_gk_data <- polen_merged2122 %>%
  filter(kmeans_cluster == 2)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
gkpolen <- ggplot(data = cluster_gk_data, aes(x = avg_location_x, y = avg_length, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Cluster 1 - Målmænd: Polens målmænd står længere fremme end Hollands målmænd\nD.Stipica står længst fremme, til gengæld skyder han ikke så langt") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster2_namespol <- unique(cluster_gk_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_gkpol <- poland_pass_2122 %>%
  filter(player.name %in% cluster2_namespol)

# ------------------------------------------------------------- Midtbane & Forsvarsspillere - Cluster ------------------------------------------------------- #
cluster_mbpol_data <- polen_merged2122 %>%
  filter(kmeans_cluster == 1)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
mbpolen <- ggplot(data = cluster_mbpol_data, aes(x = avg_location_x, y = avg_length, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Cluster 2 - Midtbane og forsvar: Polens forsvar og midtbane er centreret omkring midten af banen\nSkuder i gns. kortere end Hollands farsvar og midtbane") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster1_namespol <- unique(cluster_mbpol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_mbpol <- poland_pass_2122 %>%
  filter(player.name %in% cluster1_namespol)

# ---------------------------------------------------------------- Polen - Angribere - Cluster -------------------------------------------------------------- #
cluster_angpol_data <- polen_merged2122 %>%
  filter(kmeans_cluster == 3)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
angpolen <- ggplot(data = cluster_angpol_data, aes(x = avg_location_x, y = avg_length, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Cluster 3 - Midtbane og Angriber: Polen 2122\nH. Matynia har højt gns. længe på sine afleveringer selvom han spiller LB\nK.Kostory har længere aflevering ind over midten end hollands SS") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster3_namespol <- unique(cluster_angpol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_angpol <- poland_pass_2122 %>%
  filter(player.name %in% cluster3_namespol)


### ---------------------------------------------------------------------- SHOT opg. 3.2 ----------------------------------------------------------------- ###
#Udfør lineær regression
modelshot <- lm(shot.isGoal ~ location.x + location.y + shot.goalZone + shot.bodyPart + shot.xg + foot.x , data = poland_shot_2122)
#Næsten ingen variabler har signifikans, vi prøver at udregne vinklen til målet ved hjælp af pythagoros
#Udregning af vinkel for hver liga hver sæson
#Udregning af afstand på alle skud.
#Via wyscouts eget data er målstolperne sat til at have faste lokation
#Venstre målstolpe(100,37)
#Højre målstolpe (100,63)
# Målkoordinater
goal1_x <- 100
goal1_y <- 37
goal2_x <- 100
goal2_y <- 67

# Funktion til at beregne vinklen
beregn_vinkel <- function(player_x, player_y) {
  # Beregn længden af siderne i trekanten
  side1 <- sqrt((player_x - goal1_x)^2 + (player_y - goal1_y)^2)
  side2 <- sqrt((player_x - goal2_x)^2 + (player_y - goal2_y)^2)
  side3 <- sqrt((goal1_x - goal2_x)^2 + (goal1_y - goal2_y)^2)
  
  # Brug kosinusreglen til at beregne vinklen mellem de to linjer fra spilleren til hver målstolpe
  angle_radians <- acos((side1^2 + side2^2 - side3^2) / (2 * side1 * side2))


  # Konverter vinklen fra radianer til grader
  angle_degrees <- angle_radians * (180 / pi)
  
  # Returner den beregnede vinkel
  return(angle_degrees)
}

# Anvend funktionen på datasættet og gem vinklerne i kolonnen 'angle'
poland_shot_2122$angle <- beregn_vinkel(poland_shot_2122$location.x, poland_shot_2122$location.y)

poland_shot_2122$distance <- sqrt((100 - poland_shot_2122$location.x)^2 + ((63 + 37) / 2 - poland_shot_2122$location.y)^2)

## Udregning af vinkel til målet
poland_shot_2223$angle <- beregn_vinkel(poland_shot_2223$location.x, poland_shot_2223$location.y)

poland_shot_2223$distance <- sqrt((100 - poland_shot_2223$location.x)^2 + ((63 + 37) / 2 - poland_shot_2223$location.y)^2)

## Udregning af vinkel til målet
holland_shot_2122$angle <- beregn_vinkel(holland_shot_2122$location.x, holland_shot_2122$location.y)

holland_shot_2122$distance <- sqrt((100 - holland_shot_2122$location.x)^2 + ((63 + 37) / 2 - holland_shot_2122$location.y)^2)

## Udregning af vinkel til målet
holland_shot_2223$angle <- beregn_vinkel(holland_shot_2223$location.x, holland_shot_2223$location.y)

holland_shot_2223$distance <- sqrt((100 - holland_shot_2223$location.x)^2 + ((63 + 37) / 2 - holland_shot_2223$location.y)^2)

model2shot <- lm(shot.isGoal ~ distance + angle,data = poland_shot_2122)
summary(model2shot)
# ------------------------------------------------------------------------ Opret clustres-------------------------------------------------------------------- #
#Holland samle ligaen

#Data til clustres
Holland_combined_shot <- holland_shot_2122 %>%
  group_by(player.name) %>%
  summarize(
    avg_angle = mean(angle),
    avg_shot_xg = mean(shot.xg)
  )
# K-means clustering
set.seed(123)
kmeans_result <- kmeans(Holland_combined_shot[, -1], centers = 4)
# Tilknyt klyngeoplysninger til hver spiller
Holland_combined_shot$kmeans_cluster <- kmeans_result$cluster

# Opsummer centrale egenskaber for hver klynge
cluster_summary <- Holland_combined_shot %>%
  group_by(kmeans_cluster) %>%
  summarise(avg_angle = mean(avg_angle), avg_shot_xg2 = mean(avg_shot_xg))

# Vis opsummering
print(cluster_summary)


# Lav en klyngeanalyse for et varierende antal klynger og evaluer inerti
inertia <- numeric(10)  # Vi tester op til 10 klynger
for (i in 1:10) {
  kmeans_model <- kmeans(Holland_combined_shot[, -1], centers = i)
  inertia[i] <- kmeans_model$tot.withinss
}

# Lav et plot for at visualisere inerti versus antallet af klynger
elbow_plot <- ggplot(data.frame(K = 1:10, Inertia = inertia), aes(x = K, y = Inertia)) +
  geom_line() +
  geom_point() +
  labs(x = "Antal klynger", y = "Inertia") +
  ggtitle("Elbow plot for shot Klyngeanalyse Holland 2122") +
  theme_minimal()

print(elbow_plot)

# Lav et scatterplot af dine data, farve efter klynger
ggplot(data = Holland_combined_shot, aes(x = avg_angle, y = avg_shot_xg, color = factor(kmeans_cluster))) +
  geom_point() +
  labs(x = "Gns vinkel", y = "Gns shot Xg" , color = "Cluster") +
  ggtitle("Holland har få skud tæt på og foran målet.") +
  theme_minimal()
# ---------------------------------------------------------------------- Polen - Cluster -------------------------------------------------------------------- #

polen_combined_shot <- poland_shot_2122 %>%
  group_by(player.name) %>%
  summarize(
    avg_angle = mean(angle),
    avg_shot_xg = mean(shot.xg)
  )
# K-means clustering
set.seed(123)
kmeans_result <- kmeans(polen_combined_shot[, -1], centers = 4)
# Tilknyt klyngeoplysninger til hver spiller
polen_combined_shot$kmeans_cluster <- kmeans_result$cluster

# Opsummer centrale egenskaber for hver klynge
cluster_summary <- polen_combined_shot %>%
  group_by(kmeans_cluster) %>%
  summarise(avg_angle = mean(avg_angle), avg_shot_xg2 = mean(avg_shot_xg))

# Vis opsummering
print(cluster_summary)


# Lav en klyngeanalyse for et varierende antal klynger og evaluer inerti
inertia <- numeric(10)  # Vi tester op til 10 klynger
for (i in 1:10) {
  kmeans_model <- kmeans(polen_combined_shot[, -1], centers = i)
  inertia[i] <- kmeans_model$tot.withinss
}

# Lav et plot for at visualisere inerti versus antallet af klynger
elbow_plot <- ggplot(data.frame(K = 1:10, Inertia = inertia), aes(x = K, y = Inertia)) +
  geom_line() +
  geom_point() +
  labs(x = "Antal klynger", y = "Inertia") +
  ggtitle("Elbow plot for shot Klyngeanalyse Polen 2122") +
  theme_minimal()

print(elbow_plot)

# Lav et scatterplot af dine data, farve efter klynger
ggplot(data = polen_combined_shot, aes(x = avg_angle, y = avg_shot_xg, color = factor(kmeans_cluster))) +
  geom_point() +
  labs(x = "Gns vinkel", y = "Gns shot Xg" , color = "Cluster") +
  ggtitle("Polen har svært ved at komme helt tæt på") +
  theme_minimal()

# --------------------------------------------------------------------- Clustre beskrivelse ----------------------------------------------------------------- #
#Holland
cluster_1hol_data <- Holland_combined_shot %>%
  filter(kmeans_cluster == 1)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
hol__shot_clust1 <- ggplot(data = cluster_1hol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 1 - 91 mål ud af 1351 - Holland 2122\n24,8% i dette clustre er Forsvarsspillere\n49,6% er midtbanespillere") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster1_shot_holnames <- unique(cluster_1hol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster1hol <- holland_shot_2122 %>%
  filter(player.name %in% cluster1_shot_holnames)
# Filtrer data for forsvarsspillere i cluster 1
defenders_cluster1 <- subset(selected_players_cluster1hol, role.name == "Defender")

# Udtræk navnene på forsvarsspillere
defenders_names <- defenders_cluster1$player.name

# --------------------------------------------------------------------- Polen - Cluster 1 ------------------------------------------------------------------- #
cluster_1pol_data <- polen_combined_shot %>%
  filter(kmeans_cluster == 1)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
pol_shot_clust1 <- ggplot(data = cluster_1pol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 1 - 49 mål ud af 814 - Polen 2122\n32,6% i dette clustre er Forsvarsspillere\n9,09% er angrebsspillere og 58,4% er midtbanespillere") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster1_shot_polnames <- unique(cluster_1pol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster1pol <- poland_shot_2122 %>%
  filter(player.name %in% cluster1_shot_polnames)
# -------------------------------------------------------------------- Holland - Cluster 2 ------------------------------------------------------------------ #
#Holland
cluster_2hol_data <- Holland_combined_shot %>%
  filter(kmeans_cluster == 2)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
hol_shot_clust2 <- ggplot(data = cluster_2hol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 2 - 185 mål ud af 1082 - Holland 2122\n60,9% i dette clustre er angrebsspillere") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster2_shot_holnames <- unique(cluster_2hol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster2hol <- holland_shot_2122 %>%
  filter(player.name %in% cluster2_shot_holnames)

#Polen
cluster_2pol_data <- polen_combined_shot %>%
  filter(kmeans_cluster == 2)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
pol_shot_clust2 <- ggplot(data = cluster_2pol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 2 - 210 mål ud af 1467 - Polen 2122\nMiddle xg score med god vinkel til mål") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster2_shot_polnames <- unique(cluster_2pol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster2pol <- poland_shot_2122 %>%
  filter(player.name %in% cluster2_shot_polnames)
# -------------------------------------------------------------------- Holland - Cluster 3 ------------------------------------------------------------------ #
#Holland---Mangler at kører igennem..
cluster_3hol_data <- Holland_combined_shot %>%
  filter(kmeans_cluster == 3)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
hol_shot_clust3 <- ggplot(data = cluster_3hol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 3 - 40 mål ud af 193 - Holland 2122\n Der burde være mål?") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster3_shot_holnames <- unique(cluster_3hol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster3hol <- holland_shot_2122 %>%
  filter(player.name %in% cluster3_shot_holnames)

#Polen
cluster_3pol_data <- polen_combined_shot %>%
  filter(kmeans_cluster == 3)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
pol_shot_clust3 <- ggplot(data = cluster_3pol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 3 - 46 mål ud af 298 - Polen 2122\n Der burde være mål?") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster3_shot_polnames <- unique(cluster_3pol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster3pol <- poland_shot_2122 %>%
  filter(player.name %in% cluster3_shot_polnames)

# -------------------------------------------------------------------- Holland - Cluster 4 ------------------------------------------------------------------ #
#Holand
cluster_4hol_data <- Holland_combined_shot %>%
  filter(kmeans_cluster == 4)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
hol_shot_clust4 <- ggplot(data = cluster_4hol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 4 - 167 mål ud af 1489 - Holland 2122\n Angribernes cluster") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster4_shot_holnames <- unique(cluster_4hol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster4hol <- holland_shot_2122 %>%
  filter(player.name %in% cluster4_shot_holnames)

#Polen
cluster_4pol_data <- polen_combined_shot %>%
  filter(kmeans_cluster == 4)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
pol_shot_clust4 <- ggplot(data = cluster_4pol_data, aes(x = avg_angle, y = avg_shot_xg, color = player.name, label = player.name)) +
  geom_point() +
  geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns vinkel", y = "Gns shot XG") +
  ggtitle("Cluster 4 - 211 mål ud af 2387 - Polen 2122\nClustret med flest mål for hele den Polske liga\n 66,4 % Midtbanespillerne") +
  guides(color = "none")  # Fjern forklaringsboksen
# Antagelse: Antag at du har navnene fra cluster 1 i en vektor kaldet cluster1_names
cluster4_shot_polnames <- unique(cluster_4pol_data$player.name)

# Filtrer holland_pass_2122 for kun at inkludere de bestemte navne fra cluster 1
selected_players_cluster4pol <- poland_shot_2122 %>%
  filter(player.name %in% cluster4_shot_polnames)
