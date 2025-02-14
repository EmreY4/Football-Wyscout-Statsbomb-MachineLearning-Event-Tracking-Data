#### ----------------------------------------------------------------- xG for sæson 2024 ------------------------------------------------------------------ ####  
# Emne: Beregning af Expected Goals (xG) - Forberedelse af fodbolddata for sæson 2024  
# Dette script demonstrerer, hvordan man:  
#   1. Kombinerer forskellige datasæt for at få et samlet billede af skud- og kampdata  
#   2. Bruger Left Join til at sammenflette data og skabe en sammenhængende datastruktur  
#   3. Rensning af data ved at fjerne NA-værdier for relevante kolonner (ShotBodyPart og ShotIsGoal)  
#   4. Beregner nødvendige start- og mål-koordinater for skud (i meter) ud fra x- og y-koordinater i datasættet  
#   5. Skaber den endelige datastruktur for xG-analyse  
#   6. Visualiserer skuddata og sandsynligheden for mål baseret på faktorer som afstand, vinkel og skudzone  
#   7. Udfører beskrivende statistik på vigtige variabler som afstand og vinkel, samt kategorier som ShotGoalZone og ShotIsGoal

# Libraries  
library(dplyr)        # Data manipulation  
library(tidyr)        # Datatransformation  
library(readr)        # Læsning af CSV-filer  
library(ggplot2)      # Visualisering af data  
library(ggsoccer)     # Visualisering af fodbolddata  
library(jsonlite)     # Håndtering af JSON-data (hvis relevant)
library(logr)         # Logning af handlinger, bruges til at holde styr på eksekveringen af scriptet  
library(rpart)        # Funktioner til at bygge beslutningstræer, bruges ved analyse af skuddata  
library(rpart.plot)   # Visualisering af beslutningstræer, gør det muligt at plotte træmodeller  
library(psych)        # For general functions
library(caret)        # For training and cross validation (also calls other model libraries)
library(RColorBrewer) # Color selection for fancy tree plot
library(party)        # Alternative decision tree algorithm
library(partykit)     # Convert rpart object to BinaryTree
library(pROC)         # For ROC curves
library(ISLR)         # For the Carseat Data

# -------------------------------------------------------------- Opgave 1.6 - xG for sæson 2024 ------------------------------------------------------------- #
# Left join - Joiner "allShots22", "wys_events_22" & "allMatches"
allMatches24 <- allShots24 %>%
  left_join(wys_events_23, by = c("WyEventId", "MatchId", "PrimaryType")) # %>%
# filter(CompetitionId == 335 & SeasonId == 188039) # IKKE NØDVENDIGT MEN KUNNE VÆRE LUKSUS
# Tilføjer nu "allMatches" til ovenstående left join
allMatches24 <- allMatches24 %>%
  left_join(allMatches, by = c("MatchId"))
# Fjern NA-værdier fra de angivne kolonner
allMatches24 <- allMatches24[complete.cases(allMatches24[, c("ShotBodyPart", "ShotIsGoal")]), ]

# Koordinater - Beregning af startkoordinater og målkoordinater i meter ved at multiplicere med skaleringsfaktorerne
allMatches24$startlængde.m <- allMatches24$PossessionStartLocationX * scale.x
allMatches24$startbredde.m <- allMatches24$PossessionStartLocationY * scale.y
allMatches24$længde.m <- allMatches24$LocationX * scale.x
allMatches24$bredde.m <- allMatches24$LocationY * scale.y
# Afstand - Beregning af afstand til målet for hvert skud
allMatches24$afstand <- round(sqrt((målstolpe1.x - allMatches24$længde.m)^2 + (bredde / 2 - allMatches24$bredde.m)^2), 2)
# Anvend funktionen get_angle på hver række i allMatches22 for at beregne vinklen
allMatches24$vinkel <- round(sapply(1:nrow(allMatches24), function(i) get_angle(allMatches24$længde.m[i], allMatches24$bredde.m[i])), 1)
# Train xG model using GBM
xg_model <- train(as.factor(ShotIsGoal) ~ afstand + vinkel + ShotBodyPart, 
                  data = allMatches24,
                  method = "gbm", # Gradient Boosting Machine
                  trControl = cvcontrol,
                  verbose = FALSE)  # Avoid verbose output for brevity

# Predict xG values for all shots
allMatches24$xG <- predict(xg_model, newdata = allMatches24, type = "prob")[,"TRUE"]
allMatches24 <- cbind(allMatches24[, 1:8], xG = allMatches24$xG, allMatches24[, 9:ncol(allMatches24)])
allMatches24 <- subset(allMatches24, select = -c(47)) # Fjerner duplikante xG kolonne

# ---------------------------------------------- Opgave 1.4 - Joiner "allMatches", "allShots" & "wys_events" ------------------------------------------------ #
# Left join - Joiner "allShots22", "wys_events_22" & "allMatches"
allMatches22 <- allShots22 %>%
  left_join(wys_events_22, by = c("WyEventId", "MatchId", "PrimaryType")) # %>%
# filter(CompetitionId == 335 & SeasonId == 188039) # IKKE NØDVENDIGT MEN KUNNE VÆRE LUKSUS
# Tilføjer nu "allMatches" til ovenstående left join
allMatches22 <- allMatches22 %>%
  left_join(allMatches, by = c("MatchId"))
# Fjern NA-værdier fra de angivne kolonner
allMatches22 <- allMatches22[complete.cases(allMatches22[, c("ShotBodyPart", "ShotIsGoal")]), ]
# Konvertere forklarende variabler til faktorer
allMatches22$ShotIsGoal <- as.factor(allMatches22$ShotIsGoal)
allMatches22$ShotBodyPart <- factor(allMatches22$ShotBodyPart, levels = c("right_foot", "left_foot", "head_or_other"))

# ------------------------------------------------------------ Koordinater, Afstand & Vinkel ---------------------------------------------------------------- #
# Definér længde og bredde af spillebanen
længde <- 105
bredde <- 68
# Skaleringsfaktorer for at konvertere koordinater fra procent til meter
scale.x <- længde / 100
scale.y <- bredde / 100
# Koordinater - Beregning af startkoordinater og målkoordinater i meter ved at multiplicere med skaleringsfaktorerne
allMatches22$startlængde.m <- allMatches22$PossessionStartLocationX * scale.x
allMatches22$startbredde.m <- allMatches22$PossessionStartLocationY * scale.y
allMatches22$længde.m <- allMatches22$LocationX * scale.x
allMatches22$bredde.m <- allMatches22$LocationY * scale.y
# Koordinater - Koordinater for målstolperne (Center af mål: y = 34)
målstolpe1.x <- længde
målstolpe1.y <- (bredde / 2) - 4
målstolpe2.x <- længde
målstolpe2.y <- (bredde / 2) + 4

# Afstand - Beregning af afstand til målet for hvert skud
allMatches22$afstand <- round(sqrt((målstolpe1.x - allMatches22$længde.m)^2 + (bredde / 2 - allMatches22$bredde.m)^2), 2)
# Vinkel - Beregning af vinkel for hvert skud
get_angle <- function(shot_x, shot_y) {
  c <- sqrt((målstolpe1.x - shot_x)^2 + (målstolpe1.y - shot_y)^2)
  b <- sqrt((målstolpe2.x - shot_x)^2 + (målstolpe2.y - shot_y)^2)
  a <- sqrt((målstolpe1.x - målstolpe2.x)^2 + (målstolpe1.y - målstolpe2.y)^2)
  
  scorings_vinkel <- acos((b^2 + c^2 - a^2) / (2 * b * c)) * 180 / pi
  
  return(scorings_vinkel)
}
# Anvend funktionen get_angle på hver række i allMatches22 for at beregne vinklen
allMatches22$vinkel <- round(sapply(1:nrow(allMatches22), function(i) get_angle(allMatches22$længde.m[i], allMatches22$bredde.m[i])), 1)

# --------------------------------------------------------------- Forklarende variabler til xG -------------------------------------------------------------- #
# Konvertere forklarende variabler til faktorer
allMatches22$ShotIsGoal <- as.factor(allMatches22$ShotIsGoal)
allMatches22$ShotBodyPart <- factor(allMatches22$ShotBodyPart, levels = c("right_foot", "left_foot", "head_or_other"))

# Forklarende variabler - Træner min model udfra nedenstående variabler som jeg finder relevant ift min xG model
str(allMatches22$ShotIsGoal)
str(allMatches22$ShotBodyPart)
str(allMatches22$vinkel)
str(allMatches22$afstand)

# ----------------------------------------------------------------- Træning & Test data -------------------------------------------------------------------- #
# Angiv andelen for træningsdata (70%)
train_percentage <- 0.7
# Bestem antallet af rækker til træning baseret på andelen
num_train <- round(nrow(allMatches22) * train_percentage)
# Generer indekser til træningsdata
train_indices <- sample(1:nrow(allMatches22), num_train, replace = FALSE)
# Brug de resterende indekser til testdata
test_indices <- setdiff(1:nrow(allMatches22), train_indices)
# Opret trænings- og testdatasæt baseret på de udvalgte indekser
train_data <- allMatches22[train_indices, ]
test_data <- allMatches22[test_indices, ]

# Setting the random seed for replication
set.seed(1234)
# Setting up cross-validation
cvcontrol <- trainControl(method = "repeatedcv", number = 10,
                          allowParallel = TRUE)

# ----------------------------------------------------------------------- Signifikans ---------------------------------------------------------------------- #
# Opret en generaliseret lineær model med train_data
glm_model <- glm(formula = ShotIsGoal ~ afstand + vinkel + ShotBodyPart, family = binomial, 
                 data = train_data)
summary(glm_model)

# ------------------------------------------------------- xG - Træner og tilføjer xG til "allMatches22" ---------------------------------------------------- #
# Train xG model using GBM
xg_model <- train(as.factor(ShotIsGoal) ~ afstand + vinkel + ShotBodyPart, 
                  data = allMatches22,
                  method = "gbm", # Gradient Boosting Machine
                  trControl = cvcontrol,
                  verbose = FALSE)  # Avoid verbose output for brevity

# Predict xG values for all shots
allMatches22$xG <- predict(xg_model, newdata = allMatches22, type = "prob")[,"TRUE"]
allMatches22 <- cbind(allMatches22[, 1:8], xG = allMatches22$xG, allMatches22[, 9:ncol(allMatches22)])
allMatches22 <- subset(allMatches22, select = -c(47)) # Fjerner duplikante xG kolonne

# Opret et nyt dataframe kun med data for kampen mellem Brøndby og Vejle med MatchId 5466032 og WyEventId lig med 2109622644
Brondby_Vejle <- subset(allMatches24, MatchId == 5466032 & WyEventId == 2109622644)
# Næsten samme xg resultat
Brondby_Vejle_ens_xG <- subset(allMatches24, MatchId == 5466032 & WyEventId == 2109623031)
# Opret et nyt dataframe kun med data for MatchId 5466013
Brondby_Vejle_kamp <- subset(allMatches24, MatchId == 5466032)

# ----------------------------------------------------------------------- Beslutningstræ -------------------------------------------------------------------- #
# https://quantdev.ssri.psu.edu/sites/qdev/files/09_EnsembleMethods_2017_1127.html

# Træn en beslutningstræmodel med "ShotIsGoal" og "ShotBodyPart" som forklarende variabler
decision_tree <- rpart(ShotIsGoal ~ afstand + vinkel + ShotBodyPart, data = allMatches22, method = "class")
# Visualiser beslutningstræet
rpart.plot(decision_tree)
# Forudsig mål (1) eller ikke-mål (0) baseret på trænet beslutningstræmodel
predictions <- predict(decision_tree, allMatches22, type = "class")
# Vis de første rækker af forudsigelserne
head(predictions)

# ----------------------------------------------------------- Model 0: A Single Classification Tree --------------------------------------------------------- #
# ctree - Træningsmodel til xG forudsigelse
train.tree <- train(as.factor(ShotIsGoal) ~ afstand + vinkel + ShotBodyPart, 
                    data = train_data,
                    method = "ctree",
                    trControl = cvcontrol,
                    tuneLength = 10)
# ctree - Nøjagtighed af model gennem krydsvalidering
plot(train.tree)
# ctree - Visualisering af træningsresultater
plot(train.tree$finalModel,
     main = "Ctree - Beslutningstræ for xG baseret på afstand, vinkel, afsluttende kropsdel, skud på mål & skudretning",
     type = "extended")
# Tester beslutningstrææet - Trænings data
tree.classTrain <-  predict(train.tree, 
                            type="raw")
head(tree.classTrain)
# Confusion matrix - Beregn forvirringsmatricen for trænings data
confusionMatrix(train_data$ShotIsGoal, tree.classTrain)

# Tester beslutningstrææet - Test data
tree.classTest <- predict(train.tree,
                          newdata = test_data,
                          type = "raw")
head(tree.classTrain)
# Confusion matrix - Beregn forvirringsmatricen for test data
confusionMatrix(tree.classTest, test_data$ShotIsGoal)
# Forudsig klasserne for test data
tree.probs=predict(train.tree,
                   newdata=test_data,
                   type="prob")
head(tree.probs)
# Beregn forvirringsmatricen for test data
confusionMatrix(test_data$ShotIsGoal,tree.classTest)
# Forudsig sandsynlighederne for test data
tree.probs=predict(train.tree,
                   newdata=test_data,
                   type="prob")
head(tree.probs)
# Beregn ROC-kurven
rocCurve.tree <- roc(test_data$ShotIsGoal,tree.probs[,"TRUE"])
# Plot ROC-kurven
plot(rocCurve.tree,col=c(4))
# Beregn AUC (Area Under Curve) - Desto større desto bedre
auc(rocCurve.tree)

# ---------------------------------------------------------------- Model 1: Bagging of ctrees --------------------------------------------------------------- #
# Træning af en treebag(træposemodel) med krydsvalidering
train.bagg <- train(as.factor(ShotIsGoal) ~ afstand + vinkel + ShotBodyPart, 
                    data = train_data,
                    method = "treebag",
                    trControl = cvcontrol,
                    importance = TRUE)
# Vis resultaterne af træningen
train.bagg
# Plot betydningen af variablerne i træposemodellen
plot(varImp(train.bagg))
# Forudsig klasserne for trænings data
bagg.classTrain <-  predict(train.bagg, 
                            type = "raw")
head(bagg.classTrain)
# Beregn forvirringsmatricen for trænings data
confusionMatrix(train_data$ShotIsGoal, bagg.classTrain)
# Forudsig klasserne for test datae
bagg.classTest <-  predict(train.bagg, 
                           newdata = test_data,
                           type = "raw")
head(bagg.classTest)
# Beregn forvirringsmatricen for test data
confusionMatrix(test_data$ShotIsGoal, bagg.classTest)
# Forudsig sandsynlighederne for test data
bagg.probs <- predict(train.bagg,
                      newdata = test_data,
                      type = "prob")
head(bagg.probs)
# Beregn ROC-kurven
rocCurve.bagg <- roc(test_data$ShotIsGoal, bagg.probs[,"TRUE"])
# Plot ROC-kurven
plot(rocCurve.bagg, col = c(6))
# Beregn AUC (Area Under Curve) - Desto større desto bedre
auc(rocCurve.bagg)

# ------------------------------------------------------ Model 2: Random Forest for classification trees ---------------------------------------------------- #
# Random Forrest - Træningsmodel til xG forudsigelse
train.rf <- train(as.factor(ShotIsGoal) ~ afstand + vinkel + ShotBodyPart, 
                  data=train_data,
                  method="rf",
                  trControl=cvcontrol,
                  #tuneLength = 3,
                  importance=TRUE)
# Vis resultaterne af træningen
train.rf
# Forudsigelse af klasserne for træningsdata
rf.classTrain <-  predict(train.rf, 
                          type="raw")
head(rf.classTrain)
# Confusion matrix - Beregning af forvirringsmatricen for træningsdata
confusionMatrix(train_data$ShotIsGoal,rf.classTrain)
# Forudsigelse af klasserne for testdata
rf.classTest <-  predict(train.rf, 
                         newdata = test_data,
                         type="raw")
head(rf.classTest)
# Confusion matrix - Beregning af forvirringsmatricen for testdata
confusionMatrix(test_data$ShotIsGoal,rf.classTest)
# Forudsigelse af sandsynlighederne for testdata
rf.probs=predict(train.rf,
                 newdata=test_data,
                 type="prob")
head(rf.probs)
# Beregn ROC-kurven
rocCurve.rf <- roc(test_data$ShotIsGoal,rf.probs[,"TRUE"])
# Plot af ROC-kurven
plot(rocCurve.rf,col=c(1))
# Beregn AUC (Area Under Curve) - Desto større desto bedre
auc(rocCurve.rf)

# ------------------------------------------------------ Model 2a: CForest for Conditional Inference Tree --------------------------------------------------- #
# CForest - Træningsmodel til forudsigelse af skuds sandsynlighed for mål (xG) ved hjælp af Conditional Inference Tree
train.cf <- train(ShotIsGoal ~ afstand + vinkel + ShotBodyPart,  
                  data = train_data,
                  method = "cforest",
                  trControl = cvcontrol)  # Bemærk, at 'importance' ikke er tilgængelig her 
train.cf
# Forudsigelse af klasserne for træningsdata
cf.classTrain <- predict(train.cf, type = "raw")
head(cf.classTrain)
# Beregning af forvirringsmatricen for træningsdata
confusionMatrix(train_data$ShotIsGoal, cf.classTrain)
# Forudsigelse af klasserne for testdata
cf.classTest <- predict(train.cf, newdata = test_data, type = "raw")
head(cf.classTest)
# Beregning af forvirringsmatricen for testdata
confusionMatrix(test_data$ShotIsGoal, cf.classTest)
# Forudsigelse af sandsynlighederne for testdata
cf.probs <- predict(train.cf, newdata = test_data, type = "prob")
head(cf.probs)
# Beregn ROC-kurven
rocCurve.cf <- roc(test_data$ShotIsGoal, cf.probs[,"TRUE"])
# Plot af ROC-kurven
plot(rocCurve.cf, col = c(2))
# Beregn AUC (Area Under Curve) - Desto større, desto bedre
auc(rocCurve.cf)

# ------------------------------------------------------------ Model 3: Random Forest with Boosting --------------------------------------------------------- #
# Random Forrest med Boosting - Træningsmodel til forudsigelse af skuds sandsynlighed for mål (xG)
train.gbm <- train(as.factor(ShotIsGoal) ~ afstand + vinkel + ShotBodyPart, 
                   data = train_data,
                   method = "gbm",
                   verbose = FALSE,
                   trControl = cvcontrol)
train.gbm
# Forudsigelse af klasserne for træningsdata
gbm.classTrain <- predict(train.gbm, type = "raw")
head(gbm.classTrain)
# Beregning af forvirringsmatricen for træningsdata
confusionMatrix(train_data$ShotIsGoal, gbm.classTrain)
# Forudsigelse af klasserne for testdata
gbm.classTest <- predict(train.gbm, newdata = test_data, type = "raw")
head(gbm.classTest)
# Beregning af forvirringsmatricen for testdata
confusionMatrix(test_data$ShotIsGoal, gbm.classTest)
# Forudsigelse af sandsynlighederne for testdata
gbm.probs <- predict(train.gbm, newdata = test_data, type = "prob")
head(gbm.probs)
# Beregn ROC-kurven
rocCurve.gbm <- roc(test_data$ShotIsGoal, gbm.probs[,"TRUE"])
# Plot af ROC-kurven
plot(rocCurve.gbm, col = c(3))
# Beregn AUC (Area Under Curve) - Desto større, desto bedre
auc(rocCurve.gbm)

plot(train.gbm, num.trees = 50, main = "Variabelvigtighed i GBM-model")

# ---------------------------------------------------------------------- Model Stacking --------------------------------------------------------------------- #
# Installér pakken "stacks" for at kunne bruge modelstakning
install.packages("stacks")
library(stacks)

# Opret en liste over de modeller, du vil bruge i stakken
models <- list(train.tree, train.bagg, train.rf, train.cf, train.gbm)
# Træn en stakket model baseret på ovenstående modeller
stack_model <- stack(models, method = "glm")
# Evaluer stakket model på testdata
stack_preds <- predict(stack_model, newdata = test_data, type = "response")
# Vis resultatet af modelstakningen
confusionMatrix(test_data$ShotIsGoal, stack_preds)

plot(rocCurve.rf,add=TRUE,col=c(5)) # color cyan is rf

# --------------------------------------------------------------------- Model Comparisons ------------------------------------------------------------------- #
plot(rocCurve.tree,add=,col=c(1)) # farve blå er tree
plot(rocCurve.bagg,add=TRUE,col=c(2)) # color red is bagg
plot(rocCurve.cf,add=TRUE,col=c(3)) # color green is cforest
plot(rocCurve.gbm,add=TRUE,col=c(4)) # color blue is gbm

# Beregn og tilføj AUC-tekst til hvert plot
text(0.9, 0.2, paste("Tree AUC =", round(auc(rocCurve.tree) * 100, 1), "%"), col = 1, adj = -1.05)
text(0.9, 0.15, paste("Bagging AUC =", round(auc(rocCurve.bagg) * 100, 1), "%"), col = "red", adj = -0.89)
text(0.9, 0.1, paste("CF AUC =", round(auc(rocCurve.cf) * 100, 1), "%"), col = "green4", adj = -1.148)
text(0.9, 0.05, paste("GBM AUC =", round(auc(rocCurve.gbm) * 100, 1), "%"), col = "blue", adj = -1.025)

# -------------------------------------------------------------------------- Plots ------------------------------------------------------------------------- #
library(patchwork)
# ShotBodyPart - Procent % Heltal
ShotBodyPart1 <- ggplot(allMatches22, aes(x = ShotBodyPart, fill = ShotIsGoal)) +
  geom_bar(position = "dodge") +
  labs(title = "Afsluttende kropsdel - Afslutninger med højre ben har større sandsynlighed for at gå i mål end andre kropsdele ",
       subtitle = "(Antal skud) - Procenvise fordeling (%)",
       x = "Afsluttende Kropsdel",
       y = "Antal skud",
       fill = "Udfald") +
  geom_text(stat = 'count', aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  geom_text(stat = 'count', aes(label = paste0("(", after_stat(count), ")")), 
            position = position_dodge(width = 0.9), vjust = -2.5, hjust = 0.55, size = 3) + # Justeret vjust her
  theme_minimal() +
  theme(axis.line = element_line(color = "gray50", linewidth = 0.5),
        plot.subtitle = element_text(size = 8)) +  # Ændrer tekststørrelsen på subtitlen
  scale_x_discrete(labels = c("head_or_other" = "Hoved eller andet", 
                              "left_foot" = "Venstre fod", 
                              "right_foot" = "Højre fod")) +
  scale_fill_discrete(labels = c("FALSE" = "Ikke mål", "TRUE" = "Mål")) +
  ylim(0, 2400)
# ShotBodyPart - Procentvise fordeling ift. egen kropsdel
ShotBodyPart2 <- ggplot(allMatches22 %>%
                          group_by(ShotBodyPart, ShotIsGoal) %>%
                          summarise(count = n()) %>%
                          mutate(percentage = count / sum(count) * 100), 
                        aes(x = ShotBodyPart, y = percentage, fill = ShotIsGoal)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Afsluttende kropsdel - Resultaterne belyser at afslutninger med venstre ben ikke er ligeså effektivt som de resterende kropsdele",
       subtitle = "(Antal skud) - Procenvise fordeling (%)",
       x = "Afsluttende Kropsdel",
       y = "Procentsats",
       fill = "Udfald") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  geom_text(aes(label = paste0("(", count, ")")), 
            position = position_dodge(width = 0.9), vjust = -2.5, hjust = 0.55, size = 3) + # Justeret vjust her
  theme_minimal() +
  theme(axis.line = element_line(color = "gray50", linewidth = 0.5),
        plot.subtitle = element_text(size = 8)) +
  scale_x_discrete(labels = c("head_or_other" = "Hoved eller andet", 
                              "left_foot" = "Venstre fod", 
                              "right_foot" = "Højre fod")) +
  scale_fill_discrete(labels = c("FALSE" = "Ikke mål", "TRUE" = "Mål")) +
  ylim(0, 100)  # Justeret y-aksens grænser for procentsatser

# Kombiner plots ved hjælp af patchwork
ShotBodyPart <- ShotBodyPart1 + ShotBodyPart2 + plot_layout(ncol = 1)
# Vis det kombinerede plot
ShotBodyPart

#
# ShotGoalZone - Procent % Heltal
ggplot(allMatches22, aes(x = shotgoalzone, fill = ShotIsGoal)) +
  geom_bar(position = "dodge") +
  labs(title = "Skudzone - Selvom et skud har retning mod mål er det ikke ensbetydning med at det bliver til et mål.
Dette belyser at flere parametre har betydning for målsandsynligheden",
       subtitle = "(Antal skud) - Procenvise fordeling (%)",
       x = "Resultat af skud",
       y = "Antal skud",
       fill = "Udfald") +
  geom_text(stat = 'count', aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  geom_text(stat = 'count', aes(label = paste0("(", after_stat(count), ")")), 
            position = position_dodge(width = 0.9), vjust = -2.5, hjust = 0.58, size = 3) +
  theme_minimal() +
  theme(axis.line = element_line(color = "gray50", linewidth = 0.5),
        plot.subtitle = element_text(size = 8)) +  # Ændrer tekststørrelsen på subtitlen
  scale_fill_discrete(labels = c("FALSE" = "Ikke mål", "TRUE" = "Mål")) +
  ylim(0, 2000)

#
# ShotonTarget - Procent % Heltal
ggplot(allMatches22, aes(x = ShotonTarget, fill = ShotIsGoal)) +
  geom_bar(position = "dodge") +
  labs(title = "Målramme - Udfald af skud på mål og ikke på mål",
       subtitle = "(Antal skud) - Procenvise fordeling (%)",
       x = "Retning",
       y = "Antal observationer",
       fill = "Udfald") +
  geom_text(stat = 'count', aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  geom_text(stat = 'count', aes(label = paste0("(", after_stat(count), ")")), 
            position = position_dodge(width = 0.9), vjust = -2.5, hjust = 0.58, size = 3) +
  theme_minimal() +
  theme(axis.line = element_line(color = "gray50", linewidth = 0.5),
        plot.subtitle = element_text(size = 8)) +  # Ændrer tekststørrelsen på subtitlen
  scale_x_discrete(labels = c("FALSE" = "Forbi Mål", "TRUE" = "På Mål")) + # Ændret labels her
  scale_fill_discrete(labels = c("FALSE" = "Ikke mål", "TRUE" = "Mål")) +
  ylim(0, 3500)

#
# Afstand - Procent % Heltal # TILFØJ EN GNS AFSTANDSLINJE
ggplot(allMatches22, aes(x = cut(afstand, breaks = c(0, 5, 10, 15, 20, 25, 30, 40, Inf), 
                                 labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-40", "40+")), fill = ShotIsGoal)) +
  geom_bar(color = "black", position = "dodge") +
  labs(title = "Afstand - Størstedelen af afslutninger inden for afstanden 0-5 meter resultere i et mål. Sandsynligheden for at score er større, jo tættere man er på målet", 
       subtitle = "(Antal skud) - Procenvise fordeling (%)",
       x = "Afstand (meter)",
       y = "Antal skud",
       fill = "Udfald") +
  geom_text(stat = 'count', aes(label = paste0("(", after_stat(count), ")")), position = position_dodge(width = 0.9), vjust = -2.5, size = 3) +  # Tilføj count til hver søjle
  geom_text(stat = 'count', aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  theme_minimal() +  # Fjern baggrunden og akselinjer
  theme(axis.line = element_line(color = "gray50", linewidth = 0.5),
        plot.subtitle = element_text(size = 8)) +  # Ændrer tekststørrelsen på subtitlen
  scale_fill_discrete(labels = c("FALSE" = "Ikke mål", "TRUE" = "Mål")) +  # Juster labels for fill-æstetikken
  scale_x_discrete(expand = c(0.1, 0)) +  # Udvid x-aksens skala for at centrere søjlerne
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)), limits = c(0, 1000))  # Juster y-aksens grænser

#
# Vinkel - Procent % Heltal
ggplot(mutate(allMatches22, vinkel_group = cut(vinkel, 
                                               breaks = c(-Inf, -1.2, -0.6, -0.3, 0.3, 0.6, 1.2, Inf), 
                                               labels = c("Skarp venstre", "Venstre", "Centreret venstre","Centreret", "Centreret højre", "Højre", "Skarp højre"))), 
       aes(x = vinkel_group, fill = ShotIsGoal)) +
  geom_bar(position = "dodge") +
  labs(title = "Vinkel - Sandsynligheden for mål er højest ved en centreret afslutningsvinkel", 
       subtitle = "(Antal skud) - Procenvise fordeling (%)",
       x = "Vinklen for afslutning",
       y = "Antal skud",
       fill = "Udfald") +
  geom_text(stat = 'count', aes(label = paste0("(", after_stat(count), ")")), position = position_dodge(width = 0.9), vjust = -2.5, size = 3) +  # Tilføj count til hver søjle
  geom_text(stat = 'count', aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) * 100, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.line = element_line(color = "gray50", linewidth = 0.5),
        plot.subtitle = element_text(size = 8)) +  # Ændrer tekststørrelsen på subtitlen
  scale_fill_discrete(labels = c("FALSE" = "Ikke mål", "TRUE" = "Mål")) +
  scale_x_discrete(labels = c("Skarp venstre", "Venstre", "Centreret venstre","Centreret", "Centreret højre", "Højre", "Skarp højre")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)), limits = c(0, 1400)) # Juster y-aksens grænser

# ------------------------------------------------------------------- Beskrivende statistik ---------------------------------------------------------------- #
# Afstand
summary(allMatches22) # summary()
mean(allMatches22$afstand) # mean()
median(allMatches22$afstand) # median()
sd(allMatches22$afstand) # sd()
var(allMatches22$afstand) # var()
min(allMatches22$afstand) # min()
max(allMatches22$afstand) # max()
quantile(allMatches22$afstand) # quantile()
table(allMatches22$afstand) # table()
prop.table(table(allMatches22$afstand)) # prop.table()
hist(allMatches22$afstand) # hist()
boxplot(allMatches22$afstand) # boxplot()
# Vinkel
mean(allMatches22$vinkel) # mean()
median(allMatches22$vinkel) # median()
sd(allMatches22$vinkel) # sd()
var(allMatches22$vinkel) # var()
min(allMatches22$vinkel) # min()
max(allMatches22$vinkel) # max()
quantile(allMatches22$vinkel) # quantile()
table(allMatches22$vinkel) # table()
prop.table(table(allMatches22$vinkel)) # prop.table()
hist(allMatches22$vinkel) # hist()
boxplot(allMatches22$vinkel) # boxplot()

# ShotBodyPart
table(allMatches22$ShotBodyPart) # table()
# ShotGoalZone
table(allMatches22$shotgoalzone) # table()
# ShotonTarget
table(allMatches22$ShotonTarget) # table()

# Beskrivende statistik for ShotBodyPart
summary(allMatches22$ShotBodyPart)
