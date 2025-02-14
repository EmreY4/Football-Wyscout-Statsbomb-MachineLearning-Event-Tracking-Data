#### ------------------------------------------------ Dataindlæsning og Transformation - "Bilag - Data" Folder --------------------------------------------- ####  
# Emne: Indlæsning og Transformation af Data - Forberedelse af fodbolddata  
# Dette script demonstrerer, hvordan man:  
#   1. Indlæser flere datasæt i forskellige formater (RDS, Excel, CSV, JSON)  
#   2. Bruger nødvendige biblioteker til datatransformation og manipulation (dplyr, tidyr, ggplot2)  
#   3. Flader datastrukturer ud ved hjælp af JSON-funktioner for at sikre korrekt format til videre analyse  
#   4. Opretter dataframe-strukturer og håndterer indeksering af dataene

# Libraries  
library(dplyr)      # Data manipulation  
library(tidyr)      # Datatransformation  
library(readxl)     # Læsning af Excel-filer  
library(readr)      # Læsning af CSV-filer  
library(logr)       # Logning af handlinger  
library(mongolite)  # MongoDB funktionalitet  
library(jsonlite)   # Håndtering af JSON-data  
library(ggplot2)    # Visualisering  
library(ggsoccer)   # Visualisering af fodbolddata

# ---------------------------------------------------------------------- Indlæser data ---------------------------------------------------------------------- #
rm(allMatches, allPasses22, allPasses24, allShots22, allShots24, wys_events_22, wys_events_23)
allMatches <- readRDS("Bilag - Data/bilag.allMatches.rds")
allPasses22 <- readRDS("Bilag - Data/bilag.allPasses22.rds")
allPasses24 <- readRDS("Bilag - Data/bilag.allPasses24.rds")
allShots22 <- readRDS("Bilag - Data/bilag.allShots22.rds")
allShots24 <- readRDS("Bilag - Data/bilag.allShots24.rds")
wys_events_22 <- readRDS("Bilag - Data/bilag.wys_events_22.rds")
wys_events_23 <- readRDS("Bilag - Data/bilag.wys_events_23.rds")
events_aerialDuel <- read_excel("Bilag - Data/bilag.events_aerialDuel_cph.xlsx")
events_carry <- read_excel("Bilag - Data/bilag.events_carry_cph.xlsx")
events_groundDuel <- read_excel("Bilag - Data/bilag.events_groundDuel_cph.xlsx")
events_infraction <- read_excel("Bilag - Data/bilag.events_infraction_cph.xlsx")
events_main_info <- read_excel("Bilag - Data/bilag.events_main_info_cph.xlsx")
events_passes <- read_excel("Bilag - Data/bilag.events_passes_cph.xlsx")
events_possession_cph <- read_excel("Bilag - Data/bilag.events_possession_cph.xlsx")
events_possessionTypes <- read_excel("Bilag - Data/bilag.events_possessionTypes_cph.xlsx")
events_shots <- read_excel("Bilag - Data/bilag.events_shots_cph.xlsx")
events_typeSecondary <- read_excel("Bilag - Data/bilag.events_typeSecondary_cph.xlsx")
formations <- read_excel("Bilag - Data/bilag.formations_df_cph.xlsx")
vbob <- read_csv("Bilag - Data/bilag.vbob.csv")

# Flader datastrukturen ud - Unødvendigt
allMatches <- fromJSON(toJSON(allMatches), flatten = TRUE)
allPasses22 <- fromJSON(toJSON(allPasses22), flatten = TRUE)
allPasses24 <- fromJSON(toJSON(allPasses24), flatten = TRUE)
allShots22 <- fromJSON(toJSON(allShots24), flatten = TRUE)
allShots24 <- fromJSON(toJSON(allShots24), flatten = TRUE)
wys_events_22 <- fromJSON(toJSON(wys_events_22), flatten = TRUE)
wys_events_23 <- fromJSON(toJSON(wys_events_23), flatten = TRUE)

# Tracking Data - Indlæs JSON-filen direkte fra miljøet
vbob_meta <- fromJSON("Bilag - Data/bilag.vbob-meta.json")
# Konverter listen til en dataframe
vbob_meta <- as.data.frame(vbob_meta)
# Check de første par rækker for at se dataens struktur
head(vbob_meta)
# Check strukturen af din dataframe
str(vbob_meta)
# Sætter 'venueId' som rækkeindeks
rownames(vbob_meta) <- vbob_meta$venueId
vbob_meta$venueId <- NULL  # Fjerner 'venueId' fra dataframen
# Kontrollerer dataframe-strukturen igen
str(vbob_meta)
# Opretter en numerisk sekvens som rækkeindekser
rownames(vbob_meta) <- NULL
# Kontrollerer dataframe-strukturen igen
str(vbob_meta)


# Tracking Data
vbob_data <- fromJSON("Bilag - Data/bilag.vbob-data.json")