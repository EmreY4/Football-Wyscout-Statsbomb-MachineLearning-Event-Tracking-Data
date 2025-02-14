#### ---------------------------------------------- Shiny app til visualisering af spillerdata - Holland & Polen ----------------------------------------- ####  
# Emne: Visualisering af spillerdata baseret på afslutningsteknik og skudpræcision i den hollandske og polske liga for sæsonerne 21/22 & 22/23  
# Dette Shiny-script fokuserer på at skabe en interaktiv platform til at analysere spillerdata fra den hollandske og polske liga, med fokus på afslutningsteknik og skudpræcision i sæsonerne 21/22 og 22/23.  
# Gennem applikationen kan brugeren interaktivt udforske klynger af spillere baseret på deres skudpræstationer og afslutningsteknik, med visualiseringer der fremhæver de vigtigste mønstre og forskelle mellem spillere.  
# Denne Shiny app giver brugeren mulighed for at:  
#   1. Interaktivt vælge liga og sæson for at filtrere spillerdata  
#   2. Visualisere klyngedannelse af spillere baseret på afslutningsteknik og skudpræcision via scatter plots  
#   3. Se detaljerede beskrivelser af klyngernes karakteristika, herunder gennemsnitsvinkel og expected goals (xG)  
#   4. Evaluere klyngernes kvalitet og sammenligne spillerens præstationer på tværs af ligaerne  
#   5. Filtrere spillere baseret på position (angriber, midtbanespiller, forsvarer) og analysere deres skudvaner  
#   6. Visualisere spillerdata interaktivt for at få en bedre forståelse af spillerens rolle og præstationer på banen  

# Libraries
library(shiny)          # Web framework til opbygning af interaktive applikationer i R
library(stringr)        # Funktioner til strengmanipulation og regulære udtryk
library(ggplot2)        # Visualisering af data baseret på Grammar of Graphics
library(ggimage)        # Tilføjelse af billeder i ggplot2 visualiseringer
library(DT)             # Skaber interaktive tabeller baseret på DataTables biblioteket
library(skimr)          # Hurtig opsummering af datasæt med statistikker
library(dplyr)          # Data manipulation (filtrering, udvælgelse, mutation, summarization)
library(cluster)        # Klyngedannelsesalgoritmer som k-means og hierarkisk clustering
library(jsonlite)       # Håndtering og parsing af JSON-data, ofte brugt til API'er
library(ggrepel)        # Tilføjelse af tekstlabels i ggplot2 plots, justerer automatisk placering

# --------------------------------------------------------------------- Opgave 4 - Shiny -------------------------------------------------------------------- #
#Lav en shiny app, med afleveringclustre, skud og kampe.
#Forberede data/samle data
# Kombiner skud dataframes
shots_combined <- rbind(holland_shot_2122, holland_shot_2223, poland_shot_2122,poland_shot_2223)


# Kombiner afleverings dataframes
passes_combined <- rbind(holland_pass_2122, holland_pass_2223, poland_pass_2122, poland_pass_2223)

# Filtrering af datasæt
filtered_data <- passes_combined[, c("player.position","player.name", "competitionId", "pass.accurate", "pass.length", "pass.angle", "location.x", "location.y", "team.name", "seasonId")]

# Udskriv de første få rækker i det filtrerede datasæt for at kontrollere
head(filtered_data)

#Fjerner evt. NA værdier i datasæt
passes_combined <- na.omit(passes_combined)


# Funktion til at beregne gennemsnitlig location.x og pass.length for hver spiller
calculate_player_stats <- function(df) {
  player_stats <- df %>%
    group_by(player.name) %>%
    summarise(avg_location_x = mean(location.x, na.rm = TRUE),
              avg_pass_length = mean(pass.length, na.rm = TRUE))
  return(player_stats)
}

# Anvend funktionen på dit samlede datasæt
player_avg_stats <- calculate_player_stats(filtered_data)

# Tilslut de beregnede gennemsnitlige statistikker til filtered_data
filtered_data_with_stats <- left_join(filtered_data, player_avg_stats, by = "player.name")

# Vis de første rækker i det resulterende datasæt
head(filtered_data_with_stats)

#opsætning af app

#UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
       body {
         background-color: lightgray; /* Lysegrå baggrundsfarve */
       }
       .selectize-dropdown-content {
         color: black; /* Sort skriftfarve i dropdown menuerne, ændret fra hvid */
       }
       .selectize-input {
         color: black; /* Sikrer at input-feltet også har sort tekst */
       }
       .nav-tabs > li > a {
         color: black; /* Farve på tab tekst */
       }
       .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
         background-color: gray; /* Baggrundsfarve for aktiv tab */
         color: white; /* Tekstfarve for aktiv tab */
       }
       ")
    )
    
  ),
  div(style = "background-color: lightgray;",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Afleveringer",
          fluidRow(
            column(3,
                   selectInput(
                     "league_choice_pass",
                     "Vælg Liga",
                     choices = c("Alle ligaer", "Eredivisie (Hollandske liga)" = 635, "Ekstraklasa (Polske liga)" = 692),
                     selected = NULL
                   ),
                   selectInput(
                     "season_choice_pass",
                     "Vælg Sæson",
                     choices = c("Alle sæsoner", "Holland 2021/2022" = 187502, "Holland 2022/2023" = 188125, "Polen 2021/2022" = 186215, "Polen 2022/2023" = 188088),
                     selected = NULL
                   ),
                   actionButton("submit_button", "Opdater Data"),
                   selectInput(
                     "team_choice_pass",
                     "Vælg Hold",
                     choices = NULL,  # Opdateres dynamisk baseret på valgte liga og sæson
                     selected = NULL
                   ),
                   selectInput(
                     "role_choice_pass",
                     "Vælg Spiller Position",
                     choices = NULL,  # Opdateres dynamisk
                     selected = "Alle positioner"
                   ),
                   selectInput(
                     "player_choice_pass",
                     "Vælg Spiller",
                     choices = NULL,  # Opdateres dynamisk
                     selected = NULL
                   )
            ),
            column(9,
                   plotOutput("cluster_plot_pass")
            )
          )
        )
      )
  )
)

# -------------------------------------------------------- Hjælpe funktioner til at køre serverdel ---------------------------------------------------------- #
#filtrering af liga og sæson
filterData <- function(league, season) {
  # Antager vi har en dataset 'filtered_data_with_stats' med alle data
  filtered_data_with_stats[filtered_data_with_stats$competitionId == as.numeric(league) & filtered_data_with_stats$seasonId == as.numeric(season), ]
}

#Funktion til at finde flest med valgt position
getDominantCluster <- function(data, position) {
  # Grupperer data efter cluster og tæller antallet af spillere i hver position pr. cluster
  position_counts <- data %>%
    filter(player.position == position) %>%
    group_by(cluster) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Finder clusteret med det højeste antal spillere for den valgte position
  dominant_cluster <- position_counts %>%
    arrange(desc(count)) %>%
    slice(1) %>%
    pull(cluster)
  
  return(dominant_cluster)
}

plotWithDetails <- function(data, title_suffix) {
  if (is.null(data) || nrow(data) == 0) {
    print("Ingen data at vise.")
    return()
  }
  
  print(paste("Plotting data with", nrow(data), "datapoints."))
  plot <- ggplot(data, aes(x = avg_location_x, y = avg_pass_length, color = as.factor(cluster), label = player.name)) +
    geom_point() +
    geom_text_repel(aes(label = player.name), size = 3, max.overlaps = Inf) +
    labs(x = "Gns Location on Field", y = "Gns Pass Length", title = paste("Cluster Analysis -", title_suffix)) +
    theme_minimal() +
    guides(color = "none")
  
  print(plot)  # Debug print af det genererede plot
  return(plot)
}

# ------------------------------------------------------------------------ Server del ---------------------------------------------------------------------- #
# Server-side logik
server <- function(input, output, session) {
  reactive_cluster_data <- reactiveVal()
  
  observeEvent(input$submit_button, {
    req(input$league_choice_pass, input$season_choice_pass)
    
    filtered_data <- filterData(input$league_choice_pass, input$season_choice_pass)
    
    if (nrow(filtered_data) > 1) {
      set.seed(123)
      kmeans_result <- kmeans(filtered_data[, c("avg_location_x", "avg_pass_length")], centers = 4, nstart = 25)
      filtered_data$cluster <- kmeans_result$cluster
      reactive_cluster_data(filtered_data)
    } else {
      print("Ikke nok data til at danne clusters.")
      reactive_cluster_data(NULL)
    }
    
    # Opdater holdvalgene baseret på den filtrerede data
    unique_teams <- unique(filtered_data$team.name)
    updateSelectInput(session, "team_choice_pass", 
                      choices = c("Alle hold", setNames(unique_teams, unique_teams)),
                      selected = "Alle hold")
  })
  
  observe({
    req(input$team_choice_pass, reactive_cluster_data())
    data_to_plot <- reactive_cluster_data()
    if (input$team_choice_pass != "Alle hold") {
      data_to_plot <- data_to_plot[data_to_plot$team.name == input$team_choice_pass, ]
    }
    
    # Opdater positionvalgene baseret på filtreret data
    unique_positions <- unique(data_to_plot$player.position)
    updateSelectInput(session, "role_choice_pass", 
                      choices = c("Alle positioner", setNames(unique_positions, unique_positions)),
                      selected = "Alle positioner")
    
    # Opdater spillervalgene baseret på filtreret data
    unique_players <- unique(data_to_plot$player.name)
    updateSelectInput(session, "player_choice_pass", 
                      choices = c("Alle spillere", setNames(unique_players, unique_players)),
                      selected = "Alle spillere")
  })
  
  output$cluster_plot_pass <- renderPlot({
    req(reactive_cluster_data())
    data_to_plot <- reactive_cluster_data()
    
    if (nrow(data_to_plot) > 0) {
      if (!is.null(input$team_choice_pass) && input$team_choice_pass != "Alle hold") {
        data_to_plot <- data_to_plot[data_to_plot$team.name == input$team_choice_pass, ]
      }
      if (!is.null(input$role_choice_pass) && input$role_choice_pass != "Alle positioner") {
        data_to_plot <- data_to_plot[data_to_plot$player.position == input$role_choice_pass, ]
      }
      if (!is.null(input$player_choice_pass) && input$player_choice_pass != "Alle spillere") {
        data_to_plot <- data_to_plot[data_to_plot$player.name == input$player_choice_pass, ]
      }
      return(ggplot(data_to_plot, aes(x = avg_location_x, y = avg_pass_length, color = as.factor(cluster))) +
               geom_point() +
               labs(title = "Cluster Analysis"))
    } else {
      print("Ingen data tilgængelige for visning.")
    }
  })
}

# Kør Shiny-appen
shinyApp(ui=ui, server = server)

# ----------------------------------------------------------- Opgave 4.2 - Clustre for skud - Shiny -------------------------------------------------------- #
# Kombiner skud dataframes
shots_combined <- rbind(holland_shot_2122, holland_shot_2223, poland_shot_2122,poland_shot_2223)

# Filtrering af datasæt
filtered_data_shot <- shots_combined[, c("player.position","player.name", "competitionId", "shot.isGoal", "distance", "angle", "shot.xg", "shot.goalZone", "team.name", "seasonId")]

# Udskriv de første få rækker i det filtrerede datasæt for at kontrollere
head(filtered_data)

#Fjerner evt. NA værdier i datasæt
shots_combined <- na.omit(shots_combined)


# Funktion til at beregne gennemsnitlig location.x og pass.length for hver spiller
calculate_player_stats_shot <- function(df) {
  player_stats <- df %>%
    group_by(player.name) %>%
    summarise(avg_angle = mean(angle, na.rm = TRUE),
              avg_shotXG = mean(shot.xg, na.rm = TRUE))
  return(player_stats)
}

# Anvend funktionen på dit samlede datasæt
player_avg_stats_shot <- calculate_player_stats_shot(filtered_data_shot)

# Tilslut de beregnede gennemsnitlige statistikker til filtered_data
filtered_data_shot <- left_join(filtered_data_shot, player_avg_stats_shot, by = "player.name")

# Vis de første rækker i det resulterende datasæt
head(filtered_data_m._stats_shot)

#opsætning af app

#UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
       body {
         background-color: lightgray; /* Lysegrå baggrundsfarve */
       }
       .selectize-dropdown-content {
         color: black; /* Sort skriftfarve i dropdown menuerne, ændret fra hvid */
       }
       .selectize-input {
         color: black; /* Sikrer at input-feltet også har sort tekst */
       }
       .nav-tabs > li > a {
         color: black; /* Farve på tab tekst */
       }
       .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
         background-color: gray; /* Baggrundsfarve for aktiv tab */
         color: white; /* Tekstfarve for aktiv tab */
       }
       ")
    )
    
  ),
  div(style = "background-color: lightgray;",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Afleveringer",
          fluidRow(
            column(3,
                   selectInput(
                     "league_choice_shot",
                     "Vælg Liga",
                     choices = c("Alle ligaer", "Eredivisie (Hollandske liga)" = 635, "Ekstraklasa (Polske liga)" = 692),
                     selected = NULL
                   ),
                   selectInput(
                     "season_choice_shot",
                     "Vælg Sæson",
                     choices = c("Alle sæsoner", "Holland 2021/2022" = 187502, "Holland 2022/2023" = 188125, "Polen 2021/2022" = 186215, "Polen 2022/2023" = 188088),
                     selected = NULL
                   ),
                   actionButton("submit_button", "Opdater Data"),
                   selectInput(
                     "team_choice_shot",
                     "Vælg Hold",
                     choices = NULL,  # Opdateres dynamisk baseret på valgte liga og sæson
                     selected = NULL
                   ),
                   selectInput(
                     "role_choice_shot",
                     "Vælg Spiller Position",
                     choices = NULL,  # Opdateres dynamisk
                     selected = "Alle positioner"
                   ),
                   selectInput(
                     "player_choice_shot",
                     "Vælg Spiller",
                     choices = NULL,  # Opdateres dynamisk
                     selected = NULL
                   )
            ),
            column(9,
                   plotOutput("cluster_plot_shot")
            )
          )
        )
      )
  )
)

# --------------------------------------------------------- Hjælpe funktioner til at køre serverdel --------------------------------------------------------- #
#filtrering af liga og sæson
filterData_shot <- function(league, season) {
  # Antager vi har en dataset 'filtered_data_with_stats' med alle data
  filtered_data_m._stats_shot[filtered_data_m._stats_shot$competitionId == as.numeric(league) & filtered_data_m._stats_shot$seasonId == as.numeric(season), ]
}

#Funktion til at finde flest med valgt position
getDominantCluster_shot <- function(data, position) {
  # Grupperer data efter cluster og tæller antallet af spillere i hver position pr. cluster
  position_counts <- data %>%
    filter(player.position == position) %>%
    group_by(cluster) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Finder clusteret med det højeste antal spillere for den valgte position
  dominant_cluster <- position_counts %>%
    arrange(desc(count)) %>%
    slice(1) %>%
    pull(cluster)
  
  return(dominant_cluster)
}

plotWithDetails_shot <- function(data, title_suffix, cluster_labels) {
  if (is.null(data) || nrow(data) == 0) {
    print("Ingen data at vise.")
    return(NULL)
  }
  
  plot <- ggplot(data, aes(x = avg_angle, y = avg_shotXG, color = as.factor(cluster))) +
    geom_point() +
    scale_color_manual(values = c("red", "blue", "green", "purple"),
                       labels = cluster_labels,
                       name = "Cluster Type") +
    labs(x = "Gns Vinkel", y = "Gns Shot xG", title = paste("Cluster Analysis -", title_suffix)) +
    theme_minimal() +
    guides(color = guide_legend(title = "Cluster Description"))
  
  return(plot)
}

#Funktion til at få cluster navne:
get_cluster_descriptions <- function(centers) {
  cluster_descriptions <- character(nrow(centers))
  
  for (i in 1:nrow(centers)) {
    if (centers[i, "avg_shotXG"] <= 0.07 && centers[i, "avg_angle"] <= 64) {
      cluster_descriptions[i] <- "Chance tagerne"
    } else if (centers[i, "avg_shotXG"] <= 0.125 && centers[i, "avg_shotXG"] >= 0.08 && centers[i, "avg_angle"] >= 65 && centers[i, "avg_angle"] <= 80) {
      cluster_descriptions[i] <- "Dem med lidt bedre chancer for mål"
    } else if (centers[i, "avg_shotXG"] <= 0.17 && centers[i, "avg_shotXG"] >= 0.13 && centers[i, "avg_angle"] >= 85 && centers[i, "avg_angle"] <= 100) {
      cluster_descriptions[i] <- "Der er en rigtig god chance for mål"
    } else if (centers[i, "avg_shotXG"] >= 0.18 && centers[i, "avg_angle"] >= 101) {
      cluster_descriptions[i] <- "Der burde være mål?"
    }
  }
  
  return(cluster_descriptions)
}

# ------------------------------------------------------------------------ Server del------------------------------------------------------------------------ #
# Server-side logic
server <- function(input, output, session) {
  reactive_cluster_data_shot <- reactiveVal()
  reactive_cluster_labels <- reactiveVal()  # Store cluster labels reactively
  
  observeEvent(input$submit_button, {
    req(input$league_choice_shot, input$season_choice_shot)
    
    filtered_data_shot <- filterData_shot(input$league_choice_shot, input$season_choice_shot)
    
    if (nrow(filtered_data_shot) > 1) {
      set.seed(123)
      kmeans_result <- kmeans(filtered_data_shot[, c("avg_angle", "avg_shotXG")], centers = 4, nstart = 25)
      filtered_data_shot$cluster <- kmeans_result$cluster
      reactive_cluster_data_shot(filtered_data_shot)
      
      # Generate cluster descriptions based on the centers
      centers <- kmeans_result$centers
      cluster_labels <- get_cluster_descriptions(centers)
      reactive_cluster_labels(cluster_labels)  # Store the labels
      
      # Update team choices based on the filtered data
      unique_teams <- unique(filtered_data_shot$team.name)
      updateSelectInput(session, "team_choice_shot", 
                        choices = c("Alle hold", setNames(unique_teams, unique_teams)),
                        selected = "Alle hold")
    } else {
      showNotification("Ikke nok data til at danne clusters.", type = "error")
      reactive_cluster_data_shot(NULL)
    }
  })
  
  observe({
    req(input$team_choice_shot, reactive_cluster_data_shot())
    data_to_plot_shot <- reactive_cluster_data_shot()
    if (input$team_choice_shot != "Alle hold") {
      data_to_plot_shot <- data_to_plot_shot[data_to_plot_shot$team.name == input$team_choice_shot, ]
    }
    
    # Update position choices based on the filtered data
    unique_positions <- unique(data_to_plot_shot$player.position)
    updateSelectInput(session, "role_choice_shot", 
                      choices = c("Alle positioner", setNames(unique_positions, unique_positions)),
                      selected = "Alle positioner")
    
    # Update player choices based on the filtered data
    unique_players <- unique(data_to_plot_shot$player.name)
    updateSelectInput(session, "player_choice_shot", 
                      choices = c("Alle spillere", setNames(unique_players, unique_players)),
                      selected = "Alle spillere")
  })
  
  output$cluster_plot_shot <- renderPlot({
    req(reactive_cluster_data_shot(), reactive_cluster_labels())
    data_to_plot_shot <- reactive_cluster_data_shot()
    cluster_labels <- reactive_cluster_labels()  # Get the stored cluster labels
    
    if (is.null(data_to_plot_shot) || nrow(data_to_plot_shot) == 0) {
      print("Ingen data tilgængelige for visning.")
      return(NULL)
    }
    
    # Construct a title based on the user's choice
    title_suffix <- ifelse(nzchar(input$league_choice_shot) && nzchar(input$season_choice_shot),
                           paste(input$league_choice_shot, input$season_choice_shot, sep = " - "),
                           "Data ikke specificeret korrekt")
    
    return(plotWithDetails_shot(data_to_plot_shot, title_suffix, cluster_labels))
  })
}

# Kør Shiny-appen
shinyApp(ui=ui, server = server)

# ----------------------------------------------------------------------- Opgave 4.3 ------------------------------------------------------------------------ #
# Kombiner skud dataframes
shots_combined <- rbind(holland_shot_2122, holland_shot_2223, poland_shot_2122,poland_shot_2223)

# Filtrering af datasæt
filtered_data_match_shot <- shots_combined[, c("matchId", "competitionId", "shot.isGoal", "distance", "angle", "shot.xg", "shot.goalZone", "team.name", "seasonId", "label")]

# Funktion til at beregne gennemsnitlig location.x og pass.length for hver spiller
# Beregn gennemsnitlige skudstatistikker pr. kamp


# Beregn gennemsnitlige skudstatistikker pr. kamp
match_stats <- filtered_data_match_shot %>%
  group_by(matchId) %>%
  summarise(
    avg_angle = round(mean(angle, na.rm = TRUE), 2),
    avg_shotXG = round(mean(shot.xg, na.rm = TRUE), 2),
    .groups = "drop"
  )
# Udtræk yderligere detaljer med begge holdnavne pr. matchId
additional_details <- filtered_data_match_shot %>%
  group_by(matchId, competitionId, seasonId, label) %>%
  summarise(
    teams = str_c(unique(team.name), collapse = ' vs '),
    .groups = 'drop'
  )

# Sammenkæd de gennemsnitlige skudstatistikker med de yderligere detaljer
full_match_stats <- left_join(match_stats, additional_details, by = "matchId")

#opsætning af app

#UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
       body {
         background-color: lightgray; /* Lysegrå baggrundsfarve */
       }
       .selectize-dropdown-content {
         color: black; /* Sort skriftfarve i dropdown menuerne, ændret fra hvid */
       }
       .selectize-input {
         color: black; /* Sikrer at input-feltet også har sort tekst */
       }
       .nav-tabs > li > a {
         color: black; /* Farve på tab tekst */
       }
       .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
         background-color: gray; /* Baggrundsfarve for aktiv tab */
         color: white; /* Tekstfarve for aktiv tab */
       }
       ")
    )
  ),
  div(style = "background-color: lightgray;",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Kampoverblik",
          fluidRow(
            column(3,
                   selectInput(
                     "league_choice_shot",
                     "Vælg Liga",
                     choices = c("Alle ligaer", "Eredivisie (Hollandske liga)" = 635, "Ekstraklasa (Polske liga)" = 692),
                     selected = NULL
                   ),
                   selectInput(
                     "season_choice_shot",
                     "Vælg Sæson",
                     choices = c("Alle sæsoner", "Holland 2021/2022" = 187502, "Holland 2022/2023" = 188125, "Polen 2021/2022" = 186215, "Polen 2022/2023" = 188088),
                     selected = NULL
                   ),
                   actionButton("submit_button", "Opdater Data"),
                   selectInput(
                     "team_ranking_choice",
                     "Vælg Team Ranking",
                     choices = c("Alle", "Top Hold", "Bund Hold"),
                     selected = "Alle"
                   ),
                   selectInput(
                     "match_choice_shot",
                     "Vælg Kamp",
                     choices = NULL,  # Opdateres dynamisk
                     selected = "Alle"
                   )
            ),
            column(9,
                   plotOutput("cluster_plot_shot")
            )
          )
        )
      )
  )
)

#---------------------------------------------------------- Hjælpe funktioner til at køre serverdel --------------------------------------------------------- #
#filtrering af liga og sæson
filterData_shot_match <- function(league, season) {
  full_match_stats[as.numeric(full_match_stats$competitionId) == as.numeric(league) &
                     as.numeric(full_match_stats$seasonId) == as.numeric(season), ]
}


plotWithDetails_shot <- function(data, title_suffix, cluster_labels) {
  if (is.null(data) || nrow(data) == 0) {
    print("Ingen data at vise.")
    return(NULL)
  }
  
  plot <- ggplot(data, aes(x = avg_angle, y = avg_shotXG, color = as.factor(cluster))) +
    geom_point() +
    scale_color_manual(values = c("red", "blue", "green", "purple"),
                       labels = cluster_labels,
                       name = "Cluster Type") +
    labs(x = "Gns Vinkel", y = "Gns Shot xG", title = paste("Cluster Analysis -", title_suffix)) +
    theme_minimal() +
    guides(color = guide_legend(title = "Cluster Description"))
  
  return(plot)
}


#Funktion til at få cluster navne:
get_cluster_descriptions <- function(centers) {
  cluster_descriptions <- character(nrow(centers))
  
  for (i in 1:nrow(centers)) {
    if (centers[i, "avg_shotXG"] <= 0.06 && centers[i, "avg_angle"] >= 50 && centers[i, "avg_angle"] <= 60) {
      cluster_descriptions[i] <- "Chance tagerne"
    } else if (centers[i, "avg_shotXG"] <= 0.125 && centers[i, "avg_shotXG"] >= 0.08 && centers[i, "avg_angle"] >= 65 && centers[i, "avg_angle"] <= 80) {
      cluster_descriptions[i] <- "Dem med lidt bedre chancer for mål"
    } else if (centers[i, "avg_shotXG"] <= 0.17 && centers[i, "avg_shotXG"] >= 0.13 && centers[i, "avg_angle"] >= 85 && centers[i, "avg_angle"] <= 100) {
      cluster_descriptions[i] <- "Der er en rigtig god chance for mål"
    } else if (centers[i, "avg_shotXG"] >= 0.18 && centers[i, "avg_angle"] >= 112) {
      cluster_descriptions[i] <- "Der burde være mål?"
    }
  }
  
  return(cluster_descriptions)
}

get_teams_by_ranking <- function(league, isTop) {
  # Definitioner af top og bund hold
  top_teams <- list(
    `635` = c("Ajax", "PSV", "Feyenoord", "Twente", "AZ"),  # Holland
    `692` = c("Lech Poznań", "Raków Częstochowa", "Pogoń Szczecin", "Lechia Gdańsk", "Piast Gliwice")  # Polen
  )
  
  bottom_teams <- list(
    `635` = c("PEC Zwolle", "Williem II", "Heracles", "Sittard", "Sparta Rotterdam"),  # Holland
    `692` = c("Leczna", "Wisla", "Bruk-Bet Termallica", "Śląsk Wrocław", "Stal Mielec")  # Polen
  )
  
  # Vælg den relevante liste baseret på input
  if (isTop) {
    return(top_teams[[as.character(league)]])
  } else {
    return(bottom_teams[[as.character(league)]])
  }
}

# ------------------------------------------------------------------------ Server del ----------------------------------------------------------------------- #
# Server-side logic
server <- function(input, output, session) {
  reactive_cluster_data_shot <- reactiveVal()
  reactive_cluster_labels <- reactiveVal()
  
  observeEvent(input$submit_button, {
    req(input$league_choice_shot, input$season_choice_shot)
    
    filtered_data <- filterData_shot_match(input$league_choice_shot, input$season_choice_shot)
    print(paste("Data efter liga og sæson filtrering:", nrow(filtered_data)))
    
    if (nrow(filtered_data) > 0) {
      data_for_clustering <- filtered_data[, c("avg_angle", "avg_shotXG")]
      set.seed(123)
      kmeans_result <- kmeans(data_for_clustering, centers = 4, nstart = 25)
      filtered_data$cluster <- kmeans_result$cluster
      reactive_cluster_data_shot(filtered_data)
      
      centers <- kmeans_result$centers
      cluster_labels <- get_cluster_descriptions(centers)
      reactive_cluster_labels(cluster_labels)
      
      updateSelectInput(session, "match_choice_shot",
                        choices = c("Alle Kampe" = "", setNames(filtered_data$label, filtered_data$matchId)),
                        selected = "")
    } else {
      showNotification("Ikke nok data til at danne clusters.", type = "error")
      reactive_cluster_data_shot(NULL)
    }
  })
  
  observe({
    req(input$team_ranking_choice, reactive_cluster_data_shot())
    filtered_teams <- reactive_cluster_data_shot()
    
    if (input$team_ranking_choice == "Top Hold" || input$team_ranking_choice == "Bund Hold") {
      team_ranking <- get_teams_by_ranking(input$league_choice_shot, input$team_ranking_choice == "Top Hold")
      if (!is.null(team_ranking)) {
        filtered_teams <- filtered_teams[filtered_teams$teams %in% team_ranking, ]
      }
    }
    
    updateSelectInput(session, "match_choice_shot",
                      choices = c("Alle Kampe" = "", setNames(filtered_teams$label, filtered_teams$matchId)),
                      selected = "")
    print(paste("Data efter holdvalg:", nrow(filtered_teams)))
  })
  
  output$cluster_plot_shot <- renderPlot({
    req(reactive_cluster_data_shot(), reactive_cluster_labels())
    data_to_plot_shot <- reactive_cluster_data_shot()
    
    if (input$match_choice_shot != "" && input$match_choice_shot != "Alle Kampe") {
      data_to_plot_shot <- data_to_plot_shot[data_to_plot_shot$matchId == input$match_choice_shot, ]
    }
    
    cluster_labels <- reactive_cluster_labels()
    if (is.null(data_to_plot_shot) || nrow(data_to_plot_shot) == 0) {
      print("Ingen data tilgængelige for visning.")
      return(NULL)
    }
    
    title_suffix <- ifelse(nzchar(input$league_choice_shot) && nzchar(input$season_choice_shot),
                           paste(input$league_choice_shot, input$season_choice_shot, sep = " - "),
                           "Data ikke specificeret korrekt")
    
    print(paste("Data der plottes:", nrow(data_to_plot_shot)))  # Debugging print
    
    return(plotWithDetails_shot(data_to_plot_shot, title_suffix, cluster_labels))
  })
}

# Kør Shiny-appen
shinyApp(ui=ui, server = server)

# ------------------------------------------------------------------- Opgave 4.3 - Shiny -------------------------------------------------------------------- #
# K-means clustering
set.seed(123)
test_kampe_shot <- full_match_stats %>%
  filter(competitionId == 635, seasonId == 187502)
kmeans_result <- kmeans(test_kampe_shot[, c("avg_angle", "avg_shotXG")], centers = 4)

test_kampe_shot$kmeans_cluster <- kmeans_result$cluster

# Opsummer centrale egenskaber for hver klynge
cluster_summary <-test_kampe_shot  %>%
  group_by(kmeans_cluster) %>%
  summarise(avg_angle = mean(avg_angle), avg_shotXG = mean(avg_shotXG))

# Vis opsummering
print(cluster_summary)

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

cluster_test_data <-test_kampe_shot  %>%
  filter(kmeans_cluster == 2)

# Lav et scatterplot kun med data fra klynge 3, tilføj spillerens navn som etiketter
a <- ggplot(data = cluster_test_data, aes(x = avg_angle, y = avg_shotXG, color = teams, label = teams)) +
  geom_point() +
  geom_text_repel(aes(label = teams), size = 3, max.overlaps = Inf) +  # Juster placeringen af etiketterne for bedre synlighed
  labs(x = "Gns Location on field", y = "Gns Pass Length") +
  ggtitle("Gns. Hollandske målmand 21/22 skyder omkring 35 meter op af banen") +
  guides(color = "none")  # Fjern forklaringsboksen

#funktion:
plotData_shot <- function(data, title_suffix) {
  if (is.null(data) || nrow(data) == 0) {
    print("Ingen data at vise.")
    return(NULL)
  }
  
  ggplot(data, aes(x = label, y = avg_shotXG, fill = avg_angle)) +
    geom_bar(stat = "identity") +
    labs(x = "Kamp", y = "Gns Shot xG", fill = "Gns Vinkel", title = paste("Spilanalyse -", title_suffix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

#UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
       body {
         background-color: lightgray; /* Lysegrå baggrundsfarve */
       }
       .selectize-dropdown-content {
         color: black; /* Sort skriftfarve i dropdown menuerne, ændret fra hvid */
       }
       .selectize-input {
         color: black; /* Sikrer at input-feltet også har sort tekst */
       }
       .nav-tabs > li > a {
         color: black; /* Farve på tab tekst */
       }
       .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
         background-color: gray; /* Baggrundsfarve for aktiv tab */
         color: white; /* Tekstfarve for aktiv tab */
       }
       ")
    )
  ),
  div(style = "background-color: lightgray;",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Kampoverblik",
          fluidRow(
            column(3,
                   selectInput(
                     "league_choice_shot",
                     "Vælg Liga",
                     choices = c("Alle ligaer", "Eredivisie (Hollandske liga)" = 635, "Ekstraklasa (Polske liga)" = 692),
                     selected = NULL
                   ),
                   selectInput(
                     "season_choice_shot",
                     "Vælg Sæson",
                     choices = c("Alle sæsoner", "Holland 2021/2022" = 187502, "Holland 2022/2023" = 188125, "Polen 2021/2022" = 186215, "Polen 2022/2023" = 188088),
                     selected = NULL
                   ),
                   actionButton("submit_button", "Opdater Data"),
                   selectInput(
                     "team_ranking_choice",
                     "Vælg Team Ranking",
                     choices = c("Alle", "Top Hold", "Bund Hold"),
                     selected = "Alle"
                   ),
                   selectInput(
                     "match_choice_shot",
                     "Vælg Kamp",
                     choices = NULL,  # Opdateres dynamisk
                     selected = "Alle"
                   )
            ),
            column(9,
                   plotOutput("cluster_plot_shot")
            )
          )
        )
      )
  )
)

#serverdel
server <- function(input, output, session) {
  reactive_filtered_data <- reactiveVal()
  
  observeEvent(input$submit_button, {
    req(input$league_choice_shot, input$season_choice_shot)
    
    filtered_data <- filterData_shot_match(input$league_choice_shot, input$season_choice_shot)
    print(paste("Data efter liga og sæson filtrering:", nrow(filtered_data)))
    
    if (nrow(filtered_data) > 0) {
      reactive_filtered_data(filtered_data)
    } else {
      showNotification("Ikke nok data til at danne diagrammet.", type = "error")
      reactive_filtered_data(NULL)
    }
  })
  
  observe({
    req(input$team_ranking_choice, reactive_filtered_data())
    filtered_teams <- reactive_filtered_data()
    
    if (input$team_ranking_choice == "Top Hold" || input$team_ranking_choice == "Bund Hold") {
      team_ranking <- get_teams_by_ranking(input$league_choice_shot, input$team_ranking_choice == "Top Hold")
      if (!is.null(team_ranking)) {
        filtered_teams <- filtered_teams[filtered_teams$teams %in% team_ranking, ]
      }
    }
    
    reactive_filtered_data(filtered_teams)
  })
  
  output$cluster_plot_shot <- renderPlot({
    req(reactive_filtered_data())
    data_to_plot_shot <- reactive_filtered_data()
    
    title_suffix <- ifelse(nzchar(input$league_choice_shot) && nzchar(input$season_choice_shot),
                           paste(input$league_choice_shot, input$season_choice_shot, sep = " - "),
                           "Data ikke specificeret korrekt")
    
    return(plotData_shot(data_to_plot_shot, title_suffix))
  })
}

# Kør Shiny-appen
shinyApp(ui=ui, server = server)
