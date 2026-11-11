library(tidyverse)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(leaflet)

library(tm)
library(wordcloud)
library(RColorBrewer)


games_sales <- read_csv(
  "C:\\Users\\alici\\Desktop\\Portfolio\\Trabajo_IAE_24-25\\datos\\Video_Games_Sales_as_at_22_Dec_2016.csv",
  col_types = "cfnffdddddiidiff",
  na = c("N/A", "")
)

ui <- fluidPage(
  titlePanel("Análisis de Ventas de Videojuegos"),
  
  tabsetPanel(
    tabPanel("Ventas Globales",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plataforma", "Plataforma:", choices = unique(games_sales$Platform), selected = "PS4"),
                 selectInput("genero", "Género:", choices = unique(games_sales$Genre), selected = "Action"),
                 sliderInput("anio", "Rango de años:", min = min(games_sales$Year_of_Release, na.rm = TRUE), 
                             max = max(games_sales$Year_of_Release, na.rm = TRUE), value = c(2000, 2016), step = 1)
               ),
               mainPanel(
                 plotlyOutput("ventasPlot")
               )
             )
    ),
    
    tabPanel("Ventas por Plataforma según Género",
             sidebarLayout(
               sidebarPanel(
                 selectInput("genero_ventas", "Género:", choices = unique(games_sales$Genre), selected = "Action")
               ),
               mainPanel(
                 plotlyOutput("ventasGenero")
               )
             )
    ),
    
    tabPanel("Ventas de Juegos por Región",
             sidebarLayout(
               sidebarPanel(
                 selectInput("nombre_juego", "Nombre del Juego:", choices = unique(games_sales$Name), selected = unique(games_sales$Name)[1]),
                 checkboxGroupInput("region_comp", "Regiones de ventas:", choices = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"), selected = c("NA_Sales", "EU_Sales"))
               ),
               mainPanel(
                 plotlyOutput("regionPlot")
               )
             )
    ),
    
    tabPanel("Críticas por Género",
             sidebarLayout(
               sidebarPanel(
                 selectInput("genero_critica", "Género:", choices = unique(games_sales$Genre), selected = "Action")
               ),
               mainPanel(
                 plotlyOutput("lineCritica")
               )
             )
    ),
    
    tabPanel("Tabla Ventas Globales",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plataforma_tabla", "Plataforma:", choices = unique(games_sales$Platform), selected = "PS4"),
                 selectInput("genero_tabla", "Género:", choices = unique(games_sales$Genre), selected = "Action"),
                 sliderInput("anio_tabla", "Rango de años:", min = min(games_sales$Year_of_Release, na.rm = TRUE),
                             max = max(games_sales$Year_of_Release, na.rm = TRUE), value = c(2000, 2016), step = 1)
               ),
               mainPanel(
                 DTOutput("tablaJuegos")
               )
             )
    ),
    
    tabPanel("Distribución de Ventas por Plataforma",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plataforma_mapa", "Plataforma:", choices = unique(games_sales$Platform), selected = "PS4"),
                 sliderInput("anio_mapa", "Rango de años:", min = min(games_sales$Year_of_Release, na.rm = TRUE),
                             max = max(games_sales$Year_of_Release, na.rm = TRUE), value = c(2000, 2016), step = 1)
               ),
               mainPanel(
                 leafletOutput("mapaVentas")
               )
             )
    ),
    
    tabPanel("Nube de Palabras",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("minFrecuencia", "Frecuencia mínima:", min = 1, max = 10, value = 2),
                 sliderInput("maxPalabras", "Máximo de palabras:", min = 10, max = 200, value = 100)
               ),
               mainPanel(
                 plotOutput("wordCloud")
               )
             )
    ),
    tabPanel("Comparación de Plataformas",
             sidebarLayout(
               sidebarPanel(
                 selectInput("plataformas_comparar", "Selecciona hasta 3 plataformas:",
                             choices = unique(games_sales$Platform),
                             selected = c("PS4", "X360", "Wii"),
                             multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("comparacionPlataformas")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Ventas Globales
  output$ventasPlot <- renderPlotly({
    datos_filtrados <- games_sales %>% 
      filter(Platform == input$plataforma, 
             Genre == input$genero, 
             Year_of_Release >= input$anio[1], 
             Year_of_Release <= input$anio[2])
    
    p <- ggplot(datos_filtrados, aes(x = Year_of_Release, y = Global_Sales, fill = Genre)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Ventas Globales por Año", x = "Año", y = "Ventas (millones)")
    
    ggplotly(p)
  })
  
  # Ventas por Plataforma según Género
  output$ventasGenero <- renderPlotly({
    datos_genero <- games_sales %>%
      filter(Genre == input$genero_ventas) %>%
      group_by(Platform) %>%
      summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE))
    
    p <- ggplot(datos_genero, aes(x = reorder(Platform, Total_Sales), y = Total_Sales, fill = Platform)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Ventas por Plataforma en Género Seleccionado", x = "Plataforma", y = "Ventas (millones)")
    
    ggplotly(p)
  })
  
  # Ventas por Región
  output$regionPlot <- renderPlotly({
    datos_region <- games_sales %>%
      filter(Name == input$nombre_juego) %>%
      select(Name, all_of(input$region_comp))
    
    datos_long <- datos_region %>%
      pivot_longer(-Name, names_to = "Region", values_to = "Ventas")
    
    p <- ggplot(datos_long, aes(x = reorder(Region, Ventas), y = Ventas, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = paste("Ventas por Región para", input$nombre_juego), x = "Región", y = "Ventas (millones)")
    
    ggplotly(p)
  })
  
  # Críticas por Género
  output$lineCritica <- renderPlotly({
    datos_critica <- games_sales %>% filter(Genre == input$genero_critica)
    
    p <- ggplot(datos_critica, aes(x = Critic_Score)) +
      geom_line(stat = "count", color = "steelblue", size = 1) +
      theme_minimal() +
      labs(title = paste("Distribución de Puntuación de Crítica para", input$genero_critica), x = "Puntuación de Crítica", y = "Frecuencia")
    
    ggplotly(p)
  })
  
  # Tabla de ventas globales
  datos_tabla <- reactive({
    games_sales %>%
      filter(Platform == input$plataforma_tabla,
             Genre == input$genero_tabla,
             Year_of_Release >= input$anio_tabla[1],
             Year_of_Release <= input$anio_tabla[2])
  })
  
  output$tablaJuegos <- renderDT({
    tabla <- datos_tabla()[, c("Name", "Platform", "Genre", "Global_Sales")]
    datatable(tabla, options = list(pageLength = 15))
  })
  
  # Mapa de ventas por región
  datos_mapa <- reactive({
    games_sales %>%
      filter(Platform == input$plataforma_mapa,
             Year_of_Release >= input$anio_mapa[1],
             Year_of_Release <= input$anio_mapa[2])
  })
  
  output$mapaVentas <- renderLeaflet({
    ventas_region <- datos_mapa() %>%
      summarise(NA_Sales = sum(NA_Sales, na.rm = TRUE),
                EU_Sales = sum(EU_Sales, na.rm = TRUE),
                JP_Sales = sum(JP_Sales, na.rm = TRUE),
                Other_Sales = sum(Other_Sales, na.rm = TRUE))
    
    regiones <- data.frame(
      region = c("North America", "Europe", "Japan", "Other"),
      ventas = c(ventas_region$NA_Sales, ventas_region$EU_Sales, 
                 ventas_region$JP_Sales, ventas_region$Other_Sales),
      lat = c(40, 50, 36, 0),
      lon = c(-100, 10, 138, 30)
    )
    
    leaflet(regiones) %>%
      addTiles() %>%
      addCircles(lng = ~lon, lat = ~lat, weight = 1,
                 radius = ~ventas * 50000,
                 popup = ~paste(region, ":", ventas, "millones"))
  })
  
  output$wordCloud <- renderPlot({
    nombres <- games_sales$Name[!is.na(games_sales$Name) & games_sales$Name != ""]
    
    palabras <- Corpus(VectorSource(nombres))
    palabras <- tm_map(palabras, content_transformer(tolower))
    palabras <- tm_map(palabras, removePunctuation)
    palabras <- tm_map(palabras, removeNumbers)
    palabras <- tm_map(palabras, removeWords, stopwords("english"))
    palabras <- tm_map(palabras, stripWhitespace)
    
    dtm <- TermDocumentMatrix(palabras)
    m <- as.matrix(dtm)
    palabras_freq <- sort(rowSums(m), decreasing = TRUE)
    df <- data.frame(word = names(palabras_freq), freq = palabras_freq)
    

    wordcloud(words = df$word, freq = df$freq, min.freq = input$minFrecuencia,
              max.words = input$maxPalabras, random.order = FALSE,
              colors = brewer.pal(8, "Dark2"))
  })
  
  output$comparacionPlataformas <- renderPlotly({
    plataformas <- input$plataformas_comparar
    
    if (length(plataformas) == 0 || length(plataformas) > 3) {
      return(NULL)
    }
    
    datos_comparacion <- games_sales %>%
      filter(Platform %in% plataformas) %>%
      group_by(Year_of_Release, Platform) %>%
      summarise(Global_Sales = sum(Global_Sales, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Year_of_Release))
    
    p <- ggplot(datos_comparacion, aes(x = Year_of_Release, y = Global_Sales, color = Platform)) +
      geom_line(size = 1.2) +
      geom_point() +
      theme_minimal() +
      labs(title = "Comparación de Ventas Globales por Plataforma",
           x = "Año",
           y = "Ventas (millones)")
    
    ggplotly(p)
  })
  
}

shinyApp(ui, server)


