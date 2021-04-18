
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(DT)

# Getting data from repository
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Data Wrangling
games <- games %>%
  # create a new variable
  mutate(date = ymd(paste(year, str_sub(month, 1, 3), 1))) %>%
  #reorder months as per calendar (not alphabet)
  mutate(month = fct_reorder(month, month(date)))

# create a new vector with unique year in the data
years <- games %>%
  distinct(year) %>%
  arrange(year)

# Define a server for the Shiny app
server <- function(input, output, session) {
  
  # Getting top 10 games per selected year by peak's mean in that year
  top10 <- reactive({
    games %>%
      filter(year == input$select_year) %>%
      add_count(gamename) %>%
      group_by(gamename) %>%
      summarise(mean_peak = mean(peak)) %>%
      mutate(gamename = fct_reorder(gamename, mean_peak)) %>%
      arrange(desc(mean_peak)) %>%
      top_n(10)
  })
  
  # Render a table with Game Name and Peak's mean for the selected year
  output$table_top10 <- DT::renderDataTable(DT::datatable(top10(), 
                                                          caption = htmltools::tags$caption(paste0("Table 1. The 10 most played games in ", input$select_year), 
                                                                                            style = 'color:black; font-size:150%;'),
                                                          colnames = c("Position" = 1, "Game Name" = 2 , "Highest number of players at the same time on average" = 3)) %>%
                                              formatRound(columns = 2, digits = 0)) 
  
  # Updating select box choices depending on the top 10 games results
  observeEvent(top10(), {
    choices <- top10()$gamename
    updateSelectInput(inputId = "select_game", choices = choices)
  })
  
  # Getting a new data frame filtering by selected year and selected game
  game_trend <- reactive({
    games %>%
      filter(year == input$select_year, gamename == input$select_game)
    })

  # Rendering a barplot for selected game's year trend3 
 output$game_trend_plot <- renderPlot({
   ggplot(data = game_trend(), mapping = aes(month, avg)) +
     geom_col(fill = "#1380A1") +
     labs(x = "Months",
          y = "Average number of players at the same time",
          title = paste0("Plot 1. Year trend for ", input$select_game),
          subtitle = paste0("Data for ", input$select_year)) +
     theme(plot.title = element_text(size = 22))
 })
    
}

# Define a ui for the Shiny app  
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Give the page a title
                titlePanel("STEAM'S TOP 10 GAMES BY YEAR"),
  
  sidebarLayout(position = 'right',
    
    sidebarPanel(
      
      selectInput("select_year", label = h3("SELECT YEAR"),
                  choices = years,
                  selected = 2012),
      
      selectInput("select_game", label = h3("SELECT GAME"),
                  choices = c(1,2,3),
                  selected = 1)
    ),
    
    mainPanel(p("This aplication wants to answer two questions:"),
              p("1. Which are the 10 most played games for a specific year?"),
              p("2. How each of the top 10 games behaves in that year?"),
      
      DT::dataTableOutput("table_top10"),
      
      plotOutput("game_trend_plot") 
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)

