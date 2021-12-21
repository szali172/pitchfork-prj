library(shiny)
library(tidyverse)
library(bslib)

source("R/db-parser.R")
source("R/function.R")

ui <- fluidPage(navbarPage(
    inverse = TRUE,
    theme = bs_theme(),
    title = "Pitchfork Data Project",
    tabPanel(
      title = "All Reviews",
      titlePanel("Reviews for every genre (1999 - 2017)"),
      sidebarLayout(
        sidebarPanel(width = 4,
          selectInput(
            inputId = "genre",
            label = "Genre:",
            choices = c("All", sort(unique(reviews_summary$Genre))),
            selected = "All"
          ), 
          checkboxInput(inputId = "best",
                        label = "Best New Music",
                        FALSE
          ),
          div(dataTableOutput("table"), style = "font-size: 60%; width: 100%")
        ),
        mainPanel(plotOutput("genrePlot", width = "1100px", height = "600px"))
      )
    ),
    tabPanel(
        title = "Artists",
        titlePanel("Reviews for this Artist"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput(
                  inputId = "artist",
                  label = "Artist:",
                  choices = sort(unique(reviews_summary$Artist)),
                  selected = "radiohead"
                )
            ),
            mainPanel(plotOutput("artistPlot", width = "100%", height = "600px")))
    ),
    tabPanel(title = "About",
             includeMarkdown("about.Rmd")
             )
))

server <- function(input, output) {

    # Viewing genre data
    genre_data = reactive({
      # Call function to filter genres
      return(check_input(input$genre))
    })
    
    observeEvent(input$genre, {
      updateSelectInput(inputId = "genre",
                        choices = unique(genre_data()$genre),
                        selected = unique(genre_data()$genre))
    })
    
    # Viewing genre data + Best New Music data
    best_data = reactive({
      temp = genre_data()
        if (input$best) {
          temp = temp %>% 
          filter(`Best New Music` == 1)
          return(temp)
      }
      temp
    })
    
  
    ## Viewing specific artist tab
    artist_data = reactive({
      reviews_summary %>% 
        filter(`Artist` == input$artist) %>% 
        arrange(`Year`, .by_group = TRUE)
    })
    
    observeEvent(input$artist, {
      updateSelectInput(inputId = "artist",
                        choices = unique(artist_data()$artist),
                        selected = unique(artist_data()$artist))
    })

   
    
    output$genrePlot <- renderPlot({
      if (input$genre == "All") {
        if (input$best) {
          best_data() %>% 
            ggplot(aes(x = `Year`, y = `Score`, color = `Genre`)) +
            ggtitle(input$genre) +
            geom_point() +
            geom_smooth() +
            ylim(0, 10) +
            xlab("Year") +
            ylab("Score") +
            theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), 
                  plot.background = element_rect(fill = "grey80", colour = NA),
                  axis.line = element_line(colour = "grey50", size = 1),
                  axis.text = element_text(color = "blue", size = 12),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15))
        } else {
          genre_data() %>% 
            ggplot(aes(x = `Year`, y = `Score`)) +
            geom_point() +
            geom_smooth() +
            ylim(0, 10) +
            xlab("Year") +
            ylab("Score") +
            facet_wrap(~`Genre`) +
            theme(plot.background = element_rect(fill = "grey80", colour = NA),
                  axis.line = element_line(colour = "grey50", size = 1),
                  axis.text = element_text(color = "blue", size = 12),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15))
        }
      } else {
        if (input$best) {
          best_data() %>% 
            ggplot(aes(x = `Year`, y = `Score`)) +
            ggtitle(input$genre) +
            geom_point() +
            geom_smooth() +
            ylim(0, 10) +
            xlab("Year") +
            ylab("Score") +
            theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), 
                  plot.background = element_rect(fill = "grey80", colour = NA),
                  axis.line = element_line(colour = "grey50", size = 1),
                  axis.text = element_text(color = "blue", size = 12),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15))
        } else {
          genre_data() %>% 
            ggplot(aes(x = `Year`, y = `Score`)) +
            ggtitle(input$genre) +
            geom_point() +
            geom_smooth() +
            ylim(0, 10) +
            xlab("Year") +
            ylab("Score") +
            theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), 
                  plot.background = element_rect(fill = "grey80", colour = NA),
                  axis.line = element_line(colour = "grey50", size = 1),
                  axis.text = element_text(color = "blue", size = 12),
                  axis.title.x = element_text(size = 15),
                  axis.title.y = element_text(size = 15))
        }
      }
    })
    
    output$table <- renderDataTable({
      if (input$best) {
        best_data()
      } else {
        genre_data()
      }
    })
    
    
    output$artistPlot <- renderPlot({
      #factor(artist_data()$`Album`, levels = artist_data()$`Year`)
      ggplot(data = artist_data(), aes(x = `Album`, y = `Score`)) +
        ggtitle(input$artist) +
        geom_col() +
        geom_text(mapping = aes(label = `Score`), color = "white", size = 5, 
                  position = position_nudge(x = 0, y = -2)) + 
        xlab("Albums") +
        ylab("Score") +
        ylim(0,10) +
        theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5), 
              plot.background = element_rect(fill = "grey80", colour = NA),
              axis.line = element_line(colour = "grey50", size = 1),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              axis.text.x = element_text(size = 15, face = "bold", angle = 45, hjust = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

