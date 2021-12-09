library(shiny)
library(tidyverse)

years_ = c(sort(decreasing = FALSE, unique(reviews$pub_year)))
sort(radiohead$pub_year, decreasing = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage(
    title = "Pitchfork Data Project",
    tabPanel(
      title = "All Reviews",
      titlePanel("Reviews for every genre (1999 - 2017)"),
      sidebarLayout(
        sidebarPanel(
          dataTableOutput("table")
        ),
        mainPanel(plotOutput("genrePlot"))
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
                    choices = sort(unique(genres_and_scores$Artist)),
                    selected = "radiohead"
                )
            ),
            mainPanel(plotOutput("artistPlot")))
    ),
    tabPanel(title = "About",
             includeMarkdown("about.Rmd")
             )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Viewing specific artist tab
    artist_data = reactive({
        genres_and_scores |>
            filter(`Artist` == input$artist) %>% 
            arrange(`Year Review Published`, .by_group = TRUE)
    })
    
    observeEvent(input$artist, {
        updateSelectInput(inputId = "type",
                          choices = unique(artist_data()$artist),
                          selected = unique(artist_data()$artist))
    })
  
    
    # Need to figure out how to order based on pub_year
    output$artistPlot <- renderPlot({
        ggplot(data = artist_data(), aes(x = `Album`, y = `Score`, fill = `Year Review Published`)) +
            geom_col() +
            geom_text(mapping = aes(label = `Score`), color = "white", size = 5, position = position_nudge(x = 0, y = -2), family = "Times New Roman") + 
            xlab("Albums") +
            ylab("Score") +
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$genrePlot <- renderPlot({
        genres_and_scores %>% 
          ggplot(aes(x = `Year Review Published`, y = `Score`)) +
          geom_point() +
          geom_smooth() +
          ylim(0, 10) +
          xlab("Years") +
          ylab("Score") +
          facet_wrap(~`Genre`)
    })
    
    output$table <- renderDataTable({
      genres_and_scores
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

