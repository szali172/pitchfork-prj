library(shiny)
library(tidyverse)

years_ = c(sort(decreasing = FALSE, unique(reviews$pub_year)))
radiohead = reviews %>% 
    filter(artist == "radiohead")

new_r = sort(radiohead$pub_year, decreasing = FALSE)
    

# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage(
    title = "Pitchfork",
    tabPanel(
        title = "Artist page",
        titlePanel("Pitchfork Reviews"),
        
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "artist",
                    label = "Artist:",
                    choices = sort(unique(reviews$artist)),
                    selected = "radiohead"
                )
            ),
            mainPanel(plotOutput("distPlot")))
    ),
    tabPanel(title = "About",
             includeMarkdown("about.Rmd")
             )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Viewing specific artist tab
    artist_data = reactive({
        reviews |>
            filter(artist == input$artist)
    })
    
    observeEvent(input$artist, {
        updateSelectInput(inputId = "type",
                          choices = unique(artist_data()$artist),
                          selected = unique(artist_data()$artist))
    })
  
    
    # Need to figure out how to order based on pub_year
    output$distPlot <- renderPlot({
        ggplot(data = artist_data(), aes(x = title, y = score, fill = pub_year)) +
            xlab("Albums") +
            ylab("Score") +
            geom_col() + 
            ylim(0,10) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

