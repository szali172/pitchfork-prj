library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pitchfork Reviews"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "artist",
                        label = "Artist:",
                        choices = sort(unique(reviews$artist)),
                        selected = "Radiohead"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    artist_data = reactive(
        reviews |>
            filter(artist == input$artist)
    )
    
    observeEvent(input$pitcher, {
        updateSelectInput(inputId = "artist",
                          choices = unique(artist_data()$name),
                          selected = check_for_ff(unique(artist_data()$name)))
    })
    
    output$distPlot <- renderPlot({
        ggplot(data = artist_data(), aes(x = pub_year, y = score)) +
            geom_point() +
            geom_smooth()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

