

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Match alerts"),

            textAreaInput(
                inputId = "input_old_code",
                label = "Old code",
                width = "400px",
                rows = 100
            ),
            textAreaInput(
                inputId = "input_old_code",
                label = "New code",
                cols = 90,
                width = "400px",
                rows = 100
            )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
