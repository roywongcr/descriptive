
##install.packages("shiny")

library(shiny)
ui <- fluidPage(
    selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
    verbatimTextOutput("summary"),
    verbatimTextOutput("str"),
    tableOutput("table")
   
)

server <- function(input, output, session) {
    # Create a reactive expression
    dataset <- reactive({
        get(input$dataset, "package:datasets")
    })
    
    output$summary <- renderPrint({
        # Use a reactive expression by calling it like a function
        summary(dataset())
    })
    
    output$table <- renderTable({
        dataset()
    })
    
    output$str <- renderPrint({
       str(dataset)
    })
}


shinyApp(ui, server)
###Until point 1.4 https://mastering-shiny.org/basic-app.html

