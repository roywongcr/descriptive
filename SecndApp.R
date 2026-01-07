library(shiny)

ui <- fluidPage(
    textInput("name", "What's your name?"),
    passwordInput("password", "What's your password?"),
    textAreaInput("story", "Tell me about yourself", rows = 3),
    numericInput("num", "Number one", value = 0, min = 0, max = 100),
    sliderInput("num2", "Number two", value = 50, min = 0, max = 1000),
    sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
    dateInput("dob", "When were you born?"),
    dateRangeInput("holiday", "When do you want to go on vacation next?"),
    actionButton("click", "Click me!"),
    actionButton("drink", "Drink me!", icon = icon("cocktail")),
    fileInput("upload", NULL),
    actionButton("click", "Click me!", class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success"),
    actionButton("eat", "Eat me!", class = "btn-block")
    
)

server <- function(input, output, session) {
    
}


shinyApp(ui, server)

