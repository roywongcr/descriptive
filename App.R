# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from
library(gtsummary)
library(gt)
library(shinythemes)
library(readxl)

TRASPL_HEP <- read_excel("Data/TRASPL_HEP.xlsx")


# Loading data ----
Barley <- as.data.frame(beaven.barley)
# ui.R ----
#ui <- fluidPage(
#    titlePanel(""),  # Add a title panel
#    sidebarLayout(  # Make the layout a sidebarLayout
#        sidebarPanel(),  # Inside the sidebarLayout, add a sidebarPanel
#        mainPanel()  # Inside the sidebarLayout, add a mainPanel
#    )
#)

ui <- fluidPage(theme = shinytheme("superhero"),
    titlePanel("Título"),
      sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "gen",  # Give the input a name "genotype"
                        label = "1. Select genotype",  # Give the input a label to be displayed in the app
                        choices = c("A1" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"), selected = "a"),  # Create the choices that can be selected. e.g. Display "A" and link to value "a"
            selectInput(inputId = "col", 
                        label = "2. Select histogram colour", 
                        choices = c("blue","green","red","purple","grey"), selected = "grey"),
            sliderInput(inputId = "bin", 
                        label = "3. Select number of histogram bins", 
                        min=1, max=25, value= c(10)),
            textInput(inputId = "text", 
                      label = "4. Enter some text to be displayed", ""),
            actionButton(inputId = "action", label = "Go!"),
            radioButtons(inputId = "radio", label = "Radio Buttons", choices = c("A", "B")),
            selectInput(inputId = "select", label = "select", choices = c("A", "B"))
        ),
        mainPanel(
            plotOutput("myhist"),
            tableOutput("mytable"),
            textOutput("mytext"),
            gt_output("myTable2"),
            DT::dataTableOutput("tabla_datos")
        )
    )
)

# server.R ----
server <- function(input, output) {
    output$myhist <- renderPlot(ggplot(Barley, aes(x = yield)) + 
                                    geom_histogram(bins = input$bin, fill = input$col, group=input$gen, 
                                                   data=Barley[Barley$gen == input$gen,],
                                                   colour = "black")+theme_classic())
    
    output$mytext <- renderText(input$text)
    
    output$mytable <- renderTable(Barley %>%
                                      filter(gen == input$gen) %>%
                                      summarise("Mean" = mean(yield), 
                                                "Median" = median(yield),
                                                "STDEV" = sd(yield), 
                                                "Min" = min(yield),
                                                "Max" = max(yield)))
    output$myTable2<-render_gt(Barley %>% select(col,row,gen) %>% tbl_summary(by=gen)%>% add_ci()%>% add_p() %>% as_gt())
}
# Run the app ----
shinyApp(ui = ui, server = server)