library(shiny)
library(gtsummary)
library(gt)
# make dataset with a few variables to summarize
iris2 <- iris %>% select(Sepal.Length,  Sepal.Width, Species) %>% tbl_summary(by=Species)

# summarize the data with our package
table1 <- iris2 %>% as_gt()
table1

shinyApp(
    ui = fluidPage(
        fluidRow(
            column(12,
                   gt_output('table')
            )
        )
    ),
    server = function(input, output) {
        output$table <- render_gt(table1)
    })  