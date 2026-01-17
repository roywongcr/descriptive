# Establecer directorio de trabajo
setwd("C:/R_ANALYSIS/ShinyApp")

# Librerías
library(readxl)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(shiny)
library(gtsummary)
library(gt)

# =========================
# UI
# =========================
ui <- fluidPage(
    titlePanel("Cargar Base de Datos"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                "fileType", "Seleccionar tipo de archivo:",
                choices = c(".csv", ".xlsx"),
                selected = ".csv"
            ),
            fileInput(
                "uploadFile", "Subir archivo de datos",
                accept = c(".csv", ".xlsx")
            ),
            tags$hr(),
            helpText(
                "Cargue un archivo .csv o .xlsx. ",
                "En cada pestaña podrá elegir las columnas a visualizar ",
                "y una variable 'by' (opcional) para comparar grupos."
            )
        ),
        mainPanel(
            h3("Visualización por pestañas"),
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    "TAB 1",
                    uiOutput("col_selector_1"),
                    uiOutput("by_selector_1"),
                    gt::gt_output("gt_table_1")
                ),
                tabPanel(
                    "TAB 2",
                    uiOutput("col_selector_2"),
                    uiOutput("by_selector_2"),
                    gt::gt_output("gt_table_2")
                ),
                tabPanel(
                    "TAB 3",
                    uiOutput("col_selector_3"),
                    uiOutput("by_selector_3"),
                    gt::gt_output("gt_table_3")
                ),
                tabPanel(
                    "TAB 4",
                    uiOutput("col_selector_4"),
                    uiOutput("by_selector_4"),
                    gt::gt_output("gt_table_4")
                ),
                tabPanel(
                    "TAB 5",
                    uiOutput("col_selector_5"),
                    uiOutput("by_selector_5"),
                    gt::gt_output("gt_table_5")
                )
            )
        )
    )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
    # Cargar datos reactivamente
    data <- reactive({
        req(input$uploadFile)
        switch(
            input$fileType,
            ".csv"  = readr::read_csv(
                input$uploadFile$datapath,
                locale = readr::locale(encoding = "latin1"),
                show_col_types = FALSE
            ),
            ".xlsx" = readxl::read_excel(input$uploadFile$datapath)
        ) %>%
            # Evitar nombres vacíos o duplicados molestos
            janitor::clean_names() %>%
            tibble::as_tibble()
    })
    
    # -------- Función auxiliar para construir cada TAB --------
    build_tab <- function(i) {
        # UI dinámico del selector de columnas
        output[[paste0("col_selector_", i)]] <- renderUI({
            req(data())
            cols <- names(data())
            selectizeInput(
                inputId = paste0("cols_", i),
                label   = paste("Columnas a describir (TAB", i, "):"),
                choices = cols,
                selected = cols,      # Por defecto todas
                multiple = TRUE,
                options = list(
                    plugins = list("remove_button"),
                    placeholder = "Seleccione una o más columnas…",
                    maxItems = length(cols)
                )
            )
        })
        
        # UI dinámico del selector de 'by' (opcional)
        output[[paste0("by_selector_", i)]] <- renderUI({
            req(data())
            cols <- names(data())
            selectInput(
                inputId = paste0("by_", i),
                label   = "Variable de agrupación (by, opcional):",
                choices = c("— Sin 'by' —" = "", cols),
                selected = ""
            )
        })
        
        # Tabla gtsummary
        output[[paste0("gt_table_", i)]] <- gt::render_gt({
            req(data())
            df  <- data()
            
            # Variables seleccionadas para este TAB
            sel <- input[[paste0("cols_", i)]]
            validate(need(!is.null(sel) && length(sel) > 0,
                          "Seleccione al menos una columna."))
            
            sel <- sel[sel %in% names(df)]
            validate(need(length(sel) > 0, "Las columnas seleccionadas no existen en el dataset."))
            
            df <- df[, sel, drop = FALSE]
            
            # Variable by (opcional)
            byvar <- input[[paste0("by_", i)]]
            if (!is.null(byvar) && nzchar(byvar) && byvar %in% names(df)) {
                # Si la variable by no está en la selección, la añadimos temporalmente para el resumen
                if (!(byvar %in% names(df))) {
                    df <- dplyr::bind_cols(df, data()[, byvar, drop = FALSE])
                }
                
                tbl <- gtsummary::tbl_summary(
                    data = df,
                    by   = byvar,           # by acepta el nombre como string
                    missing = "ifany"       # muestra categoría de faltantes si aplica
                ) %>%
                    gtsummary::add_n() %>%  # añade N por fila
                    gtsummary::add_p() %>%  # añade p-values (pruebas apropiadas al tipo de variable)
                    gtsummary::bold_labels()
            } else {
                tbl <- gtsummary::tbl_summary(
                    data = df,
                    missing = "ifany"
                ) %>%
                    gtsummary::add_n() %>%
                    gtsummary::bold_labels()
            }
            
            # Convertir a gt para renderizar en Shiny
            gtsummary::as_gt(tbl)
        })
    }
    
    # Construir las 5 pestañas
    lapply(1:5, build_tab)
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)

