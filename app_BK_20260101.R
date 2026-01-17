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
library(janitor)
library(DT)

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
                "En la pestaña 'Parámetros' podrá renombrar variables (solo para visualización) ",
                "y definir opciones de estimación.",
                "En cada pestaña podrá elegir columnas y una variable 'by' (opcional) para comparar grupos."
            )
        ),
        mainPanel(
            h3("Visualización por pestañas"),
            tabsetPanel(
                id = "tabs",
                
                # =========================
                # PESTAÑA PARÁMETROS
                # =========================
                tabPanel(
                    "Parámetros",
                    
                    h4("Renombrar variables"),
                    p("Renombre variables para su visualización. Si no edita un nombre, se mantiene el del archivo."),
                    DTOutput("rename_table"),
                    tags$br(),
                    actionButton("reset_names", "Restablecer nombres del archivo"),
                    
                    tags$hr(),
                    
                    # =========================
                    # NUEVA SECCIÓN: ESTIMACIONES
                    # =========================
                    h4("Estimaciones"),
                    h5("Variables categóricas"),
                    tags$div(style = "color:#6c757d; font-size: 0.95em;",
                             "(PE: Sexo, Provincia)"),
                    tags$br(),
                    checkboxInput(
                        "cat_ci95",
                        "Incluir intervalo de confianza (IC) al 95%",
                        value = FALSE
                    ),
                    tags$div(style = "color:#6c757d; font-size: 0.95em; margin-top:6px;",
                             "(Opción recomendada si el análisis es de una muestra y no de una población).")
                ),
                
                tabPanel("TAB 1", uiOutput("col_selector_1"), uiOutput("by_selector_1"), gt::gt_output("gt_table_1")),
                tabPanel("TAB 2", uiOutput("col_selector_2"), uiOutput("by_selector_2"), gt::gt_output("gt_table_2")),
                tabPanel("TAB 3", uiOutput("col_selector_3"), uiOutput("by_selector_3"), gt::gt_output("gt_table_3")),
                tabPanel("TAB 4", uiOutput("col_selector_4"), uiOutput("by_selector_4"), gt::gt_output("gt_table_4")),
                tabPanel("TAB 5", uiOutput("col_selector_5"), uiOutput("by_selector_5"), gt::gt_output("gt_table_5"))
            )
        )
    )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
    
    # Mapeo: internal_name -> display_name
    label_map <- reactiveVal(NULL)
    
    # Dataset reactivo (df con nombres internos + nombres originales)
    dataset <- reactive({
        req(input$uploadFile)
        
        df_raw <- switch(
            input$fileType,
            ".csv"  = readr::read_csv(
                input$uploadFile$datapath,
                locale = readr::locale(encoding = "latin1"),
                show_col_types = FALSE
            ),
            ".xlsx" = readxl::read_excel(input$uploadFile$datapath)
        ) %>% tibble::as_tibble()
        
        orig_names <- names(df_raw)
        
        # Limpiar nombres internos y asegurar unicidad
        internal_names <- janitor::make_clean_names(orig_names)
        internal_names <- make.unique(internal_names, sep = "_")
        
        df <- df_raw
        names(df) <- internal_names
        
        list(
            df = df,
            orig_names = orig_names,
            internal_names = internal_names
        )
    })
    
    # Inicializar mapeo al cargar archivo
    observeEvent(dataset(), {
        ds <- dataset()
        label_map(setNames(ds$orig_names, ds$internal_names))
    }, ignoreInit = FALSE)
    
    # Restablecer nombres a los del archivo
    observeEvent(input$reset_names, {
        req(dataset())
        ds <- dataset()
        label_map(setNames(ds$orig_names, ds$internal_names))
    })
    
    # -------------------------
    # Tabla editable (Parámetros)
    # -------------------------
    output$rename_table <- renderDT({
        req(dataset(), label_map())
        
        ds <- dataset()
        lm <- label_map()
        
        df_names <- tibble::tibble(
            variable_en_archivo = ds$orig_names,
            nombre_mostrado     = unname(lm[ds$internal_names]),
            .internal_name      = ds$internal_names
        )
        
        DT::datatable(
            df_names %>% dplyr::select(variable_en_archivo, nombre_mostrado),
            rownames = FALSE,
            options = list(pageLength = 10, autoWidth = TRUE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        )
    })
    
    # Capturar edición y actualizar label_map
    observeEvent(input$rename_table_cell_edit, {
        req(dataset(), label_map())
        info <- input$rename_table_cell_edit
        ds <- dataset()
        lm <- label_map()
        
        internal <- ds$internal_names[info$row]
        new_label <- as.character(info$value)
        
        # Si queda vacío -> se mantiene el nombre original del archivo
        if (!nzchar(trimws(new_label))) {
            new_label <- ds$orig_names[info$row]
        }
        
        lm[internal] <- new_label
        label_map(lm)
    })
    
    # -------- Función auxiliar para construir cada TAB --------
    build_tab <- function(i) {
        
        # Selector de columnas (muestra etiquetas, devuelve nombres internos)
        output[[paste0("col_selector_", i)]] <- renderUI({
            req(dataset(), label_map())
            ds <- dataset()
            lm <- label_map()
            
            choices_named <- stats::setNames(ds$internal_names, unname(lm[ds$internal_names]))
            
            selectizeInput(
                inputId = paste0("cols_", i),
                label   = paste("Columnas a describir (TAB", i, "):"),
                choices = choices_named,
                selected = ds$internal_names,
                multiple = TRUE,
                options = list(
                    plugins = list("remove_button"),
                    placeholder = "Seleccione una o más columnas…",
                    maxItems = length(ds$internal_names)
                )
            )
        })
        
        # Selector de 'by' (opcional)
        output[[paste0("by_selector_", i)]] <- renderUI({
            req(dataset(), label_map())
            ds <- dataset()
            lm <- label_map()
            
            choices_named <- stats::setNames(ds$internal_names, unname(lm[ds$internal_names]))
            
            selectInput(
                inputId = paste0("by_", i),
                label   = "Variable de agrupación (by, opcional):",
                choices = c("— Sin 'by' —" = "", choices_named),
                selected = ""
            )
        })
        
        # Tabla gtsummary
        output[[paste0("gt_table_", i)]] <- gt::render_gt({
            req(dataset(), label_map())
            
            ds <- dataset()
            df_full <- ds$df
            lm <- label_map()
            
            # Variables seleccionadas (internas)
            sel <- input[[paste0("cols_", i)]]
            validate(need(!is.null(sel) && length(sel) > 0, "Seleccione al menos una columna."))
            sel <- sel[sel %in% names(df_full)]
            validate(need(length(sel) > 0, "Las columnas seleccionadas no existen en el dataset."))
            
            # Variable by (opcional)
            byvar <- input[[paste0("by_", i)]]
            has_by <- !is.null(byvar) && nzchar(byvar) && byvar %in% names(df_full)
            
            # Asegurar que el by esté incluido
            cols_to_use <- unique(c(sel, if (has_by) byvar))
            df_use <- df_full[, cols_to_use, drop = FALSE]
            
            # Convertir a factor SOLO: character/logical/factor (NO Date/POSIX)
            df_use <- df_use %>%
                dplyr::mutate(
                    across(
                        .cols = where(~ is.character(.x) || is.logical(.x) || is.factor(.x)),
                        .fns  = ~ as.factor(.x)
                    )
                )
            
            # Etiquetas según el usuario
            label_list <- as.list(unname(lm[names(df_use)]))
            names(label_list) <- names(df_use)
            
            # Estadísticas solicitadas
            stat_list <- list(
                gtsummary::all_continuous()  ~ "{mean} ({sd})",
                gtsummary::all_categorical() ~ "{n} ({p}%)"
            )
            
            # Base tbl_summary
            if (has_by) {
                tbl <- gtsummary::tbl_summary(
                    data = df_use,
                    by = byvar,
                    statistic = stat_list,
                    label = label_list,
                    missing = "ifany"
                ) %>%
                    gtsummary::add_n() %>%
                    gtsummary::add_p()
            } else {
                tbl <- gtsummary::tbl_summary(
                    data = df_use,
                    statistic = stat_list,
                    label = label_list,
                    missing = "ifany"
                ) %>%
                    gtsummary::add_n()
            }
            
            # =========================
            # NUEVO: IC95% para variables categóricas (opcional)
            # - Si el checkbox está marcado, agrega columna IC95% (solo categóricas)
            # =========================
            if (isTRUE(input$cat_ci95)) {
                tbl <- tbl %>%
                    gtsummary::add_ci(include = gtsummary::all_categorical()) %>%
                    gtsummary::modify_header(ci = "IC95%")  # nombre de columna
            }
            
            # Estilo
            tbl <- tbl %>% gtsummary::bold_labels()
            
            gtsummary::as_gt(tbl)
        })
    }
    
    lapply(1:5, build_tab)
}

shinyApp(ui = ui, server = server)
