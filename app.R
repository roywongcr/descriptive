
# ============================================================
# Shiny App: Tablas descriptivas + Parámetros + Gemini (ellmer)
# Modelo: gemini-2.5-flash
#
# FIXES:
# - Compilar resultados usa store (tbl_store) y NO depende del reactive actual.
# - "Overall/Global" -> "Total"
# - Spanning header SOLO a stat_1..stat_k (excluye stat_0 = Total)
# - Descarga Word: NO usa officer::body_add_flextable (no exportado en algunas versiones)
#   Usa SIEMPRE flextable::body_add_flextable con tryCatch y asegura .docx
# ============================================================

#setwd("C:/R_ANALYSIS/ShinyApp")

library(readxl)
library(tidyverse)
library(shiny)
library(gtsummary)
library(gt)
library(janitor)
library(DT)
library(ellmer)

library(officer)
library(flextable)

# =========================
# UI
# =========================

tab_parametros <- tabPanel(
    "Parámetros",
    h4("Renombrar variables"),
    p("Renombre variables para su visualización. Si no edita un nombre, se mantiene el del archivo."),
    DTOutput("rename_table"),
    tags$br(),
    actionButton("reset_names", "Restablecer nombres del archivo"),
    tags$hr(),
    
    h4("Formato e idioma"),
    numericInput("decimals", "Número de Decimales:", value = 1, min = 0, max = 4, step = 1),
    selectInput("lang", "Idioma:", choices = c("Español" = "es", "Inglés" = "en"), selected = "es"),
    tags$hr(),
    
    h4("Estimaciones"),
    h5("Variables categóricas"),
    tags$div(style="color:#6c757d; font-size:0.95em;", "(PE: Sexo, Provincia)"),
    tags$br(),
    checkboxInput("cat_ci95", "Incluir intervalo de confianza (IC) al 95%", value = FALSE),
    tags$hr(),
    
    h5("Variables cuantitativas"),
    tags$div(style="color:#6c757d; font-size:0.95em;", "(PE: Edad, IMC, Presión arterial)"),
    tags$br(),
    radioButtons("q_desc", "Medida descriptiva:",
                 choices = c("Media (promedio)" = "mean", "Mediana" = "median"),
                 selected = "mean", inline = TRUE),
    radioButtons("q_disp", "Medida de dispersión:",
                 choices = c("Desviación Estándar" = "sd",
                             "Rango" = "range",
                             "Intervalo Intercuartílico" = "iqr",
                             "IC95%" = "ci95"),
                 selected = "sd", inline = FALSE),
    tags$div(style="color:#6c757d; font-size:0.95em; margin-top:6px;",
             "Nota: Si selecciona IC95% como dispersión, se presentará una columna adicional de IC95% para variables cuantitativas.")
)

tab_list_1_5 <- lapply(1:5, function(i) {
    tabPanel(
        paste0("TAB ", i),
        uiOutput(paste0("col_selector_", i)),
        uiOutput(paste0("by_selector_", i)),
        uiOutput(paste0("cmp_selector_", i)),
        uiOutput(paste0("overall_selector_", i)),
        tags$hr(),
        textInput(paste0("title_", i), "Título del cuadro:", value = "Título del cuadro"),
        textInput(paste0("matrix_col_", i), "Nombre de la Columna matriz:", value = "Characteristics"),
        textInput(paste0("group_header_", i), "Nombre encabezado de grupo:", value = "Grupo"),
        textInput(paste0("source_", i), "Fuente:", value = "Null"),
        tags$br(),
        actionButton(paste0("gen_desc_", i), "Generar descripción"),
        tags$br(), tags$br(),
        uiOutput(paste0("desc_", i)),
        tags$hr(),
        gt::gt_output(paste0("gt_table_", i))
    )
})

tab_compilar <- tabPanel(
    "Compilar Resultados",
    tags$div(
        style="max-width:950px;",
        h4("Compilar resultados (TAB 1 a TAB 5)"),
        p("Visualice o descargue en Word todos los resultados (texto + tabla) en orden de TAB."),
        actionButton("view_compiled", "Visualizador"),
        tags$span(style="margin-left:10px;"),
        downloadButton("download_word", "Descarga en Word"),
        tags$hr(),
        tags$div(style="color:#6c757d;",
                 "Nota: Se compilan los TABs con una tabla válida guardada (última tabla construida).")
    )
)

tab_metodos <- tabPanel(
    "Métodos",
    tags$div(
        style="max-width:900px;",
        h4("Métodos de análisis"),
        p("Presione el botón para generar la sección de Métodos basada en los parámetros y tablas configuradas en las TABs de resultados."),
        actionButton("gen_methods", "Generar Métodos"),
        tags$br(), tags$br(),
        uiOutput("methods_text")
    )
)

tabs_main <- c(list(tab_parametros), tab_list_1_5, list(tab_compilar), list(tab_metodos))

ui <- fluidPage(
    titlePanel("Análisis Descriptivo Automatizado"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("fileType", "Tipo de archivo:", choices = c(".csv", ".xlsx"), selected = ".csv"),
            fileInput("uploadFile", "Subir base de datos", accept = c(".csv", ".xlsx")),
            tags$hr(),
            helpText("Use la pestaña 'Parámetros' para renombrar variables y ajustar estimaciones.")
        ),
        mainPanel(
            do.call(tabsetPanel, c(list(id = "tabs"), tabs_main))
        )
    )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
    
    # -------- Helpers --------
    get_decimals <- function() {
        d <- suppressWarnings(as.integer(input$decimals))
        if (is.na(d)) d <- 1L
        max(0L, min(4L, d))
    }
    get_lang <- function() {
        lang <- input$lang
        if (!lang %in% c("es", "en")) lang <- "es"
        lang
    }
    
    observeEvent(input$lang, {
        lang <- get_lang()
        dec_mark <- if (lang == "es") "," else "."
        tryCatch(gtsummary::theme_gtsummary_language(lang, decimal.mark = dec_mark),
                 error = function(e) NULL)
    }, ignoreInit = FALSE)
    
    # -------- GEMINI --------
    chat <- reactiveVal(NULL)
    
    make_google_chat <- function(key) {
        if (!nzchar(key)) return(NULL)
        ns <- asNamespace("ellmer")
        if (exists("chat_google_gemini", where = ns, inherits = FALSE)) {
            return(ellmer::chat_google_gemini(model = "gemini-2.5-flash", api_key = key, echo = "none"))
        }
        if (exists("chat_google", where = ns, inherits = FALSE)) {
            return(ellmer::chat_google(model = "gemini-2.5-flash", api_key = key))
        }
        stop("Tu versión de 'ellmer' no soporta Gemini. Actualiza con install.packages('ellmer').")
    }
    
    observeEvent(input$uploadFile, {
        key <- Sys.getenv("GOOGLE_API_KEY")
        if (!nzchar(key)) chat(NULL) else chat(tryCatch(make_google_chat(key), error = function(e) NULL))
    }, ignoreInit = TRUE)
    
    gemini_chat_text <- function(prompt) {
        ch <- chat()
        if (is.null(ch)) return("Gemini no está disponible. Verifique GOOGLE_API_KEY y vuelva a cargar el archivo.")
        resp <- tryCatch(ch$chat(prompt), error = function(e) paste0("Error Gemini: ", e$message))
        if (is.null(resp)) return("")
        if (is.character(resp)) return(paste(resp, collapse = "\n"))
        if (is.list(resp)) return(paste(unlist(resp), collapse = "\n"))
        as.character(resp)
    }
    
    make_table_text <- function(tbl_gts) {
        tb <- tryCatch(gtsummary::as_tibble(tbl_gts, col_labels = TRUE),
                       error = function(e) gtsummary::as_tibble(tbl_gts))
        tb <- tb %>% mutate(across(everything(), as.character))
        paste(capture.output(print(tb, n = Inf, width = Inf)), collapse = "\n")
    }
    
    # -------- DATASET + RENOMBRADO --------
    label_map <- reactiveVal(NULL)
    ds_cache  <- reactiveVal(NULL)
    
    observeEvent(input$uploadFile, {
        req(input$uploadFile)
        
        df_raw <- switch(
            input$fileType,
            ".csv"  = readr::read_csv(input$uploadFile$datapath, show_col_types = FALSE),
            ".xlsx" = readxl::read_excel(input$uploadFile$datapath)
        ) %>% tibble::as_tibble()
        
        orig_names <- names(df_raw)
        internal_names <- make.unique(janitor::make_clean_names(orig_names), sep = "_")
        
        df <- df_raw
        names(df) <- internal_names
        
        ds_cache(list(df = df, orig_names = orig_names, internal_names = internal_names))
        label_map(setNames(orig_names, internal_names))
    }, ignoreInit = FALSE)
    
    observeEvent(input$reset_names, {
        req(ds_cache())
        ds <- ds_cache()
        label_map(setNames(ds$orig_names, ds$internal_names))
    })
    
    output$rename_table <- renderDT({
        req(ds_cache(), label_map())
        ds <- ds_cache()
        lm <- label_map()
        
        df_names <- tibble::tibble(
            variable_en_archivo = ds$orig_names,
            nombre_mostrado     = unname(lm[ds$internal_names]),
            .internal_name      = ds$internal_names
        )
        
        DT::datatable(
            df_names,
            rownames = FALSE,
            options = list(pageLength = 10, autoWidth = TRUE,
                           columnDefs = list(list(targets = 2, visible = FALSE))),
            editable = list(target = "cell", disable = list(columns = c(0, 2)))
        )
    })
    
    observeEvent(input$rename_table_cell_edit, {
        req(ds_cache(), label_map())
        info <- input$rename_table_cell_edit
        ds <- ds_cache()
        lm <- label_map()
        if (info$col != 1) return(NULL)
        internal <- ds$internal_names[info$row]
        new_label <- as.character(info$value)
        if (!nzchar(trimws(new_label))) new_label <- ds$orig_names[info$row]
        lm[internal] <- new_label
        label_map(lm)
    })
    
    # -------- Construcción de tablas --------
    build_tbl_summary <- function(df_use, byvar = NULL, label_list = NULL,
                                  do_compare = FALSE, include_overall = TRUE,
                                  group_header = "Grupo") {
        
        dec  <- get_decimals()
        lang <- get_lang()
        
        df_use <- df_use %>%
            mutate(across(where(~ is.character(.x) || is.logical(.x) || is.factor(.x)), as.factor))
        
        desc_choice <- input$q_desc
        disp_choice <- input$q_disp
        
        cont_stat <- {
            if (disp_choice == "ci95") {
                if (desc_choice == "mean") "{mean}" else "{median}"
            } else if (desc_choice == "mean" && disp_choice == "sd") {
                "{mean} ({sd})"
            } else if (desc_choice == "mean" && disp_choice == "range") {
                "{mean} ({min}, {max})"
            } else if (desc_choice == "mean" && disp_choice == "iqr") {
                "{mean} ({p25}, {p75})"
            } else if (desc_choice == "median" && disp_choice == "sd") {
                "{median} ({sd})"
            } else if (desc_choice == "median" && disp_choice == "range") {
                "{median} ({min}, {max})"
            } else if (desc_choice == "median" && disp_choice == "iqr") {
                "{median} ({p25}, {p75})"
            } else {
                "{mean} ({sd})"
            }
        }
        
        stat_list <- list(
            all_continuous()  ~ cont_stat,
            all_categorical() ~ "{n} ({p}%)"
        )
        digits_list <- list(
            all_continuous()  ~ dec,
            all_categorical() ~ dec
        )
        
        has_by <- !is.null(byvar) && nzchar(byvar)
        
        if (has_by) {
            tbl <- tbl_summary(
                data = df_use,
                by = byvar,
                statistic = stat_list,
                digits = digits_list,
                label = label_list,
                missing = "ifany"
            ) %>% add_n()
            
            if (isTRUE(include_overall)) {
                tbl <- tbl %>%
                    add_overall() %>%
                    modify_header(stat_0 = "Total")
            }
            
            if (isTRUE(do_compare)) {
                tbl <- tbl %>% add_p(pvalue_fun = function(x) gtsummary::style_pvalue(x, digits = dec))
            }
            
            if (!is.null(group_header) && nzchar(trimws(group_header))) {
                tbl <- tbl %>% modify_spanning_header(matches("^stat_[1-9]") ~ group_header)
            }
            
        } else {
            tbl <- tbl_summary(
                data = df_use,
                statistic = stat_list,
                digits = digits_list,
                label = label_list,
                missing = "ifany"
            ) %>% add_n()
        }
        
        ci_include <- NULL
        if (isTRUE(input$cat_ci95)) ci_include <- c(ci_include, all_categorical())
        if (identical(disp_choice, "ci95")) ci_include <- c(ci_include, all_continuous())
        
        if (!is.null(ci_include)) {
            tbl <- tbl %>%
                add_ci(include = ci_include) %>%
                modify_header(ci = if (lang == "es") "IC95%" else "95% CI")
        }
        
        if (!isTRUE(do_compare) && "p.value" %in% names(tbl$table_body)) {
            tbl$table_body <- tbl$table_body %>% select(-p.value)
            if (!is.null(tbl$table_styling$header)) {
                tbl$table_styling$header <- tbl$table_styling$header %>% filter(column != "p.value")
            }
        }
        
        if ("p.value" %in% names(tbl$table_body)) {
            tbl <- tbl %>% modify_header(p.value = if (lang == "es") "Valor p" else "p-value")
        }
        
        tbl %>% bold_labels()
    }
    
    # -------- Descripciones + Store --------
    desc_vals <- lapply(1:5, function(i) reactiveVal(""))
    tbl_store <- lapply(1:5, function(i) reactiveVal(NULL))
    
    make_gt_with_meta <- function(i, tbl_gts) {
        lang <- get_lang()
        
        title_txt <- input[[paste0("title_", i)]]
        if (is.null(title_txt) || !nzchar(trimws(title_txt))) title_txt <- "Título del cuadro"
        
        source_txt <- input[[paste0("source_", i)]]
        if (is.null(source_txt) || !nzchar(trimws(source_txt))) source_txt <- "Null"
        
        prefix_source <- if (lang == "es") "Fuente: " else "Source: "
        
        gtsummary::as_gt(tbl_gts) %>%
            gt::tab_header(title = gt::md(title_txt)) %>%
            gt::tab_source_note(source_note = gt::md(paste0(prefix_source, source_txt)))
    }
    
    # -------- TABS 1-5 --------
    tab_tbls <- vector("list", 5)
    
    build_tab <- function(i) {
        
        output[[paste0("col_selector_", i)]] <- renderUI({
            req(ds_cache(), label_map())
            ds <- ds_cache()
            lm <- label_map()
            choices_named <- setNames(ds$internal_names, unname(lm[ds$internal_names]))
            
            selectizeInput(
                inputId = paste0("cols_", i),
                label   = paste("Columnas a describir (TAB", i, "):"),
                choices = choices_named,
                selected = ds$internal_names,
                multiple = TRUE,
                options = list(plugins = list("remove_button"),
                               placeholder = "Seleccione una o más columnas…",
                               maxItems = length(ds$internal_names))
            )
        })
        
        output[[paste0("by_selector_", i)]] <- renderUI({
            req(ds_cache(), label_map())
            ds <- ds_cache()
            lm <- label_map()
            choices_named <- setNames(ds$internal_names, unname(lm[ds$internal_names]))
            
            selectInput(
                inputId = paste0("by_", i),
                label   = "Variable de agrupación (by, opcional):",
                choices = c("— Sin 'by' —" = "", choices_named),
                selected = ""
            )
        })
        
        output[[paste0("cmp_selector_", i)]] <- renderUI({
            byv <- input[[paste0("by_", i)]]
            if (is.null(byv) || !nzchar(byv)) return(NULL)
            checkboxInput(paste0("do_compare_", i),
                          "Comparación con prueba estadística (valor p)",
                          value = TRUE)
        })
        
        output[[paste0("overall_selector_", i)]] <- renderUI({
            byv <- input[[paste0("by_", i)]]
            if (is.null(byv) || !nzchar(byv)) return(NULL)
            checkboxInput(paste0("include_overall_", i),
                          "Incluir totales",
                          value = TRUE)
        })
        
        tab_tbls[[i]] <- reactive({
            req(ds_cache(), label_map())
            ds <- ds_cache()
            df_full <- ds$df
            lm <- label_map()
            
            sel <- input[[paste0("cols_", i)]]
            validate(need(!is.null(sel) && length(sel) > 0, "Seleccione al menos una columna."))
            
            sel <- sel[sel %in% names(df_full)]
            validate(need(length(sel) > 0, "Las columnas seleccionadas no existen en el dataset."))
            
            byvar <- input[[paste0("by_", i)]]
            has_by <- !is.null(byvar) && nzchar(byvar) && byvar %in% names(df_full)
            
            do_compare <- if (has_by) isTRUE(input[[paste0("do_compare_", i)]]) else FALSE
            include_overall <- if (has_by) isTRUE(input[[paste0("include_overall_", i)]]) else FALSE
            
            cols_to_use <- unique(c(sel, if (has_by) byvar))
            df_use <- df_full[, cols_to_use, drop = FALSE]
            
            label_list <- as.list(unname(lm[names(df_use)]))
            names(label_list) <- names(df_use)
            
            group_header <- input[[paste0("group_header_", i)]]
            if (is.null(group_header) || !nzchar(trimws(group_header))) {
                group_header <- if (get_lang() == "es") "Grupo" else "Group"
            }
            
            tbl <- build_tbl_summary(
                df_use,
                byvar = if (has_by) byvar else NULL,
                label_list = label_list,
                do_compare = do_compare,
                include_overall = include_overall,
                group_header = group_header
            )
            
            matrix_name <- input[[paste0("matrix_col_", i)]]
            if (is.null(matrix_name) || !nzchar(trimws(matrix_name))) matrix_name <- "Characteristics"
            
            tbl %>% modify_header(label = matrix_name)
        })
        
        observe({
            req(ds_cache(), label_map())
            tbl_try <- tryCatch(tab_tbls[[i]](), error = function(e) NULL)
            if (!is.null(tbl_try)) tbl_store[[i]](tbl_try)
        })
        
        output[[paste0("gt_table_", i)]] <- gt::render_gt({
            req(tab_tbls[[i]]())
            make_gt_with_meta(i, tab_tbls[[i]]())
        })
        
        output[[paste0("desc_", i)]] <- renderUI({
            txt <- desc_vals[[i]]()
            if (!nzchar(trimws(txt))) return(NULL)
            tags$div(
                style = "background:#f8f9fa; border:1px solid #e9ecef; padding:12px; border-radius:8px;",
                tags$b("Descripción (Gemini):"),
                tags$br(), tags$br(),
                tags$div(style = "white-space:pre-wrap;", txt)
            )
        })
        
        observeEvent(input[[paste0("gen_desc_", i)]], {
            tbl_now <- tryCatch(tab_tbls[[i]](), error = function(e) NULL)
            if (is.null(tbl_now)) tbl_now <- tbl_store[[i]]()
            req(tbl_now)
            
            desc_vals[[i]]("Generando descripción...")
            tabla_texto <- make_table_text(tbl_now)
            
            prompt <- paste0(
                "Actúe como epidemiólogo y estadístico. ",
                "Redacte un párrafo profesional y claro basado en la siguiente tabla de resultados descriptivos. ",
                "Si hay comparación por grupos, mencione diferencias relevantes y valores p si aparecen. ",
                "No incluya títulos ni introducción.\n\n",
                "TABLA:\n", tabla_texto
            )
            desc_vals[[i]](gemini_chat_text(prompt))
        }, ignoreInit = TRUE)
    }
    
    lapply(1:5, build_tab)
    
    # ==========================================================
    # COMPILAR RESULTADOS
    # ==========================================================
    lapply(1:5, function(i) {
        output[[paste0("gt_comp_", i)]] <- gt::render_gt({
            req(ds_cache(), label_map())
            tbl <- tbl_store[[i]]()
            validate(need(!is.null(tbl), ""))
            make_gt_with_meta(i, tbl)
        })
    })
    
    output$compiled_view <- renderUI({
        req(ds_cache(), label_map())
        
        blocks <- lapply(1:5, function(i) {
            tbl_gts <- tbl_store[[i]]()
            if (is.null(tbl_gts)) return(NULL)
            
            title_txt <- input[[paste0("title_", i)]]
            if (is.null(title_txt) || !nzchar(trimws(title_txt))) title_txt <- paste0("TAB ", i)
            
            desc_txt <- desc_vals[[i]]()
            has_desc <- nzchar(trimws(desc_txt)) && !identical(desc_txt, "Generando descripción...")
            
            tags$div(
                style = "margin-bottom: 26px;",
                tags$h4(style = "margin-bottom:10px;", paste0("TAB ", i, ": ", title_txt)),
                if (has_desc) tags$div(
                    style = "background:#f8f9fa; border:1px solid #e9ecef; padding:12px; border-radius:8px; margin-bottom:10px;",
                    tags$b("Texto:"),
                    tags$br(), tags$br(),
                    tags$div(style = "white-space:pre-wrap;", desc_txt)
                ) else NULL,
                gt::gt_output(paste0("gt_comp_", i)),
                tags$hr()
            )
        })
        
        blocks <- Filter(Negate(is.null), blocks)
        
        if (length(blocks) == 0) {
            return(tags$div(
                style = "color:#6c757d;",
                "No hay TABs para compilar. Abra al menos un TAB (1–5) para que se guarde la tabla y luego vuelva a Compilar Resultados."
            ))
        }
        
        do.call(tagList, blocks)
    })
    
    observeEvent(input$view_compiled, {
        showModal(modalDialog(
            title = "Compilación de Resultados (TAB 1 a TAB 5)",
            size = "l",
            easyClose = TRUE,
            footer = modalButton("Cerrar"),
            uiOutput("compiled_view")
        ))
    }, ignoreInit = TRUE)
    
    # ==========================================================
    # DESCARGA WORD (CORREGIDO)
    # ==========================================================
    output$download_word <- downloadHandler(
        filename = function() paste0("Resultados_", format(Sys.Date(), "%Y%m%d"), ".docx"),
        contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        content = function(file) {
            
            if (!grepl("\\.docx$", file, ignore.case = TRUE)) file <- paste0(file, ".docx")
            
            lang <- get_lang()
            src_prefix <- if (lang == "es") "Fuente: " else "Source: "
            
            safe_docx <- function(msg) {
                d <- officer::read_docx()
                d <- officer::body_add_par(d, msg, style = "Normal")
                print(d, target = file)
            }
            
            tryCatch({
                
                doc <- officer::read_docx()
                included_any <- FALSE
                
                for (i in 1:5) {
                    tbl_gts <- tbl_store[[i]]()
                    if (is.null(tbl_gts)) next
                    
                    included_any <- TRUE
                    
                    title_txt <- input[[paste0("title_", i)]]
                    if (is.null(title_txt) || !nzchar(trimws(title_txt))) title_txt <- paste0("TAB ", i)
                    
                    source_txt <- input[[paste0("source_", i)]]
                    if (is.null(source_txt) || !nzchar(trimws(source_txt))) source_txt <- "Null"
                    
                    desc_txt <- desc_vals[[i]]()
                    has_desc <- nzchar(trimws(desc_txt)) && !identical(desc_txt, "Generando descripción...")
                    
                    doc <- officer::body_add_par(doc, paste0("TAB ", i, ": ", title_txt), style = "heading 1")
                    
                    if (has_desc) {
                        doc <- officer::body_add_par(doc, "Texto:", style = "heading 2")
                        doc <- officer::body_add_par(doc, desc_txt, style = "Normal")
                    }
                    
                    doc <- officer::body_add_par(doc, "Tabla:", style = "heading 2")
                    
                    ft <- tryCatch(gtsummary::as_flex_table(tbl_gts), error = function(e) NULL)
                    
                    if (is.null(ft)) {
                        doc <- officer::body_add_par(
                            doc,
                            "No fue posible convertir la tabla a formato Word (flextable).",
                            style = "Normal"
                        )
                    } else {
                        ft <- flextable::autofit(ft)
                        
                        # INSERTAR SIEMPRE con flextable (NO officer)
                        doc <- tryCatch(
                            flextable::body_add_flextable(doc, value = ft),
                            error = function(e) {
                                officer::body_add_par(
                                    doc,
                                    paste0("Error al insertar la tabla en Word: ", e$message),
                                    style = "Normal"
                                )
                            }
                        )
                    }
                    
                    doc <- officer::body_add_par(doc, paste0(src_prefix, source_txt), style = "Normal")
                    doc <- officer::body_add_par(doc, " ", style = "Normal")
                }
                
                if (!included_any) {
                    doc <- officer::body_add_par(doc, "No se encontraron TABs con resultados para compilar.", style = "Normal")
                }
                
                print(doc, target = file)
                
            }, error = function(e) {
                safe_docx(paste0("Error al generar el Word: ", e$message))
            })
        }
    )
    
    # ==========================================================
    # MÉTODOS
    # ==========================================================
    methods_val <- reactiveVal("")
    
    output$methods_text <- renderUI({
        txt <- methods_val()
        if (!nzchar(trimws(txt))) return(NULL)
        tags$div(
            style = "background:#f8f9fa; border:1px solid #e9ecef; padding:12px; border-radius:8px;",
            tags$b("Métodos (Gemini):"),
            tags$br(), tags$br(),
            tags$div(style = "white-space:pre-wrap;", txt)
        )
    })
    
    observeEvent(input$gen_methods, {
        req(ds_cache(), label_map())
        methods_val("Generando métodos...")
        
        lang <- get_lang()
        dec  <- get_decimals()
        desc_choice <- input$q_desc
        disp_choice <- input$q_disp
        cat_ci <- isTRUE(input$cat_ci95)
        
        cont_desc_es <- if (disp_choice == "ci95") {
            if (desc_choice == "mean") "media con IC95% (columna adicional de IC)" else "mediana con IC95% (columna adicional de IC)"
        } else if (desc_choice == "mean" && disp_choice == "sd") "media (desviación estándar)"
        else if (desc_choice == "mean" && disp_choice == "range") "media (mínimo, máximo)"
        else if (desc_choice == "mean" && disp_choice == "iqr") "media (percentiles 25 y 75)"
        else if (desc_choice == "median" && disp_choice == "sd") "mediana (desviación estándar)"
        else if (desc_choice == "median" && disp_choice == "range") "mediana (mínimo, máximo)"
        else if (desc_choice == "median" && disp_choice == "iqr") "mediana (rango intercuartílico; percentiles 25 y 75)"
        else "media (desviación estándar)"
        
        cont_desc_en <- if (disp_choice == "ci95") {
            if (desc_choice == "mean") "mean with 95% CI (additional CI column)" else "median with 95% CI (additional CI column)"
        } else if (desc_choice == "mean" && disp_choice == "sd") "mean (standard deviation)"
        else if (desc_choice == "mean" && disp_choice == "range") "mean (minimum, maximum)"
        else if (desc_choice == "mean" && disp_choice == "iqr") "mean (25th, 75th percentiles)"
        else if (desc_choice == "median" && disp_choice == "sd") "median (standard deviation)"
        else if (desc_choice == "median" && disp_choice == "range") "median (minimum, maximum)"
        else if (desc_choice == "median" && disp_choice == "iqr") "median (interquartile range; 25th, 75th percentiles)"
        else "mean (standard deviation)"
        
        cat_desc_es <- if (cat_ci) "frecuencias y porcentajes, con IC95% cuando fue seleccionado" else "frecuencias y porcentajes"
        cat_desc_en <- if (cat_ci) "counts and percentages, with 95% confidence intervals when selected" else "counts and percentages"
        
        prompt_methods <- if (lang == "es") {
            paste0(
                "Actúe como estadístico experto en redacción de artículos científicos.\n",
                "Redacte la sección de Métodos de análisis estadístico (solo métodos), en español, en tiempo pasado.\n",
                "Describa EXACTAMENTE:\n",
                "1) Tablas descriptivas en R con RStudio (última versión disponible al momento del análisis).\n",
                "2) Continuas: ", cont_desc_es, ".\n",
                "3) Categóricas: ", cat_desc_es, ".\n",
                "4) Redondeo uniforme de ", dec, " decimales para porcentajes, estimaciones, rangos, IC y valores p.\n",
                "5) Cuando hubo agrupación, se mostraron resultados por grupo; si se seleccionó, se incluyó columna Total.\n",
                "6) Cuando se activó comparación, se calcularon valores p con pruebas apropiadas sin especificar nombres.\n",
                "7) Se incluyeron conteos (n) y se manejaron datos faltantes cuando correspondió.\n",
                "8) Si se seleccionó, se reportaron IC95%.\n\n",
                "Salida: 1-2 párrafos, sin títulos, sin mencionar Shiny ni botones."
            )
        } else {
            paste0(
                "Act as an expert biostatistician writing for a scientific manuscript.\n",
                "Write the Statistical Analysis Methods section (methods only), in English, in past tense.\n",
                "Describe EXACTLY:\n",
                "1) Descriptive tables in R using RStudio (latest available version at time of analysis).\n",
                "2) Continuous: ", cont_desc_en, ".\n",
                "3) Categorical: ", cat_desc_en, ".\n",
                "4) Uniform rounding of ", dec, " decimals for percentages, estimates, ranges, CIs and p-values.\n",
                "5) When grouped, group-wise results were shown; if selected, a Total column was included.\n",
                "6) When comparisons were enabled, p-values were computed with appropriate tests without naming them.\n",
                "7) Counts (n) included and missing handled when present.\n",
                "8) If selected, 95% CIs were reported.\n\n",
                "Output: 1–2 paragraphs, no headings, do not mention Shiny/buttons."
            )
        }
        
        methods_val(gemini_chat_text(prompt_methods))
    }, ignoreInit = TRUE)
    
}

shinyApp(ui = ui, server = server)








