library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(DT)
library(shinyWidgets)
library(tidyr)
library(purrr)
library(rmarkdown)
library(knitr)


sanitize_names <- function(nms) {
  nms <- as.character(nms)
  nms[is.na(nms)] <- ""
  nms <- trimws(nms)
  
  prazdne <- nms == ""
  if (any(prazdne)) {
    nms[prazdne] <- paste0("Unnamed_", seq_len(sum(prazdne)))
  }
  
  nms <- make.unique(nms, sep = "_dup_")
  nms
}



# -------------------------
# 1) HELPERS
# -------------------------
smart_read_csv <- function(path) {
  txt <- readLines(path, n = 5, warn = FALSE, encoding = "UTF-8")
  first <- paste(txt, collapse = "\n")
  semi_n <- stringr::str_count(first, ";")
  comma_n <- stringr::str_count(first, ",")

  if (semi_n > comma_n) {
    out <- tryCatch(
      read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
      error = function(e) read_delim(path, delim = ";", show_col_types = FALSE, name_repair = "minimal")
    )
  } else {
    out <- tryCatch(
      read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
      error = function(e) read_csv(path, show_col_types = FALSE, name_repair = "minimal")
    )
  }

  out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- sanitize_names(names(out))
  out
}

normalize_key <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[.]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower()
}

clean_question_label <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("[.]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

to_numeric <- function(x) parse_number(as.character(x), locale = locale(decimal_mark = ".", grouping_mark = " "))

is_good_numeric <- function(x_num, min_n = 5, min_share_parsed = 0.5) {
  n_total <- length(x_num)
  n_ok <- sum(!is.na(x_num))
  share <- if (n_total == 0) 0 else n_ok / n_total
  if (n_ok < min_n) return(FALSE)
  if (share < min_share_parsed) return(FALSE)
  if (length(unique(x_num[!is.na(x_num)])) <= 1) return(FALSE)
  TRUE
}

is_scale_1_5 <- function(x_num, min_n = 5) {
  x <- x_num[!is.na(x_num)]
  if (length(x) < min_n) return(FALSE)
  u <- sort(unique(x))
  all(u %in% 1:5) && length(u) >= 2
}

invert_1_5 <- function(x_num) ifelse(is.na(x_num), NA_real_, 6 - x_num)

numeric_summary <- function(x_num) {
  n_total <- length(x_num)
  n_ok <- sum(!is.na(x_num))
  mu <- if (n_ok > 0) mean(x_num, na.rm = TRUE) else NA_real_
  sdv <- if (n_ok > 1) sd(x_num, na.rm = TRUE) else NA_real_
  med <- if (n_ok > 0) median(x_num, na.rm = TRUE) else NA_real_
  pct_miss <- if (n_total > 0) (1 - n_ok / n_total) * 100 else NA_real_
  list(n = n_ok, mean = mu, sd = sdv, median = med, pct_missing = pct_miss)
}

extract_subject <- function(colname) {
  m <- str_match(colname, "\\(([^()]*)\\)\\s*$")
  subj <- m[, 2]
  ifelse(is.na(subj) | subj == "", "Obecné otázky", subj)
}

metadata_cols_present <- function(df) {
  nm <- names(df)
  nm_norm <- normalize_key(nm)
  idx_drop <- nm_norm %in% c("časová značka", "casova znacka", "emailová adresa", "emailova adresa") |
    str_detect(nm_norm, "^unnamed")
  nm[idx_drop]
}

class_col_detect <- function(df) {
  nm <- names(df)
  nm_norm <- normalize_key(nm)
  hit <- nm[which(nm_norm %in% c("do jaké třídy chodíte", "do jake tridy chodite"))]
  if (length(hit) > 0) return(hit[1])
  NULL
}

legend_file_default <- function() {
  p <- file.path(getwd(), "legenda.csv")
  if (file.exists(p)) p else NULL
}

load_legend <- function(path, questions) {
  base <- data.frame(
    question = questions,
    label_1 = NA_character_,
    label_5 = NA_character_,
    stringsAsFactors = FALSE
  )

  if (is.null(path) || str_trim(path) == "") return(base)
  if (!file.exists(path)) {
    tryCatch(save_legend(base, path), error = function(e) NULL)
    return(base)
  }

  lg <- smart_read_csv(path)
  names(lg) <- sanitize_names(clean_question_label(names(lg)))
  req_cols <- c("question", "label_1", "label_5")
  if (!all(req_cols %in% names(lg))) {
    tryCatch(save_legend(base, path), error = function(e) NULL)
    return(base)
  }

  lg <- lg %>%
    transmute(
      question = as.character(.data$question),
      label_1 = as.character(.data$label_1),
      label_5 = as.character(.data$label_5),
      key = normalize_key(.data$question)
    ) %>%
    distinct(key, .keep_all = TRUE)

  base$key <- normalize_key(base$question)
  out <- base %>%
    left_join(select(lg, key, label_1_file = label_1, label_5_file = label_5), by = "key") %>%
    mutate(
      label_1 = coalesce(label_1_file, label_1),
      label_5 = coalesce(label_5_file, label_5)
    ) %>%
    select(question, label_1, label_5)

  out
}

save_legend <- function(legend_df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(legend_df %>% select(question, label_1, label_5), path)
}

legend_text_for_question <- function(q, lt) {
  if (is.null(lt) || nrow(lt) == 0) return(NA_character_)
  qq <- normalize_key(q)
  hit <- lt %>% mutate(key = normalize_key(question)) %>% filter(key == qq)
  if (nrow(hit) == 0) return(NA_character_)
  l1 <- hit$label_1[1]
  l5 <- hit$label_5[1]
  if (all(is.na(c(l1, l5))) || (isTRUE(l1 == "") && isTRUE(l5 == ""))) return(NA_character_)
  paste0(coalesce(l1, ""), "  |  ", coalesce(l5, ""))
}

question_choices_grouped <- function(df) {
  cls <- class_col_detect(df)
  drops <- c(metadata_cols_present(df), cls)
  qs <- setdiff(names(df), drops)
  split(qs, extract_subject(qs))
}

check_plot_export_pkgs <- function(format) {
  pkgs <- c("rmarkdown", "knitr", "ggplot2")
  
  if (identical(format, "pdf")) {
    pkgs <- c(pkgs, "webshot2")
  }
  
  missing <- pkgs[
    !vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  
  if (length(missing) > 0) {
    stop(
      "Pro export chybí tyto balíčky: ",
      paste(missing, collapse = ", "),
      ". Nainstaluj je přes install.packages(...)."
    )
  }
}

export_ggplot_report <- function(plot_obj,
                                 file,
                                 title = "Výstup",
                                 subtitle = "",
                                 format = c("html", "pdf")) {
  format <- match.arg(format)
  check_plot_export_pkgs(format)
  
  subtitle <- if (is.null(subtitle)) "" else as.character(subtitle)
  
  workdir <- file.path(
    tempdir(),
    paste0("plot_report_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  )
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  
  plot_path <- file.path(workdir, "plot.rds")
  saveRDS(plot_obj, plot_path)
  
  rmd_path <- file.path(workdir, "plot_report.Rmd")
  html_path <- file.path(workdir, "report.html")
  pdf_path  <- file.path(workdir, "report.pdf")
  
  writeLines(
    c(
      "---",
      "title: \"Export grafu\"",
      "output:",
      "  html_document:",
      "    self_contained: true",
      "    toc: false",
      "params:",
      "  title: NULL",
      "  subtitle: NULL",
      "  plot_path: NULL",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(ggplot2)",
      "knitr::opts_chunk$set(",
      "  echo = FALSE,",
      "  message = FALSE,",
      "  warning = FALSE,",
      "  fig.width = 14,",
      "  fig.height = 9",
      ")",
      "```",
      "",
      "```{r nadpis, results='asis'}",
      "cat('# ', params$title, '\\n\\n', sep = '')",
      "if (!is.null(params$subtitle) && nzchar(params$subtitle)) {",
      "  cat(params$subtitle, '\\n\\n')",
      "}",
      "```",
      "",
      "```{r graf}",
      "p <- readRDS(params$plot_path)",
      "print(p)",
      "```"
    ),
    con = rmd_path,
    useBytes = TRUE
  )
  
  rmarkdown::render(
    input = rmd_path,
    output_format = "html_document",
    output_file = basename(html_path),
    output_dir = workdir,
    quiet = TRUE,
    params = list(
      title = title,
      subtitle = subtitle,
      plot_path = plot_path
    ),
    envir = new.env(parent = globalenv())
  )
  
  if (identical(format, "pdf")) {
    html_url <- paste0(
      "file:///",
      normalizePath(html_path, winslash = "/", mustWork = TRUE)
    )
    
    webshot2::webshot(
      url = html_url,
      file = pdf_path,
      delay = 1,
      vwidth = 1600,
      vheight = 1100
    )
    
    file.copy(pdf_path, file, overwrite = TRUE)
  } else {
    file.copy(html_path, file, overwrite = TRUE)
  }
}




# -------------------------
# 2) UI
# -------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f6f8fb; }
      .app-title { margin-bottom: 16px; }
      .app-title h2 { color: #1f3c88; font-weight: 700; margin-bottom: 4px; }
      .app-title p { color: #5b657a; margin-bottom: 0; }
      .well { background: #ffffff; border: 1px solid #e3e8f2; border-radius: 14px; box-shadow: 0 4px 14px rgba(31,60,136,0.06); }
      .nav-tabs { border-bottom: 1px solid #dfe5f0; }
      .nav-tabs > li > a { color: #33415c; font-weight: 600; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover { background: #ffffff; color: #1f3c88; border-top: 3px solid #1f3c88; }
      .control-label, h4 { color: #23314d; font-weight: 700; }
      .btn-default, .btn-primary { border-radius: 10px; }
      .irs-bar, .irs-bar-edge, .irs-single { background: #3b82f6; border-color: #3b82f6; }
      .irs-from, .irs-to, .irs-single { background: #1f3c88; }
    "))
  ),
  div(
    class = "app-title",
    h2("Přetížení – analytická Shiny aplikace"),
    p("Přehled otázek, srovnání tříd, korelace a otevřené odpovědi na jednom místě.")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Nahraj CSV s daty:", accept = c(".csv")),
      tags$small("Pokud nic nenahraješ, aplikace se pokusí použít data_pretizeni/data_pretizenik z pracovní složky."),
      hr(),
      uiOutput("class_filter_ui"),
      hr(),
      selectInput("subject_filter", "Předmět:", choices = "Vše", selected = "Vše"),
      pickerInput(
        "questions",
        "Vyber otázky:",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3",
          size = 15,
          `live-search-placeholder` = "Piš název otázky nebo předmět..."
        )
      ),
      checkboxInput("facet_by_class", "Rozdělit podle tříd", value = FALSE),
      sliderInput("top_n", "TOP kategorií (zbytek = Other):", min = 4, max = 20, value = 10, step = 1),
      hr(),
      h4("Škály 1–5"),
      pickerInput(
        "invert_questions",
        "Invertovat (1 ↔ 5):",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      hr(),
      h4("Korelace a statistiky"),
      pickerInput(
        "vars_corr",
        "Proměnné pro korelace:",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      sliderInput("absr_range", "|r| interval:", min = 0, max = 1, value = c(0, 1), step = 0.01),
      pickerInput(
        "vars_stats",
        "Proměnné pro přehled statistik:",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      hr(),
      h4("Legenda"),
      textInput("legend_path", "Cesta k legenda.csv:", value = legend_file_default()),
      fluidRow(
        column(6, actionButton("reload_legend", "Načíst legendu", width = "100%")),
        column(6, actionButton("save_legend", "Uložit legendu", width = "100%"))
      ),
      br(),
      textOutput("legend_status")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Histogramy",
          fluidRow(
            column(
              4,
              selectInput(
                "hist_export_format",
                "Formát exportu:",
                choices = c("HTML" = "html", "PDF" = "pdf"),
                selected = "html"
              )
            ),
            column(
              4,
              br(),
              downloadButton("hist_download", "Stáhnout histogramy")
            )
          ),
          plotOutput("hist_grid", height = "900px")
        ),
        
        tabPanel(
          "Boxploty",
          fluidRow(
            column(
              4,
              selectInput(
                "box_export_format",
                "Formát exportu:",
                choices = c("HTML" = "html", "PDF" = "pdf"),
                selected = "html"
              )
            ),
            column(
              4,
              br(),
              downloadButton("box_download", "Stáhnout boxploty")
            )
          ),
          plotOutput("box_all", height = "900px")
        ),
        tabPanel("Korelace & regrese",
                 h4("Páry proměnných"),
                 DTOutput("pairs_dt"),
                 hr(),
                 h4("Korelační matice"),
                 DTOutput("corr_dt"),
                 hr(),
                 fluidRow(
                   column(4, selectInput("reg_x", "X proměnná:", choices = NULL)),
                   column(4, selectInput("reg_y", "Y proměnná:", choices = NULL)),
                   column(4, br(), actionButton("run_reg", "Spočítat regresi", width = "100%"))
                 ),
                 br(),
                 plotOutput("reg_plot", height = "380px"),
                 verbatimTextOutput("reg_summary")
        ),
        tabPanel("Přehled statistik", DTOutput("stats_dt")),
        tabPanel("Legenda", DTOutput("legend_dt")),
        tabPanel("Otevřené odpovědi", uiOutput("open_ui")),
        tabPanel("Diagnostika", verbatimTextOutput("diag"))
      )
    )
  )
)

# -------------------------
# 3) SERVER
# -------------------------
server <- function(input, output, session) {
  dat <- reactiveVal(NULL)
  legend_override <- reactiveVal(NULL)
  legend_status <- reactiveVal("Legenda zatím nenačtena.")

  load_data_into_app <- function(df) {
    names(df) <- sanitize_names(names(df))
    dat(df)
    cls <- class_col_detect(df)
    drops <- c(metadata_cols_present(df), cls)
    questions <- setdiff(names(df), drops)
    lg <- load_legend(input$legend_path, questions)
    legend_override(lg)
  }

  observe({
    if (!is.null(input$file)) return()

    candidate_paths <- c(
      file.path(getwd(), "data_pretizeni.csv"),
      file.path(getwd(), "data_pretizenik.csv"),
      "/mnt/data/data_pretizenik.csv"
    )
    candidate_paths <- unique(candidate_paths[file.exists(candidate_paths)])
    if (length(candidate_paths) == 0) return()

    df <- smart_read_csv(candidate_paths[1])
    load_data_into_app(df)
  })

  observeEvent(input$file, {
    req(input$file)
    df <- smart_read_csv(input$file$datapath)
    load_data_into_app(df)
  })

  observeEvent(input$reload_legend, {
    df <- dat()
    req(df)
    cls <- class_col_detect(df)
    drops <- c(metadata_cols_present(df), cls)
    questions <- setdiff(names(df), drops)
    lg <- load_legend(input$legend_path, questions)
    legend_override(lg)
    if (file.exists(input$legend_path)) {
      legend_status(paste("Legenda načtena z:", input$legend_path))
    } else {
      legend_status(paste("Legenda vytvořena jako šablona v:", input$legend_path))
    }
  }, ignoreInit = FALSE)

  observeEvent(input$save_legend, {
    lg <- legend_override()
    req(lg)
    tryCatch({
      save_legend(lg, input$legend_path)
      legend_status(paste("Legenda uložena do:", input$legend_path))
    }, error = function(e) {
      legend_status(paste("Uložení legendy selhalo:", e$message))
    })
  })

  output$legend_status <- renderText(legend_status())

  output$class_filter_ui <- renderUI({
    df <- dat()
    req(df)
    cls <- class_col_detect(df)
    validate(need(!is.null(cls), "V datech chybí sloupec 'Do jaké třídy chodíte?'."))
    
    classes <- sort(unique(na.omit(trimws(as.character(df[[cls]])))))
    checkboxGroupInput("classes", "Z jakých tříd chceš data?", choices = classes, selected = classes)
  })

  filtered_df <- reactive({
    df <- dat()
    req(df)
    cls <- class_col_detect(df)
    req(cls)
    req(input$classes)
    
    class_vec <- trimws(as.character(df[[cls]]))
    keep <- class_vec %in% trimws(input$classes)
    df[keep %in% TRUE, , drop = FALSE]
  })

  question_groups <- reactive({
    df <- dat()
    req(df)
    question_choices_grouped(df)
  })
  
  observe({
    groups <- question_groups()
    req(groups)
    predmety <- names(groups)
    
    selected_subject <- isolate(input$subject_filter)
    if (is.null(selected_subject) || !selected_subject %in% c("Vše", predmety)) {
      selected_subject <- "Vše"
    }
    
    updateSelectInput(
      session,
      "subject_filter",
      choices = c("Vše", predmety),
      selected = selected_subject
    )
  })
  
  observe({
    groups <- question_groups()
    req(groups)
    
    shown_groups <- groups
    if (!is.null(input$subject_filter) && input$subject_filter != "Vše") {
      shown_groups <- groups[names(groups) %in% input$subject_filter]
    }
    
    current <- isolate(input$questions)
    valid_selected <- intersect(current, unlist(shown_groups, use.names = FALSE))
    updatePickerInput(session, "questions", choices = shown_groups, selected = valid_selected)
  })
  

  question_info <- reactive({
    df <- filtered_df()
    req(df)
    qs <- input$questions
    if (is.null(qs) || length(qs) == 0) return(list())

    out <- lapply(qs, function(q) {
      x_raw <- df[[q]]
      x_chr <- as.character(x_raw)
      x_num <- suppressWarnings(to_numeric(x_raw))
      q_norm <- normalize_key(q)

      if (str_detect(q_norm, "okomentovat")) {
        list(type = "open", x_num = NULL, x_chr = x_chr, is_scale = FALSE)
      } else if (is_good_numeric(x_num)) {
        list(type = "numeric", x_num = x_num, x_chr = x_chr, is_scale = is_scale_1_5(x_num))
      } else {
        list(type = "categorical", x_num = NULL, x_chr = x_chr, is_scale = FALSE)
      }
    })
    names(out) <- qs
    out
  })

  observe({
    qi <- question_info()
    if (length(qi) == 0) {
      updatePickerInput(session, "invert_questions", choices = character())
      return()
    }
    scale_qs <- names(qi)[vapply(qi, function(z) z$type == "numeric" && z$is_scale, logical(1))]
    updatePickerInput(session, "invert_questions", choices = scale_qs,
                      selected = intersect(input$invert_questions, scale_qs))
  })

  legend_lookup <- reactive({
    lo <- legend_override()
    if (is.null(lo)) return(NULL)
    inv <- input$invert_questions
    inv <- if (is.null(inv)) character() else inv
    lo %>% mutate(
      label_1_display = ifelse(question %in% inv, label_5, label_1),
      label_5_display = ifelse(question %in% inv, label_1, label_5)
    )
  })

  # Histogramy / categorical bars
  hist_plot <- reactive({
    df <- filtered_df()
    qi <- question_info()
    lt <- legend_lookup()
    req(df)
    validate(need(length(qi) > 0, "Vyber otázky."))
    
    facet_class <- isTRUE(input$facet_by_class)
    class_col <- class_col_detect(df)
    top_n <- input$top_n
    class_levels <- if (facet_class) input$classes else "ALL"
    
    num_qs <- names(qi)[vapply(qi, function(z) z$type == "numeric", logical(1))]
    cat_qs <- names(qi)[vapply(qi, function(z) z$type == "categorical", logical(1))]
    
    d_scale <- tibble()
    stat_scale <- tibble()
    
    if (length(num_qs) > 0) {
      d_scale <- bind_rows(lapply(num_qs, function(q) {
        x <- qi[[q]]$x_num
        if (qi[[q]]$is_scale && !is.null(input$invert_questions) && q %in% input$invert_questions) {
          x <- invert_1_5(x)
        }
        tibble(
          question = q,
          class = if (facet_class) trimws(as.character(df[[class_col]])) else "ALL",
          value = x
        )
      })) %>%
        filter(!is.na(value)) %>%
        mutate(
          class = factor(class, levels = class_levels),
          value = factor(value, levels = sort(unique(value)))
        ) %>%
        count(question, class, value, name = "n") %>%
        group_by(question, class) %>%
        mutate(p = 100 * n / sum(n)) %>%
        ungroup() %>%
        mutate(type = "Numerické")
      
      stat_scale <- bind_rows(lapply(num_qs, function(q) {
        x <- qi[[q]]$x_num
        if (qi[[q]]$is_scale && !is.null(input$invert_questions) && q %in% input$invert_questions) {
          x <- invert_1_5(x)
        }
        tmp <- tibble(
          question = q,
          class = if (facet_class) trimws(as.character(df[[class_col]])) else "ALL",
          value = x
        )
        tmp %>%
          filter(!is.na(value)) %>%
          group_by(question, class) %>%
          summarise(
            n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            .groups = "drop"
          )
      })) %>%
        mutate(
          class = factor(class, levels = class_levels),
          label = paste0("n=", n, "\nmean=", signif(mean, 3), "\nsd=", signif(sd, 3)),
          legend = map_chr(question, ~coalesce(legend_text_for_question(.x, lt), "")),
          panel = clean_question_label(question)
        )
    }
    
    d_cat <- tibble()
    if (length(cat_qs) > 0) {
      d_cat <- bind_rows(lapply(cat_qs, function(q) {
        x <- str_trim(as.character(qi[[q]]$x_chr))
        x[is.na(x) | x == ""] <- "(missing)"
        x <- fct_lump_n(factor(x), n = top_n, other_level = "Other")
        tibble(
          question = q,
          class = if (facet_class) trimws(as.character(df[[class_col]])) else "ALL",
          value = x
        )
      })) %>%
        mutate(class = factor(class, levels = class_levels)) %>%
        count(question, class, value, name = "n") %>%
        group_by(question, class) %>%
        mutate(p = 100 * n / sum(n)) %>%
        ungroup() %>%
        mutate(type = "Kategorické")
    }
    
    d_all <- bind_rows(d_scale, d_cat)
    validate(need(nrow(d_all) > 0, "Není co vykreslit."))
    
    d_all <- d_all %>%
      mutate(panel = clean_question_label(question))
    
    p <- ggplot(d_all, aes(x = value, y = n)) +
      labs(x = NULL, y = "Absolutní četnost") +
      theme_minimal(base_size = 12) +
      theme(
        strip.text = element_text(size = 10, face = "bold", colour = "#23314d"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 20, 10, 10),
        panel.grid.minor = element_blank(),
        legend.position = if (facet_class) "top" else "none"
      )
    
    if (facet_class) {
      p <- p +
        geom_col(aes(fill = class), position = position_dodge(width = 0.85), width = 0.75) +
        geom_text(
          aes(label = sprintf("%.1f%%", p), group = class),
          position = position_dodge(width = 0.85),
          vjust = -0.2,
          size = 3
        )
    } else {
      p <- p +
        geom_col(width = 0.75) +
        geom_text(aes(label = sprintf("%.1f%%", p)), vjust = -0.2, size = 3)
    }
    
    if (nrow(stat_scale) > 0) {
      p <- p +
        geom_text(
          data = stat_scale,
          aes(x = Inf, y = Inf, label = paste(label, legend, sep = "\n")),
          inherit.aes = FALSE,
          hjust = 1.05, vjust = 1.1,
          size = 3
        )
    }
    
    p + facet_wrap(type ~ panel, scales = "free_y", ncol = 2)
  })

  output$hist_grid <- renderPlot({
    req(hist_plot())
    hist_plot()
  })
  
  output$hist_download <- downloadHandler(
    filename = function() {
      ext <- input$hist_export_format
      paste0("histogramy-", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(hist_plot())
      
      subtitle <- paste0(
        "Vybrané třídy: ",
        paste(input$classes, collapse = ", "),
        "<br>",
        "Rozdělení podle tříd: ",
        ifelse(isTRUE(input$facet_by_class), "ano", "ne")
      )
      
      export_ggplot_report(
        plot_obj = hist_plot(),
        file = file,
        title = "Histogramy",
        subtitle = subtitle,
        format = input$hist_export_format
      )
    }
  )
  
  
  observe({
    df <- dat()
    req(df)
    groups <- question_choices_grouped(df)
    all_q <- unlist(groups, use.names = FALSE)
    
    current_corr <- isolate(input$vars_corr)
    if (is.null(current_corr)) current_corr <- character(0)
    
    updatePickerInput(
      session,
      "vars_corr",
      choices = groups,
      selected = intersect(current_corr, all_q)
    )
    
    updatePickerInput(
      session,
      "vars_stats",
      choices = groups,
      selected = all_q
    )
  })
  
  box_plot <- reactive({
    df <- filtered_df()
    qi <- question_info()
    lt <- legend_lookup()
    req(df)
    
    qs <- names(qi)
    num_qs <- qs[vapply(qi, function(z) z$type == "numeric", logical(1))]
    validate(need(length(num_qs) > 0, "Vyber aspoň jednu numerickou otázku."))
    
    facet_class <- isTRUE(input$facet_by_class)
    class_col <- class_col_detect(df)
    
    dlong <- bind_rows(lapply(num_qs, function(q) {
      x <- qi[[q]]$x_num
      if (qi[[q]]$is_scale && !is.null(input$invert_questions) && q %in% input$invert_questions) {
        x <- invert_1_5(x)
      }
      
      tibble(
        question = clean_question_label(q),
        original_question = q,
        value = x,
        class = if (facet_class) trimws(as.character(df[[class_col]])) else "ALL"
      )
    })) %>%
      filter(!is.na(value))
    
    validate(need(nrow(dlong) > 1, "Málo dat pro boxplot."))
    
    box_stats <- dlong %>%
      group_by(question, class) %>%
      summarise(
        n = n(),
        mean = mean(value),
        sd = sd(value),
        ymax = max(value),
        .groups = "drop"
      ) %>%
      mutate(label = paste0("n=", n, "  mean=", signif(mean, 3), "  sd=", signif(sd, 3)))
    
    cap_lines <- lapply(num_qs, function(q) {
      txt <- legend_text_for_question(q, lt)
      if (is.na(txt)) return(NULL)
      paste0("• ", clean_question_label(q), " — ", txt)
    })
    cap_lines <- Filter(Negate(is.null), cap_lines)
    cap <- if (length(cap_lines) == 0) NULL else paste(cap_lines, collapse = "\n")
    
    if (facet_class) {
      p <- ggplot(dlong, aes(x = question, y = value, fill = class)) +
        geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, outlier.alpha = 0.5) +
        stat_summary(
          aes(group = class),
          fun = mean,
          geom = "point",
          position = position_dodge(width = 0.8),
          size = 2
        )
    } else {
      p <- ggplot(dlong, aes(x = question, y = value)) +
        geom_boxplot(fill = "#cfe0ff", colour = "#355070", outlier.alpha = 0.5) +
        stat_summary(fun = mean, geom = "point", size = 2.2, colour = "#d94841")
    }
    
    p <- p +
      geom_text(
        data = box_stats,
        aes(x = question, y = ymax, label = label, group = class),
        inherit.aes = FALSE,
        position = if (facet_class) position_dodge(width = 0.8) else position_identity(),
        vjust = -0.6,
        size = 3
      ) +
      coord_cartesian(clip = "off") +
      labs(
        x = NULL,
        y = "Hodnota",
        title = "Boxploty + mean + sd",
        caption = cap
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        plot.caption = element_text(hjust = 0),
        plot.margin = margin(10, 10, 20, 10),
        panel.grid.minor = element_blank(),
        legend.position = if (facet_class) "top" else "none"
      )
    
    p
  })

  output$box_all <- renderPlot({
    req(box_plot())
    box_plot()
  })
  
  output$box_download <- downloadHandler(
    filename = function() {
      ext <- input$box_export_format
      paste0("boxploty-", Sys.Date(), ".", ext)
    },
    content = function(file) {
      req(box_plot())
      
      subtitle <- paste0(
        "Vybrané třídy: ",
        paste(input$classes, collapse = ", "),
        "<br>",
        "Rozdělení podle tříd: ",
        ifelse(isTRUE(input$facet_by_class), "ano", "ne")
      )
      
      export_ggplot_report(
        plot_obj = box_plot(),
        file = file,
        title = "Boxploty",
        subtitle = subtitle,
        format = input$box_export_format
      )
    }
  )
  
  output$legend_dt <- renderDT({
    lo <- legend_lookup()
    validate(need(!is.null(lo), "Legenda není načtená."))
    view <- lo %>%
      transmute(
        question = clean_question_label(question),
        label_1 = label_1_display,
        label_5 = label_5_display
      )
    datatable(view, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })


  output$open_ui <- renderUI({
    qi <- question_info()
    if (length(qi) == 0) return(tags$div("Vyber otázky vlevo."))
    open_qs <- names(qi)[vapply(qi, function(z) z$type == "open", logical(1))]
    if (length(open_qs) == 0) return(tags$div("Žádná vybraná otázka neobsahuje 'okomentovat'."))

    tagList(lapply(seq_along(open_qs), function(i) {
      q <- open_qs[i]
      tid <- paste0("open__", i)
      tagList(tags$h4(clean_question_label(q)), DTOutput(tid), tags$hr())
    }))
  })

  observe({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    open_qs <- names(qi)[vapply(qi, function(z) z$type == "open", logical(1))]

    for (i in seq_along(open_qs)) {
      local({
        q <- open_qs[i]
        tid <- paste0("open__", i)
        output[[tid]] <- renderDT({
          x <- str_trim(as.character(df[[q]]))
          x <- x[!is.na(x) & x != ""]
          datatable(data.frame(odpoved = x), rownames = FALSE, options = list(pageLength = 10))
        })
      })
    }
  })

  numeric_df_corr <- reactive({
    df <- filtered_df()
    req(df)
    cols <- input$vars_corr
    if (is.null(cols) || length(cols) == 0) return(NULL)

    tmp <- lapply(cols, function(cn) to_numeric(df[[cn]]))
    names(tmp) <- cols
    keep <- vapply(tmp, is_good_numeric, logical(1))
    if (!any(keep)) return(NULL)

    nd <- as.data.frame(tmp[keep], check.names = FALSE)
    inv <- input$invert_questions
    if (!is.null(inv) && length(inv) > 0) {
      for (q in intersect(names(nd), inv)) if (is_scale_1_5(nd[[q]])) nd[[q]] <- invert_1_5(nd[[q]])
    }
    nd
  })

  

  corr_mat <- reactive({
    nd <- numeric_df_corr()
    if (is.null(nd) || ncol(nd) < 2) return(NULL)
    cor(nd, use = "pairwise.complete.obs")
  })

  output$pairs_dt <- renderDT({
    cm <- corr_mat()
    validate(need(!is.null(cm), "Vyber aspoň dvě rozumně numerické proměnné."))
    idx <- which(upper.tri(cm), arr.ind = TRUE)
    pairs <- tibble(
      var1 = colnames(cm)[idx[, 1]],
      var2 = colnames(cm)[idx[, 2]],
      r = cm[idx]
    ) %>%
      mutate(abs_r = abs(r)) %>%
      filter(abs_r >= input$absr_range[1], abs_r <= input$absr_range[2]) %>%
      arrange(desc(abs_r))

    datatable(
      pairs %>% mutate(var1 = clean_question_label(var1), var2 = clean_question_label(var2)),
      options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE
    ) %>% formatRound(c("r", "abs_r"), 3)
  })

  output$corr_dt <- renderDT({
    cm <- corr_mat()
    validate(need(!is.null(cm), "Vyber aspoň dvě rozumně numerické proměnné."))
    cm2 <- round(cm, 3)
    cm_df <- data.frame(Proměnná = clean_question_label(rownames(cm2)), cm2, check.names = FALSE)
    names(cm_df)[-1] <- clean_question_label(names(cm_df)[-1])
    datatable(cm_df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  observe({
    nd <- numeric_df_corr()
    if (is.null(nd)) {
      updateSelectInput(session, "reg_x", choices = character())
      updateSelectInput(session, "reg_y", choices = character())
      return()
    }
    vars <- names(nd)
    lab <- setNames(vars, clean_question_label(vars))
    updateSelectInput(session, "reg_x", choices = lab, selected = vars[1])
    updateSelectInput(session, "reg_y", choices = lab, selected = vars[min(2, length(vars))])
  })

  reg_result <- eventReactive(input$run_reg, {
    nd <- numeric_df_corr()
    req(nd, input$reg_x, input$reg_y)
    x <- nd[[input$reg_x]]
    y <- nd[[input$reg_y]]
    dfxy <- data.frame(x = x, y = y) %>% filter(complete.cases(.))
    validate(need(nrow(dfxy) >= 5, "Málo pozorování pro regresi."))
    fit <- lm(y ~ x, data = dfxy)
    list(df = dfxy, fit = fit)
  })

  output$reg_plot <- renderPlot({
    rr <- reg_result()
    req(rr)
    ggplot(rr$df, aes(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = clean_question_label(input$reg_x), y = clean_question_label(input$reg_y), title = "Lineární regrese (y ~ x)")
  })

  output$reg_summary <- renderPrint({
    rr <- reg_result()
    req(rr)
    sm <- summary(rr$fit)
    cat("Počet pozorování:", nrow(rr$df), "\n")
    cat("R-squared:", signif(sm$r.squared, 4), "\n\n")
    print(coef(sm))
  })

  numeric_df_stats <- reactive({
    df <- filtered_df()
    req(df)
    cols <- input$vars_stats
    if (is.null(cols) || length(cols) == 0) return(NULL)

    tmp <- lapply(cols, function(cn) to_numeric(df[[cn]]))
    names(tmp) <- cols
    keep <- vapply(tmp, is_good_numeric, logical(1))
    if (!any(keep)) return(NULL)

    nd <- as.data.frame(tmp[keep], check.names = FALSE)
    inv <- input$invert_questions
    if (!is.null(inv) && length(inv) > 0) {
      for (q in intersect(names(nd), inv)) if (is_scale_1_5(nd[[q]])) nd[[q]] <- invert_1_5(nd[[q]])
    }
    nd
  })

  output$stats_dt <- renderDT({
    nd <- numeric_df_stats()
    lt <- legend_lookup()
    validate(need(!is.null(nd), "Žádné numerické proměnné po filtru nebo nic nevybráno."))

    stats <- bind_rows(lapply(names(nd), function(v) {
      s <- numeric_summary(nd[[v]])
      data.frame(
        variable = clean_question_label(v),
        subject = extract_subject(v),
        legend_1_5 = legend_text_for_question(v, lt),
        n = s$n,
        mean = s$mean,
        median = s$median,
        sd = s$sd,
        pct_missing = s$pct_missing,
        stringsAsFactors = FALSE
      )
    }))

    datatable(stats, options = list(pageLength = 20, order = list(list(4, "desc")), scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("mean", "median", "sd", "pct_missing"), 3)
  })

  output$diag <- renderPrint({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    cls <- class_col_detect(df)
    cat("N řádků po filtru tříd:", nrow(df), "\n")
    cat("Sloupec s třídou:", cls, "\n")
    cat("Automaticky odfiltrované metadata:\n")
    print(metadata_cols_present(df))
    cat("\nPočet vybraných otázek:", length(qi), "\n\n")

    if (length(qi) == 0) {
      cat("Vyber otázky.\n")
    } else {
      cat("Typy otázek:\n")
      for (q in names(qi)) {
        cat("-", clean_question_label(q), "=>", qi[[q]]$type,
            if (qi[[q]]$type == "numeric") paste0(" | škála_1_5=", qi[[q]]$is_scale) else "", "\n")
      }
    }
  })
}

shinyApp(ui, server)
