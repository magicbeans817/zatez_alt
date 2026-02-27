# app.R
# install.packages(c("shiny","dplyr","ggplot2","readr","stringr","forcats","DT","shinyWidgets"))

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(DT)
library(shinyWidgets)

# -------------------------
# 1) LEGENDA ŠKÁL 1–5 (z JS jako pravidla)
# -------------------------
legend_rules <- list(
  list(
    pattern = "^Ohodnoťte, jak je pro Vás celková zátěž školou konzistentní\\.$",
    label_1 = "1 – Perfektně konzistentní",
    label_5 = "5 – Velmi volatilní"
  ),
  list(
    pattern = "^Ohodnoťte, jak (důležitý|důležitá) je pro Vás .+\\.$",
    label_1 = "1 – Nezajímá mě",
    label_5 = "5 – Velmi důležitá/ý a baví mě"
  ),
  list(
    pattern = "^Ohodnoťte, jak moc Vás v .+ zatěžují úkoly na doma\\.$",
    label_1 = "1 – Téměř vůbec",
    label_5 = "5 – Opravdu hodně"
  ),
  list(
    pattern = "^Ohodnoťte, jak moc Vás v .+ zatěžuje příprava na zkouškové\\.$",
    label_1 = "1 – Téměř vůbec",
    label_5 = "5 – Opravdu hodně"
  ),
  list(
    pattern = "^Ohodnoťte, jak je pro Vás zátěž v .+ konzistentní\\.$",
    label_1 = "1 – Konzistentní",
    label_5 = "5 – Velmi volatilní"
  ),
  list(
    pattern = "^Ohodnoťte kvalitu zpětné vazby a kritérií hodnocení v .+\\.$",
    label_1 = "1 – Jasná",
    label_5 = "5 – Nejasná"
  ),
  list(
    pattern = "^Jak je pro Vás v .+ srozumitelný systém hodnocení procesu \\(ABC\\)\\?$",
    label_1 = "1 – Přesně chápu",
    label_5 = "5 – Vůbec nefunguje"
  ),
  list(
    pattern = "^Ohodnoťte, jak moc přesně můžete očekávat, jakou známku ve zkouškovém dostanete\\.$",
    label_1 = "1 – Přesně vím",
    label_5 = "5 – Nevím vůbec"
  ),
  list(
    pattern = "^Jak Vám vyhovuje délka bloku\\?$",
    label_1 = "1 – Naprosto vyhovuje",
    label_5 = "5 – Nevyhovující"
  ),
  list(
    pattern = "^Tandem učitelů v tomto předmětu v tomto školním roce považuji za:$",
    label_1 = "1 – Funkční",
    label_5 = "5 – Nefunguje"
  ),
  list(
    pattern = "^Jak velkou zátěž pro mne znamená četba\\?$",
    label_1 = "1 – Naprosto zásadní",
    label_5 = "5 – Žádnou"
  )
)

legend_from_rules <- function(q_title) {
  for (r in legend_rules) {
    if (str_detect(q_title, regex(r$pattern))) {
      return(list(label_1 = r$label_1, label_5 = r$label_5))
    }
  }
  NULL
}

# -------------------------
# 2) HELPERS
# -------------------------
to_numeric <- function(x) parse_number(as.character(x))

is_good_numeric <- function(x_num, min_n = 10, min_share_parsed = 0.6) {
  n_total <- length(x_num)
  n_ok <- sum(!is.na(x_num))
  share <- if (n_total == 0) 0 else n_ok / n_total
  if (n_ok < min_n) return(FALSE)
  if (share < min_share_parsed) return(FALSE)
  if (length(unique(x_num[!is.na(x_num)])) <= 1) return(FALSE)
  TRUE
}

is_scale_1_5 <- function(x_num, min_n = 10) {
  x <- x_num[!is.na(x_num)]
  if (length(x) < min_n) return(FALSE)
  u <- sort(unique(x))
  all(u %in% 1:5) && length(u) >= 2
}

is_open_text <- function(x_chr, min_n = 15, uniq_ratio = 0.35, median_len = 20) {
  x <- str_trim(as.character(x_chr))
  x <- x[!is.na(x) & x != ""]
  if (length(x) < min_n) return(FALSE)
  ur <- length(unique(x)) / length(x)
  ml <- median(nchar(x), na.rm = TRUE)
  (ur >= uniq_ratio) && (ml >= median_len)
}

invert_1_5 <- function(x_num) ifelse(is.na(x_num), NA_real_, 6 - x_num)

numeric_summary <- function(x_num) {
  n_total <- length(x_num)
  n_ok <- sum(!is.na(x_num))
  mu <- if (n_ok > 0) mean(x_num, na.rm = TRUE) else NA_real_
  sdv <- if (n_ok > 1) sd(x_num, na.rm = TRUE) else NA_real_
  pct_miss <- if (n_total > 0) (1 - n_ok / n_total) * 100 else NA_real_
  list(n = n_ok, mean = mu, sd = sdv, pct_missing = pct_miss)
}

# -------------------------
# 3) KATEGORIZACE (suffix čísla + fix Aj otázky) + barevné štítky
# -------------------------
subjects_by_suffix <- c(
  "SAK", "PIV", "Matematika", "Angličtina", "Čeština a komunikace", "Druhý cizí jazyk", "OSV", "Tvorba"
)

extract_suffix_num <- function(colname) {
  m <- str_match(str_trim(colname), "^(.*)\\s+(\\d+)$")
  if (is.na(m[1,1])) return(list(base = colname, num = NA_integer_))
  list(base = str_trim(m[1,2]), num = as.integer(m[1,3]))
}

categorize_questions <- function(cols) {
  drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
  cols <- setdiff(cols, drop_cols)
  
  is_aj_load <- str_detect(cols, regex("^Z hlediska průběžné práce mne více zatěžuje", ignore_case = TRUE))
  
  base_groups <- list(
    "Obecné otázky" = cols[str_detect(cols, regex("Na jaké hodnocení|Z aktivit uvedených|celková zátěž školou", ignore_case = TRUE))],
    "SAK" = cols[str_detect(cols, regex("\\bSAK\\b|\\bSAKu\\b", ignore_case = TRUE))],
    "PIV" = cols[str_detect(cols, regex("\\bPIV\\b|\\bPIVu\\b", ignore_case = TRUE))],
    "Matematika" = cols[str_detect(cols, regex("Matemat", ignore_case = TRUE))],
    "Angličtina" = unique(c(cols[str_detect(cols, regex("Angličt", ignore_case = TRUE))], cols[is_aj_load])),
    "Čeština a komunikace" = cols[str_detect(cols, regex("Čeština a komunikace|Češtině a komunikaci", ignore_case = TRUE))],
    "Druhý cizí jazyk" = cols[str_detect(cols, regex("Druhý cizí jazyk|Druhém cizím jazyce", ignore_case = TRUE))],
    "OSV" = cols[str_detect(cols, regex("\\bOSV\\b", ignore_case = TRUE))],
    "Tvorba" = cols[str_detect(cols, regex("Tvorb", ignore_case = TRUE))]
  )
  
  used <- unique(unlist(base_groups))
  rest <- setdiff(cols, used)
  
  rest_info <- lapply(rest, extract_suffix_num)
  rest_num  <- vapply(rest_info, \(z) z$num, integer(1))
  
  rest_group <- vapply(seq_along(rest), function(i) {
    if (is.na(rest_num[i])) "SAK" else {
      idx <- rest_num[i] + 1
      if (idx >= 1 && idx <= length(subjects_by_suffix)) subjects_by_suffix[idx] else "Ostatní"
    }
  }, character(1))
  
  groups <- base_groups
  for (g in unique(rest_group)) {
    to_add <- rest[rest_group == g]
    if (length(to_add) > 0) groups[[g]] <- unique(c(groups[[g]], to_add))
  }
  
  seen <- character()
  for (nm in names(groups)) {
    groups[[nm]] <- groups[[nm]][!groups[[nm]] %in% seen]
    seen <- c(seen, groups[[nm]])
  }
  
  groups[lengths(groups) > 0]
}

group_badge <- c(
  "Obecné otázky"="⬜",
  "SAK"="🟥",
  "PIV"="🟦",
  "Matematika"="🟩",
  "Angličtina"="🟨",
  "Čeština a komunikace"="🟪",
  "Druhý cizí jazyk"="🟧",
  "OSV"="🟫",
  "Tvorba"="⬛",
  "Ostatní"="🔸"
)
badge_groups <- function(groups) {
  nm <- names(groups)
  names(groups) <- paste0(ifelse(nm %in% names(group_badge), group_badge[nm], "🔸"), " ", nm)
  groups
}

# -------------------------
# 4) UI
# -------------------------
ui <- fluidPage(
  titlePanel("ALT – filtrování a rychlé závěry (lokálně)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Nahraj CSV export z Google Forms:", accept = c(".csv")),
      hr(),
      uiOutput("class_filter_ui"),
      hr(),
      
      pickerInput(
        "questions",
        "Vyber otázky (můžeš i napříč předměty):",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 3")
      ),
      
      checkboxInput("facet_by_class", "Rozdělit grafy podle tříd (facet)", value = FALSE),
      sliderInput("top_n", "TOP kategorií (zbytek = Other):", min = 5, max = 30, value = 10, step = 1),
      
      hr(),
      h4("Škály 1–5: volitelná inverze"),
      pickerInput(
        "invert_questions",
        "Invertovat (1↔5):",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      hr(),
      h4("Korelace (výběr podle kategorií)"),
      pickerInput(
        "vars_corr",
        "Proměnné pro korelace:",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE, style = "btn-danger")
      ),
      sliderInput("absr_range", "|r| interval:", min = 0, max = 1, value = c(0, 1), step = 0.01),
      fluidRow(
        column(6, actionButton("preset_medium", "Střední (0.4–0.7)")),
        column(6, actionButton("preset_strong", "Silná (>0.7)"))
      ),
      
      hr(),
      h4("Statistiky (výběr podle kategorií)"),
      pickerInput(
        "vars_stats",
        "Proměnné pro statistiky:",
        choices = NULL,
        multiple = TRUE,
        options = pickerOptions(`actions-box` = TRUE, `live-search` = TRUE, style = "btn-info")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogramy (mřížka)", uiOutput("hist_grid_ui")),
        tabPanel("Boxploty (1 graf)", uiOutput("box_all_ui")),
        tabPanel("Korelace & regrese",
                 h4("Tabulka párů (filtrovaná podle |r|)"),
                 DTOutput("pairs_dt"),
                 hr(),
                 h4("Korelační matice (zúžená podle filtru)"),
                 DTOutput("corr_dt"),
                 hr(),
                 fluidRow(
                   column(4, selectInput("reg_x", "X proměnná:", choices = NULL)),
                   column(4, selectInput("reg_y", "Y proměnná:", choices = NULL)),
                   column(4, actionButton("run_reg", "Spočítat regresi"))
                 ),
                 br(),
                 plotOutput("reg_plot", height = "380px"),
                 verbatimTextOutput("reg_summary")
        ),
        tabPanel("Přehled statistik", DTOutput("stats_dt")),
        tabPanel("Legenda (read-only)", DTOutput("legend_dt")),
        tabPanel("Legenda editor (1/5)",
                 tags$p("Dvojklikni do buněk a uprav text pro 1 a 5. Když je otázka invertovaná, legenda se ve výstupech automaticky prohodí."),
                 DTOutput("legend_edit_dt")),
        tabPanel("Otevřené odpovědi", uiOutput("open_ui")),
        tabPanel("Diagnostika", verbatimTextOutput("diag"))
      )
    )
  )
)

# -------------------------
# 5) SERVER
# -------------------------
server <- function(input, output, session) {
  
  dat <- reactiveVal(NULL)
  legend_override <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    dat(df)
    
    drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
    cols <- setdiff(names(df), drop_cols)
    
    base <- bind_rows(lapply(cols, function(q) {
      lab <- legend_from_rules(q)
      data.frame(
        question = q,
        label_1 = if (is.null(lab)) NA_character_ else lab$label_1,
        label_5 = if (is.null(lab)) NA_character_ else lab$label_5,
        stringsAsFactors = FALSE
      )
    }))
    
    legend_override(base)
  })
  
  output$class_filter_ui <- renderUI({
    df <- dat()
    req(df)
    
    class_col <- "Do jaké třídy chodíte?"
    validate(need(class_col %in% names(df), paste0("V datech chybí sloupec '", class_col, "'.")))
    
    classes <- sort(unique(na.omit(as.character(df[[class_col]]))))
    checkboxGroupInput("classes", "Z jakých tříd chceš data?", choices = classes, selected = classes)
  })
  
  filtered_df <- reactive({
    df <- dat()
    req(df, input$classes)
    df %>% filter(.data[["Do jaké třídy chodíte?"]] %in% input$classes)
  })
  
  question_groups <- reactive({
    df <- dat()
    req(df)
    badge_groups(categorize_questions(names(df)))
  })
  
  observe({
    groups <- question_groups()
    updatePickerInput(session, "questions", choices = groups)
  })
  
  question_info <- reactive({
    df <- filtered_df()
    qs <- input$questions
    if (is.null(qs) || length(qs) == 0) return(list())
    
    out <- lapply(qs, function(q) {
      x_raw <- df[[q]]
      x_chr <- as.character(x_raw)
      x_num <- to_numeric(x_raw)
      
      if (is_good_numeric(x_num)) {
        list(type="numeric", x_num=x_num, x_chr=x_chr, is_scale=is_scale_1_5(x_num))
      } else if (is_open_text(x_chr)) {
        list(type="open", x_num=NULL, x_chr=x_chr, is_scale=FALSE)
      } else {
        list(type="categorical", x_num=NULL, x_chr=x_chr, is_scale=FALSE)
      }
    })
    names(out) <- qs
    out
  })
  
  # inverze nabídka jen pro škály 1..5
  observe({
    qi <- question_info()
    if (length(qi) == 0) {
      updatePickerInput(session, "invert_questions", choices = character())
      return()
    }
    scale_qs <- names(qi)[vapply(qi, \(z) z$type=="numeric" && z$is_scale, logical(1))]
    updatePickerInput(session, "invert_questions", choices = scale_qs,
                      selected = intersect(input$invert_questions, scale_qs))
  })
  
  # legenda lookup + invert display
  legend_lookup <- reactive({
    lo <- legend_override()
    if (is.null(lo)) return(NULL)
    
    inv <- input$invert_questions
    inv <- if (is.null(inv)) character() else inv
    
    lo2 <- lo
    lo2$label_1_display <- lo2$label_1
    lo2$label_5_display <- lo2$label_5
    
    idx <- lo2$question %in% inv
    lo2$label_1_display[idx] <- lo2$label_5[idx]
    lo2$label_5_display[idx] <- lo2$label_1[idx]
    lo2
  })
  
  legend_text_for_question <- function(q_title, legend_tbl) {
    if (is.null(legend_tbl)) return(NA_character_)
    row <- legend_tbl[match(q_title, legend_tbl$question), , drop = FALSE]
    if (nrow(row) == 0) return(NA_character_)
    if (is.na(row$label_1_display[1]) || is.na(row$label_5_display[1])) return(NA_character_)
    paste0("1 = ", row$label_1_display[1], " | 5 = ", row$label_5_display[1])
  }
  
  # -------------------------
  # Korelace/Statistiky pickery podle kategorií (jen numeric-able)
  # -------------------------
  numeric_candidates_by_group <- reactive({
    df <- filtered_df()
    req(df)
    groups0 <- categorize_questions(names(df))
    groups <- lapply(groups0, function(cols) {
      cols <- intersect(cols, names(df))
      ok <- vapply(cols, function(cn) is_good_numeric(to_numeric(df[[cn]])), logical(1))
      cols[ok]
    })
    groups <- groups[lengths(groups) > 0]
    badge_groups(groups)
  })
  
  observe({
    ng <- numeric_candidates_by_group()
    if (length(ng) == 0) {
      updatePickerInput(session, "vars_corr", choices = character(), selected = character())
      updatePickerInput(session, "vars_stats", choices = character(), selected = character())
      return()
    }
    all_vars <- unique(unlist(ng))
    
    if (is.null(input$vars_corr) || length(input$vars_corr) == 0) {
      updatePickerInput(session, "vars_corr", choices = ng, selected = all_vars)
    } else {
      updatePickerInput(session, "vars_corr", choices = ng, selected = intersect(input$vars_corr, all_vars))
    }
    
    if (is.null(input$vars_stats) || length(input$vars_stats) == 0) {
      updatePickerInput(session, "vars_stats", choices = ng, selected = all_vars)
    } else {
      updatePickerInput(session, "vars_stats", choices = ng, selected = intersect(input$vars_stats, all_vars))
    }
  })
  
  # -------------------------
  # Histogramy: jeden ggplot + dynamická výška + mean/sd/n (jen pro numeric)
  # -------------------------
  output$hist_grid_ui <- renderUI({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    
    qs <- names(qi)
    if (length(qs) == 0) return(plotOutput("hist_grid", height = "300px"))
    
    # počet panelů = počet vybraných otázek * (počet tříd nebo 1)
    k <- if (isTRUE(input$facet_by_class)) length(unique(df[["Do jaké třídy chodíte?"]])) else 1
    n_panels <- length(qs) * k
    
    ncol <- 3
    nrow <- ceiling(n_panels / ncol)
    h <- max(650, 240 * nrow)
    
    plotOutput("hist_grid", height = paste0(h, "px"))
  })
  
  output$hist_grid <- renderPlot({
    df <- filtered_df()
    qi <- question_info()
    lt <- legend_lookup()
    req(df)
    
    qs <- names(qi)
    validate(need(length(qs) > 0, "Vyber otázky vlevo."))
    
    top_n <- if (is.null(input$top_n)) 10 else input$top_n
    facet_class <- isTRUE(input$facet_by_class)
    class_col <- "Do jaké třídy chodíte?"
    
    # ---- SCALE 1..5 as bar ----
    scale_qs <- qs[vapply(qi, \(z) z$type=="numeric" && z$is_scale, logical(1))]
    d_scale <- tibble()
    stat_scale <- tibble()
    
    if (length(scale_qs) > 0) {
      dlong <- bind_rows(lapply(scale_qs, function(q) {
        x <- qi[[q]]$x_num
        if (!is.null(input$invert_questions) && q %in% input$invert_questions) x <- invert_1_5(x)
        tibble(
          question = q,
          class = if (facet_class) as.character(df[[class_col]]) else "ALL",
          value = factor(x, levels = 1:5),
          value_num = x
        )
      })) %>% filter(!is.na(value))
      
      # counts + %
      d_scale <- dlong %>%
        count(question, class, value, name = "n") %>%
        group_by(question, class) %>%
        mutate(p = 100 * n / sum(n)) %>%
        ungroup() %>%
        mutate(
          type = "Škála 1–5",
          facet_lab = paste0(question, "\n", coalesce(legend_text_for_question(question, lt), ""))
        )
      
      # mean/sd/n label per panel
      stat_scale <- dlong %>%
        filter(!is.na(value_num)) %>%
        group_by(question, class) %>%
        summarise(
          n = sum(!is.na(value_num)),
          mean = mean(value_num, na.rm = TRUE),
          sd = sd(value_num, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          type = "Škála 1–5",
          facet_lab = paste0(question, "\n", coalesce(legend_text_for_question(question, lt), "")),
          label = paste0("n=", n, "\nmean=", signif(mean, 3), "\nsd=", signif(sd, 3))
        )
    }
    
    # ---- CATEGORICAL as bar ----
    cat_qs <- qs[vapply(qi, \(z) z$type=="categorical", logical(1))]
    d_cat <- tibble()
    if (length(cat_qs) > 0) {
      d_cat <- bind_rows(lapply(cat_qs, function(q) {
        x <- str_trim(as.character(qi[[q]]$x_chr))
        x[is.na(x) | x == ""] <- "(missing)"
        x <- fct_lump_n(factor(x), n = top_n, other_level = "Other")
        
        tibble(
          question = q,
          class = if (facet_class) as.character(df[[class_col]]) else "ALL",
          value = x
        )
      })) %>%
        count(question, class, value, name = "n") %>%
        group_by(question, class) %>%
        mutate(p = 100 * n / sum(n)) %>%
        ungroup() %>%
        mutate(type = "Kategorické", facet_lab = question)
    }
    
    d_all <- bind_rows(d_scale, d_cat) %>%
      mutate(panel = if (facet_class) paste0(class, " | ", facet_lab) else facet_lab)
    
    validate(need(nrow(d_all) > 0, "Není co vykreslit."))
    
    # stat labels map to same panel
    stat_all <- stat_scale %>%
      mutate(panel = if (facet_class) paste0(class, " | ", facet_lab) else facet_lab)
    
    ggplot(d_all, aes(x = value, y = n)) +
      geom_col() +
      geom_text(aes(label = sprintf("%.1f%%", p)), vjust = -0.2, size = 3) +
      geom_text(
        data = stat_all,
        aes(x = Inf, y = Inf, label = label),
        inherit.aes = FALSE,
        hjust = 1.05, vjust = 1.1,
        size = 3
      ) +
      facet_wrap(type ~ panel, scales = "free_y", ncol = 3) +
      labs(x = NULL, y = "Absolutní četnost") +
      theme(
        strip.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(10, 10, 10, 10)
      )
  })
  
  # -------------------------
  # Boxploty: jeden graf + dynamická výška + mean/sd/n text + legenda v caption
  # -------------------------
  output$box_all_ui <- renderUI({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    
    num_q <- sum(vapply(qi, \(z) z$type=="numeric", logical(1)))
    if (num_q == 0) return(plotOutput("box_all", height = "300px"))
    
    k <- if (isTRUE(input$facet_by_class)) length(unique(df[["Do jaké třídy chodíte?"]])) else 1
    h <- max(650, 130 * num_q * k)
    plotOutput("box_all", height = paste0(h, "px"))
  })
  
  output$box_all <- renderPlot({
    df <- filtered_df()
    qi <- question_info()
    lt <- legend_lookup()
    req(df)
    
    qs <- names(qi)
    num_qs <- qs[vapply(qi, \(z) z$type=="numeric", logical(1))]
    validate(need(length(num_qs) > 0, "Vyber aspoň jednu numerickou otázku."))
    
    facet_class <- isTRUE(input$facet_by_class)
    class_col <- "Do jaké třídy chodíte?"
    
    dlong <- bind_rows(lapply(num_qs, function(q) {
      x <- qi[[q]]$x_num
      if (qi[[q]]$is_scale && !is.null(input$invert_questions) && q %in% input$invert_questions) {
        x <- invert_1_5(x)
      }
      tibble(
        question = q,
        value = x,
        class = if (facet_class) as.character(df[[class_col]]) else "ALL"
      )
    })) %>% filter(!is.na(value))
    
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
    
    # caption legenda (jen pro ty otázky, kde ji známe)
    cap_lines <- lapply(num_qs, function(q) {
      txt <- legend_text_for_question(q, lt)
      if (is.na(txt)) return(NULL)
      paste0("• ", str_trunc(q, 90), " — ", txt)
    })
    cap_lines <- Filter(Negate(is.null), cap_lines)
    cap <- if (length(cap_lines) == 0) NULL else paste(cap_lines, collapse = "\n")
    
    p <- ggplot(dlong, aes(x = question, y = value)) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", size = 2) +
      stat_summary(fun.data = function(x) {
        m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
        data.frame(y = m, ymin = m - s, ymax = m + s)
      }, geom = "errorbar", width = 0.15) +
      geom_text(
        data = box_stats,
        aes(x = question, y = ymax, label = label),
        inherit.aes = FALSE,
        vjust = -0.6,
        size = 3
      ) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = "Hodnota", title = "Boxploty + mean + sd", caption = cap) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        plot.caption = element_text(hjust = 0),
        plot.margin = margin(10, 10, 20, 10)
      )
    
    if (facet_class) p <- p + facet_wrap(~ class, scales = "free_y")
    p
  })
  
  # -------------------------
  # Legenda read-only + editor (DT editable)
  # -------------------------
  output$legend_dt <- renderDT({
    lt <- legend_lookup()
    validate(need(!is.null(lt), "Nahraj CSV nejdřív."))
    
    view <- lt %>% transmute(question, label_1 = label_1_display, label_5 = label_5_display)
    datatable(view, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$legend_edit_dt <- renderDT({
    lo <- legend_override()
    validate(need(!is.null(lo), "Nahraj CSV nejdřív."))
    
    inv <- input$invert_questions
    inv <- if (is.null(inv)) character() else inv
    
    lo2 <- lo %>%
      mutate(
        inverted = question %in% inv,
        label_1_after_invert = ifelse(inverted, label_5, label_1),
        label_5_after_invert = ifelse(inverted, label_1, label_5)
      )
    
    datatable(
      lo2,
      editable = list(target = "cell", disable = list(columns = c(1,4,5,6))),
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$legend_edit_dt_cell_edit, {
    info <- input$legend_edit_dt_cell_edit
    lo <- legend_override()
    req(lo)
    
    i <- info$row
    j <- info$col
    v <- info$value
    
    # 1=question, 2=label_1, 3=label_5 ...
    if (j %in% c(2, 3)) {
      lo[i, j] <- v
      legend_override(lo)
    }
  })
  
  # -------------------------
  # Otevřené odpovědi
  # -------------------------
  output$open_ui <- renderUI({
    qi <- question_info()
    if (length(qi) == 0) return(tags$div("Vyber otázky vlevo."))
    open_qs <- names(qi)[vapply(qi, \(z) z$type=="open", logical(1))]
    if (length(open_qs) == 0) return(tags$div("Žádná vybraná otázka nebyla rozpoznána jako otevřená."))
    
    tagList(lapply(seq_along(open_qs), function(i) {
      q <- open_qs[i]
      tid <- paste0("open__", i)
      tagList(tags$h4(q), DTOutput(tid), tags$hr())
    }))
  })
  
  observe({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    open_qs <- names(qi)[vapply(qi, \(z) z$type=="open", logical(1))]
    
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
  
  # -------------------------
  # Korelace & regrese (ponechávám, jen pickery + presety)
  # -------------------------
  observeEvent(input$preset_medium, { updateSliderInput(session, "absr_range", value = c(0.4, 0.7)) })
  observeEvent(input$preset_strong, { updateSliderInput(session, "absr_range", value = c(0.7, 1.0)) })
  
  numeric_df_corr <- reactive({
    df <- filtered_df()
    req(df)
    
    drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
    cols <- setdiff(names(df), drop_cols)
    
    if (!is.null(input$vars_corr) && length(input$vars_corr) > 0) cols <- intersect(cols, input$vars_corr) else return(NULL)
    if (length(cols) == 0) return(NULL)
    
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
  
  pair_table <- reactive({
    nd <- numeric_df_corr()
    if (is.null(nd)) return(NULL)
    vars <- names(nd)
    if (length(vars) < 2) return(NULL)
    
    combs <- combn(vars, 2, simplify = FALSE)
    res <- bind_rows(lapply(combs, function(vp) {
      x <- nd[[vp[1]]]; y <- nd[[vp[2]]]
      cc <- complete.cases(x, y)
      n <- sum(cc)
      r <- if (n >= 3) cor(x[cc], y[cc]) else NA_real_
      data.frame(var1 = vp[1], var2 = vp[2], r = r, n = n, stringsAsFactors = FALSE)
    }))
    res %>% mutate(abs_r = abs(r))
  })
  
  filtered_pairs <- reactive({
    pt <- pair_table()
    if (is.null(pt)) return(NULL)
    rng <- input$absr_range
    pt %>% filter(!is.na(r), abs_r >= rng[1], abs_r <= rng[2]) %>% arrange(desc(abs_r), desc(n))
  })
  
  output$pairs_dt <- renderDT({
    fp <- filtered_pairs()
    validate(need(!is.null(fp) && nrow(fp) > 0, "Žádné páry v zadaném intervalu |r|."))
    datatable(fp %>% select(var1, var2, r, abs_r, n),
              rownames = FALSE,
              options = list(pageLength = 15, order = list(list(4, "desc"))),
              selection = "single") %>% formatRound(c("r","abs_r"), 3)
  })
  
  observeEvent(input$pairs_dt_rows_selected, {
    fp <- filtered_pairs()
    req(fp)
    idx <- input$pairs_dt_rows_selected
    if (length(idx) != 1) return()
    updateSelectInput(session, "reg_x", selected = fp$var1[idx])
    updateSelectInput(session, "reg_y", selected = fp$var2[idx])
  })
  
  corr_mat <- reactive({
    nd <- numeric_df_corr()
    fp <- filtered_pairs()
    if (is.null(nd)) return(NULL)
    if (!is.null(fp) && nrow(fp) > 0) {
      vars <- unique(c(fp$var1, fp$var2))
      nd <- nd[, vars, drop = FALSE]
    }
    cor(nd, use = "pairwise.complete.obs")
  })
  
  output$corr_dt <- renderDT({
    cm <- corr_mat()
    validate(need(!is.null(cm), "Žádná korelační matice k zobrazení."))
    dfc <- as.data.frame(cm, check.names = FALSE)
    dfc <- cbind(Variable = rownames(dfc), dfc)
    rownames(dfc) <- NULL
    
    datatable(dfc, options = list(scrollX = TRUE, pageLength = 10), selection = "single") %>%
      formatRound(columns = names(dfc)[-1], digits = 3) %>%
      formatStyle(
        columns = names(dfc)[-1],
        backgroundColor = styleInterval(
          c(-0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8),
          c("#67001f","#b2182b","#d6604d","#f4a582","#f7f7f7","#92c5de","#4393c3","#2166ac","#053061")
        ),
        color = "black"
      )
  })
  
  observeEvent(input$corr_dt_cell_clicked, {
    info <- input$corr_dt_cell_clicked
    cm <- corr_mat()
    if (is.null(cm)) return()
    row <- info$row; col <- info$col
    if (is.null(row) || is.null(col) || col == 1) return()
    vars <- colnames(cm)
    updateSelectInput(session, "reg_x", selected = vars[col - 1])
    updateSelectInput(session, "reg_y", selected = vars[row])
  })
  
  observe({
    nd <- numeric_df_corr()
    if (is.null(nd)) {
      updateSelectInput(session, "reg_x", choices = character())
      updateSelectInput(session, "reg_y", choices = character())
      return()
    }
    vars <- names(nd)
    updateSelectInput(session, "reg_x", choices = vars, selected = vars[1])
    updateSelectInput(session, "reg_y", choices = vars, selected = vars[min(2, length(vars))])
  })
  
  reg_result <- eventReactive(input$run_reg, {
    nd <- numeric_df_corr()
    req(nd, input$reg_x, input$reg_y)
    x <- nd[[input$reg_x]]; y <- nd[[input$reg_y]]
    dfxy <- data.frame(x=x, y=y) %>% filter(complete.cases(.))
    validate(need(nrow(dfxy) >= 5, "Málo pozorování pro regresi."))
    fit <- lm(y ~ x, data=dfxy)
    list(df=dfxy, fit=fit)
  })
  
  output$reg_plot <- renderPlot({
    rr <- reg_result()
    req(rr)
    ggplot(rr$df, aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm", se=FALSE) +
      labs(x=input$reg_x, y=input$reg_y, title="Lineární regrese (y ~ x)")
  })
  
  output$reg_summary <- renderPrint({
    rr <- reg_result()
    req(rr)
    sm <- summary(rr$fit)
    cat("Počet pozorování:", nrow(rr$df), "\n")
    cat("R-squared:", signif(sm$r.squared, 4), "\n\n")
    print(coef(sm))
  })
  
  # -------------------------
  # Statistiky (mean/sd + legenda)
  # -------------------------
  numeric_df_stats <- reactive({
    df <- filtered_df()
    req(df)
    
    drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
    cols <- setdiff(names(df), drop_cols)
    
    if (!is.null(input$vars_stats) && length(input$vars_stats) > 0) cols <- intersect(cols, input$vars_stats) else return(NULL)
    if (length(cols) == 0) return(NULL)
    
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
    validate(need(!is.null(nd), "Žádné numerické proměnné po filtru (nebo nic nevybráno)."))
    
    stats <- bind_rows(lapply(names(nd), function(v) {
      s <- numeric_summary(nd[[v]])
      data.frame(
        variable = v,
        legend_1_5 = legend_text_for_question(v, lt),
        n = s$n,
        mean = s$mean,
        sd = s$sd,
        pct_missing = s$pct_missing,
        stringsAsFactors = FALSE
      )
    }))
    
    datatable(stats, options = list(pageLength = 20, order = list(list(4, "desc")), scrollX = TRUE), rownames = FALSE) %>%
      formatRound(c("mean","sd","pct_missing"), 3)
  })
  
  # -------------------------
  # Diagnostika
  # -------------------------
  output$diag <- renderPrint({
    df <- filtered_df()
    qi <- question_info()
    cat("N řádků po filtru tříd:", nrow(df), "\n\n")
    
    if (length(qi) == 0) {
      cat("Vyber otázky.\n")
    } else {
      cat("Typy otázek:\n")
      for (q in names(qi)) {
        cat("-", q, "=>", qi[[q]]$type,
            if (qi[[q]]$type=="numeric" && qi[[q]]$is_scale) " (škála 1–5)" else "",
            "\n")
      }
    }
  })
}

shinyApp(ui, server)