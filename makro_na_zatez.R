# app.R
# install.packages(c("shiny","dplyr","ggplot2","readr","stringr","forcats","DT","shinyWidgets","digest"))

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(DT)
library(shinyWidgets)
library(digest)

# -------------------------
# 1) LEGENDA ŠKÁL 1–5 (z tvého JS)
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

legend_labels_for_question <- function(q_title) {
  for (r in legend_rules) {
    if (str_detect(q_title, regex(r$pattern))) {
      return(list(label_1 = r$label_1, label_5 = r$label_5))
    }
  }
  NULL
}

legend_text_for_question <- function(q_title) {
  lab <- legend_labels_for_question(q_title)
  if (is.null(lab)) return(NA_character_)
  paste0("1 = ", lab$label_1, " | 5 = ", lab$label_5)
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

# škála 1..5: po vyčištění musí být subset {1,2,3,4,5} a musí obsahovat aspoň 2 různé hodnoty
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

numeric_summary <- function(x_num) {
  n_total <- length(x_num)
  n_ok <- sum(!is.na(x_num))
  mu <- if (n_ok > 0) mean(x_num, na.rm = TRUE) else NA_real_
  sdv <- if (n_ok > 1) sd(x_num, na.rm = TRUE) else NA_real_
  pct_miss <- if (n_total > 0) (1 - n_ok / n_total) * 100 else NA_real_
  list(n = n_ok, mean = mu, sd = sdv, pct_missing = pct_miss)
}

# inverze škály 1..5 (1<->5, 2<->4)
invert_1_5 <- function(x_num) {
  ifelse(is.na(x_num), NA_real_, 6 - x_num)
}

# -------------------------
# 3) KATEGORIZACE (včetně duplicit se suffix číslem)
# -------------------------
subjects_by_suffix <- c(
  "SAK",                  # bez čísla
  "PIV",                  # 1
  "Matematika",           # 2
  "Angličtina",           # 3
  "Čeština a komunikace", # 4
  "Druhý cizí jazyk",     # 5
  "OSV",                  # 6
  "Tvorba"                # 7
)

extract_suffix_num <- function(colname) {
  m <- str_match(str_trim(colname), "^(.*)\\s+(\\d+)$")
  if (is.na(m[1,1])) return(list(base = colname, num = NA_integer_))
  list(base = str_trim(m[1,2]), num = as.integer(m[1,3]))
}

categorize_questions_v3 <- function(cols) {
  drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
  cols <- setdiff(cols, drop_cols)
  
  # explicitní fix: Aj otázka “Z hlediska průběžné práce...”
  is_aj_load <- str_detect(cols, regex("^Z hlediska průběžné práce mne více zatěžuje", ignore_case = TRUE))
  
  base_groups <- list(
    "Obecné otázky" = cols[str_detect(cols, regex("Na jaké hodnocení|Z aktivit uvedených|celková zátěž školou", ignore_case = TRUE))],
    "SAK" = cols[str_detect(cols, regex("\\bSAK\\b|\\bSAKu\\b", ignore_case = TRUE))],
    "PIV" = cols[str_detect(cols, regex("\\bPIV\\b|\\bPIVu\\b", ignore_case = TRUE))],
    "Matematika" = cols[str_detect(cols, regex("Matemat", ignore_case = TRUE))],
    "Angličtina" = unique(c(
      cols[str_detect(cols, regex("Angličt", ignore_case = TRUE))],
      cols[is_aj_load]
    )),
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
    if (is.na(rest_num[i])) {
      "SAK"
    } else {
      idx <- rest_num[i] + 1
      if (idx >= 1 && idx <= length(subjects_by_suffix)) subjects_by_suffix[idx] else "Ostatní"
    }
  }, character(1))
  
  groups <- base_groups
  for (g in unique(rest_group)) {
    to_add <- rest[rest_group == g]
    if (length(to_add) > 0) groups[[g]] <- unique(c(groups[[g]], to_add))
  }
  
  # de-dupe napříč skupinami (ponech první výskyt)
  seen <- character()
  for (nm in names(groups)) {
    groups[[nm]] <- groups[[nm]][!groups[[nm]] %in% seen]
    seen <- c(seen, groups[[nm]])
  }
  
  groups <- groups[lengths(groups) > 0]
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
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3",
          `count-selected-text` = "{0} vybraných",
          `none-selected-text` = "Vyber alespoň jednu otázku"
        )
      ),
      
      checkboxInput("facet_by_class", "Rozdělit grafy podle tříd (facet)", value = FALSE),
      
      hr(),
      h4("Škály 1–5: volitelná inverze"),
      pickerInput(
        "invert_questions",
        "Invertovat (1↔5) pro vybrané otázky:",
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      hr(),
      h4("Korelace"),
      sliderInput("absr_range", "|r| interval:", min = 0, max = 1, value = c(0.0, 1.0), step = 0.01),
      fluidRow(
        column(6, actionButton("preset_medium", "Středně silná (0.4–0.7)")),
        column(6, actionButton("preset_strong", "Silná (>0.7)"))
      ),
      
      h4("Výběr proměnných podle kategorií"),
      pickerInput(
        "vars_corr",
        "Proměnné pro korelace (podle kategorií):",
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        "vars_stats",
        "Proměnné pro statistiky (podle kategorií):",
        choices = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      
      
      hr(),
      helpText("Histogramy/boxploty jsou porovnatelné na první pohled. Korelace: filtruje se podle |r| a regrese jde z kliknutí na pár.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogramy (mřížka)", plotOutput("hist_grid", height = "750px")),
        tabPanel("Boxploty (1 graf)", plotOutput("box_all", height = "750px")),
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
        tabPanel("Legenda škál (1/5)", DTOutput("legend_dt")),
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
  
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv(input$file$datapath, show_col_types = FALSE)
    dat(df)
  })
  
  output$class_filter_ui <- renderUI({
    df <- dat()
    req(df)
    
    class_col <- "Do jaké třídy chodíte?"
    validate(need(class_col %in% names(df),
                  paste0("V datech chybí sloupec '", class_col, "'.")))
    
    classes <- sort(unique(na.omit(as.character(df[[class_col]]))))
    checkboxGroupInput("classes", "Z jakých tříd chceš data?",
                       choices = classes, selected = classes)
  })
  
  observe({
    df <- dat()
    req(df)
    groups <- categorize_questions_v3(names(df))
    updatePickerInput(session, "questions", choices = groups)
  })
  
  
  # --- pomocná funkce: z kategorií nech jen sloupce, které jsou numeric-able ---
  numeric_candidates_by_group <- reactive({
    df <- filtered_df()
    req(df)
    
    groups <- categorize_questions_v3(names(df))
    
    # převeď skupiny na "jen ty sloupce, které jdou smysluplně na numeric"
    num_groups <- lapply(groups, function(cols) {
      cols <- intersect(cols, names(df))
      ok <- vapply(cols, function(cn) is_good_numeric(to_numeric(df[[cn]])), logical(1))
      cols[ok]
    })
    
    # vyhoď prázdné skupiny
    num_groups <- num_groups[lengths(num_groups) > 0]
    num_groups
  })
  
  observe({
    ng <- numeric_candidates_by_group()
    if (length(ng) == 0) {
      updatePickerInput(session, "vars_corr", choices = character(), selected = character())
      updatePickerInput(session, "vars_stats", choices = character(), selected = character())
      return()
    }
    
    # default: vybrat všechno (aby se chování nelišilo od současného)
    all_vars <- unique(unlist(ng))
    
    updatePickerInput(session, "vars_corr", choices = ng, selected = all_vars)
    updatePickerInput(session, "vars_stats", choices = ng, selected = all_vars)
  })
  
  
  filtered_df <- reactive({
    df <- dat()
    req(df, input$classes)
    class_col <- "Do jaké třídy chodíte?"
    df %>% filter(.data[[class_col]] %in% input$classes)
  })
  
  # otázky -> typy
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
  
  # nabídka pro inverzi (jen pro numeric škály 1..5)
  observe({
    qi <- question_info()
    if (length(qi) == 0) {
      updatePickerInput(session, "invert_questions", choices = character())
      return()
    }
    scale_qs <- names(qi)[vapply(qi, \(z) z$type=="numeric" && z$is_scale, logical(1))]
    updatePickerInput(session, "invert_questions", choices = scale_qs, selected = intersect(input$invert_questions, scale_qs))
  })
  
  # -------------------------
  # 5A) HISTOGRAMY (mřížka)
  # - škály 1..5: barplot count + % nad sloupci
  # - ostatní numeric: histogram count + % nad biny
  # - categorical: barplot count + %
  # -------------------------
  output$hist_grid <- renderPlot({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    
    qs <- names(qi)
    validate(need(length(qs) > 0, "Vyber otázky vlevo."))
    
    class_col <- "Do jaké třídy chodíte?"
    facet_class <- isTRUE(input$facet_by_class) && (class_col %in% names(df))
    
    # připrav data long pro různé typy
    plot_blocks <- list()
    
    # 1) škály 1..5
    scale_qs <- qs[vapply(qi, \(z) z$type=="numeric" && z$is_scale, logical(1))]
    if (length(scale_qs) > 0) {
      dlong <- bind_rows(lapply(scale_qs, function(q) {
        x <- qi[[q]]$x_num
        if (!is.null(input$invert_questions) && q %in% input$invert_questions) {
          x <- invert_1_5(x)
        }
        tibble(
          question = q,
          value = factor(x, levels = 1:5),
          class = if (facet_class) as.character(df[[class_col]]) else "ALL"
        )
      })) %>% filter(!is.na(value))
      
      # spočti % v rámci question (+ případně class)
      dsum <- dlong %>%
        count(question, class, value, name = "n") %>%
        group_by(question, class) %>%
        mutate(p = 100 * n / sum(n)) %>%
        ungroup()
      
      p1 <- ggplot(dsum, aes(x = value, y = n)) +
        geom_col() +
        geom_text(aes(label = sprintf("%.1f%%", p)), vjust = -0.2, size = 3) +
        labs(x = NULL, y = "Absolutní četnost", title = "Škály 1–5 (count + %)") +
        facet_wrap(~ question + class, scales = "free_y") +
        theme(axis.text.x = element_text(angle = 0))
      plot_blocks[["scale"]] <- p1
    }
    
    # 2) ostatní numeric
    cont_qs <- qs[vapply(qi, \(z) z$type=="numeric" && !z$is_scale, logical(1))]
    if (length(cont_qs) > 0) {
      dlong <- bind_rows(lapply(cont_qs, function(q) {
        tibble(
          question = q,
          x = qi[[q]]$x_num,
          class = if (facet_class) as.character(df[[class_col]]) else "ALL"
        )
      })) %>% filter(!is.na(x))
      
      # histogram s % nad biny
      p2 <- ggplot(dlong, aes(x = x)) +
        geom_histogram(bins = 20, aes(y = after_stat(count))) +
        geom_text(
          stat = "bin", bins = 20,
          aes(
            y = after_stat(count),
            label = sprintf("%.1f%%", 100 * after_stat(count) / sum(after_stat(count)))
          ),
          vjust = -0.2, size = 3
        ) +
        labs(x = NULL, y = "Absolutní četnost", title = "Numerické (histogram count + %)") +
        facet_wrap(~ question + class, scales = "free") +
        theme(axis.text.x = element_text(angle = 0))
      plot_blocks[["cont"]] <- p2
    }
    
    # 3) kategorické
    cat_qs <- qs[vapply(qi, \(z) z$type=="categorical", logical(1))]
    if (length(cat_qs) > 0) {
      dlong <- bind_rows(lapply(cat_qs, function(q) {
        x <- str_trim(as.character(qi[[q]]$x_chr))
        x[is.na(x) | x == ""] <- "(missing)"
        tibble(
          question = q,
          value = fct_lump_n(factor(x), n = input$top_n, other_level = "Other"),
          class = if (facet_class) as.character(df[[class_col]]) else "ALL"
        )
      }))
      
      dsum <- dlong %>%
        count(question, class, value, name = "n") %>%
        group_by(question, class) %>%
        mutate(p = 100 * n / sum(n)) %>%
        ungroup()
      
      p3 <- ggplot(dsum, aes(x = value, y = n)) +
        geom_col() +
        geom_text(aes(label = sprintf("%.1f%%", p)), vjust = -0.2, size = 3) +
        labs(x = NULL, y = "Absolutní četnost", title = "Kategorické (count + %)") +
        facet_wrap(~ question + class, scales = "free_y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      plot_blocks[["cat"]] <- p3
    }
    
    validate(need(length(plot_blocks) > 0, "Není co vykreslit (zvolené otázky nemají data po filtru)."))
    
    # “vše” do jednoho plátna: vykreslíme postupně (base grafika neumí patchwork),
    # proto využijeme layout: pokud jsou >1 bloky, dáme je pod sebe.
    # RStudio to zvládne přes par(mfrow).
    nblocks <- length(plot_blocks)
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mfrow = c(nblocks, 1), mar = c(4, 4, 3, 1))
    
    for (nm in names(plot_blocks)) {
      print(plot_blocks[[nm]])
    }
  })
  
  # -------------------------
  # 5B) BOXPLOTY (všechny numeric v jednom)
  # - mean bod + sd errorbar
  # -------------------------
  output$box_all <- renderPlot({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    
    qs <- names(qi)
    num_qs <- qs[vapply(qi, \(z) z$type=="numeric", logical(1))]
    validate(need(length(num_qs) > 0, "Vyber aspoň jednu numerickou otázku."))
    
    class_col <- "Do jaké třídy chodíte?"
    facet_class <- isTRUE(input$facet_by_class) && (class_col %in% names(df))
    
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
    
    p <- ggplot(dlong, aes(x = question, y = value)) +
      geom_boxplot() +
      stat_summary(fun = mean, geom = "point", size = 2) +
      stat_summary(fun.data = function(x) {
        m <- mean(x, na.rm = TRUE)
        s <- sd(x, na.rm = TRUE)
        data.frame(y = m, ymin = m - s, ymax = m + s)
      }, geom = "errorbar", width = 0.15) +
      labs(x = NULL, y = "Hodnota", title = "Boxploty + mean + sd") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
    
    if (facet_class) p <- p + facet_wrap(~ class, scales = "free_y")
    
    p
  })
  
  # -------------------------
  # 5C) NUMERIC DF pro korelace a statistiky
  # -------------------------
  numeric_df <- reactive({
    df <- filtered_df()
    req(df)
    
    drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
    cols <- setdiff(names(df), drop_cols)
    
    # !!! nově: omez podle výběru pro korelace
    if (!is.null(input$vars_corr) && length(input$vars_corr) > 0) {
      cols <- intersect(cols, input$vars_corr)
    } else {
      # když uživatel odklikne všechno, nech to prázdné
      return(NULL)
    }
    
    if (length(cols) == 0) return(NULL)
    
    tmp <- lapply(cols, function(cn) to_numeric(df[[cn]]))
    names(tmp) <- cols
    
    keep <- vapply(tmp, is_good_numeric, logical(1))
    if (!any(keep)) return(NULL)
    
    nd <- as.data.frame(tmp[keep], check.names = FALSE)
    
    # inverze škál (jak už máš)
    inv <- input$invert_questions
    if (!is.null(inv) && length(inv) > 0) {
      for (q in intersect(names(nd), inv)) {
        if (is_scale_1_5(nd[[q]])) nd[[q]] <- invert_1_5(nd[[q]])
      }
    }
    
    nd
  })
  
  numeric_df_stats <- reactive({
    df <- filtered_df()
    req(df)
    
    drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
    cols <- setdiff(names(df), drop_cols)
    
    # !!! nově: omez podle výběru pro statistiky
    if (!is.null(input$vars_stats) && length(input$vars_stats) > 0) {
      cols <- intersect(cols, input$vars_stats)
    } else {
      return(NULL)
    }
    
    if (length(cols) == 0) return(NULL)
    
    tmp <- lapply(cols, function(cn) to_numeric(df[[cn]]))
    names(tmp) <- cols
    
    keep <- vapply(tmp, is_good_numeric, logical(1))
    if (!any(keep)) return(NULL)
    
    nd <- as.data.frame(tmp[keep], check.names = FALSE)
    
    # inverze škál i tady (aby stats odpovídaly grafům)
    inv <- input$invert_questions
    if (!is.null(inv) && length(inv) > 0) {
      for (q in intersect(names(nd), inv)) {
        if (is_scale_1_5(nd[[q]])) nd[[q]] <- invert_1_5(nd[[q]])
      }
    }
    
    nd
  })
  
  # korelace + N pro každý pár
  pair_table <- reactive({
    nd <- numeric_df()
    if (is.null(nd)) return(NULL)
    vars <- names(nd)
    
    # spočti korelace a n pro každý pár
    combs <- combn(vars, 2, simplify = FALSE)
    res <- lapply(combs, function(vp) {
      x <- nd[[vp[1]]]
      y <- nd[[vp[2]]]
      cc <- complete.cases(x, y)
      n <- sum(cc)
      r <- if (n >= 3) cor(x[cc], y[cc]) else NA_real_
      data.frame(var1 = vp[1], var2 = vp[2], r = r, n = n, stringsAsFactors = FALSE)
    }) |> bind_rows()
    
    res <- res %>% mutate(abs_r = abs(r))
    res
  })
  
  # preset buttons
  observeEvent(input$preset_medium, {
    updateSliderInput(session, "absr_range", value = c(0.4, 0.7))
  })
  observeEvent(input$preset_strong, {
    updateSliderInput(session, "absr_range", value = c(0.7, 1.0))
  })
  
  filtered_pairs <- reactive({
    pt <- pair_table()
    if (is.null(pt)) return(NULL)
    rng <- input$absr_range
    pt %>%
      filter(!is.na(r), abs_r >= rng[1], abs_r <= rng[2]) %>%
      arrange(desc(abs_r), desc(n))
  })
  
  # tabulka párů
  output$pairs_dt <- renderDT({
    fp <- filtered_pairs()
    validate(need(!is.null(fp), "Žádné numerické proměnné po filtru tříd."))
    
    datatable(
      fp %>% select(var1, var2, r, abs_r, n),
      rownames = FALSE,
      options = list(pageLength = 15, order = list(list(4, "desc"))),
      selection = "single"
    ) %>% formatRound(c("r", "abs_r"), digits = 3)
  })
  
  # klik na řádek v pairs -> vyplň regresi
  observeEvent(input$pairs_dt_rows_selected, {
    fp <- filtered_pairs()
    req(fp)
    idx <- input$pairs_dt_rows_selected
    if (length(idx) != 1) return()
    updateSelectInput(session, "reg_x", selected = fp$var1[idx])
    updateSelectInput(session, "reg_y", selected = fp$var2[idx])
  })
  
  # korelační matice: omez na proměnné, které se objevují v filtrovaných párech
  corr_mat <- reactive({
    nd <- numeric_df()
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
    
    datatable(
      dfc,
      options = list(scrollX = TRUE, pageLength = 10),
      selection = "single"
    ) %>%
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
  
  # klik na buňku matice -> vyplň regresi (X=sloupec, Y=řádek)
  observeEvent(input$corr_dt_cell_clicked, {
    info <- input$corr_dt_cell_clicked
    cm <- corr_mat()
    if (is.null(cm)) return()
    row <- info$row
    col <- info$col
    if (is.null(row) || is.null(col)) return()
    if (col == 1) return()
    
    vars <- colnames(cm)
    x_var <- vars[col - 1]
    y_var <- vars[row]
    if (!is.null(x_var) && !is.null(y_var) && x_var != y_var) {
      updateSelectInput(session, "reg_x", selected = x_var)
      updateSelectInput(session, "reg_y", selected = y_var)
    }
  })
  
  # naplň selecty pro regresi
  observe({
    nd <- numeric_df()
    if (is.null(nd)) {
      updateSelectInput(session, "reg_x", choices = character())
      updateSelectInput(session, "reg_y", choices = character())
      return()
    }
    vars <- names(nd)
    updateSelectInput(session, "reg_x", choices = vars, selected = vars[1])
    updateSelectInput(session, "reg_y", choices = vars, selected = vars[min(2, length(vars))])
  })
  
  # regrese: po stisku tlačítka
  reg_result <- eventReactive(input$run_reg, {
    nd <- numeric_df()
    req(nd, input$reg_x, input$reg_y)
    validate(need(input$reg_x %in% names(nd), "X není numerická proměnná."))
    validate(need(input$reg_y %in% names(nd), "Y není numerická proměnná."))
    
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
      labs(
        x = input$reg_x,
        y = input$reg_y,
        title = "Lineární regrese (y ~ x)"
      )
  })
  
  output$reg_summary <- renderPrint({
    rr <- reg_result()
    req(rr)
    sm <- summary(rr$fit)
    
    cat("Počet pozorování (complete cases):", nrow(rr$df), "\n")
    cat("R-squared:", signif(sm$r.squared, 4), "\n\n")
    cat("Koeficienty:\n")
    print(coef(sm))
  })
  
  # -------------------------
  # 5D) Statistiky + legenda 1/5
  # -------------------------
  output$stats_dt <- renderDT({
    nd <- numeric_df_stats()
    validate(need(!is.null(nd), "Žádné numerické proměnné po filtru tříd."))
    
    stats <- lapply(names(nd), function(v) {
      s <- numeric_summary(nd[[v]])
      data.frame(
        variable = v,
        legend_1_5 = legend_text_for_question(v),
        n = s$n,
        mean = s$mean,
        sd = s$sd,
        pct_missing = s$pct_missing,
        stringsAsFactors = FALSE
      )
    }) |> bind_rows()
    
    datatable(
      stats,
      options = list(pageLength = 20, order = list(list(4, "desc"))),
      rownames = FALSE
    ) %>%
      formatRound(c("mean","sd","pct_missing"), digits = 3)
  })
  
  # -------------------------
  # 5E) Legenda škál tabulka
  # -------------------------
  output$legend_dt <- renderDT({
    df <- dat()
    req(df)
    
    drop_cols <- c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa")
    cols <- setdiff(names(df), drop_cols)
    
    leg <- lapply(cols, function(q) {
      lab <- legend_labels_for_question(q)
      data.frame(
        question = q,
        label_1 = if (is.null(lab)) NA_character_ else lab$label_1,
        label_5 = if (is.null(lab)) NA_character_ else lab$label_5,
        stringsAsFactors = FALSE
      )
    }) |> bind_rows()
    
    datatable(
      leg,
      options = list(pageLength = 25, order = list(list(1, "asc"))),
      rownames = FALSE
    )
  })
  
  # -------------------------
  # 5F) Otevřené odpovědi
  # -------------------------
  output$open_ui <- renderUI({
    qi <- question_info()
    if (length(qi) == 0) return(tags$div("Vyber otázky vlevo."))
    open_qs <- names(qi)[vapply(qi, \(z) z$type == "open", logical(1))]
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
    open_qs <- names(qi)[vapply(qi, \(z) z$type == "open", logical(1))]
    
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
    
    nd <- numeric_df()
    cat("\nPočet numerických proměnných pro korelace:", if (is.null(nd)) 0 else ncol(nd), "\n")
    fp <- filtered_pairs()
    cat("Počet párů po filtru |r|:", if (is.null(fp)) 0 else nrow(fp), "\n")
  })
}

shinyApp(ui, server)