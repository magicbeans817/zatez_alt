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
# 1) LEGENDA ŠKÁL 1–5 (z JS)
# -------------------------
# Legendy budou doplněny buď:
# - přes exact match (title == ...)
# - nebo přes prefix/regex match (u rodově proměnných vět)

legend_rules <- list(
  # Obecné
  list(
    pattern = "^Ohodnoťte, jak je pro Vás celková zátěž školou konzistentní\\.$",
    label_1 = "1 – Perfektně konzistentní",
    label_5 = "5 – Velmi volatilní"
  ),
  
  # V každé sekci/předmětu – důležitost (rodové varianty)
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
  
  # Jen SAK/PIV
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
  
  # Jen SAK
  list(
    pattern = "^Jak velkou zátěž pro mne znamená četba\\?$",
    label_1 = "1 – Naprosto zásadní",
    label_5 = "5 – Žádnou"
  )
)

legend_for_question <- function(q_title) {
  for (r in legend_rules) {
    if (str_detect(q_title, regex(r$pattern, ignore_case = FALSE))) {
      return(paste0("Legenda škály: 1 = ", r$label_1, " | 5 = ", r$label_5))
    }
  }
  return(NULL)
}

# -------------------------
# 2) HELPERS: typ otázky
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

# otevřená otázka: hodně unikátů + delší text
is_open_text <- function(x_chr, min_n = 15, uniq_ratio = 0.35, median_len = 20) {
  x <- str_trim(as.character(x_chr))
  x <- x[!is.na(x) & x != ""]
  if (length(x) < min_n) return(FALSE)
  ur <- length(unique(x)) / length(x)
  ml <- median(nchar(x), na.rm = TRUE)
  (ur >= uniq_ratio) && (ml >= median_len)
}

prep_pie <- function(x_chr, top_n = 10, include_missing = TRUE) {
  x <- str_trim(as.character(x_chr))
  if (include_missing) {
    x[is.na(x) | x == ""] <- "(missing)"
  } else {
    x <- x[!is.na(x) & x != ""]
  }
  tab <- sort(table(x), decreasing = TRUE)
  if (length(tab) == 0) return(data.frame(label=character(), prop=numeric()))
  if (length(tab) > top_n) {
    top <- tab[1:top_n]
    other <- sum(tab[(top_n + 1):length(tab)])
    tab <- c(top, Other = other)
  }
  data.frame(label = names(tab),
             prop = as.numeric(tab) / sum(tab),
             n = as.numeric(tab))
}

# -------------------------
# 3) Kategorizace otázek do sekcí (bez JS zásahu, heuristika podle názvu sloupce)
# -------------------------
categorize_questions <- function(cols) {
  cols <- setdiff(cols, c("Do jaké třídy chodíte?", "Časová značka", "Emailová adresa"))
  
  groups <- list(
    "Obecné otázky" = cols[str_detect(cols, regex("Na jaké hodnocení|Z aktivit uvedených|celková zátěž školou", ignore_case = TRUE))],
    "SAK" = cols[str_detect(cols, regex("\\bSAK\\b|\\bSAKu\\b", ignore_case = TRUE))],
    "PIV" = cols[str_detect(cols, regex("\\bPIV\\b|\\bPIVu\\b", ignore_case = TRUE))],
    "Matematika" = cols[str_detect(cols, regex("Matemat", ignore_case = TRUE))],
    "Angličtina" = cols[str_detect(cols, regex("Angličt", ignore_case = TRUE))],
    "Čeština a komunikace" = cols[str_detect(cols, regex("Čeština a komunikace|Češtině a komunikaci", ignore_case = TRUE))],
    "Druhý cizí jazyk" = cols[str_detect(cols, regex("Druhý cizí jazyk|Druhém cizím jazyce", ignore_case = TRUE))],
    "OSV" = cols[str_detect(cols, regex("\\bOSV\\b", ignore_case = TRUE))],
    "Tvorba" = cols[str_detect(cols, regex("Tvorb", ignore_case = TRUE))]
  )
  
  used <- unique(unlist(groups))
  rest <- setdiff(cols, used)
  groups[["Ostatní"]] <- rest
  groups[lengths(groups) > 0]
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
      
      sliderInput("top_n", "TOP kategorií v koláči (zbytek = Other):",
                  min = 5, max = 20, value = 10, step = 1),
      checkboxInput("show_missing", "Zahrnout (missing) u kategorií", value = TRUE),
      
      hr(),
      helpText("Legenda škál (1 vs 5) se bere z tvého JS kódu a zobrazuje se u škálových otázek.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Grafy", uiOutput("plots_ui")),
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
    df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
    dat(df)
  })
  
  output$class_filter_ui <- renderUI({
    df <- dat()
    req(df)
    
    class_col <- "Do jaké třídy chodíte?"
    validate(need(class_col %in% names(df),
                  paste0("V datech chybí sloupec '", class_col, "'.")))
    
    classes <- sort(unique(na.omit(as.character(df[[class_col]]))))
    checkboxGroupInput("classes", "Z jakých tříd chceš data?", choices = classes, selected = classes)
  })
  
  observe({
    df <- dat()
    req(df)
    groups <- categorize_questions(names(df))
    updatePickerInput(session, "questions", choices = groups)
  })
  
  filtered_df <- reactive({
    df <- dat()
    req(df, input$classes)
    class_col <- "Do jaké třídy chodíte?"
    df %>% filter(.data[[class_col]] %in% input$classes)
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
        list(type="numeric", x_num=x_num, x_chr=x_chr)
      } else if (is_open_text(x_chr)) {
        list(type="open", x_num=NULL, x_chr=x_chr)
      } else {
        list(type="categorical", x_num=NULL, x_chr=x_chr)
      }
    })
    names(out) <- qs
    out
  })
  
  output$plots_ui <- renderUI({
    qi <- question_info()
    if (length(qi) == 0) return(tags$div("Vyber otázky vlevo."))
    
    tagList(lapply(names(qi), function(q) {
      pid <- paste0("plot__", digest(q))
      leg <- legend_for_question(q)
      
      tagList(
        tags$h4(q),
        if (!is.null(leg)) tags$div(
          style="background:#f7f7f7;padding:8px;border-radius:6px;margin-bottom:8px;",
          tags$strong(leg)
        ),
        plotOutput(pid, height = "320px"),
        tags$hr()
      )
    }))
  })
  
  observe({
    df <- filtered_df()
    qi <- question_info()
    req(df)
    
    for (q in names(qi)) {
      local({
        qq <- q
        pid <- paste0("plot__", digest(qq))
        info <- qi[[qq]]
        
        output[[pid]] <- renderPlot({
          if (info$type == "numeric") {
            x <- info$x_num
            x <- x[!is.na(x)]
            validate(need(length(x) > 1, "Málo číselných hodnot."))
            
            mu <- mean(x)
            s  <- sd(x)
            
            hist(x, main = qq, xlab = qq)
            abline(v = mu, lwd = 2)
            usr <- par("usr")
            text(usr[2], usr[4],
                 labels = paste0("mean = ", signif(mu, 4), "\n",
                                 "sd   = ", signif(s, 4)),
                 adj = c(1,1))
            
          } else if (info$type == "categorical") {
            pie_df <- prep_pie(info$x_chr, top_n = input$top_n, include_missing = input$show_missing)
            validate(need(nrow(pie_df) > 0, "Bez dat pro koláč."))
            
            ggplot(pie_df, aes(x="", y=prop, fill=label)) +
              geom_col(width=1) +
              coord_polar(theta="y") +
              theme_void() +
              labs(title = "Relativní četnosti")
            
          } else {
            ggplot() +
              annotate("text", x=0, y=0, label="Otevřená otázka → viz záložka 'Otevřené odpovědi'") +
              theme_void()
          }
        })
      })
    }
  })
  
  output$open_ui <- renderUI({
    qi <- question_info()
    if (length(qi) == 0) return(tags$div("Vyber otázky vlevo."))
    open_qs <- names(qi)[vapply(qi, \(z) z$type == "open", logical(1))]
    if (length(open_qs) == 0) return(tags$div("Žádná vybraná otázka nebyla rozpoznána jako otevřená."))
    
    tagList(lapply(seq_along(open_qs), function(i) {
      q <- open_qs[i]
      tid <- paste0("open__", i)
      tagList(
        tags$h4(q),
        DTOutput(tid),
        tags$hr()
      )
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
  
  output$diag <- renderPrint({
    df <- filtered_df()
    qi <- question_info()
    cat("N řádků po filtru tříd:", nrow(df), "\n\n")
    if (length(qi) == 0) {
      cat("Vyber otázky.\n")
    } else {
      cat("Typy otázek:\n")
      for (q in names(qi)) cat("-", q, "=>", qi[[q]]$type, "\n")
      cat("\nLegenda nalezena pro (škály 1–5):\n")
      for (q in names(qi)) {
        lg <- legend_for_question(q)
        if (!is.null(lg)) cat("-", q, "\n")
      }
    }
  })
}

shinyApp(ui, server)