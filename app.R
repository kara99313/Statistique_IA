# ============================================================
# APP Shiny : ETL + Univarié + Bivarié + IA (Rapport & Chat)
# ============================================================

# ---- LIBRAIRIES ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(gridExtra)
library(plotly)
library(visdat)
library(DMwR2)
library(RColorBrewer)
library(reshape2)
library(markdown)    # Markdown -> HTML
library(htmltools)
library(httr)
library(jsonlite)

options(shiny.maxRequestSize = 1000*1024^2)

# ============================================================
#                   PARAMETRES IA (Groq)
# ============================================================
GROQ_URL   <- "https://api.groq.com/openai/v1/chat/completions"
GROQ_MODEL <- "llama-3.1-8b-instant"

# 👉 Chargement sécurisé de la clé Groq
if (file.exists(".Renviron")) readRenviron(".Renviron")
GROQ_API_KEY <- Sys.getenv("GROQ_API_KEY", unset = "")

if (!nzchar(GROQ_API_KEY)) {
  stop("❌ Clé GROQ_API_KEY manquante. Vérifiez votre fichier .Renviron et redémarrez R.")
}

# petit utilitaire %||% (évite erreurs quand x est NULL)
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

# ============================================================
#                   FONCTIONS OUTILS
# ============================================================

# Détection quanti
is_quantitative <- function(x) is.numeric(x) || is.integer(x)

# Tableau QT
akposso_qt_tableau <- function(vecteur) {
  T <- table(vecteur, useNA = "no")
  Tc <- c(T)
  data.frame(
    Effectifs = Tc,
    Eff_Cum_crois = cumsum(Tc),
    Eff_Cum_decrois = rev(cumsum(rev(Tc))),
    Frequence = round(Tc/sum(Tc), 4),
    Freq_Cum_crois = round(cumsum(Tc)/sum(Tc), 4),
    Freq_Cum_decrois = round(rev(cumsum(rev(Tc)))/sum(Tc), 4),
    check.names = FALSE
  )
}

# Graphs QT
akposso_qt_graph <- function(vecteur) {
  df <- data.frame(vecteur = vecteur)
  p1 <- ggplot(df, aes(x = vecteur)) + geom_bar() +
    labs(title = "Diagramme en barres", x = "Valeurs", y = "Effectifs")
  p2 <- ggplot(df, aes(x = vecteur)) + stat_ecdf() +
    labs(title = "Fréquences cumulées", x = "Valeurs", y = "F cumulée")
  p3 <- ggplot(df, aes(x = vecteur)) +
    geom_histogram(aes(y = ..density..), bins = 30, color = "black") +
    geom_density(alpha = .2) + labs(title = "Histogramme & Densité", x = "Valeurs", y = "Densité")
  p4 <- ggplot(df, aes(x = "", y = vecteur)) + geom_boxplot() +
    labs(title = "Boîte à moustaches", x = "", y = "Valeurs")
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# Résumés QT
akposso_qt_resume <- function(vecteur) {
  suppressWarnings({
    m <- mean(vecteur, na.rm = TRUE)
    s <- sd(vecteur,   na.rm = TRUE)
    n <- sum(!is.na(vecteur))
    skew <- if (n > 2 && s > 0) mean(((vecteur - m)/s)^3, na.rm = TRUE) else NA_real_
    kurt <- if (n > 3 && s > 0) mean(((vecteur - m)/s)^4, na.rm = TRUE) else NA_real_
    list(
      minimum = min(vecteur, na.rm = TRUE),
      maximum = max(vecteur, na.rm = TRUE),
      mode    = {ux <- unique(vecteur); ux[which.max(tabulate(match(vecteur, ux)))]},
      mediane = median(vecteur, na.rm = TRUE),
      moyenne = m,
      quantile = quantile(vecteur, na.rm = TRUE),
      coefficient_variation = if (m != 0) s/m else NA_real_,
      variance = var(vecteur, na.rm = TRUE),
      ecart_type = s,
      coefficient_asymetrie = skew,
      interpretation_skewness = ifelse(is.na(skew), NA, ifelse(skew < 0,"étalée à gauche","étalée à droite")),
      coefficent_aplatissement = kurt,
      interpretation_kurtosis = ifelse(is.na(kurt), NA, ifelse(kurt < 3,"platycurtique","leptocurtique"))
    )
  })
}

# QL tableau & graphs
akposso_ql_tableau <- function(facteur) {
  T <- table(facteur, useNA = "no")
  data.frame(Modalite = names(T), Effectif = as.vector(T),
             Frequence = round(as.vector(T)/sum(T), 4), check.names = FALSE)
}
akposso_ql_graph <- function(facteur) {
  df <- data.frame(table(facteur))
  df$freq_rel <- round(100 * df$Freq/sum(df$Freq), 2)
  p1 <- ggplot(df, aes(x = facteur, y = Freq, fill = facteur)) +
    geom_bar(stat = "identity") + labs(title = "Barres (effectifs)", x = "", y = "") +
    theme(legend.position = "none")
  p2 <- ggplot(df, aes(x = facteur, y = freq_rel, fill = facteur)) +
    geom_bar(stat = "identity") + coord_flip() +
    labs(title = "Barres (fréquences %)", x = "", y = "") +
    theme(legend.position = "none")
  p3 <- ggplot(df, aes(x = "", y = Freq, fill = facteur)) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y") +
    labs(title = "Secteurs (effectifs)", x = "", y = "") + theme_void()
  p4 <- ggplot(df, aes(x = "", y = freq_rel, fill = facteur)) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y") +
    labs(title = "Secteurs (fréquences %)", x = "", y = "") + theme_void()
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

# Liaison
akposso_2ql_tableau <- function(f1,f2) {
  m <- table(f1,f2)
  list(
    Tableau_Contingence = m,
    Tableau_Frequence   = round(prop.table(m), 2),
    Tableau_Profil_Ligne= round(prop.table(m,1), 2),
    Tableau_Profil_Colonne=round(prop.table(m,2), 2)
  )
}
akposso_2ql_graph <- function(f1,f2) {
  df12 <- as.data.frame(table(f1,f2)); names(df12) <- c("f1","f2","Freq")
  p1 <- ggplot(df12, aes(x=f1, fill=f2)) + geom_bar() +
    labs(title = "Barres empilées (f1|f2)", x="", y="") + theme(legend.position="bottom")
  p3 <- ggplot(df12, aes(x=f1, y=Freq, fill=f2)) + geom_col(position=position_dodge()) +
    labs(title="Bâtons groupés", x="", y="") + theme(legend.position = "bottom")
  grid.arrange(p1, p3, ncol=2)
}
akposso_2qt_liaison <- function(x,y) {
  r <- suppressWarnings(cor(x,y, method="pearson", use="complete.obs"))
  r2 <- r^2
  p <- try(cor.test(x,y), silent = TRUE)
  list(
    Correlation_Pearson = r,
    Coefficient_Determination = r2,
    p.value = if(inherits(p,"try-error")) NA_real_ else p$p.value,
    Significativite = if(inherits(p,"try-error")) NA_character_ else ifelse(p$p.value<0.05,"significative","non significative")
  )
}
akposso_qtql_liaison <- function(x, f) {
  f <- as.factor(f)
  fit <- lm(x ~ f)
  a  <- anova(fit)
  p  <- tryCatch(a$`Pr(>F)`[1], error=function(e) NA_real_)
  ss_tot <- sum((x-mean(x,na.rm=TRUE))^2, na.rm=TRUE)
  ss_mod <- sum((fitted(fit)-mean(x,na.rm=TRUE))^2, na.rm=TRUE)
  eta2   <- ifelse(ss_tot>0, ss_mod/ss_tot, NA_real_)
  list(Anova = a, p.value = p, eta2 = eta2)
}

# Winsorisation maison (compat DescTools)
winsorize_vec <- function(v, probs = c(0.01, 0.99)) {
  if (!is.numeric(v)) return(v)
  if (all(is.na(v))) return(v)
  ql <- suppressWarnings(quantile(v, probs[1], na.rm=TRUE))
  qh <- suppressWarnings(quantile(v, probs[2], na.rm=TRUE))
  v2 <- v
  idx <- !is.na(v2)
  v2[idx & v2 < ql] <- ql
  v2[idx & v2 > qh] <- qh
  v2
}

# ---------- NA VISUALS ROBUSTES ----------
sanitize_for_vis <- function(df) {
  if (!is.data.frame(df)) df <- as.data.frame(df, check.names = FALSE)
  conv_col <- function(x) {
    if (is.factor(x) || is.atomic(x)) return(x)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(x))
    if (is.list(x)) {
      return(vapply(x, function(el) {
        if (length(el) == 0) return(NA_character_)
        if (length(el) == 1) return(as.character(el))
        paste0("[", length(el), " éléments]")
      }, character(1)))
    }
    as.character(x)
  }
  as.data.frame(lapply(df, conv_col), stringsAsFactors = FALSE, check.names = FALSE)
}

na_heatmap <- function(df, title = "") {
  if (NROW(df) == 0 || NCOL(df) == 0) {
    ggplot() + theme_void() + ggtitle(paste(title, "(aucune donnée)"))
  } else {
    long <- data.frame(
      row = rep(seq_len(nrow(df)), times = ncol(df)),
      col = rep(names(df), each = nrow(df)),
      na  = as.vector(is.na(df))
    )
    ggplot(long, aes(x = col, y = row, fill = na)) +
      geom_raster() +
      scale_y_reverse() +
      scale_fill_manual(values = c("#f3f3f3", "#00bcd4"),
                        labels = c("NON", "OUI"), name = "NA") +
      labs(title = title, x = "Variables", y = "Lignes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

safe_vis_plot <- function(df, title = "") {
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(ggplot() + theme_void() + ggtitle(paste(title, "(aucune donnée)")))
  }
  df2 <- sanitize_for_vis(df)
  p <- try(visdat::vis_miss(df2) + ggplot2::labs(title = title), silent = TRUE)
  if (inherits(p, "try-error")) na_heatmap(df2, title) else p
}

# Markdown -> HTML joli
render_md_block <- function(md_text) {
  if (is.null(md_text) || !nzchar(md_text)) return(HTML("<div class='md-block'>Aucun contenu</div>"))
  html <- markdown::markdownToHTML(text = md_text, fragment.only = TRUE)
  HTML(sprintf("<div class='md-block'>%s</div>", html))
}

# ============================================================
#                           UI
# ============================================================

custom_css <- "
/* Styles généraux & markdown */
.content-wrapper { background:#f7f8fb; }
.box { border-radius:14px; box-shadow:0 4px 14px rgba(0,0,0,.05); border: none; }
.md-block { background:#fff; border-radius:12px; padding:16px 18px; color:#2b2b2b; line-height:1.6; font-size:15px; box-shadow:0 4px 12px rgba(0,0,0,.05); }
.md-block h1,.md-block h2,.md-block h3,.md-block h4 { color:#4b4df2; font-weight:600; margin-top:10px; }
.md-block h3{font-size:18px;} .md-block h4{font-size:16px;}
.md-block ul,.md-block ol{margin:6px 0 10px 20px;}
/* Chat bubbles */
.chat-bubble-ai { display:inline-block; max-width:90%; background:#f7f7ff; color:#2b2b2b; border:1px solid #e7e7ff; border-radius:14px 14px 14px 4px; padding:12px 14px; margin:6px 0; }
.chat-bubble-user { display:inline-block; max-width:90%; background:#6c63ff; color:#fff; border-radius:14px 14px 4px 14px; padding:12px 14px; margin:6px 0; }
.chat-time { opacity:.6; font-size:12px; display:block; margin-bottom:4px; }
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = tags$span("STAT •", tags$b("DESC"))),
  dashboardSidebar(
    width = 320,
    tags$style(HTML(custom_css)),
    br(),
    h4("Importer les données"),
    fileInput("file_input", "Fichier CSV", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    radioGroupButtons(
      "delimiter_input", "Délimiteur",
      choices = c("Virgule"=",","Point-virgule"=";","Tabulation"="\t","Espace"=" ","Autre"),
      selected = ",", justified = TRUE, status = "primary", size = "sm"
    ),
    conditionalPanel("input.delimiter_input == 'Autre'",
                     textInput("custom_delimiter", "Délimiteur personnalisé", "")),
    hr(),
    uiOutput("ui_select_transform"),
    hr(),
    p(tags$strong("Didier Martial AKPOSSO")), p(em("Directeur - Fondateur de INSSEDS")),
    p("Tel : +225 07-77-24-19-96"), p("Email : admartial@gmail.com"),
    p(a("Site Web INSSEDS", href = "https://insseds-edu.com/", target = "_blank"))
  ),
  dashboardBody(
    # JS pour faire défiler le chat
    tags$script(
      'Shiny.addCustomMessageHandler("scrollToAnchor", function(id){
         var el = document.getElementById(id);
         if(el){ el.scrollIntoView({behavior:"smooth", block:"end"}); }
       });'
    ),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = HTML("1) Préparation <span class='badge'>ETL</span>"),
        br(),
        fluidRow(
          box(width = 12, title = "Jeu de données importé", status = "primary", solidHeader = TRUE,
              DTOutput("display_data"))
        ),
        fluidRow(
          column(6, box(width = 12, title = "Avant transformation (structure)", status = "info", solidHeader = TRUE,
                        verbatimTextOutput("data_before"))),
          column(6, box(width = 12, title = "Après transformation (structure)", status = "success", solidHeader = TRUE,
                        verbatimTextOutput("data_after")))
        ),
        fluidRow(
          column(4, actionButton("transform_variables", "Appliquer le prétraitement", icon = icon("magic"), class = "btn btn-success")),
          column(8, p(class="small-note", "Transformation QT→QL (si sélectionnées) + traitement manquants + winsorisation."))
        ),
        br(),
        fluidRow(
          box(width = 12, title = "Valeurs manquantes", status = "primary", solidHeader = TRUE,
              splitLayout(cellWidths = c("50%","50%"),
                          tagList(h5("Avant imputation"),
                                  plotOutput("before_imputation_plot", height = "360px"),
                                  plotOutput("before_missing_bar", height = "140px")),
                          tagList(h5("Après imputation / filtrage"),
                                  uiOutput("after_imputation_right"))
              )
          ),
          box(width = 12, title = "Valeurs extrêmes (Winsorisation)", status = "warning", solidHeader = TRUE,
              splitLayout(cellWidths = c("50%","50%"),
                          plotOutput("before_winsorize_plot", height = "420px"),
                          plotOutput("winsorized_boxplots", height = "420px")))
        ),
        fluidRow(
          box(width = 12, title = "Aperçu final & export", status = "success", solidHeader = TRUE,
              verbatimTextOutput("data_structure"),
              downloadButton("download_data", "Télécharger (CSV)", class = "btn-primary"))
        )
      ),
      tabPanel(
        title = HTML("2) Statistiques <span class='badge'>UNIVARIÉE</span>"),
        br(),
        fluidRow(
          box(width = 12, title = "Jeu de données prétraité", status = "primary", solidHeader = TRUE,
              DTOutput("dataset_uni"))
        ),
        fluidRow(
          box(width = 4, title = "Sélection", status = "info", solidHeader = TRUE,
              uiOutput("uni_select_var")),
          box(width = 8, title = "Résumé du jeu de données", status = "info", solidHeader = TRUE,
              verbatimTextOutput("dataset_summary"))
        ),
        fluidRow(
          box(width = 6, title = "Tableau statistique", status = "primary", solidHeader = TRUE,
              tableOutput("uni_table")),
          box(width = 6, title = "Graphiques", status = "primary", solidHeader = TRUE,
              plotOutput("uni_plot", height = "480px"))
        ),
        fluidRow(
          box(width = 12, title = "Résumés numériques (QT)", status = "success", solidHeader = TRUE,
              verbatimTextOutput("uni_summary"))
        )
      ),
      tabPanel(
        title = HTML("3) Statistiques <span class='badge'>BIVARIÉE</span>"),
        br(),
        fluidRow(
          box(width = 12, title = "Jeu de données prétraité", status = "primary", solidHeader = TRUE,
              DTOutput("dataset_bi"))
        ),
        fluidRow(
          box(width = 12, title = "Sélection des variables", status = "info", solidHeader = TRUE,
              splitLayout(cellWidths = c("50%","50%"),
                          uiOutput("bi_var1_selector"), uiOutput("bi_var2_selector")))
        ),
        fluidRow(
          box(width = 6, title = "Tableau statistique", status = "primary", solidHeader = TRUE,
              verbatimTextOutput("bi_tableau_stat")),
          box(width = 6, title = "Graphiques", status = "primary", solidHeader = TRUE,
              plotOutput("bi_graphique", height = "520px"))
        ),
        fluidRow(
          box(width = 12, title = "Tests / Régression", status = "success", solidHeader = TRUE,
              verbatimTextOutput("bi_resume"))
        )
      ),
      tabPanel(
        title = HTML("4) <span class='badge'>IA</span> Rapport & Chat"),
        br(),
        fluidRow(
          box(width = 4, title = "Génération de rapport IA", status = "primary", solidHeader = TRUE,
              textAreaInput("user_question", "Consignes pour le rapport :", rows = 5,
                            placeholder = "Ex: Insister sur la distribution de AGE et la relation AGE~SEXE..."),
              awesomeRadio("report_type","Type de rapport :",
                           choices = c("Synthèse"="summary","Détaillé"="detailed","Recommandations"="reco"),
                           selected = "summary"),
              sliderInput("ia_temp","Température :", min=0,max=1,value=.7,step=.05),
              sliderInput("ia_tokens","Longueur (tokens) :", min=200,max=2000,value=1000,step=50),
              actionBttn("gen_report","Générer le rapport", icon=icon("robot"), style="material-flat", color="primary")
          ),
          box(width = 8, title = "Rapport généré (Markdown rendu)", status = "info", solidHeader = TRUE,
              div(style="height: 520px; overflow-y:auto;", uiOutput("llm_report_html")))
        ),
        fluidRow(
          box(width = 4, title = "Chat IA", status = "primary", solidHeader = TRUE,
              textAreaInput("chat_question","Votre question :", rows = 3),
              sliderInput("chat_temp","Température :", min=0,max=1,value=.7,step=.05),
              sliderInput("chat_tokens","Longueur :", min=100,max=1500,value=500,step=50),
              actionBttn("send_chat","Envoyer", icon=icon("paper-plane"), style="material-flat", color="primary")
          ),
          box(width = 8, title = "Réponses du chatbot", status = "info", solidHeader = TRUE,
              div(style="height: 480px; overflow-y:auto;", uiOutput("chat_response_html")))
        )
      )
    )
  )
)

# ============================================================
#                         SERVER
# ============================================================

server <- function(input, output, session) {

  # ---------- Helper: appel Groq robuste ----------
  groq_post <- function(body) {
    res <- tryCatch(
      httr::POST(
        url = GROQ_URL,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::add_headers(
          Authorization = paste("Bearer", GROQ_API_KEY),
          "Content-Type" = "application/json"
        ),
        httr::timeout(30)
      ),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      showNotification(paste("Erreur réseau :", res$message), type = "error")
      return(NULL)
    }
    if (httr::status_code(res) >= 400) {
      msg <- try(httr::content(res, as = "text", encoding = "UTF-8"), silent = TRUE)
      showNotification(paste("Erreur API :", httr::status_code(res), msg %||% ""), type = "error")
      return(NULL)
    }
    httr::content(res, as = "parsed")
  }

  # ================= Import & lecture =================
  data_raw <- reactive({
    req(input$file_input)
    delimiter <- if (identical(input$delimiter_input, "Autre")) input$custom_delimiter else input$delimiter_input
    if (is.null(delimiter) || !nzchar(delimiter)) {
      showNotification("Veuillez préciser un délimiteur.", type = "error")
      return(NULL)
    }
    tryCatch(
      read.csv(input$file_input$datapath, sep = delimiter, stringsAsFactors = TRUE, check.names = FALSE),
      error = function(e) { showNotification(paste("Erreur de lecture :", e$message), type="error"); NULL }
    )
  })

  output$display_data <- renderDT({
    req(data_raw())
    datatable(data_raw(), options = list(scrollX=TRUE, pageLength=10))
  })

  output$ui_select_transform <- renderUI({
    req(data_raw())
    quant_vars <- names(which(sapply(data_raw(), is.numeric)))
    if (length(quant_vars)==0) return(helpText("Aucune variable quantitative détectée."))
    tagList(
      pickerInput("quant_variables","Variables QT → QL (optionnel)", choices = quant_vars,
                  multiple = TRUE, options = list(`actions-box`=TRUE, `live-search`=TRUE)),
      helpText("Laissez vide si vous ne souhaitez pas transformer.")
    )
  })

  output$data_before <- renderPrint({ req(data_raw()); str(data_raw()) })
  output$data_after  <- renderPrint({ req(data_preprocessed()); str(data_preprocessed()) })

  # ================= Prétraitement =====================
  data_preprocessed <- reactiveVal(NULL)

  observeEvent(input$transform_variables, {
    req(data_raw())
    data0 <- data_raw()

    # 1) QT sélectionnées -> facteurs (optionnel)
    if (!is.null(input$quant_variables) && length(input$quant_variables)>0) {
      inter <- intersect(input$quant_variables, names(data0))
      for (v in inter) data0[[v]] <- as.factor(data0[[v]])
    }

    # 2) Manquants (seuil 5%)
    missing_rows_pct <- round(sum(!complete.cases(data0))/nrow(data0)*100, 2)
    if (missing_rows_pct <= 5) {
      data1 <- stats::na.omit(data0)
    } else {
      num_cols <- names(which(sapply(data0, is.numeric)))
      fac_cols <- names(which(sapply(data0, is.factor)))
      data1 <- data0
      if (length(num_cols)>0) {
        set.seed(123)
        tmp <- DMwR2::knnImputation(data0[, num_cols, drop=FALSE], k = 5)
        data1[, num_cols] <- tmp
      }
      if (length(fac_cols)>0) {
        for (fc in fac_cols) {
          v <- data1[[fc]]
          if (anyNA(v)) {
            tab <- sort(table(v), decreasing = TRUE)
            v[is.na(v)] <- names(tab)[1]
            data1[[fc]] <- droplevels(v)
          }
        }
      }
    }

    # 3) Winsorisation num
    num_cols2 <- names(which(sapply(data1, is.numeric)))
    if (length(num_cols2)>0) for (v in num_cols2) data1[[v]] <- winsorize_vec(data1[[v]])

    data_preprocessed(data1)
    showNotification("Prétraitement terminé.", type="message")
  })

  # ======= Valeurs manquantes (visdat + heatmap NA) =======
  output$before_imputation_plot <- renderPlot({
    req(data_raw())
    safe_vis_plot(data_raw(), "Avant imputation")
  })

  output$before_missing_bar <- renderPlot({
    req(data_raw())
    df <- data_raw()
    na_pct <- sapply(df, function(x) mean(is.na(x))*100)
    bar_df <- data.frame(var = names(na_pct), pct = as.numeric(na_pct))
    ggplot(bar_df, aes(x=reorder(var,pct), y=pct)) + geom_col() + coord_flip() +
      labs(x="", y="% NA", title="Pourcentage de NA par variable (avant)") + theme_minimal()
  })

  output$after_imputation_right <- renderUI({
    req(data_preprocessed())
    tagList(
      plotOutput("after_imputation_plot", height = "360px"),
      br(),
      verbatimTextOutput("missing_percentage"),
      verbatimTextOutput("missing_data_message")
    )
  })

  output$after_imputation_plot <- renderPlot({
    req(data_preprocessed())
    safe_vis_plot(data_preprocessed(), "Après imputation / filtrage")
  })

  output$missing_percentage <- renderText({
    req(data_preprocessed())
    d <- data_preprocessed()
    paste("Pourcentage d'individus ayant encore des données manquantes :",
          round(nrow(d[!complete.cases(d),])/nrow(d)*100, 2), "%")
  })

  output$missing_data_message <- renderText({
    req(data_raw())
    p <- round(sum(!complete.cases(data_raw()))/nrow(data_raw())*100, 2)
    if (p <= 5) "Règle appliquée : individus incomplets supprimés (<= 5%)."
    else "Règle appliquée : imputation KNN (numériques) + mode (facteurs) (> 5%)."
  })

  # ======= Winsor plots =======
  output$before_winsorize_plot <- renderPlot({
    req(data_raw())
    d <- data_raw(); qs <- names(which(sapply(d, is.numeric)))
    if (length(qs)==0) { plot.new(); title("Aucune colonne numérique à afficher."); return() }
    ggplot(stack(d[qs]), aes(x=ind, y=values, fill=ind)) + geom_boxplot() +
      labs(x="", y="", title="Avant winsorisation") + theme(axis.text.x = element_text(angle=45, hjust=1))
  })
  output$winsorized_boxplots <- renderPlot({
    req(data_preprocessed())
    d <- data_preprocessed(); qs <- names(which(sapply(d, is.numeric)))
    if (length(qs)==0) { plot.new(); title("Aucune colonne numérique à afficher."); return() }
    ggplot(stack(d[qs]), aes(x=ind, y=values, fill=ind)) + geom_boxplot() +
      labs(x="", y="", title="Après winsorisation") + theme(axis.text.x = element_text(angle=45, hjust=1))
  })

  # ======= Export / structure =======
  output$data_structure <- renderPrint({ req(data_preprocessed()); str(data_preprocessed()) })
  output$download_data <- downloadHandler(
    filename = function() paste0("donnees_pretraitees_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(data_preprocessed(), file, row.names = FALSE, na = "")
  )

  # ================== UNIVARIE ==================
  output$dataset_uni <- renderDT({ req(data_preprocessed()); datatable(data_preprocessed(), options=list(scrollX=TRUE, pageLength=10)) })
  output$uni_select_var <- renderUI({ req(data_preprocessed()); selectInput("select_var_uni","Choisir une variable", choices = names(data_preprocessed())) })
  output$dataset_summary <- renderPrint({ req(data_preprocessed()); summary(data_preprocessed()) })
  output$uni_table <- renderTable({
    req(data_preprocessed(), input$select_var_uni)
    v <- data_preprocessed()[, input$select_var_uni]
    if (is_quantitative(v)) head(round(akposso_qt_tableau(v),3),10) else akposso_ql_tableau(as.factor(v))
  })
  output$uni_plot <- renderPlot({
    req(data_preprocessed(), input$select_var_uni)
    v <- data_preprocessed()[, input$select_var_uni]
    if (is_quantitative(v)) akposso_qt_graph(v) else akposso_ql_graph(as.factor(v))
  })
  output$uni_summary <- renderPrint({
    req(data_preprocessed(), input$select_var_uni)
    v <- data_preprocessed()[, input$select_var_uni]
    if (is_quantitative(v)) akposso_qt_resume(v) else cat("Les résumés numériques ne s'appliquent qu'aux variables quantitatives.")
  })

  # ================== BIVARIE ==================
  output$dataset_bi <- renderDT({ req(data_preprocessed()); datatable(data_preprocessed(), options=list(scrollX=TRUE, pageLength=10)) })
  output$bi_var1_selector <- renderUI({ req(data_preprocessed()); selectInput("bi_var1","Variable 1", choices = names(data_preprocessed())) })
  output$bi_var2_selector <- renderUI({ req(data_preprocessed()); selectInput("bi_var2","Variable 2", choices = names(data_preprocessed())) })

  output$bi_tableau_stat <- renderPrint({
    req(data_preprocessed(), input$bi_var1, input$bi_var2)
    v1 <- data_preprocessed()[[input$bi_var1]]; v2 <- data_preprocessed()[[input$bi_var2]]
    if (is.factor(v1) && is.factor(v2)) akposso_2ql_tableau(v1, v2)
    else if (!is.factor(v1) && !is.factor(v2)) cat("Le tableau croisé n'est pas pertinent pour 2 quantitatives.")
    else print(table(v1, v2))
  })

  output$bi_graphique <- renderPlot({
    req(data_preprocessed(), input$bi_var1, input$bi_var2)
    v1 <- data_preprocessed()[[input$bi_var1]]; v2 <- data_preprocessed()[[input$bi_var2]]
    df <- data_preprocessed()
    if (is.factor(v1) && is.factor(v2)) {
      akposso_2ql_graph(v1, v2)
    } else if (!is.factor(v1) && !is.factor(v2)) {
      ggplot(df, aes_string(x=input$bi_var1, y=input$bi_var2)) + geom_point(alpha=.7) +
        geom_smooth(method="lm", se=FALSE) + labs(title="Nuage de points & droite de régression")
    } else {
      ggplot(df, aes_string(x=input$bi_var1, y=input$bi_var2, fill=input$bi_var1)) +
        geom_boxplot() + labs(title="Diagramme en boîte", x="", y="")
    }
  })

  output$bi_resume <- renderPrint({
    req(data_preprocessed(), input$bi_var1, input$bi_var2)
    v1 <- data_preprocessed()[[input$bi_var1]]; v2 <- data_preprocessed()[[input$bi_var2]]
    if (is.factor(v1) && is.factor(v2)) {
      m <- table(v1,v2); chi <- tryCatch(chisq.test(m), error=function(e) NULL)
      v <- if(!is.null(chi)) { n <- sum(m); k <- min(nrow(m), ncol(m)); sqrt(as.numeric(chi$statistic)/(n*(k-1))) } else NA_real_
      list(Chi2 = chi, V_Cramer = v)
    } else if (!is.factor(v1) && !is.factor(v2)) {
      akposso_2qt_liaison(v1, v2)
    } else {
      if (is_quantitative(v1) && is.factor(v2)) akposso_qtql_liaison(v1, v2) else akposso_qtql_liaison(v2, v1)
    }
  })

  # ================== IA : Rapport ==================
  rv <- reactiveValues(rapport_ia = "", chat_history = list())

  output$llm_report_html <- renderUI({ req(rv$rapport_ia); render_md_block(rv$rapport_ia) })

  observeEvent(input$gen_report, {
    req(data_preprocessed())
    d <- data_preprocessed()
    numeric_vars <- names(which(sapply(d, is.numeric)))
    factor_vars  <- names(which(sapply(d, is.factor)))
    brief <- paste0(
      "Variables numériques: ", paste(numeric_vars, collapse=", "), "\n",
      "Variables qualitatives: ", paste(factor_vars, collapse=", "), "\n",
      "Taille: ", nrow(d), " lignes x ", ncol(d), " colonnes."
    )
    prompt <- paste0(
      "Rédige un rapport **Markdown** bien structuré (titres ##, listes, paragraphes).\n",
      "Sections attendues : ## Introduction, ## Données, ## Méthodes, ## Résultats, ## Interprétation, ## Limites, ## Conclusion.\n",
      "Style professionnel, concis, en français.\n\n",
      "Contexte de l'analyse (auto) :\n", brief, "\n\n",
      if (nzchar(input$user_question)) paste0("Consignes spécifiques :\n", input$user_question, "\n\n"),
      "N'invente pas de chiffres précis si non fournis."
    )
    body <- list(
      model = GROQ_MODEL,
      messages = list(list(role="user", content=prompt)),
      temperature = input$ia_temp,
      max_tokens  = input$ia_tokens,
      top_p = 0.9
    )

    cont <- groq_post(body)
    if (is.null(cont)) return(NULL)

    rv$rapport_ia <- cont$choices[[1]]$message$content %||% "Réponse vide."
    showNotification("Rapport IA généré.", type="message")
  })

  # ================== IA : Chat ==================
  update_chat_output <- function() {
    output$chat_response_html <- renderUI({
      if (length(rv$chat_history)==0) {
        return(div(style="text-align:center; color: #999; padding:20px;", "Posez votre première question à l'IA..."))
      }
      tagList(lapply(rv$chat_history, function(msg) {
        if (identical(msg$role,"user")) {
          div(style="margin-bottom:12px; text-align:right;",
              div(class="chat-bubble-user",
                  span(class="chat-time", msg$time),
                  HTML(htmlEscape(msg$content))
              ))
        } else {
          div(style="margin-bottom:12px; text-align:left;",
              div(class="chat-bubble-ai",
                  span(class="chat-time", msg$time),
                  render_md_block(msg$content)
              ))
        }
      }), tags$div(id="anchor"))
    })
    session$sendCustomMessage("scrollToAnchor","anchor")
  }

  observeEvent(input$send_chat, {
    req(input$chat_question)
    rv$chat_history <- c(rv$chat_history, list(list(role="user", content=input$chat_question, time=format(Sys.time(), "%H:%M"))))
    update_chat_output()

    ctx <- ""
    if (!is.null(data_preprocessed())) {
      d <- data_preprocessed()
      ctx <- paste0("Données disponibles: ", nrow(d), " lignes x ", ncol(d), " colonnes. ",
                    "Variables: ", paste(names(d), collapse=", "), ". Réponds en français.")
    }
    body <- list(
      model = GROQ_MODEL,
      messages = list(
        list(role="system", content="Tu es un assistant expert en statistique descriptive, concis et pédagogue."),
        list(role="user", content=paste(ctx, "\nQuestion :", input$chat_question))
      ),
      temperature = input$chat_temp,
      max_tokens  = input$chat_tokens,
      top_p = 0.9
    )

    cont <- groq_post(body)
    if (is.null(cont)) {
      rv$chat_history <- c(rv$chat_history, list(list(role="assistant",
                                                      content="(Aucune réponse : problème réseau/API)",
                                                      time=format(Sys.time(), "%H:%M"))))
      update_chat_output()
      return(NULL)
    }

    reply <- cont$choices[[1]]$message$content %||% "Réponse vide."
    rv$chat_history <- c(rv$chat_history, list(list(role="assistant", content=reply, time=format(Sys.time(), "%H:%M"))))
    update_chat_output()
  })
}

# ============================================================
#                      LANCEMENT APP
# ============================================================
shinyApp(ui, server)
