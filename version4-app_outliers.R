# app.R — Jeu interactif outliers
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("🎯 Comprendre les outliers en régression : le jeu des 3 défis "),
  p("Cher.e étudiant.e, es-tu un.e expert.e des données aberrantes en régression linéaire ? Ce mini-jeu est fait pour le savoir."),  
  
  p("Dans ce jeu, ton rôle est simple: ajouter UN point (rouge) en choisissant ses coordonnées X et Y ci-dessous."),
  
  p("TON BUT: que ce point devienne un OUTLIER !"),
  
  p("👉 Trois défis t'attendent :"),
  tags$ul(
    tags$li("LEVIER : Créer UNIQUEMENT un outlier sur X (levier élevé), sans dépasser le seuil Résidu & Cook"),
    tags$li("RÉSIDU : Créer UNIQUEMENT un outlier sur Y (résidu studentisé élevé), sans dépasser le seuil Levier & Cook"),
    tags$li("COOK: Créer UNIQUEMENT un outlier influent (distance de Cook élevée), sans dépasser le seuil Levier & Résidu")
  ),
  
  p("Une fois que tu as réussi les trois défis avec un jeu de données (X prédit Y), clique sur 'Nouveau jeu de données' pour obtenir un nuage différent."),
  
  p("Tes SCORES sont mesurés: Un défi relevé = 1 point, sauf pour le défi COOK (2 points) car il est plus difficile."),
  
  p("Bonne chance à toi !😊 "),
  p("NB : Pourquoi 2 points pour le défi COOK ? Le défi COOK est difficile à réaliser SEUL car la distance de Cook dépend de la valeur du Levier (déviant sur X) et de la valeur du Résidu (déviant sur Y) de l’observation. Trouver le point qui dépasse le seuil COOK sans dépasser le seuil LEVIER et le seuil RÉSIDU n’est donc pas une mince affaire 😉 "),
  hr(),
  sidebarLayout(
    sidebarPanel(
      actionButton("newdata", "🎲 Nouveau jeu de données"),
      actionButton("resetScore", "🔄 Réinitialiser le score"),
      tags$hr(),
      h4("Coordonnées du point rouge"),
      uiOutput("xSlider"),
      uiOutput("ySlider"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(7, plotOutput("scatterPlot", height = 420)),
        column(5,
               h4("Diagnostics du point rouge"),
               tableOutput("diagPoint"),
               p(em("Seuils (calculés à chaque jeu) :")),
               textOutput("seuilsTxt"),
               tags$hr(),
               h4("Résultats des défis (exclusifs)"),
               textOutput("defi_levier"),
               textOutput("defi_resid"),
               textOutput("defi_cook"),
               tags$hr(),
               h3("Score global"),
               textOutput("scoreTxt"),
               textOutput("scoreDetail")
        )
      ),
      tags$hr(),
      h4("Tableau des diagnostics (tous les points)"),
      tableOutput("diagTable")
    )
  )
)

server <- function(input, output, session) {
  
  # état global
  state <- reactiveValues(
    score = 0,
    lev_done = FALSE,
    resid_done = FALSE,
    cook_done = FALSE,
    dataset_id = 1
  )
  
  observeEvent(input$resetScore, {
    state$score <- 0
    state$lev_done <- FALSE
    state$resid_done <- FALSE
    state$cook_done <- FALSE
  })
  
  # générateur jeu de données
  gen_dataset <- function(seed = NULL){
    if (!is.null(seed)) set.seed(seed)
    n <- sample(10:14, 1)
    x <- sort(runif(n, 0, 10))
    b0 <- runif(1, 1, 5)
    b1 <- sample(c(-1.2, -0.8, -0.4, 0, 0.3, 0.7, 1.2, 1.8), 1)
    sigma <- sample(c(0.4, 0.8, 1.2, 2, 3), 1)
    y <- b0 + b1 * x + rnorm(n, 0, sigma)
    data.frame(x = x, y = y)
  }
  
  base_data <- reactiveVal(gen_dataset(123))
  
  observeEvent(input$newdata, {
    seed <- as.integer(Sys.time()) %% 10000 + state$dataset_id
    base_data(gen_dataset(seed))
    state$dataset_id <- state$dataset_id + 1
    state$lev_done <- FALSE; state$resid_done <- FALSE; state$cook_done <- FALSE
  })
  
  # sliders dynamiques
  output$xSlider <- renderUI({
    df <- base_data(); req(df)
    xr <- range(df$x); margin <- max(0.5, diff(xr) * 0.25)
    sliderInput("x_new", "X du point rouge :", min = floor(xr[1] - margin),
                max = ceiling(xr[2] + margin), value = round(mean(xr), 1), step = 0.1)
  })
  output$ySlider <- renderUI({
    df <- base_data(); req(df)
    yr <- range(df$y); margin <- max(0.5, diff(yr) * 0.25)
    sliderInput("y_new", "Y du point rouge :", min = floor(yr[1] - margin),
                max = ceiling(yr[2] + margin), value = round(mean(yr), 1), step = 0.1)
  })
  
  # données complètes
  data_all <- reactive({
    df <- base_data(); req(df)
    if (is.null(input$x_new) || is.null(input$y_new)) return(df)
    df2 <- rbind(df, data.frame(x = input$x_new, y = input$y_new))
    df2$id <- seq_len(nrow(df2))
    df2$nouveau_point <- ifelse(seq_len(nrow(df2)) == nrow(df2), "OUI", "NON")
    df2
  })
  
  # diagnostics
  diag_all <- reactive({
    df <- data_all(); req(df)
    m <- tryCatch(stats::lm(y ~ x, data = df), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    n <- nrow(df)
    
    lev <- tryCatch(hatvalues(m), error = function(e) rep(NA_real_, n))
    if (length(lev) != n) lev <- rep(NA_real_, n)
    
    stud <- tryCatch(rstudent(m), error = function(e) rep(NA_real_, n))
    if (!is.numeric(stud) || length(stud) != n) stud <- rep(NA_real_, n)
    
    cook <- tryCatch(cooks.distance(m), error = function(e) rep(NA_real_, n))
    if (length(cook) != n) cook <- rep(NA_real_, n)
    
    data.frame(
      ID = seq_len(n),
      X = df$x,
      Y = df$y,
      Levier = round(as.numeric(lev), 3),
      R_student = round(as.numeric(stud), 3),
      Cook = round(as.numeric(cook), 3),
      nouveau_point = df$nouveau_point,
      stringsAsFactors = FALSE
    )
  })
  
  seuils <- reactive({
    d <- diag_all(); req(d)
    n <- nrow(d); k <- 2
    list(lev = 2*k/n, stud = 2, cook = 4/n, n = n)
  })
  
  # graphique
  output$scatterPlot <- renderPlot({
    df <- data_all(); req(df)
    m <- tryCatch(stats::lm(y ~ x, data = df), error = function(e) NULL)
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(color = nouveau_point), size = 3) +
      scale_color_manual(values = c("NON"="black", "OUI"="red"), guide=FALSE) +
      theme_minimal()
    if (!is.null(m)) p <- p + geom_smooth(method = "lm", se = FALSE, color = "dodgerblue")
    p
  })
  
  output$diagTable <- renderTable({ diag_all() }, digits = 3)
  output$diagPoint <- renderTable({
    d <- diag_all(); req(d)
    d[nrow(d), , drop = FALSE]
  }, digits = 3)
  output$seuilsTxt <- renderText({
    s <- seuils()
    paste0("Seuil levier: ", round(s$lev, 3),
           " | Seuil |résidu|: ", s$stud,
           " | Seuil Cook: ", round(s$cook, 3),
           " (n = ", s$n, ")")
  })
  
  # --- logique des défis (reactive + outputs) ---
  defis <- reactive({
    d <- diag_all(); req(d); s <- seuils()
    last <- d[nrow(d), ]
    lev <- last$Levier; stud <- last$R_student; cook <- last$Cook
    
    list(
      lev_ok_only  = (lev > s$lev) && (abs(stud) <= s$stud) && (cook <= s$cook),
      stud_ok_only = (abs(stud) > s$stud) && (lev <= s$lev) && (cook <= s$cook),
      cook_ok_only = (cook > s$cook) && (lev <= s$lev) && (abs(stud) <= s$stud),
      lev = lev, stud = stud, cook = cook, s = s
    )
  })
  
  output$defi_levier <- renderText({
    d <- defis()
    if (d$lev_ok_only) {
      if (!state$lev_done) { state$score <- state$score + 1; state$lev_done <- TRUE }
      paste0("✅ LEVIER réussi (", round(d$lev,3), " > ", round(d$s$lev,3), ")")
    } else {
      paste0("❌ LEVIER non atteint (", round(d$lev,3), " ≤ ", round(d$s$lev,3), ")")
    }
  })
  
  output$defi_resid <- renderText({
    d <- defis()
    if (d$stud_ok_only) {
      if (!state$resid_done) { state$score <- state$score + 1; state$resid_done <- TRUE }
      paste0("✅ RÉSIDU réussi (|", round(d$stud,3), "| > ", d$s$stud, ")")
    } else {
      paste0("❌ RÉSIDU non atteint (|", round(d$stud,3), "| ≤ ", d$s$stud, ")")
    }
  })
  
  output$defi_cook <- renderText({
    d <- defis()
    if (d$cook_ok_only) {
      if (!state$cook_done) { state$score <- state$score + 2; state$cook_done <- TRUE } # 2 points ici
      paste0("✅ COOK réussi (", round(d$cook,3), " > ", round(d$s$cook,3), ")")
    } else {
      paste0("❌ COOK non atteint (", round(d$cook,3), " ≤ ", round(d$s$cook,3), ")")
    }
  })
  
  output$scoreTxt <- renderText({ paste("Score cumulé :", state$score, "points") })
  output$scoreDetail <- renderText({
    paste0("Défis validés dans ce jeu : ",
           if (state$lev_done) "LEVIER " else "",
           if (state$resid_done) "RÉSIDU " else "",
           if (state$cook_done) "COOK" else "(aucun)")
  })
  
}

shinyApp(ui, server)
