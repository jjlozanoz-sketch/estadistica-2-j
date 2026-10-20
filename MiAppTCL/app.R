# ============================================================
# SIMULACIÓN DEL TEOREMA CENTRAL DEL LÍMITE (TCL)
# Distribuciones: Uniforme, Binomial, Poisson,
# Hipergeométrica y Exponencial
# ============================================================

library(shiny)
library(ggplot2)
library(gridExtra)


# ============================================================
# INTERFAZ DE USUARIO
# ============================================================

ui <- fluidPage(
  
  titlePanel("Simulación del Teorema Central del Límite (TCL)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --------------------------------------------------------
      # Selección de distribución
      # --------------------------------------------------------
      
      selectInput(
        "dist",
        "Distribución:",
        choices = c(
          "Uniforme",
          "Binomial",
          "Poisson",
          "Hipergeométrica",
          "Exponencial"
        )
      ),
      
      
      # ========================================================
      # DISTRIBUCIÓN UNIFORME
      # ========================================================
      
      conditionalPanel(
        condition = "input.dist == 'Uniforme'",
        
        numericInput(
          "a",
          "Mínimo (a):",
          value = 0
        ),
        
        numericInput(
          "b",
          "Máximo (b):",
          value = 1
        )
      ),
      
      
      # ========================================================
      # DISTRIBUCIÓN BINOMIAL
      # ========================================================
      
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        
        numericInput(
          "size",
          "Ensayos (n):",
          value = 10,
          min = 1,
          step = 1
        ),
        
        sliderInput(
          "p",
          "Probabilidad de éxito (p):",
          min = 0,
          max = 1,
          value = 0.30,
          step = 0.01
        ),
        
        # Mostrar el valor actual de p
        textOutput("valor_p")
      ),
      
      
      # ========================================================
      # DISTRIBUCIÓN POISSON
      # ========================================================
      
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        
        numericInput(
          "lambda_pois",
          "Lambda:",
          value = 4,
          min = 0.1
        )
      ),
      
      
      # ========================================================
      # DISTRIBUCIÓN HIPERGEOMÉTRICA
      # ========================================================
      
      conditionalPanel(
        condition = "input.dist == 'Hipergeométrica'",
        
        numericInput(
          "Npop",
          "Tamaño población (N):",
          value = 1000,
          min = 10
        ),
        
        numericInput(
          "K",
          "Éxitos en población (K):",
          value = 300,
          min = 1
        ),
        
        numericInput(
          "m",
          "Tamaño de muestra (m):",
          value = 20,
          min = 1
        )
      ),
      
      
      # ========================================================
      # DISTRIBUCIÓN EXPONENCIAL
      # ========================================================
      
      conditionalPanel(
        condition = "input.dist == 'Exponencial'",
        
        numericInput(
          "lambda_exp",
          "Lambda:",
          value = 1.5,
          min = 0.1
        )
      ),
      
      
      # --------------------------------------------------------
      # Separador
      # --------------------------------------------------------
      
      hr(),
      
      
      # ========================================================
      # PARÁMETROS DEL TCL
      # ========================================================
      
      numericInput(
        "n",
        "Tamaño de muestra (TCL):",
        value = 30,
        min = 1,
        step = 1
      ),
      
      numericInput(
        "Nsim",
        "Número de simulaciones:",
        value = 10000,
        min = 1000,
        step = 1000
      ),
      
      
      # --------------------------------------------------------
      # Botón de simulación
      # --------------------------------------------------------
      
      actionButton(
        "simular",
        "Simular",
        class = "btn-primary"
      )
    ),
    
    
    # ==========================================================
    # PANEL PRINCIPAL
    # ==========================================================
    
    mainPanel(
      
      plotOutput(
        "plots",
        height = "450px"
      )
    )
  )
)


# ============================================================
# SERVIDOR
# ============================================================

server <- function(input, output) {
  
  
  # ==========================================================
  # MOSTRAR EL VALOR DE p
  # ==========================================================
  
  output$valor_p <- renderText({
    
    paste(
      "Valor seleccionado de p:",
      round(input$p, 2)
    )
  })
  
  
  # ==========================================================
  # SIMULACIÓN
  # ==========================================================
  
  sim_data <- eventReactive(input$simular, {
    
    # Semilla para reproducibilidad
    set.seed(123)
    
    # Tamaño de la población simulada
    N <- 100000
    
    
    # ========================================================
    # DISTRIBUCIÓN UNIFORME
    # ========================================================
    
    if (input$dist == "Uniforme") {
      
      # Generar población
      poblacion <- runif(
        N,
        min = input$a,
        max = input$b
      )
      
      # Distribución muestral de la media
      medias <- replicate(
        input$Nsim,
        mean(
          runif(
            input$n,
            min = input$a,
            max = input$b
          )
        )
      )
      
      # Media poblacional
      mu <- (input$a + input$b) / 2
      
      # Desviación estándar poblacional
      sigma <- (input$b - input$a) / sqrt(12)
    }
    
    
    # ========================================================
    # DISTRIBUCIÓN BINOMIAL
    # ========================================================
    
    else if (input$dist == "Binomial") {
      
      # ------------------------------------------------------
      # POBLACIÓN BINOMIAL
      # ------------------------------------------------------
      
      poblacion <- rbinom(
        n = N,
        size = input$size,
        prob = input$p
      )
      
      
      # ------------------------------------------------------
      # DISTRIBUCIÓN MUESTRAL DE LA MEDIA
      # ------------------------------------------------------
      
      medias <- replicate(
        input$Nsim,
        mean(
          rbinom(
            n = input$n,
            size = input$size,
            prob = input$p
          )
        )
      )
      
      
      # ------------------------------------------------------
      # MEDIA DE LA DISTRIBUCIÓN BINOMIAL
      # ------------------------------------------------------
      
      mu <- input$size * input$p
      
      
      # ------------------------------------------------------
      # DESVIACIÓN ESTÁNDAR DE LA BINOMIAL
      # ------------------------------------------------------
      
      sigma <- sqrt(
        input$size *
          input$p *
          (1 - input$p)
      )
    }
    
    
    # ========================================================
    # DISTRIBUCIÓN POISSON
    # ========================================================
    
    else if (input$dist == "Poisson") {
      
      # Generar población
      poblacion <- rpois(
        N,
        lambda = input$lambda_pois
      )
      
      # Distribución muestral
      medias <- replicate(
        input$Nsim,
        mean(
          rpois(
            input$n,
            lambda = input$lambda_pois
          )
        )
      )
      
      # Media
      mu <- input$lambda_pois
      
      # Desviación estándar
      sigma <- sqrt(input$lambda_pois)
    }
    
    
    # ========================================================
    # DISTRIBUCIÓN HIPERGEOMÉTRICA
    # ========================================================
    
    else if (input$dist == "Hipergeométrica") {
      
      # Generar población
      poblacion <- rhyper(
        N,
        m = input$K,
        n = input$Npop - input$K,
        k = input$m
      )
      
      # Distribución muestral
      medias <- replicate(
        input$Nsim,
        mean(
          rhyper(
            input$n,
            m = input$K,
            n = input$Npop - input$K,
            k = input$m
          )
        )
      )
      
      # Media
      mu <- input$m *
        (input$K / input$Npop)
      
      # Desviación estándar
      sigma <- sqrt(
        input$m *
          (input$K / input$Npop) *
          (1 - input$K / input$Npop) *
          ((input$Npop - input$m) /
             (input$Npop - 1))
      )
    }
    
    
    # ========================================================
    # DISTRIBUCIÓN EXPONENCIAL
    # ========================================================
    
    else if (input$dist == "Exponencial") {
      
      # Generar población
      poblacion <- rexp(
        N,
        rate = input$lambda_exp
      )
      
      # Distribución muestral
      medias <- replicate(
        input$Nsim,
        mean(
          rexp(
            input$n,
            rate = input$lambda_exp
          )
        )
      )
      
      # Media
      mu <- 1 / input$lambda_exp
      
      # Desviación estándar
      sigma <- 1 / input$lambda_exp
    }
    
    
    # ========================================================
    # DEVOLVER RESULTADOS
    # ========================================================
    
    list(
      poblacion = poblacion,
      medias = medias,
      mu = mu,
      sigma = sigma
    )
  })
  
  
  # ==========================================================
  # GRÁFICOS
  # ==========================================================
  
  output$plots <- renderPlot({
    
    # Obtener datos
    data <- sim_data()
    
    
    # --------------------------------------------------------
    # Data frame población
    # --------------------------------------------------------
    
    df_pob <- data.frame(
      x = data$poblacion
    )
    
    
    # --------------------------------------------------------
    # Data frame distribución muestral
    # --------------------------------------------------------
    
    df_med <- data.frame(
      x = data$medias
    )
    
    
    # ========================================================
    # GRÁFICO DE LA POBLACIÓN
    # ========================================================
    
    g1 <- ggplot(
      df_pob,
      aes(x = x)
    ) +
      
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 50,
        fill = "lightgreen",
        color = "black"
      ) +
      
      stat_function(
        fun = dnorm,
        args = list(
          mean = data$mu,
          sd = data$sigma
        ),
        color = "darkgreen",
        linewidth = 1
      ) +
      
      labs(
        title = "Población",
        x = "Valores",
        y = "Densidad"
      ) +
      
      theme_minimal()
    
    
    # ========================================================
    # GRÁFICO DE LA DISTRIBUCIÓN MUESTRAL
    # ========================================================
    
    g2 <- ggplot(
      df_med,
      aes(x = x)
    ) +
      
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "lightblue",
        color = "black"
      ) +
      
      stat_function(
        fun = dnorm,
        args = list(
          mean = data$mu,
          sd = data$sigma / sqrt(input$n)
        ),
        color = "red",
        linewidth = 1.2
      ) +
      
      labs(
        title = "Distribución muestral de la media",
        x = "Media muestral",
        y = "Densidad"
      ) +
      
      theme_minimal()
    
    
    # ========================================================
    # MOSTRAR LOS DOS GRÁFICOS
    # ========================================================
    
    grid.arrange(
      g1,
      g2,
      ncol = 2
    )
  })
}


# ============================================================
# EJECUTAR LA APLICACIÓN SHINY
# ============================================================

shinyApp(
  ui = ui,
  server = server
)