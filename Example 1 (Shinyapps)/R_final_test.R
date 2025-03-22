library(shiny)
library(ggplot2)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(HTML("
      function sendToPARENT(status, score) {
        window.parent.postMessage({ status: status, score: score }, '*');
      }
      Shiny.addCustomMessageHandler('sendToPARENT', function(message) {
        sendToPARENT(message.status, message.score);
      });
    "))
  ),
  titlePanel("Simulation of the Central Limit Theorem"),
  withMathJax(),
  uiOutput("instructions"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_variables", "Number of Variables:", min = 100, max = 500, value = 100),
      sliderInput("num_sumas", "Number of Sample Means:", min = 100, max = 500, value = 100),
      selectInput("distribucion", "Origin Distribution:",
                  choices = c("Uniform" = "unif", "Exponential" = "exp", "Binomial" = "binom")
      ),
      conditionalPanel(
        condition = "input.distribucion == 'unif'",
        numericInput("unif_min", "Lower Bound:", value = 0),
        numericInput("unif_max", "Upper Bound:", value = 1)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'exp'",
        numericInput("exp_lambda", "Lambda Value:", value = 1)
      ),
      conditionalPanel(
        condition = "input.distribucion == 'binom'",
        numericInput("binom_n", "Number of Trials (n):", value = 1, min = 1),
        numericInput("binom_p", "Success Probability (p):", value = 0.5, min = 0, max = 1)
      ),
      textInput("semilla", "Seed (Enter this value for reproducibility):"),
      actionButton("simular", "Simulate"),
      textOutput("semilla_generada")
    ),
    mainPanel(
      plotOutput("histograma"),
      numericInput("respuesta_media", "What is the mean (rounded to two decimals) of the standarized data?", value = NA),
      uiOutput("resultado_media"),
      numericInput("respuesta_sd", "What is the standard deviation (rounded to three decimals) of the standarized data?", value = NA),
      uiOutput("resultado_sd"),
      actionButton("calificar", "SUBMIT"),
      textOutput("score")
    )
  )
)

server <- function(input, output, session) {
  semilla_predeterminada <- 1
  semilla_aleatoria <- sample(1:10000, 1)
  num_variables_inicial <- sample(100:500, 1)
  num_sumas_inicial <- sample(100:500, 1)
  distribucion_inicial <- sample(c("unif", "exp", "binom"), 1)
  unif_min_inicial <- runif(1, 0, 5)
  unif_max_inicial <- runif(1, 5, 10)
  exp_lambda_inicial <- runif(1, 0.1, 2)
  binom_n_inicial <- sample(1:10, 1)
  binom_p_inicial <- runif(1, 0, 1)
  set.seed(semilla_aleatoria)
  if (distribucion_inicial == "unif") {
    muestras_iniciales <- replicate(num_sumas_inicial, mean(runif(num_variables_inicial, min = unif_min_inicial, max = unif_max_inicial)))
    media <- (unif_min_inicial + unif_max_inicial) / 2
    desviacion <- sqrt((unif_max_inicial - unif_min_inicial)^2 / 12)
  } else if (distribucion_inicial == "exp") {
    muestras_iniciales <- replicate(num_sumas_inicial, mean(rexp(num_variables_inicial, rate = exp_lambda_inicial)))
    media <- 1 / exp_lambda_inicial
    desviacion <- 1 / exp_lambda_inicial
  } else if (distribucion_inicial == "binom") {
    muestras_iniciales <- replicate(num_sumas_inicial, mean(rbinom(num_variables_inicial, size = binom_n_inicial, prob = binom_p_inicial)))
    media <- binom_n_inicial * binom_p_inicial
    desviacion <- sqrt(binom_n_inicial * binom_p_inicial * (1 - binom_p_inicial))
  }
  media_calculada <- round(mean((muestras_iniciales - media) / (desviacion / sqrt(num_variables_inicial))), 2)
  sd_calculada <- round(sd((muestras_iniciales - media) / (desviacion / sqrt(num_variables_inicial))), 3)
  
  output$instructions <- renderUI({
    instrucciones <- htmltools::HTML(paste0("
      <p>To verify the Central Limit Theorem (CLT) by simulation, consider a set of \\( m \\) independent samples, each of size \\( n \\), drawn from any distribution with finite mean \\( \\mu \\) and positive finite variance \\( \\sigma^2 > 0 \\). The simulation procedure is described as follows:</p>
      <ol>
        <li>We fix \\( n \\in \\mathbb{N} \\) as the sample size and \\( m \\in \\mathbb{N} \\) as the number of samples.</li>
        <li>For each sample \\( i \\) (\\( i = 1, 2, \\ldots, m \\)):
          <ul>
            <li>We generate a random sample of \\( n \\) values from the given distribution.</li>
            <li>We compute the sample mean \\( \\bar{X}_{i,n} \\) of the random sample.</li>
          </ul>
        </li>
        <li>We standardize each sample mean using the expression: \\( Z_i = \\frac{\\bar{X}_{i,n} - \\mu}{\\frac{\\sqrt{n}}{\\sigma}} \\), where \\( \\mu \\) and \\( \\sigma \\) are the mean and standard deviation of the original distribution, respectively.</li>
        <li>A histogram of the standardized values \\( Z_i \\) is constructed to observe its empirical behavior.</li>
      </ol>
      <p>According to the CLT, as the sample size \\( n \\) increases, the distribution of \\( Z_i \\) tends to a standard normal distribution \\( \\mathcal{N}(0, 1) \\), regardless of the shape of the original distribution (provided that \\( \\mu \\) and \\( \\sigma^2 \\) are finite).</p>
      <p>In the simulation, we plot the empirical density of the values \\( Z_i \\) along with the theoretical density of \\( \\mathcal{N}(0, 1) \\) to verify the convergence.</p>
      "
    ))
    instrucciones <- withMathJax(htmltools::HTML(paste0(instrucciones, "Choose the Number of Variables equal to ", num_variables_inicial,
                                            ", the Number of Sample Means equal to ", num_sumas_inicial,
                                            ", and the distribution ", ifelse(distribucion_inicial == "unif", "Uniform ",
                                                                              ifelse(distribucion_inicial == "exp", "Exponential ", "Binomial ")),
                                            "with the parameters: ")))
    if (distribucion_inicial == "unif") {
      instrucciones <- htmltools::HTML(paste0(instrucciones, "Lower Bound = ", round(unif_min_inicial, 2),
                                              " and Upper Bound = ", round(unif_max_inicial, 2)))
    } else if (distribucion_inicial == "exp") {
      instrucciones <- htmltools::HTML(paste0(instrucciones, "Lambda = ", round(exp_lambda_inicial, 2)))
    } else if (distribucion_inicial == "binom") {
      instrucciones <- htmltools::HTML(paste0(instrucciones, "Number of Trials = ", binom_n_inicial,
                                              " and Success Probability = ", round(binom_p_inicial, 2)))
    }
    instrucciones <- htmltools::HTML(paste0(instrucciones, ". The calculated mean of the standarized data is ", media_calculada,
                                            " and the calculated standard deviation is ", sd_calculada))
    instrucciones
  })
  
  output$semilla_generada <- renderText({
    paste0("Use the following seed for reproducibility: ", semilla_aleatoria)
  })
  
  observeEvent(input$simular, {
    semilla_usada <- ifelse(input$semilla == "", semilla_predeterminada, as.numeric(input$semilla))
    set.seed(semilla_usada)
    if (input$distribucion == "unif") {
      muestras <- replicate(input$num_sumas, mean(runif(input$num_variables, min = input$unif_min, max = input$unif_max)))
      media_elegida <- (input$unif_min + input$unif_max) / 2
      desviacion_elegida <- sqrt((input$unif_max - input$unif_min)^2 / 12)
    } else if (input$distribucion == "exp") {
      muestras <- replicate(input$num_sumas, mean(rexp(input$num_variables, rate = input$exp_lambda)))
      media_elegida <- 1 / input$exp_lambda
      desviacion_elegida <- 1 / input$exp_lambda
    } else if (input$distribucion == "binom") {
      muestras <- replicate(input$num_sumas, mean(rbinom(input$num_variables, size = input$binom_n, prob = input$binom_p)))
      media_elegida <- input$binom_n * input$binom_p
      desviacion_elegida <- sqrt(input$binom_n * input$binom_p * (1 - input$binom_p))
    }
    muestras_estandarizadas <- (muestras - media_elegida) / (desviacion_elegida / sqrt(input$num_variables))
    output$histograma <- renderPlot({
      ggplot(data.frame(suma = muestras_estandarizadas), aes(x = suma)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 1) +
        labs(
          title = "Convergence to the Normal Distribution",
          x = "Standardized Sum Value", y = "Density"
        ) +
        theme_minimal()
    })
  })
  
  observeEvent(input$calificar, {
    score <- 0
    media_correcta <- !is.na(input$respuesta_media) && input$respuesta_media == media_calculada
    if (media_correcta) score <- score + 50
    sd_correcta <- !is.na(input$respuesta_sd) && input$respuesta_sd == sd_calculada
    if (sd_correcta) score <- score + 50
    output$score <- renderText({ paste("Your score is", score) })
    if (score == 100) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "passed", score = score))
    }
    if (score == 50) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "incomplete", score = score))
    }
    if (score == 0) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "failed", score = score))
    }
    output$resultado_media <- renderUI({
      if (media_correcta) {
        tags$span("✓", style = "color: green; font-weight: bold;")
      } else {
        tags$span("✗", style = "color: red; font-weight: bold;")
      }
    })
    output$resultado_sd <- renderUI({
      if (sd_correcta) {
        tags$span("✓", style = "color: green; font-weight: bold;")
      } else {
        tags$span("✗", style = "color: red; font-weight: bold;")
      }
    })
    disable("num_variables")
    disable("num_sumas")
    disable("distribucion")
    disable("unif_min")
    disable("unif_max")
    disable("exp_lambda")
    disable("binom_n")
    disable("binom_p")
    disable("semilla")
    disable("simular")
    disable("respuesta_media")
    disable("respuesta_sd")
    disable("calificar")
  })
}

shinyApp(ui = ui, server = server)