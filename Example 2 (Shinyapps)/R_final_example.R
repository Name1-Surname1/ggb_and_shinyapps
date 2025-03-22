library(shiny)
library(shinyjs)

ui <- function(request) {
  fluidPage(
    ######################################
    # Lines to be added for any shinyapp #
    ######################################
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        function sendToPARENT(status, score) {
          window.parent.postMessage({
            status: status,
            score: score
          }, '*');
        }
        Shiny.addCustomMessageHandler('sendToPARENT',
          function(message) {
            sendToPARENT(message.status, message.score);
          }
        );
      "))
    ),
    ######################################
    # Lines to be added for any shinyapp #
    ######################################
    titlePanel("Towers of Hanoi"),
    
    uiOutput("instructions"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("numDisks", "Number of disks:", min = 3, max = 7, value = 3, step = 1, width = '100%')
      ),
      mainPanel(
        plotOutput("hanoiPlot", click = "plot_click"),
        textOutput("moveCounter")
      )
    )
  )
}

server <- function(input, output, session) {
  
  game <- reactiveValues()
  game$score <- NULL
  game$finished <- FALSE
  game$m <- sample(3:7, 1)
  
  output$instructions <- renderUI({
    tagList(
      htmltools::HTML(paste0("
        <p>The Towers of Hanoi is a classic mathematical puzzle involving three pegs and n disks of different sizes. The disks start stacked in decreasing          size on one peg, and the goal is to move all disks to the third peg following these rules:</p>
          <ol>
           <li> Only one disk can be moved at a time.
           <li> A disk can only be placed on an empty peg or on top of a larger disk.
          </ol>
        To play, adjust the slider to the indicated number of disks and try to complete the game in the fewest moves possible. Please, set the slider to the          value ", game$m, "."
      )),
      
      p("Scoring system:"),
      tableOutput("scoreTable")
    )
  })
  
  output$scoreTable <- renderTable({
    data.frame(
      "Condition" = c(
        paste("If you finish in the minimum number of moves", 2^(game$m) - 1, "and the slider is set to", game$m),
        paste("If you finish between", 2^(game$m) - 1 + 1, "and", 2^(game$m) - 1 + 5, "moves and the slider is set to", game$m),
        paste("If you finish in", 2^(game$m) - 1 + 6, "or more moves and the slider is set to", game$m),
        paste("If you finish but the slider is not set to", game$m),
        "If you do not finish the game"
      ),
      "Score" = c("100", "90", "70", "50", "0")
    )
  }, align = 'l', sanitize.text.function = function(x) x)
  
  observeEvent(input$numDisks, {
    numDisks <- input$numDisks
    game$pegs <- list(
      peg1 = rev(1:numDisks),
      peg2 = c(),
      peg3 = c()
    )
    game$selectedDisk <- NULL
    game$selectedPeg <- NULL
    game$numMoves <- 0
    game$score <- NULL
    game$finished <- FALSE
    
    enable("numDisks")
  })
  
  output$hanoiPlot <- renderPlot({
    numDisks <- input$numDisks
    pegs <- game$pegs
    plot(1, type = "n", xlim = c(0.5, 3.5), ylim = c(0, numDisks + 2), xaxt = 'n', yaxt = 'n', 
         xlab = '', ylab = '', bty = 'n')
    
    rect(0.5, 0, 3.5, 0.2, col = 'brown')
    
    pegWidth <- 1 / (2 * numDisks)
    
    rect(1 - pegWidth/2, 0.2, 1 + pegWidth/2, numDisks + 1.2, col = 'brown')
    rect(2 - pegWidth/2, 0.2, 2 + pegWidth/2, numDisks + 1.2, col = 'brown')
    rect(3 - pegWidth/2, 0.2, 3 + pegWidth/2, numDisks + 1.2, col = 'brown')
    
    for (i in 1:3) {
      peg <- pegs[[paste0('peg', i)]]
      if (length(peg) > 0) {
        for (j in 1:length(peg)) {
          diskSize <- peg[j]
          y_bottom <- 0.2 + (j - 1)
          y_top <- y_bottom + 1
          rect(
            i - diskSize/numDisks/2,
            y_bottom,
            i + diskSize/numDisks/2,
            y_top,
            col = rainbow(numDisks)[diskSize],
            border = 'black'
          )
        }
      }
    }
  })
  
  observeEvent(input$plot_click, {
    if (game$finished) return()
    
    click <- input$plot_click
    x <- click$x
    y <- click$y
    pegNum <- round(x)
    if (pegNum < 1 || pegNum > 3) return()
    pegs <- game$pegs
    if (is.null(game$selectedDisk)) {
      peg <- pegs[[paste0('peg', pegNum)]]
      if (length(peg) == 0) {
        return()
      } else {
        game$selectedDisk <- peg[length(peg)]
        game$selectedPeg <- pegNum
      }
    } else {
      fromPegNum <- game$selectedPeg
      disk <- game$selectedDisk
      destPeg <- pegs[[paste0('peg', pegNum)]]
      if (length(destPeg) == 0 || disk < destPeg[length(destPeg)]) {
        pegs[[paste0('peg', fromPegNum)]] <- pegs[[paste0('peg', fromPegNum)]][-length(pegs[[paste0('peg', fromPegNum)]])]
        pegs[[paste0('peg', pegNum)]] <- c(pegs[[paste0('peg', pegNum)]], disk)
        game$pegs <- pegs
        game$numMoves <- game$numMoves + 1
        if (length(game$pegs$peg3) == input$numDisks) {
          n <- input$numDisks
          minMoves <- 2^n - 1
          
          if (input$numDisks == game$m) {
            if (game$numMoves == minMoves) {
              game$score <- 100
              message <- "Perfect! You have completed the game in the minimum number of moves."
            } else if (game$numMoves <= minMoves + 5) {
              game$score <- 90
              message <- paste("Congratulations! You have completed the game in", game$numMoves, "moves.")
            } else if (game$numMoves <= minMoves + 10) {
              game$score <- 70
              message <- paste("Congratulations! You have completed the game in", game$numMoves, "moves.")
            } else {
              game$score <- 70
              message <- paste("Congratulations! You have completed the game in", game$numMoves, "moves.")
            }
          } else {
            game$score <- 50
            message <- "You did it! But you did not set the slider to the indicated number of disks."
          }
          
          showModal(modalDialog(
            title = "Game Finished!",
            paste(message, "\nScore:", game$score),
            easyClose = TRUE
          ))
          
          disable("numDisks")
          
          game$finished <- TRUE
        }
      } else {
        showNotification("Invalid move", type = "error")
      }
      game$selectedDisk <- NULL
      game$selectedPeg <- NULL
    }
  })
  
  output$moveCounter <- renderText({
    paste("Number of moves made:", game$numMoves)
  })
  
  ######################################
  # Lines to be added for any shinyapp #
  ######################################
  observeEvent(game$score, {
    if (game$score == 100) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "passed", score = game$score))
    }
    if (game$score == 90) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "completed", score = game$score))
    }
    if (game$score == 70) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "completed", score = game$score))
    }
    if (game$score == 50) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "partial", score = game$score))
    }
    if (is.null(game$score)) {
      session$sendCustomMessage(type = 'sendToPARENT', message = list(status = "failed", score = 0))
    }
  })
  ######################################
  # Lines to be added for any shinyapp #
  ######################################
}

shinyApp(ui, server)
