library(shiny)
library(data.table)
library(dplyr)

freq2 <- fread("freq2.csv")
freq3 <- fread("freq3.csv")
freq4 <- fread("freq4.csv")
profanity <- fread("profanity.csv")

ui <- fluidPage(
  
  titlePanel("A Simple Text Prediction Algorithm"),

  verticalLayout(  
    headerPanel(
      h5("Begin typing into the text box below.  As you type, words will appear in the suggestion box based on what you have typed so far.  
      In some cases, the algorithm may be unable to generate any suggestions.")
    ),
    
    wellPanel(
      tags$i("Note that there is a small processing delay in between the time you stop typing and when the suggestion box populates!"),
      tags$br(), tags$br(),
      textInput("text", "Type here:", value = "Thank you for the"),
      selectInput("sugg", label = NULL, choices = NULL, size = 7, selectize = FALSE),
      actionButton("copy", label = "Take suggestion"),
      actionButton("clear", label = "Clear"),
      checkboxInput("profanity", label = "Allow profanity in suggestions?", value = FALSE)
    ),
  
    h5("Sean Yarborough, Capstone Project, December 2016")
  )
)
   
server <- function(input, output, session) {
  
  searchQuadgramsFor <- function(x, n) {
    head(arrange(freq4[grep(paste0("^",as.character(x), " "), freq4$V1, ignore.case = TRUE, useBytes = TRUE)], -temp), n)
  } 
  
  searchTrigramsFor <- function(x, n) {
    head(arrange(freq3[grep(paste0("^",as.character(x), " "), freq3$V1, ignore.case = TRUE, useBytes = TRUE)], -temp), n)
  } 
  
  searchBigramsFor <- function(x, n) {
    head(arrange(freq2[grep(paste0("^",as.character(x), " "), freq2$V1, ignore.case = TRUE, useBytes = TRUE)], -temp), n)
  } 
  
  completeText <- function(x, p = FALSE) {
    # Count the number of words the user entered (minimum of 2)
    n <- sapply(gregexpr("[[:alpha:]]+", x), function(x) sum(x > 0))
    res <- data.table(NA)
    
    # Set the flags t, u, and v to determine which n-grams to search
    t <- 0; u <- 0; v <- 0
    if (n == 1) {
      t <- 1
    }
    if (n == 2) {
      t <- 1; u <- 1
    }
    if (n >= 3) {
      t <- 1; u <- 1; v <- 1
      if (n > 3) {
        x <- word(x, start = -3, end = -1)  # Three words is the max we'll consider for history
      }
    }
    
    if (v == 1) {  # Quadgram search
      res <- data.table(searchQuadgramsFor(x, 15))
      if (nrow(res) != 0) {
        if (res[1]$temp > 1) {
          res <- res[temp > 1] # If we got multiple hits, remove all of the single hits
        }
        res <- sub(paste0(tolower(x)," "), "", tolower(res$V1))
      } else { # If no results, leave res as a null char vector and shorten the input
        res <- NA
        x <- word(x, start = -2, end = -1)
      }
    }

    if ((v == 1 & is.na(res) == TRUE) || (v == 0 & u == 1)) {  # Trigram search or backoff from quadgram w/ no results
      res <- data.table(searchTrigramsFor(x, 15))
      if (nrow(res) != 0) {
        if (res[1]$temp > 1) {
          res <- res[temp > 1]
        }
        res <- sub(paste0(tolower(x)," "), "", tolower(res$V1))
      } else {
        res <- NA
        x <- word(x, start = -1)
      }
    }
    
    if ((u == 1 & is.na(res) == TRUE) || (u == 0 & t == 1)) {  # Bigram search or backoff from trigram w/ no results
      res <- data.table(searchBigramsFor(x, 15))
      if (nrow(res) != 0) {
        if (res[1]$temp > 1) {
          res <- res[temp > 1]
        }
        res <- sub(paste0(tolower(x)," "), "", tolower(res$V1))
      } else {
        res <- NA  # If we get this far, we've got nothing to suggest
      }
    }
    
    if (p == FALSE) { # If the profanity filter is on, remove profane words from the result set before returning
      res <- res[!res %in% profanity]
    }
    
    if (is.na(res) == TRUE) {
      res <- NULL
    }
    
    return(res)
  } 
  
  observeEvent(input$text, {
    withProgress(message = "Thinking...", value = 0.15, {
      tryCatch({
        suppressWarnings(
          updateSelectInput(session, "sugg", choices = completeText(trimws(input$text), input$profanity))
        )
      }, error = function(e) {updateSelectInput(session, "sugg", choices = "")})
      setProgress(value = 1)
    })
  })
  
  observeEvent(input$profanity, {
    withProgress(message = "Thinking...", value = 0.15, {
      tryCatch({
        suppressWarnings(
          updateSelectInput(session, "sugg", choices = completeText(trimws(input$text), input$profanity))
        )
      }, error = function(e) {updateSelectInput(session, "sugg", choices = "")})
    })
  })
  
  observeEvent(input$copy, {
    updateTextInput(session, "text", value = 
      if (length(input$sugg) > 0) {
        paste(trimws(input$text), input$sugg)
      })
  })
  
  observeEvent(input$clear, {
    updateTextInput(session, "text", value = "")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

