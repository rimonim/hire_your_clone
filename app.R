
# Setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shiny)

# Data --------------------------------------------------------------------

# load("data/results.RData")

# ui ----------------------------------------------------------------------

ui <- navbarPage("Hire Your Clone",
  
  tabPanel("My Results", icon = icon("house"),
    fluidRow(
      # Inputs
      column(5, offset = 1,
        h3(HTML(
          as.character(actionLink(inputId = "you", 
                                  label = "", 
                                  icon = icon("user-large"))), "&nbsp &nbsp You")), 
        textInput("first_name", "Your first name?", value = "Smee", width = "100%"),
        selectInput("gender", "Your gender?", selected = 0, width = "100%",
                    choices = list(Man = 0, Woman = 1, Other = 2)),
        selectInput("race", "Your race?", selected = "white", width = "100%",
                    choices = list(Asian = "asian", Black = "black", 
                                   Hispanic = "hispanic", White = "white",
                                   Other = "other"))),
      column(5, 
        h3(HTML(
          as.character(actionLink(inputId = "boss", 
                                  label = "", 
                                  icon = icon("user-tie"))), "&nbsp &nbsp Your Boss")), 

        textInput("boss_first_name", "Your boss's first name?", value = "Hook", width = "100%"),
        selectInput("boss_gender", "Your boss's gender?", selected = 0, width = "100%",
                    choices = list(Man = 0, Woman = 1, Other = 2)),
        selectInput("boss_race", "Your boss's race?", selected = "white", width = "100%",
                    choices = list(Asian = "asian", Black = "black", 
                                   Hispanic = "hispanic", White = "white",
                                   Other = "other"))
      ),
      hr(),
      column(8, offset = 2, actionButton("update_output", h4("Let's see my odds!"), width = "100%")),
      hr(),
      column(5, offset = 1,
        br(),
        htmlOutput("gender_number"), 
        h5(textOutput("prediction_gender"), br()),
        h3(htmlOutput("race_number")),
        h5(textOutput("prediction_race"), br()),
        h3(htmlOutput("same_name_number")),
        h5(textOutput("prediction_same_name"))
      )
    )
  ),
  
  tabPanel("Explore the Data", icon = icon("chart-line")),
  
  tabPanel("Learn More", icon = icon("circle-info"))
)


# server ------------------------------------------------------------------
## Inputs:
# actionButton: update_output
# first_name
# boss_first_name
# gender
# boss_gender
# race
# boss_race

## Outputs:
# prediction_gender
# prediction_race
# prediction_same_name

server <- function(input, output) {

  r <- reactiveValues(same_first_name = 0,
                      dyad_gender_bothmale = -0.04,
                      dyad_gender_bossmale = 0,
                      dyad_gender_bossfemale = 0,
                      dyad_gender_bothfemale = 0,
                      same_race_asian = 0,
                      same_race_black = 0,
                      same_race_hispanic = 0,
                      same_race_white = 0.12,
                      same_race_other = 0)
  
  observe({
    r$same_first_name <- as.integer(input$first_name == input$boss_first_name)
    if(input$gender == 2){
      r$dyad_gender_bothmale <- 0
      r$dyad_gender_bossmale <- 0
      r$dyad_gender_bossfemale <- 0
      r$dyad_gender_bothfemale <- 0
    }else if(input$gender == 0 & input$boss_gender == 0){
      r$dyad_gender_bothmale <- -0.04
      r$dyad_gender_bossmale <- 0
      r$dyad_gender_bossfemale <- 0
      r$dyad_gender_bothfemale <- 0
    }else if(input$gender == 1 & input$boss_gender == 0){
      r$dyad_gender_bothmale <- 0
      r$dyad_gender_bossmale <- -0.19
      r$dyad_gender_bossfemale <- 0
      r$dyad_gender_bothfemale <- 0
    }else if(input$gender == 0 & input$boss_gender == 1){
      r$dyad_gender_bothmale <- 0
      r$dyad_gender_bossmale <- 0
      r$dyad_gender_bossfemale <- -0.27
      r$dyad_gender_bothfemale <- 0
    }else if(input$gender == 1 & input$boss_gender == 1){
      r$dyad_gender_bothmale <- 0
      r$dyad_gender_bossmale <- 0
      r$dyad_gender_bossfemale <- 0
      r$dyad_gender_bothfemale <- 0.12
    }
    
    r$same_race_asian <- (0.96)*as.integer(input$race == "asian")*as.integer(input$race == input$boss_race)
    r$same_race_black <- (0.55)*as.integer(input$race == "black")*as.integer(input$race == input$boss_race)
    r$same_race_hispanic <- (1.37)*as.integer(input$race == "hispanic")*as.integer(input$race == input$boss_race)
    r$same_race_white <- (0.12)*as.integer(input$race == "white")*as.integer(input$race == input$boss_race)
    r$same_race_other <- (0.25)*as.integer(input$race == "other")*as.integer(input$race == input$boss_race)
  }) |> bindEvent(input$update_output)
  
  output$gender_number <- renderText({
    if(r$dyad_gender_bothmale + r$dyad_gender_bossmale + r$dyad_gender_bossfemale + r$dyad_gender_bothfemale != 0){paste0('<b> <span style=\"font-size:', 
           as.character(2 + abs(1 - exp(r$dyad_gender_bothmale + r$dyad_gender_bossmale + r$dyad_gender_bossfemale + r$dyad_gender_bothfemale))), 'em;',
           'color:', c("red", "green")[1L + (r$dyad_gender_bothmale + r$dyad_gender_bossmale + r$dyad_gender_bossfemale + r$dyad_gender_bothfemale > 0)], 
           '\">× ', as.character(round(exp(r$dyad_gender_bothmale + r$dyad_gender_bossmale + r$dyad_gender_bossfemale + r$dyad_gender_bothfemale), 2)), 
           '</span> </b>')}
    })

  output$prediction_gender <- renderText({
    aggregate <- r$dyad_gender_bothmale + r$dyad_gender_bossmale + r$dyad_gender_bossfemale + r$dyad_gender_bothfemale
    text <- c("The fact that ",
      c("both you and your boss are men ",
        "your boss is a man but you are a woman ",
        "your boss is a woman but you are a man ",
        "both you and your boss are women ")[
          which(
            as.logical(c(r$dyad_gender_bothmale,
                         r$dyad_gender_bossmale,
                         r$dyad_gender_bossfemale,
                         r$dyad_gender_bothfemale))
            )
          ] ,
      "makes your odds of working for ",
      c("him ", "him ", "her ", "her ")[
        which(
          as.logical(c(r$dyad_gender_bothmale,
                       r$dyad_gender_bossmale,
                       r$dyad_gender_bossfemale,
                       r$dyad_gender_bothfemale))
        )
      ],
      as.character(round(exp(aggregate), 2)),
      " times what it would be otherwise."
      )
    if(aggregate == 0){"The impact of your gender cannot be inferred from this dataset."}else{text}
    })
  
  output$race_number <- renderText({
    aggregate <- r$same_race_asian + r$same_race_black + r$same_race_hispanic + r$same_race_white + r$same_race_other
    if(aggregate != 0){
      paste0('<b> <span style=\"font-size:', 
             as.character(2 + abs(1 - exp(aggregate))), 'em;',
             'color:', c("red", "green")[1L + (aggregate > 0)], 
             '\">× ', as.character(round(exp(aggregate), 2)), 
             '</span> </b>')
    }
  })

  output$prediction_race <- renderText({
    aggregate <- r$same_race_asian + r$same_race_black + r$same_race_hispanic + r$same_race_white + r$same_race_other
    text <- c("The fact that ",
      c("both you and your boss are asian ",
        "both you and your boss are black ",
        "both you and your boss are hispanic ",
        "both you and your boss are white ",
        "both you and your boss are other ")[
          which(
            as.logical(c(r$same_race_asian,
                         r$same_race_black,
                         r$same_race_hispanic,
                         r$same_race_white,
                         r$same_race_other))
            )
          ] ,
      "makes your odds of working for ",
      c("him ", "him ", "her ", "her ")[
        which(
          as.logical(c(r$dyad_gender_bothmale,
                       r$dyad_gender_bossmale,
                       r$dyad_gender_bossfemale,
                       r$dyad_gender_bothfemale))
        )
      ],
      as.character(round(exp(aggregate), 2)),
      " times what it would be otherwise."
      )
    if(aggregate != 0){text}
    })
  
  output$same_name_number <- renderText({
    if(r$same_first_name==1){
      paste0('<b> <span style=\"font-size:', 
             as.character(2 + .79), 'em; color:green',
             '\">× ', as.character(1.79), 
             '</span> </b>')
    }
  })
  output$prediction_same_name <- renderText({
    if(r$same_first_name==1){
      c("The fact that you and your boss have the same first name makes it makes your odds of working for ",
        c("him ", "him ", "her ", "her ")[
          which(
            as.logical(c(r$dyad_gender_bothmale,
                         r$dyad_gender_bossmale,
                         r$dyad_gender_bossfemale,
                         r$dyad_gender_bothfemale))
          )
        ],
        "1.79 times what it would be otherwise")
      }
    })
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)

