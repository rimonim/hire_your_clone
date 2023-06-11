
# Setup and Functions -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shiny)

is_common_name <- function(name){
  if(name %in% name_popularity$first_name){
    return(name_popularity$common_name[name_popularity$first_name == name])
  }else{
    return(0)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# save(mod_america_posterior_medians, d_america_allvars, name_popularity, calculated_odds, example_employees, file = "data/show.RData")
load("data/show.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# UI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ui <- navbarPage("Hire Your Clone",
  
  tabPanel("My Results", icon = icon("house"),
    fluidRow(
      # Inputs
      column(5, offset = 1,
        h3(HTML(
          as.character(actionLink(inputId = "you", 
                                  label = "", 
                                  icon = icon("user-large"))), "&nbsp &nbsp You")), 
        textInput("first_name", "Your first name?", value = "Jasper", width = "100%"),
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

        textInput("boss_first_name", "Your boss's first name?", value = "Cruella", width = "100%"),
        selectInput("boss_gender", "Your boss's gender?", selected = 1, width = "100%",
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
        htmlOutput("gender_number"), h5(textOutput("prediction_gender"), br()),
        h3(htmlOutput("race_number")),
        h5(textOutput("prediction_race"), br()),
        h3(htmlOutput("both_uncommon_names_number")),
        h5(textOutput("prediction_both_uncommon_names"), br()),
        h3(htmlOutput("same_name_number")),
        h5(textOutput("prediction_same_name"))
      )
    )
  ),
  
  tabPanel("Explore the Data", icon = icon("chart-line")),
  
  tabPanel("Learn More", icon = icon("circle-info"))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# SERVER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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

  # Reactives for Home Screen
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  r <- reactiveValues(same_first_name = 0,
                      both_male = 0,
                      both_uncommon_names = mod_america_posterior_medians$b_both_uncommon_names,
                      boss_male = 0,
                      boss_female = mod_america_posterior_medians$b_boss_female,
                      both_female = 0,
                      both_asian = 0,
                      both_black = 0,
                      both_hispanic = 0,
                      both_white = mod_america_posterior_medians$b_both_white,
                      boss_white = 0)
  
  observe({
    r$same_first_name <- mod_america_posterior_medians$b_same_first_name*(input$first_name == input$boss_first_name)
    r$both_uncommon_names <- mod_america_posterior_medians$b_both_uncommon_names*((is_common_name(input$first_name) == 0) & (is_common_name(input$boss_first_name) == 0))
    if(input$gender == 2){
      r$both_male <- 0
      r$boss_male <- 0
      r$boss_female <- 0
      r$both_female <- 0
    }else if(input$gender == 0 & input$boss_gender == 0){
      r$both_male <- mod_america_posterior_medians$b_both_male
      r$boss_male <- 0
      r$boss_female <- 0
      r$both_female <- 0
    }else if(input$gender == 1 & input$boss_gender == 0){
      r$both_male <- 0
      r$boss_male <- mod_america_posterior_medians$b_boss_male
      r$boss_female <- 0
      r$both_female <- 0
    }else if(input$gender == 0 & input$boss_gender == 1){
      r$both_male <- 0
      r$boss_male <- 0
      r$boss_female <- mod_america_posterior_medians$b_boss_female
      r$both_female <- 0
    }else if(input$gender == 1 & input$boss_gender == 1){
      r$both_male <- 0
      r$boss_male <- 0
      r$boss_female <- 0
      r$both_female <- mod_america_posterior_medians$b_both_female
    }
    
    r$both_asian <- (mod_america_posterior_medians$b_both_asian)*(input$race == "asian")*(input$race == input$boss_race)
    r$both_black <- (mod_america_posterior_medians$b_both_black)*(input$race == "black")*(input$race == input$boss_race)
    r$both_hispanic <- (mod_america_posterior_medians$b_both_white)*(input$race == "hispanic")*(input$race == input$boss_race)
    r$both_white <- (mod_america_posterior_medians$b_both_white)*(input$race == "white")*(input$race == input$boss_race)
    r$boss_white <- (mod_america_posterior_medians$b_boss_white)*(input$race != "white" & input$boss_race == "white")
  }) |> bindEvent(input$update_output)
  
  # Outputs for Home Screen
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Gender
  output$gender_number <- renderText({
    aggregate_gender <- r$both_male + r$boss_male + r$boss_female + r$both_female
    if(aggregate_gender != 0){paste0('<b> <span style=\"font-size:', 
           as.character(30 + 100*abs(exp(aggregate_gender) - 1)), 'px;',
           'color:', c("red", "green")[1L + (aggregate_gender > 0)], 
           '\">', as.character(round(100*(exp(aggregate_gender) - 1))), "%",
           '</span> </b>')}
    })

  output$prediction_gender <- renderText({
    aggregate_gender <- r$both_male + r$boss_male + r$boss_female + r$both_female
    text <- c("The fact that ",
      c("both you and your boss are men ",
        "your boss is a man but you are a woman ",
        "your boss is a woman but you are a man ",
        "both you and your boss are women ")[
          which(
            as.logical(c(r$both_male,
                         r$boss_male,
                         r$boss_female,
                         r$both_female))
            )
          ] ,
      "makes your odds of working for ",
      c("him ", "him ", "her ", "her ")[
        which(
          as.logical(c(r$both_male,
                       r$boss_male,
                       r$boss_female,
                       r$both_female))
        )
      ],
      as.character(round(exp(aggregate_gender), 2)),
      " times what it would be otherwise."
      )
    if(aggregate_gender == 0){"The impact of your gender cannot be inferred from this dataset."}else{text}
    })
  
  # Race
  output$race_number <- renderText({
    aggregate_race <- r$both_asian + r$both_black + r$both_hispanic + r$both_white + r$boss_white
    if(aggregate_race != 0){
      paste0('<b> <span style=\"font-size:', 
             as.character(30 + 100*abs(exp(aggregate_race) - 1)), 'px;',
             'color:', c("red", "green")[1L + (aggregate_race > 0)], 
             '\"> ', as.character(round(100*(exp(aggregate_race) - 1))), "%",
             '</span> </b>')
    }
  })

  output$prediction_race <- renderText({
    aggregate_race <- r$both_asian + r$both_black + r$both_hispanic + r$both_white + r$boss_white
    text <- c("The fact that ",
      c("both you and your boss are asian ",
        "both you and your boss are black ",
        "both you and your boss are hispanic ",
        "both you and your boss are white ",
        "your boss is white but you are not ")[
          which(
            as.logical(c(r$both_asian,
                         r$both_black,
                         r$both_hispanic,
                         r$both_white,
                         r$boss_white))
            )
          ] ,
      "makes your odds of working for ",
      c("him ", "him ", "her ", "her ")[
        which(
          as.logical(c(r$both_male,
                       r$boss_male,
                       r$boss_female,
                       r$both_female))
        )
      ],
      as.character(round(exp(aggregate_race), 2)),
      " times what it would be otherwise."
      )
    if(aggregate_race == 0){"The impact of your race cannot be inferred from this dataset."}else{text}
    })
  
  # Both Uncommon Names
  output$both_uncommon_names_number <- renderText({
    if(r$both_uncommon_names != 0){
      paste0('<b> <span style=\"font-size:', 
             as.character(30 + 100*abs(exp(r$both_uncommon_names) - 1)), 'px; color:green',
             '\"> ', as.character(round(100*(exp(r$both_uncommon_names) - 1))), "%",
             '</span> </b>')
    }
  })
  output$prediction_both_uncommon_names <- renderText({
    if(r$both_uncommon_names != 0){
      c("The fact that you and your boss both have uncommon names makes your odds of working for ",
        c("him ", "him ", "her ", "her ")[
          which(
            as.logical(c(r$both_male,
                         r$boss_male,
                         r$boss_female,
                         r$both_female))
          )
        ],
        "1.03 times what it would be otherwise")
    }
  })
  
  # Same First Name
  output$same_name_number <- renderText({
    if(r$same_first_name != 0){
      paste0('<b> <span style=\"font-size:', 
             as.character(30 + 100*abs(exp(r$same_first_name) - 1)), 'px; color:green',
             '\"> ', as.character(round(100*(exp(r$same_first_name) - 1))), "%",
             '</span> </b>')
    }
  })
  output$prediction_same_name <- renderText({
    if(r$same_first_name != 0){
      c("The fact that you and your boss have the same first name makes your odds of working for ",
        c("him ", "him ", "her ", "her ")[
          which(
            as.logical(c(r$both_male,
                         r$boss_male,
                         r$boss_female,
                         r$both_female))
          )
        ],
        "1.32 times what it would be otherwise")
      }
    })
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# RUN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

shinyApp(ui, server)

