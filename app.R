
# Setup and Functions -------------------------------------------------------------------

library(dplyr)
library(tibble)
library(shiny)
library(stringr)
# library(networkD3)

is_common_name <- function(name){
  if(name %in% name_popularity$first_name){
    return(name_popularity$common_name[name_popularity$first_name == name])
  }else{
    return(0)
  }
}

# Function takes information on employee and boss and outputs an example comparison 
# with a different employee and the same boss (in html)
generate_example_comparison <- function(boss_first_name, boss_common_name, boss_gender, boss_ethnicity, 
                                        same_first_name, both_uncommon_names, 
                                        both_male, boss_male, boss_female, both_female,
                                        both_EastAsian, both_EastEuropean, both_Japanese, 
                                        both_Indian, both_African, both_Muslim, 
                                        both_WestEuropean, both_Jewish, both_Hispanic,
                                        both_Italian, WE_Asian, WE_Indian,
                                        WE_Muslim, WE_Hispanic, WE_Jewish){
  # User's odds
  user_odds <- exp(median_params$b_same_first_name*same_first_name 
                   + median_params$b_both_uncommon_names*both_uncommon_names 
                   + median_params$b_both_male*both_male
                   + median_params$b_boss_male*boss_male
                   + median_params$b_boss_female*boss_female
                   + median_params$b_both_female*both_female
                   + median_params$b_both_EastAsian*both_EastAsian
                   + median_params$b_both_EastEuropean*both_EastEuropean
                   + median_params$b_both_Japanese*both_Japanese
                   + median_params$b_both_Indian*both_Indian
                   + median_params$b_both_African*both_African
                   + median_params$b_both_Muslim*both_Muslim
                   + median_params$b_both_WestEuropean*both_WestEuropean
                   + median_params$b_both_Jewish*both_Jewish
                   + median_params$b_both_Hispanic*both_Hispanic
                   + median_params$b_both_Italian*both_Italian
                   + median_params$b_WE_Asian*WE_Asian
                   + median_params$b_WE_Indian*WE_Indian
                   + median_params$b_WE_Muslim*WE_Muslim
                   + median_params$b_WE_Hispanic*WE_Hispanic
                   + median_params$b_WE_Jewish*WE_Jewish)
  
  # Fill in boss info 
  boss <- data.frame(first_name = boss_first_name, 
                     common_name = boss_common_name,
                     gender = boss_gender, 
                     ethnicity = boss_ethnicity) %>% 
    left_join(ethnicity_avg_probs) %>% 
    rename_with( ~ paste0("boss_", .x))
  
  # Pick a comparison employee, add boss info, 
  # calculate dyad properties, and join with pre-calculated odds
  example_employee <- average_employees %>% slice_sample(n = 1) %>% 
    bind_cols(boss) %>% 
    left_join(ethnicity_avg_probs) %>% 
    mutate(ex_same_first_name = as.integer(first_name == boss_first_name),
           ex_both_uncommon_names = as.integer(boss_common_name == 0 & common_name == 0),
           ex_both_male = as.integer(boss_gender == 0 & gender == 0),
           ex_boss_male = as.integer(boss_gender == 0 & gender == 1),
           ex_boss_female = as.integer(boss_gender == 1 & gender == 0),
           ex_both_female = as.integer(boss_gender == 1 & gender == 1),
           ex_both_EastAsian = EastAsian*boss_EastAsian,
           ex_both_EastEuropean = EastEuropean*boss_EastEuropean,
           ex_both_Japanese = Japanese*boss_Japanese,
           ex_both_Indian = Indian*boss_Indian,
           ex_both_African = African*boss_African,
           ex_both_Muslim = Muslim*boss_Muslim,
           ex_both_WestEuropean = WestEuropean*boss_WestEuropean,
           ex_both_Jewish = Jewish*boss_Jewish,
           ex_both_Hispanic = Hispanic*boss_Hispanic,
           ex_both_Italian = Italian*boss_Italian,
           ex_WE_Asian = (EastAsian + Japanese)*boss_WestEuropean,
           ex_WE_Indian = Indian*boss_WestEuropean,
           ex_WE_Muslim = Muslim*boss_WestEuropean,
           ex_WE_Hispanic = Hispanic*boss_WestEuropean,
           ex_WE_Jewish = Jewish*boss_WestEuropean,
           # Final calculation
           odds = exp(median_params$b_same_first_name*ex_same_first_name 
                      + median_params$b_both_uncommon_names*ex_both_uncommon_names 
                      + median_params$b_both_male*ex_both_male
                      + median_params$b_boss_male*ex_boss_male
                      + median_params$b_boss_female*ex_boss_female
                      + median_params$b_both_female*ex_both_female
                      + median_params$b_both_EastAsian*ex_both_EastAsian
                      + median_params$b_both_EastEuropean*ex_both_EastEuropean
                      + median_params$b_both_Japanese*ex_both_Japanese
                      + median_params$b_both_Indian*ex_both_Indian
                      + median_params$b_both_African*ex_both_African
                      + median_params$b_both_Muslim*ex_both_Muslim
                      + median_params$b_both_WestEuropean*ex_both_WestEuropean
                      + median_params$b_both_Jewish*ex_both_Jewish
                      + median_params$b_both_Hispanic*ex_both_Hispanic
                      + median_params$b_both_Italian*ex_both_Italian
                      + median_params$b_WE_Asian*ex_WE_Asian
                      + median_params$b_WE_Indian*ex_WE_Indian
                      + median_params$b_WE_Muslim*ex_WE_Muslim
                      + median_params$b_WE_Hispanic*ex_WE_Hispanic
                      + median_params$b_WE_Jewish*ex_WE_Jewish))
  
  return( paste0(
    "Your odds of working for this boss are ",
    as.character(round(abs(100*(user_odds/example_employee$odds - 1)))), "% ",
    if(user_odds >= example_employee$odds){"more "}else{"less "},
    "than those of ", c("an ", "a ")[(!example_employee$ethnicity %in% c("African", "Indian", "Italian")) + 1],
    str_to_title(example_employee$ethnicity), c(" man ", " woman ")[example_employee$gender + 1], "named ",
    example_employee$first_name, "."
  ) )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# save(median_params, name_popularity,
#      average_employees, ethnicity_nodes, ethnicity_avg_probs,
#      # d_america_allvars, ethnicity_links,
#      file = "data/show.RData")

load("data/show.RData")

# write.csv(
#   data.frame(firstName = c(d$first_name, d$boss_first_name),
#              lastName = c(d$last_name, d$boss_last_name)) %>% 
#     distinct() %>% 
#     tidyr::drop_na(),
#   file = "unique_names.csv"
# )

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
        selectInput("ethnicity", "Your ethnicity?", selected = "Western European or African American", width = "100%",
                    choices = list(African = "African", `Chinese or Korean` = "Chinese or Korean", 
                                   `Eastern European` = "Eastern European", Hispanic = "Hispanic",
                                   Indian = "Indian", Jewish = "Jewish", Muslim = "Muslim",
                                   `Western European or African American` = "Western European or African American",
                                   Other = "other"))),
      column(5, 
        h3(HTML(
          as.character(actionLink(inputId = "boss", 
                                  label = "", 
                                  icon = icon("user-tie"))), "&nbsp &nbsp Your Boss")), 

        textInput("boss_first_name", "Your boss's first name?", value = "Cruella", width = "100%"),
        selectInput("boss_gender", "Your boss's gender?", selected = 1, width = "100%",
                    choices = list(Man = 0, Woman = 1, Other = 2)),
        selectInput("boss_ethnicity", "Your boss's ethnicity?", selected = "Western European or African American", width = "100%",
                    choices = list(African = "African", `Chinese or Korean` = "Chinese or Korean", 
                                   `Eastern European` = "Eastern European", Hispanic = "Hispanic",
                                   Indian = "Indian", Jewish = "Jewish", Muslim = "Muslim",
                                   `Western European or African American` = "Western European or African American",
                                   Other = "other"))
      ),
      hr(),
      column(8, offset = 2, actionButton("update_output", h4("Let's see my odds!"), width = "100%")),
      hr(),
      column(5, offset = 1,
        br(),
        htmlOutput("gender_number"), h5(textOutput("prediction_gender"), br()),
        h3(htmlOutput("ethnicity_number")),
        h5(textOutput("prediction_ethnicity"), br()),
        h3(htmlOutput("both_uncommon_names_number")),
        h5(textOutput("prediction_both_uncommon_names"), br()),
        h3(htmlOutput("same_name_number")),
        h5(textOutput("prediction_same_name"))
      ),
      column(5, 
       br(), br(), br(),
       h4(htmlOutput("example_comparison1", style="color:grey;"), br(), br(), br()),
       h4(htmlOutput("example_comparison2", style="color:grey;"))
      )
    )
  ),
  
  # tabPanel("Explore the Data", icon = icon("chart-line"),
  #          fluidRow(
  #            column(1, HTML('<p style="margin-top:290px;"><b>Employees</b></p>')),
  #            column(10, sankeyNetworkOutput("sankey_ethnicity", height = "600px")),
  #            column(1, HTML('<p style="margin-top:290px;"><b>Bosses</b></p>'))
  #          )),
  
  tabPanel("Learn More", icon = icon("circle-info"),
           fluidRow(
             column(10, offset = 1,
                    includeMarkdown("learn_more.md"))
             )
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# SERVER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

server <- function(input, output) {

  # Reactives for Home Screen
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  r <- reactiveValues(same_first_name = 0,
                      both_male = 0,
                      both_uncommon_names = median_params$b_both_uncommon_names,
                      boss_male = 0,
                      boss_female = median_params$b_boss_female,
                      both_female = 0,
                      boss_ethnicity = "Western European or African American",
                      ethnicity = "Western European or African American",
                      both_EastAsian = median_params$b_both_EastAsian * ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_EastEuropean = median_params$b_both_EastEuropean * ethnicity_avg_probs$EastEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$EastEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_Japanese = median_params$b_both_Japanese * ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_Indian = median_params$b_both_Indian * ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_African = median_params$b_both_African * ethnicity_avg_probs$African[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$African[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_Muslim = median_params$b_both_Muslim * ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_WestEuropean = median_params$b_both_WestEuropean * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_Jewish = median_params$b_both_Jewish * ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_Hispanic = median_params$b_both_Hispanic * ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      both_Italian = median_params$b_both_Italian * ethnicity_avg_probs$Italian[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$Italian[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      WE_Asian = median_params$b_WE_Asian * (ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == "Western European or African American"] + ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == "Western European or African American"]) * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      WE_Indian = median_params$b_WE_Indian * ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      WE_Muslim = median_params$b_WE_Muslim * ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      WE_Hispanic = median_params$b_WE_Hispanic * ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      WE_Jewish = median_params$b_WE_Jewish * ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == "Western European or African American"] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_first_name = "Cruella",
                      boss_gender = 1,
                      boss_EastAsian = ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_EastEuropean = ethnicity_avg_probs$EastEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_Japanese = ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_Indian = ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_African = ethnicity_avg_probs$African[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_Muslim = ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_WestEuropean = ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_Jewish = ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_Hispanic = ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == "Western European or African American"],
                      boss_Italian = ethnicity_avg_probs$Italian[ethnicity_avg_probs$ethnicity == "Western European or African American"])
  
  observe({
    r$boss_first_name <- input$boss_first_name
    r$boss_gender <- input$boss_gender
    r$boss_ethnicity <- input$boss_ethnicity
    r$ethnicity <- input$ethnicity
    r$same_first_name <- median_params$b_same_first_name*(input$first_name == input$boss_first_name)
    r$both_uncommon_names <- median_params$b_both_uncommon_names*((is_common_name(input$first_name) == 0) & (is_common_name(input$boss_first_name) == 0))
    if(input$gender == 2){
      r$both_male <- 0
      r$boss_male <- 0
      r$boss_female <- 0
      r$both_female <- 0
    }else if(input$gender == 0 & input$boss_gender == 0){
      r$both_male <- median_params$b_both_male
      r$boss_male <- 0
      r$boss_female <- 0
      r$both_female <- 0
    }else if(input$gender == 1 & input$boss_gender == 0){
      r$both_male <- 0
      r$boss_male <- median_params$b_boss_male
      r$boss_female <- 0
      r$both_female <- 0
    }else if(input$gender == 0 & input$boss_gender == 1){
      r$both_male <- 0
      r$boss_male <- 0
      r$boss_female <- median_params$b_boss_female
      r$both_female <- 0
    }else if(input$gender == 1 & input$boss_gender == 1){
      r$both_male <- 0
      r$boss_male <- 0
      r$boss_female <- 0
      r$both_female <- median_params$b_both_female
    }
    
    r$both_EastAsian <- median_params$b_both_EastAsian * ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_EastEuropean <- median_params$b_both_EastEuropean * ethnicity_avg_probs$EastEuropean[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$EastEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_Japanese <- median_params$b_both_Japanese * ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_Indian <- median_params$b_both_Indian * ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_African <- median_params$b_both_African * ethnicity_avg_probs$African[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$African[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_Muslim <- median_params$b_both_Muslim * ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_WestEuropean <- median_params$b_both_WestEuropean * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_Jewish <- median_params$b_both_Jewish * ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_Hispanic <- median_params$b_both_Hispanic * ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$both_Italian <- median_params$b_both_Italian * ethnicity_avg_probs$Italian[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$Italian[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$WE_Asian <- median_params$b_WE_Asian * (ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == input$ethnicity] + ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == input$ethnicity]) * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$WE_Indian <- median_params$b_WE_Indian * ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$WE_Muslim <- median_params$b_WE_Muslim * ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$WE_Hispanic <- median_params$b_WE_Hispanic * ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$WE_Jewish <- median_params$b_WE_Jewish * ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == input$ethnicity] * ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    
    r$boss_EastAsian <- ethnicity_avg_probs$EastAsian[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_EastEuropean <- ethnicity_avg_probs$EastEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_Japanese <- ethnicity_avg_probs$Japanese[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_Indian <- ethnicity_avg_probs$Indian[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_African <- ethnicity_avg_probs$African[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_Muslim <- ethnicity_avg_probs$Muslim[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_WestEuropean <- ethnicity_avg_probs$WestEuropean[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_Jewish <- ethnicity_avg_probs$Jewish[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_Hispanic <- ethnicity_avg_probs$Hispanic[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
    r$boss_Italian <- ethnicity_avg_probs$Italian[ethnicity_avg_probs$ethnicity == input$boss_ethnicity]
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
  
  # ethnicity
  output$ethnicity_number <- renderText({
    if(r$ethnicity == "Other" | r$boss_ethnicity == "Other" | (r$ethnicity != r$boss_ethnicity & r$boss_ethnicity != "Western European or African American")){
      
    }else{
      aggregate_ethnicity <- r$both_EastAsian + r$both_EastEuropean + r$both_Japanese + r$both_Indian + r$both_African + r$both_Muslim + r$both_WestEuropean + r$both_Jewish + r$both_Hispanic + r$both_Italian + r$WE_Asian + r$WE_Indian + r$WE_Muslim + r$WE_Hispanic + r$WE_Jewish
      paste0('<b> <span style=\"font-size:', 
             as.character(30 + 100*abs(exp(aggregate_ethnicity) - 1)), 'px;',
             'color:', c("red", "green")[1L + (aggregate_ethnicity > 0)], 
             '\"> ', as.character(round(100*(exp(aggregate_ethnicity) - 1))), "%",
             '</span> </b>')
    }
  })
   
  output$prediction_ethnicity <- renderText({
    if(r$ethnicity == "Other" | r$boss_ethnicity == "Other" | (r$ethnicity != r$boss_ethnicity & r$boss_ethnicity != "Western European or African American")){
      text <- "The impact of your ethnicity cannot be inferred from this dataset."
    }else{
      aggregate_ethnicity <- r$both_EastAsian + r$both_EastEuropean + r$both_Japanese + r$both_Indian + r$both_African + r$both_Muslim + r$both_WestEuropean + r$both_Jewish + r$both_Hispanic + r$both_Italian + r$WE_Asian + r$WE_Indian + r$WE_Muslim + r$WE_Hispanic + r$WE_Jewish
      text <- c("The fact that ",
        c("both you and your boss are Chinese or Korean ",
          "both you and your boss are Eastern European ",
          "both you and your boss are Japanese ",
          "both you and your boss are Indian ",
          "both you and your boss are African ",
          "both you and your boss are Muslim ",
          "both you and your boss are Western European or African American ",
          "both you and your boss are Jewish ",
          "both you and your boss are Hispanic ",
          "both you and your boss are Italian ",
          "your boss is Western European or African American but you are East Asian ",
          "your boss is Western European or African American but you are Indian ",
          "your boss is Western European or African American but you are Muslim ",
          "your boss is Western European or African American but you are Hispanic ",
          "your boss is Western European or African American but you are Jewish ")[
            which(
              as.logical(c(r$ethnicity == "Chinese or Korean" & r$boss_ethnicity == "Chinese or Korean",
                           r$ethnicity == "Eastern European" & r$boss_ethnicity == "Eastern European",
                           r$ethnicity == "Japanese" & r$boss_ethnicity == "Japanese",
                           r$ethnicity == "Indian" & r$boss_ethnicity == "Indian",
                           r$ethnicity == "African" & r$boss_ethnicity == "African",
                           r$ethnicity == "Muslim" & r$boss_ethnicity == "Muslim",
                           r$ethnicity == "Western European or African American" & r$boss_ethnicity == "Western European or African American",
                           r$ethnicity == "Jewish" & r$boss_ethnicity == "Jewish",
                           r$ethnicity == "Hispanic" & r$boss_ethnicity == "Hispanic",
                           r$ethnicity == "Italian" & r$boss_ethnicity == "Italian",
                           r$ethnicity %in% c("Chinese or Korean", "Japanese") & r$boss_ethnicity == "Western European or African American",
                           r$ethnicity == "Indian" & r$boss_ethnicity == "Western European or African American",
                           r$ethnicity == "Muslim" & r$boss_ethnicity == "Western European or African American",
                           r$ethnicity == "Hispanic" & r$boss_ethnicity == "Western European or African American",
                           r$ethnicity == "Jewish" & r$boss_ethnicity == "Western European or African American"))
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
        as.character(round(exp(aggregate_ethnicity), 2)),
        " times what it would be otherwise."
        )
    }
    text
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
        "1.02 times what it would be otherwise")
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
        "1.56 times what it would be otherwise")
      }
    })
  
  # EXAMPLE COMPARISONS
  output$example_comparison1 <- renderText({
    generate_example_comparison(r$boss_first_name, is_common_name(r$boss_first_name), r$boss_gender, r$boss_ethnicity,
                                r$same_first_name, r$both_uncommon_names,
                                r$both_male, r$boss_male, r$boss_female, r$both_female,
                                r$both_EastAsian, r$both_EastEuropean, r$both_Japanese, 
                                r$both_Indian, r$both_African, r$both_Muslim, 
                                r$both_WestEuropean, r$both_Jewish,r$both_Hispanic,
                                r$both_Italian, r$WE_Asian, r$WE_Indian,
                                r$WE_Muslim, r$WE_Hispanic, r$WE_Jewish)
  })
  output$example_comparison2 <- renderText({
    generate_example_comparison(r$boss_first_name, is_common_name(r$boss_first_name), r$boss_gender, r$boss_ethnicity,
                                r$same_first_name, r$both_uncommon_names,
                                r$both_male, r$boss_male, r$boss_female, r$both_female,
                                r$both_EastAsian, r$both_EastEuropean, r$both_Japanese, 
                                r$both_Indian, r$both_African, r$both_Muslim, 
                                r$both_WestEuropean, r$both_Jewish,r$both_Hispanic,
                                r$both_Italian, r$WE_Asian, r$WE_Indian,
                                r$WE_Muslim, r$WE_Hispanic, r$WE_Jewish)
  })
    
    # output$sankey_gender <- renderSankeyNetwork({
    #   sankeyNetwork(Links = gender_links, Nodes = gender_nodes,
    #                 Source = "gender", Target = "boss_gender",
    #                 Value = "n", NodeID = "gender", colourScale = 'd3.scaleOrdinal() .range(["#9B3634", "#96C9DC"])',
    #                 units = "occurrences", fontFamily = "arial",
    #                 sinksRight = TRUE, nodeWidth = 40, fontSize = 14, nodePadding = 10)
    # })
    # output$sankey_ethnicity <- renderSankeyNetwork({
    #   color_scale <- 'd3.scaleOrdinal() .range(["#BD3E3E", "#9B3634", "#61A0AF", "#BE8A60", "#C5D86D",
    #                   "#61AD55", "#56AE96", "#5E8463", "#3F6246", "#96C9DC"])'
    #   sankeyNetwork(Links = ethnicity_links, Nodes = ethnicity_nodes,
    #                 Source = "ethnicity", Target = "boss_ethnicity",
    #                 Value = "n", NodeID = "ethnicity", colourScale = color_scale,
    #                 units = "occurrences", fontFamily = "arial",
    #                 sinksRight=TRUE, nodeWidth=40, fontSize=14, nodePadding=10)
    # })
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# RUN
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

shinyApp(ui, server)

