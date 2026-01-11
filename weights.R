library(tidyverse)
library(ggplot2)
library(lubridate)
library(shiny)
library(bslib)
setwd("/Users/masonyoung/Desktop/Personal_Projects/WeightDataExploration")
weights <- read.csv("weights.csv", header = TRUE)

head(weights)

# weights <- weights |> rename(1 = "MasonLbs", 2 = "MasonKg", 3 = "SofiLbs", 4 = "SofiKg", 5 = "Creatine",
#                              6 = "SleepScore", 7 = "HoursSlept")

ui <- fluidPage(
  theme = bs_theme(
    bg = "#F5F5F5",         
    fg = "#141414",         
    primary = "#007BFF",    
    base_font = font_google("Roboto") 
  ),
  titlePanel("Sofia and Mason's Weights Over Time"),
  tabsetPanel(
    tabPanel("Weights Over Full Period",
      sidebarLayout(
        sidebarPanel(
          div(class="text-danger",
              sliderInput(
                inputId = "days",
                label = "Days since August 25th: ",
                min = 0,
                max = 101,
                value = 0,
                step = 1
              )
          ),
          checkboxInput(
            inputId = "smooth",
            label = "Line of best fit"
          ),
          htmlOutput("percent")
        ),
        mainPanel(
          plotOutput("weightsGraph"),
          htmlOutput("defecit")
        )
      )
    ),
    tabPanel("Sleep",
             sidebarLayout(
               sidebarPanel(
                 div(
                   class="text-danger",
                   sliderInput(
                     inputId = "hours",
                     label = "Hours slept: ",
                     min = 0,
                     max = 12,
                     value = 0,
                     step = 1
                   )
                 )
               ),
               mainPanel(
                 htmlOutput("note"),
                 plotOutput("sleep_graph"),
                 htmlOutput("sleep_mdl_out"),
                 htmlOutput("sleep_pred")
               )
             )
            )
  )
)

server <- function(input, output) {
  start <- as.Date("2025-08-25", "%Y-%m-%d")
  weights <- weights |> mutate(Date = seq(start, by='day', length.out = length(weights$Date)))
  weights
  
  mason_weights <- weights |> filter(!(is.na(Mason.s.Weight..lbs.)), !(is.na(Mason.s.Weight.kg.)),
                                     Mason.s.Weight..lbs. > 0, Mason.s.Weight.kg. > 0) 
  
  both_weights <- weights |> filter(!(is.na(Mason.s.Weight..lbs.)), !(is.na(Mason.s.Weight.kg.)),
                                    !(is.na(Sofi.s.Weight..kg.)), !(is.na(Sofi.s.Weight..lbs.)),
                                    Mason.s.Weight..lbs. > 0, Mason.s.Weight.kg. > 0,
                                    Sofi.s.Weight..kg. > 0, Sofi.s.Weight..lbs. > 0)
  
  daily_start <- as.numeric(1)
  
  mason_weights <- mason_weights |> mutate(Day = difftime(Date, start, units = "days"))
  sofi_weights <- both_weights |> mutate(Day = difftime(Date, start, units = "days"))
  
  mason_daily <- reactive({
    mason_weights |> filter(as.numeric(Day) <= input$days)
  })
  
  sofi_daily <- reactive ({
    sofi_weights |> filter(as.numeric(Day) <= input$days)
  })
  
  sleep_data <- weights |> filter((!is.na(Sleep.Score)), !(is.na(Hours.Slept)))
  
  sleep_mdl <- lm(Hours.Slept ~ Sleep.Score, data = sleep_data)
  sleep_int <- summary(sleep_mdl)$coefficients["(Intercept)", "Estimate"]
  sleep_coef <- summary(sleep_mdl)$coefficients["Sleep.Score", "Estimate"]
  sleep_p <- summary(sleep_mdl)$coefficients["Sleep.Score", "Pr(>|t|)"]
  
  
  output$weightsGraph <- renderPlot({
    p <- ggplot() + geom_point(data = mason_daily(), aes(x = Date, y = Mason.s.Weight..lbs., color = "Mason")) +
      geom_point(data = sofi_daily(), aes(x = Date, y = Sofi.s.Weight..lbs., color = "Sofi")) +
      labs(
        y = "Weights (lbs)",
        title = "Mason and Sofi's Weigths Over Time (lbs)",
        color = "Person"
      ) +
      theme_bw()
    
    if (input$smooth == TRUE) {
      p <- p + geom_smooth(data = sofi_daily(), method = "lm", aes(x = Date, y = Sofi.s.Weight..lbs.)) +
        geom_smooth(data = mason_daily(), method = "lm", aes(x = Date, y = Mason.s.Weight..lbs.))
    }
    p
  })
  
  output$percent <- renderUI({
    mason <- mason_daily()
    sofi <- sofi_daily()
    
    mason_min <- min(mason$Mason.s.Weight..lbs.)
    mason_max <- max(mason$Mason.s.Weight..lbs.)
    
    sofi_min <- min(sofi$Sofi.s.Weight..lbs.)
    sofi_max <- max(sofi$Sofi.s.Weight..lbs.)
    
    mason_percent_diff <- ((mason_max - mason_min) / mason_min) * 100
    sofi_percent_diff <- ((sofi_max - sofi_min) / sofi_min) * 100
    
    HTML(paste0("Mason percent change: <b>-", round(mason_percent_diff, 2), "%</b><br><br>
                Sofi percent change: <b>-", round(sofi_percent_diff, 2), "%</b>" ))
    
  })
  
  output$defecit <- renderUI({
    # evaluate the p_value
    p_eval <- function(p_value) {
      if (p_value < 0.05) {
        return("most likely")
      }
      else {
        return("maybe")
      }
    }
    
    # evaluate the coefficient
    c_eval <- function(coef) {
      if (coef < 0) {
        return("cutting")
      }
      else if (coef == 0) {
        return("maintaining")
      }
      else {
        return("bulking")
      }
    }
      mason_data <- mason_daily()
      sofi_data <- sofi_daily()
      
      if (nrow(mason_data) < 10) {
        return(HTML("<b>Not enough observations to accurately run a regression</b>"))
      }
      
      mason_mdl <- lm(Mason.s.Weight..lbs. ~ Date, data = mason_data)
      sofi_mdl <- lm(Sofi.s.Weight..lbs. ~ Date, data = sofi_data)
      
      m_p <- summary(mason_mdl)$coefficients["Date", "Pr(>|t|)"]
      s_p <- summary(sofi_mdl)$coefficients["Date", "Pr(>|t|)"]
      m_c <- coef(mason_mdl)
      s_c <- coef(sofi_mdl)
      
      HTML(paste0("<b>Mason</b> is<b> ", p_eval(m_p), "</b> ", c_eval(m_c["Date"]), " losing
      an average of <b> ", round(m_c["Date"], 2), "</b>lbs per day (p-value: ", round(m_p, 3), ") <br><br><b>Sofi</b>
                is<b> ", p_eval(s_p), "</b> ", c_eval(s_c["Date"]), " losing
                  an average of <b> ", round(s_c["Date"], 2), "</b>lbs per day
                  (p-value: ", round(s_p, 3), ")"))
  })
  
  output$note <- renderUI({
    HTML(paste0("<b>Note: Not enough data yet for accurate predictions</b><br><br>"))
  })
  
  output$sleep_graph <- renderPlot({
    sleep_data <- weights |> filter(!(is.na(Sleep.Score)), !(is.na(Hours.Slept)))
    
    sleep_data |> ggplot() + 
      geom_point(aes(x = Hours.Slept, y = Sleep.Score)) +
      geom_smooth(data = sleep_data, method = "lm", aes(x = Hours.Slept, y = Sleep.Score)) +
      labs(
        title = "Hours Slept vs. Sleep Score",
        x = "Hours Slept",
        y = "Sleep Score"
      ) + 
      theme_minimal()
  })
  
  output$sleep_mdl_out <- renderUI({
    
    HTML(paste0("On average, with each additional hour of sleep, sleep score increases by <b>",
                round(sleep_coef, 2), "</b> points (p-value: ", round(sleep_p, 3), ")"))
  })
  
  output$sleep_pred <- renderUI({
    estimate <- (sleep_coef * input$hours) + sleep_int
    HTML(paste0("<br><br>Based on the hours you inputted, I estimate that I slept <b>", 
                round(estimate, 2), "</b> hours."))
  })
}

shinyApp(ui, server)
