library(tidyverse)
library(ggplot2)
library(lubridate)
library(shiny)
setwd("/Users/masonyoung/Desktop/Personal_Projects/WeightsOverTime")
weights <- read.csv("weights.csv", header = TRUE)

head(weights)

# weights <- weights |> rename(1 = "MasonLbs", 2 = "MasonKg", 3 = "SofiLbs", 4 = "SofiKg", 5 = "Creatine",
#                              6 = "SleepScore", 7 = "HoursSlept")


ui <- fluidPage(
  titlePanel("Sofia and Mason's Weights Over Time"),
  tabsetPanel(
    tabPanel("Weights Over Full Period",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "days",
            label = "Days since August 25th: ",
            min = 0,
            max = 101,
            value = 0,
            step = 1
          )
        ),
        mainPanel(
          plotOutput("weightsGraph"),
          htmlOutput("defecit")
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

  mason_min <- min(weights$Mason.s.Weight..lbs.)
  sofi_min <- min(weights$Sofi.s.Weight..lbs.)
  mason_min_kg <- min(weights$Mason.s.Weight.kg.)
  sofi_min_kg <- min(weights$Sofi.s.Weight..kg.)
  
  mason_max <- max(weights$Mason.s.Weight..lbs.)
  sofi_max <- max(weights$Sofi.s.Weight..lbs.)
  sofi_max_kg <- max(weights$Sofi.s.Weight..kg.)
  
  diff_mason <- mason_max - mason_min
  diff_mason
  
  diff_sofi <- sofi_max - sofi_min
  diff_sofi
  
  percent_diff_mason <- (diff_mason / mason_max) * 100
  percent_diff_sofi <- (diff_sofi / sofi_max) * 100
  
  round(percent_diff_mason, 2)
  round(percent_diff_sofi, 2)
  
  daily_start <- as.numeric(1)
  
  mason_weights <- mason_weights |> mutate(Day = difftime(Date, start, units = "days"))
  sofi_weights <- both_weights |> mutate(Day = difftime(Date, start, units = "days"))
  
  mason_daily <- reactive({
    mason_weights |> filter(as.numeric(Day) <= input$days)
  })
  
  sofi_daily <- reactive ({
    sofi_weights |> filter(as.numeric(Day) <= input$days)
  })
  
  output$weightsGraph <- renderPlot({
    ggplot() + geom_point(data = mason_daily(), aes(x = Date, y = Mason.s.Weight..lbs., color = "Mason")) +
      geom_point(data = sofi_daily(), aes(x = Date, y = Sofi.s.Weight..lbs., color = "Sofi")) +
      labs(
        y = "Weights (lbs)",
        title = "Mason and Sofi's Weigths Over Time (lbs)",
        color = "Person"
      ) +
      geom_smooth(data = sofi_daily(), method = "lm", aes(x = Date, y = Sofi.s.Weight..lbs.)) +
      geom_smooth(data = mason_daily(), method = "lm", aes(x = Date, y = Mason.s.Weight..lbs.)) +
      theme_bw()
  })
  
  output$defecit <- renderUI({
    # evaluate the p_value
    p_eval <- function(p_value) {
      if (p_value < 0.05) {
        return("definitely")
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
}

shinyApp(ui, server)
