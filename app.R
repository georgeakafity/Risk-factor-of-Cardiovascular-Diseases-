library(shiny)
library(tidyverse)
library(janitor)
library(DT)
cvd_clean <- read_csv("cvd_clean_final.csv")
ui <- fluidPage(
  titlePanel("Cardiovascular Disease Dataset Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "ageRange",
        "Select Age Range:",
        min = min(cvd_clean$age, na.rm = TRUE),
        max = max(cvd_clean$age, na.rm = TRUE),
        value = c(min(cvd_clean$age, na.rm = TRUE), max(cvd_clean$age, na.rm = TRUE))
      ),
      selectInput(
        "bmiFilter",
        "Select BMI Category:",
        choices = c("All", levels(cvd_clean$bmi_category)),
        selected = "All"
      ),
      selectInput(
        "sexFilter",
        "Select Sex:",
        choices = c("All", levels(cvd_clean$sex)),
        selected = "All"
      ),
      selectInput(
        "hypertensionFilter",
        "Hypertension Status:",
        choices = c("All", levels(cvd_clean$hypertension)),
        selected = "All"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Statistics",
                 verbatimTextOutput("summaryStats")),
        tabPanel("Histograms",
                 plotOutput("ageHist"),
                 plotOutput("bmiHist")),
        tabPanel("Data Table",
                 DTOutput("dataTable"))
      )
    )
  )
)
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- cvd_clean %>%
      filter(age >= input$ageRange[1], age <= input$ageRange[2])
    if(input$bmiFilter != "All"){
      data <- data %>% filter(bmi_category == input$bmiFilter)
    }
    if(input$sexFilter != "All"){
      data <- data %>% filter(sex == input$sexFilter)
    }
    if(input$hypertensionFilter != "All"){
      data <- data %>% filter(hypertension == input$hypertensionFilter)
    }
    return(data)
  })
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    summarise(
      data,
      mean_age = mean(age, na.rm = TRUE),
      sd_age = sd(age, na.rm = TRUE),
      mean_bmi = mean(bmi, na.rm = TRUE),
      mean_systolic_bp = mean(systolic_bp, na.rm = TRUE),
      mean_diastolic_bp = mean(diastolic_bp, na.rm = TRUE),
      mean_cholesterol = mean(total_cholesterol_mg_d_l, na.rm = TRUE),
      mean_fasting_blood_sugar_mg_d_l = mean(fasting_blood_sugar_mg_d_l, na.rm = TRUE),
      mean_abdominal_circumference_cm = mean(abdominal_circumference_cm, na.rm = TRUE)
    )
  })
  output$ageHist <- renderPlot({
    ggplot(filtered_data(), aes(x = age)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution", x = "Age", y = "Count")
  })
  output$bmiHist <- renderPlot({
    ggplot(filtered_data(), aes(x = bmi_category)) +
      geom_bar(fill = "salmon") +
      labs(title = "BMI Category Distribution", x = "BMI Category", y = "Count")
  })
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
}
shinyApp(ui = ui, server = server)
