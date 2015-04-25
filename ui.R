library(shiny)
source('load_data.R')
shinyUI(fluidPage(
  
  titlePanel("Community Health Status Indicators"),
  p("Community Health Status Indicators (CHSI) to combat obesity,
    heart disease, and cancer are major components of the Community
    Health Data Initiative. This dataset provides key health indicators
    for local communities and encourages dialogue about actions that
    can be taken to improve community health
    (e.g., obesity, heart disease, cancer)."),
  p(tags$a(href = "https://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer",
           "The Community Health Status Indicators (CHSI) to Combat Obesity, Heart Disease and Cancer Dataset.")),
  p("The 'Data' tab displays the relationship between health risk factors
    and average life expectancy in 3,141 United States counties."),
  p("In the 'Prediction' tab you can predict average life expectancy of a community
    by providing values for risk factors."),
    tabsetPanel(
      tabPanel("Data", 
               inputPanel(
                 radioButtons("risk", "Select a Risk Factor:",
                              c("No Exercise" = "No_Exercise",
                                "Few Fruits/Vegetables" = "Few_Fruit_Veg",
                                "Obesity" = "Obesity",
                                "High Blood Pressure" = "High_Blood_Pres",
                                "Smoker" = "Smoker",                   
                                "Diabetes" = "Diabetes"))
               ),
               textOutput("scatterPlotDescription"),
               plotOutput("scatterPlot")),
      tabPanel("Prediction",
               wellPanel(
                p("The following app predicts the average life expectancy 
                  of a community based on the percentage of people that
                  do not exercise, are obese, have high blood pressure,
                  smoke or have diabetes. The 'Few Fruits/Vegetables' factor 
                  was not used in the model because it isn't significant."),
                p("The values of risk factors are set by defaut to mean values for the US."),
                p("Move the sliders below to calculate the predicted average life expectancy based on given risk factors.")
               ),
               inputPanel(
                 sliderInput("No_Exercise", "% No Exercise:",
                              round(mean(data$No_Exercise)),
                              min = 0,
                              max = 100),
                 sliderInput("Obesity", "% Obesity:", 
                              round(mean(data$Obesity)),
                             min = 0,
                             max = 100),
                 sliderInput("High_Blood_Pres", "% High Blood Pressure:",
                              round(mean(data$High_Blood_Pres)),
                              min = 0,
                              max = 100),
                 sliderInput("Smoker", "% Smokers:",
                              round(mean(data$Smoker)),
                              min = 0,
                              max = 100),
                 sliderInput("Diabetes", "% Diabetes:",
                              round(mean(data$Diabetes)),
                              min = 0,
                              max = 100)
               ),
               wellPanel(
                  "Predicted Average Life Expectancy is: ",
                 tags$b(textOutput("predictValue")))
            )
      )

))