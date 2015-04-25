library(shiny)
library(ggplot2)
source('load_data.R')

factors <- c("No_Exercise" = "% No Exercise in Community",
             "Few_Fruit_Veg" = "% Few Fruit/Vegetables in Community",
             "Obesity" = "% Obesity in Community",
             "High_Blood_Pres" = "% High Blood Pressure in Community",
             "Smoker" = "% Smokers in Community",
             "Diabetes" = "% Diabetes in Community")

shinyServer(function(input, output) {
  output$scatterPlot <- renderPlot({
    risk <- input$risk
    ggplot(data, aes_string(x=risk, y='ALE')) +
      geom_point(col=c('skyblue')) +
      geom_smooth(method=lm, col=c('darkred')) +
      xlab(factors[risk]) +
      ylab('Average life expectancy')
  })
  output$scatterPlotDescription <- renderText({
    risk <- input$risk
    if (risk == "No_Exercise"){
      return("The following plot displays the relationship between the percent of people
      that do not exercise and the average life expectancy of a community.")
    }
    if (risk == "Few_Fruit_Veg"){
      return("The following plot displays the relationship between the percent of people
      that eat few fruits and vegetables and the average life expectancy of a community.")
    }
    if (risk == "Obesity"){
      return("The following plot displays the relationship between the percent of obese people
      and the average life expectancy of a community.")
    }
    if (risk == "High_Blood_Pres"){
      return("The following plot displays the relationship between the percent of people
    with high blood pressure and the average life expectancy of a community.")  
    }
    if (risk == "Smoker"){
      return("The following plot displays the relationship between the percent of people
      that smoke and the average life expectancy of a community.")
    }
    if (risk == "Diabetes"){
      return("The following plot displays the relationship between the percent of people
      with diabetes and the average life expectancy of a community.")   
    }
  })
  output$predictValue <- renderText({
    newdata <- data.frame(
      No_Exercise = c(input$No_Exercise),
      Obesity = c(input$Obesity),
      High_Blood_Pres = c(input$High_Blood_Pres),
      Smoker = c(input$Smoker),
      Diabetes = c(input$Diabetes)
    )
    round(predict(model, newdata = newdata),2)
  })
})