data <- read.csv('./data/data.csv')
data[data == -1111.1] <- NA
data[data == -2222.2] <- NA
data <- na.omit(data)

model <- lm(formula = ALE ~ No_Exercise + Obesity + High_Blood_Pres + 
              Smoker + Diabetes, data = data)
