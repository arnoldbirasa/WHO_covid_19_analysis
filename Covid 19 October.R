#### TASK 1

# Loading the dataset - data <- read.csv("covid19_data.csv")
musana= read.csv("WHO-COVID-19-global-data.csv", header=TRUE)

# Inspect the structure
str(musana)
attach(musana)

# Calculating basic summary statistics
summary(musana$New_cases)
summary(musana$New_deaths)
summary(musana$Cumulative_cases)
summary(musana$Cumulative_cases)
# could not perform summary for population due to missing varriable [summary(musana$Population)]

colnames(musana)[1]= "Date"
str(musana)

# Visualize trends in NewCases and NewDeaths over time
musana$Date= as.Date.character(musana$Date, c("%Y-%m-%d"))
str(musana)

library(ggplot2)
ggplot(musana, aes(x = Date)) +
  geom_line(aes(y = New_cases), color = "blue") +
  geom_line(aes(y = New_deaths), color = "red") +
  labs(title = "Trends in NewCases and NewDeaths", x = "Date", y = "New Cases/Deaths")
# plot will is named 'Rplot.png'

## TASK 2: DESCRIPTIVE ANALYSIS

# Calculate daily incidence rate and daily mortality rate
#since there is no data for population, it is impossible to calculate incidence rate but the following are codes to calculate incidence rate 
#   data$IncidenceRate <- (data$NewCases / data$Population) * 100000
#since there is no data for population, it is impossible to calculate mortality rate but the following are codes to calculate mortality rate 
#   data$MortalityRate <- (data$NewDeaths / data$Population) * 100000

# Identify date with highest and lowest rates
#    highest_incidence_date <- data$Date[which.max(data$IncidenceRate)]
#    lowest_incidence_date <- data$Date[which.min(data$IncidenceRate)]
#    highest_mortality_date <- data$Date[which.max(data$MortalityRate)]
#    lowest_mortality_date <- data$Date[which.min(data$MortalityRate)]

## TASK 3: TIME SERIES ANALYSIS

# Perform time series decomposition for NewCases
NewCases_tseries <- ts(musana$New_cases, frequency = 365)
NewCases_decomp <- decompose(NewCases_tseries)
par(mfrow= c(2,2))
plot(NewCases_decomp$trend)
plot(NewCases_decomp$random)
plot(NewCases_decomp$x)
plot(NewCases_decomp$figure)

# Perform time series decomposition for NewDeaths
NewDeaths_tseries <- ts(musana$New_deaths, frequency = 365)
NewDeaths_decomp <- decompose(NewDeaths_tseries)
par(mfrow= c(2,2))
plot(NewDeaths_decomp$trend)
plot(NewDeaths_decomp$random)
plot(NewDeaths_decomp$x)
plot(NewDeaths_decomp$figure)

#### TASK 4: CORRELATION ANALYSIS

# Calculate correlation between NewCases and NewDeaths
correlation <- cor(musana$New_cases, musana$New_deaths)
correlation
# A correlation coefficient of 0.31768 indicates a positive but weak correlation between the number of COVID-19 deaths and new cases
# a positive correlation suggests that as the number of COVID-19 new cases increases, there is a tendency for the number of COVID_19 deaths to also increase, although the relationship is not very strong
# a correlation coefficient of 0.31768 indicates a relatively weak relationship, suggesting that the number of cases alone does not account for a large proportion of the variation in the number of deaths
# correlation does not imply causation

# Investigate lag effect in correlation
correlation_lag <- cor(musana$New_cases[-1], musana$New_deaths[-length(musana$New_deaths)])
correlation_lag
# A lag effect of 0.22177 for COVID_19 NewCases and COVID_19 deaths, suggests that changes in COVID_19 new cases precedes changes in COVID_19 deaths by approximately 0.22177 time units
# in this context since we are analyzing daily data, the lag effect is approximately 4 to 5 hours

#### TASK 5: PREDICTIVE MODELING

# Split data into training and testing sets
training <- floor(0.8 * nrow(musana))
train_data <- musana[1:training, ]
test_data <- musana[(training + 1):nrow(musana), ]

# Build predictive model
model_1<- lm(New_cases ~ Date, data = train_data)

# Forecast NewCases for the next 7 days
forecast= predict(model_1, newdata = data.frame(Date =seq(as.Date(max(musana$Date)) + 1, by="day", length.out=7)), interval = "prediction")
forecast

# Evaluate model's performance on the testing dataset
predictions= predict(model_1, newdata = test_data)
rmse <- sqrt(mean((test_data$New_cases - predictions)^2))
rmse
# Root Mean Square Error measures how close the predicted values are to the true values
# a rmse of 23893.65 indicates a larger average difference between the predicted and actual values
# simply put, 23893.65 suggests that on average, the model's predictions have a higher degree of deviation from the actual values

mae <- mean(abs(test_data$New_cases - predictions))
mae
# Mean Absolute Error measures the average magnitude of the absolute differences between the predicted values and the actual values
# a mae of 4638.88 indicate larger errors between the predicted and actual values

# RMSE is more sensitive to outliers as it squares the differences, while MAE evaluates the absolute differences without squaring them

#### TASK 6: EPIDEMIOLOGICAL ANALYSIS

# Calculate basic reproductive number R0 = (Total Number of New Cases) / (Total Number of Cases in the Previous Generation)
R0= sum(musana$New_cases) / sum(musana$New_cases[-nrow(musana)])
R0
# an R0 of 1 can suggest two things:
# First, on average each individual infected with COVID_19 is likely to transmit to exactly one person
# Secondly, an R0 of 1 also indicates that there is no significant increase or decrease of COVID_19, meaning it is stable

#### RECOMMENDATION

#After critical analysis and interpretation, the following are some of epidemiological and statistical recommendation and suggestions:
  
#1. Missing data: There was missing data in the dataset hence preventing further analysis which would have provided more insight on the dataset such as incidence rate and mortality rate
#2. Correlation Analysis: The weak correlation (0.31768) between COVID_19 new cases and COVID_19 deaths signifies that there are other factors influencing death rates apart from the new cases alone. Before making any significant decisions or predictions using this correlation, consider further research and analysis to identify other potential factors
#3. Investigating the Lag Effect: The lag effect of 0.22177 for COVID_19 is not insignificant. Furthermore, it is also vital for temporal analyses, especially for predicting and controlling the spread of COVID_19 epidemic/pandemic. Conducting future studies to analyse the lag effect for more lag periods can provide more insightful results
#4. Predictive Modelling: The RMSE and MAE are quite too large, suggesting the predictions have a higher degree of deviation from the actual values. To improve in such cases, further studies need to be done and more data need to be collected to get a deeper and better insights 
#5. Epidemiological Analysis: An R0 of 1 indicates a stable condition of COVID_19 spread in the community. However, epidemiologically speaking it is bad if one individual gets sick and affects one individual, it should be kept lower than that. Furthermore, considering the dynamic nature of COVID_19, continue monitoring the R0 over time
#6. Further Analysis and Data Collection: More data collection should be done and in a more comprehensive way. A more detailed analysis including factors such as population density, vaccination rate, mortality rate, and more, might provide more accurate insights

#### DATA SOURCE

#Website:  https://covid19.who.int/data

#Daily cases and deaths by date reported to WHO
#Download link: https://covid19.who.int/WHO-COVID-19-global-data.csv
