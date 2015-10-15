# load the forecast library
library(forecast)

# read the csv file into df data frame from file DATA.csv
df <- read.csv("DATA.csv")

# Get unique product names
uniq <- unique(df$Product)

# initialize the list that will contain the time series for each unique product
productsTS <- list()

# Loop to create each time series
for (i in 1:length(uniq)) {
  # create a temporary data frame that is a subset of the current product
  tempDF <- data.frame(subset(df, Product == uniq[i]))
  # find the starting year of the series
  startYear <- min(tempDF$Year)
  # find the starting month of the series
  startMonth <- min(subset(tempDF, Year == startYear)$Month)
  # insert the time series of he current product into the list productsTS
  productsTS[[i]] <- ts(tempDF$QTY, start = c(startYear, startMonth), frequency = 12, 
                        deltat = 1/12)
}

# initialize productModels list
productModels <- list()

# loop to create each model
for (i in 1:length(productsTS)) {
  # This will take a while as it is brute forcing the model
  productModels[[i]] <- auto.arima(productsTS[[i]], stepwise = FALSE, parallel = TRUE, 
                                   approximation = FALSE, max.order = 10)
}

# initialize productsPrediction list
productsPrediction <- list()

# loop through creating each the predictions for each time series
for (i in 1:length(productModels)) {
  # Create a forecast for each model
  productsPrediction[[i]] <- forecast(productModels[[i]], h = 5)
}

# craete the list of output data fames
outputDFs <- list()

# loop to create each data frame for outputing
for (i in 1:length(uniq)) {
  outputDFs[[i]] <- data.frame(lwr95 = productsPrediction[[i]]$lower[, 2], lwr80 = productsPrediction[[i]]$lower[, 
                                                                                                                 1], mean = productsPrediction[[i]]$mean, upr80 = productsPrediction[[i]]$upper[, 
                                                                                                                                                                                                1], upr95 = productsPrediction[[i]]$upper[, 2])
  # change the names of each row to the corresponding date
  rownames(outputDFs[[i]]) <- as.Date(time(productsPrediction[[i]]$mean), format = "%m/%Y")
  # change the column names to the corresponding prediction
  colnames(outputDFs[[i]]) <- array(c("Lower 95%", "Lower 80%", "Mean", "Upper 80%", "Upper 95%"))
}

# loop to create each output file
for (i in 1:length(uniq)) {
  # write the prediction data frame to the file 'ProductName.csv'
  write.csv(outputDFs[[i]], file = paste(uniq[i], ".csv", sep = ""))
} 