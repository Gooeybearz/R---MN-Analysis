library(dygraphs)
library(forecast)
library(kableExtra)
library(quantmod)
library(tidyverse)
library(tseries)
library(urca)
library(vars)
# library(randomForest)
library(ggplot2)
library(sf)
library(dplyr)
library(readxl)

#Load csv data aggregated from NICS data for Minnesota
# Data was cleaned from https://www.fbi.gov/file-repository/nics_firearm_checks_-_month_year_by_state.pdf
data <- read.csv("C:/Users/boss0/OneDrive/Documents/minnesota_applications.csv")

data$Date <- as.Date(with(data, paste(Year, Month, 1, sep = "-")), "%Y-%m-%d")
data <- data[,4]
data <- data[order(as.Date(data$Date, format = "%Y-%m-%d")),]
data <- data[1:306,]

data_ts <- data %>%
  subset(,1) %>%
  ts(start = c(1999, 1), frequency = 12)
data_ts

# Unemployment Rate in Minnesota (MNURN) data was taken from FRED
getSymbols("MNURN", src = "FRED", from = "1999-01-01")

mnurn_ts <- ts(MNURN, start = c(1999, 1), frequency = 12)
mnurn_ts


# Consumer Price Index for All Urban Consumers: All Items in U.S. City Average (CPIAUCNS) data was taken from FRED
getSymbols("CPIAUCNS", src = "FRED", from = "1999-01-01")

cpi_ts <- ts(CPIAUCNS, start = c(1999, 1), frequency = 12)
cpi_ts



# Combine all three data sets in time series format
combined <- cbind(data_ts, CPIAUCNS, MNURN)
colnames(combined) <- c("apps", "cpi", "unemploy")
combined

combined_ts <- ts(combined, start = c(1999, 1), frequency = 12)
combined_ts


# Break data up into test and train

data_train <- window(combined_ts, end = c(2023, 12))
data_test <- window(combined_ts, start = c(2024, 1))
data_test_y <- data_test[,1]

# Graphs

# Graph of MN population density based on county population in 2022
# Data was downloaded from https://gisdata.mn.gov/dataset/bdry-counties-in-minnesota
mapdata <- read.csv("C:/Users/boss0/Downloads/mn-county-estimates-sdc-2022.csv")

mapdata[, 3] <- as.numeric(gsub(",", "", data[, 3]))  # Remove commas if any and convert to numeric


mapdata <- mapdata %>%
  mutate(Population_Percentage = (mapdata[,3] / sum(mapdata[,3])) * 100)

mn_counties <- st_read("C:/Users/boss0/Downloads/shp_bdry_counties_in_minnesota/mn_county_boundaries.shp")

merged_data <- mn_counties %>%
  left_join(mapdata, by = c("CTY_NAME" = "County.Name"))
merged_data <- merged_data[1:87,]

ggplot(data = merged_data) +
  geom_sf(aes(fill = Population_Percentage)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Population Percentage by County in Minnesota (2022)",
       fill = "Population (%)")
# Hennepin county and the adjacent counties have approximately 55% of the entire population of MN.
# > 22.29752684 + 6.34771912 + 9.50046443 + 7.66981588 + 2.66117110 + 1.89831756 + 2.59794900 + 1.76282441
# [1] 54.73579


# Graph of firearms incidents in Minneapolis

firearms_data <- read_excel("C:/Users/boss0/OneDrive/Documents/firearms.xlsx")
mpls_districts <- st_read("C:/Users/boss0/Downloads/Minneapolis_Neighborhoods/Minneapolis_Neighborhoods.shp")

firearms_sf <- firearms_data %>%
  st_as_sf(coords = c("wgsXAnon", "wgsYAnon"), crs = st_crs(mpls_districts))

# Plot using ggplot2
ggplot() +
  geom_sf(data = mpls_districts, fill = "lightblue", color = "black") +  # Plot district boundaries
  geom_sf(data = firearms_sf, color = "red", size = 2) +  # Plot crime data
  theme_minimal() +
  labs(title = "Data from Minneapolis PD Involving Firearms", x = "Longitude", y = "Latitude")


# Graph of 4473 Applications in Minnesota
data_ts %>%
  dygraph( main = "Form 4473 Filed in Minnesota (Monthly)", ylab = "Number of Applications") %>%
  dySeries("Applications", label = "Number of Applications",
           strokeWidth = 2.5, strokeBorderWidth = 1, color = "black") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 1.5, labelsKMB = TRUE) %>%
  dyHighlight(highlightCircleSize = 3,
              highlightSeriesBackgroundAlpha = 0.75,
              hideOnMouseOut = TRUE) %>%
  dyShading(from= "2001-03-01", to = "2001-11-01", color = "lightgrey") %>%
  dyShading(from= "2007-12-01", to = "2009-06-01", color = "lightgrey") %>%
  dyShading(from= "2020-01-31", to = "2023-05-11", color = "lightgrey") %>%
  dyEvent("2001-03-01", "Dot-Com Bubble", labelLoc = "bottom", strokePattern = "solid") %>% ##Different type
  dyEvent("2007-12-01", "Global Financial Crisis", labelLoc = "bottom", strokePattern = "solid") %>% ##Different type
  dyEvent("2020-01-31", "Covid-19", labelLoc = "bottom", strokePattern = "solid") %>% ## Different type
  dyEvent("1999-01-01") %>%
  dyUnzoom() %>%
  dyCrosshair(direction = "vertical")


# Graph of MN unemployment
mnurn_ts %>%
  dygraph( main = "Unemployment Rate in Minnesota (Monthly)", ylab = "Percent (%)") %>%
  dySeries("MNURN", label = "Percent",
           strokeWidth = 3, strokeBorderWidth = 1, color = "black") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 1.5, labelsKMB = TRUE) %>%
  dyHighlight(highlightCircleSize = 3,
              highlightSeriesBackgroundAlpha = 0.75,
              hideOnMouseOut = TRUE) %>%
  dyShading(from= "2001-03-01", to = "2001-11-01", color = "lightgrey") %>%
  dyShading(from= "2007-12-01", to = "2009-06-01", color = "lightgrey") %>%
  dyShading(from= "2020-01-31", to = "2023-05-11", color = "lightgrey") %>%
  dyEvent("2001-03-01", "Dot-Com Bubble", labelLoc = "bottom", strokePattern = "solid") %>% ##Different type
  dyEvent("2007-12-01", "Global Financial Crisis", labelLoc = "bottom", strokePattern = "solid") %>% ##Different type
  dyEvent("2020-01-31", "Covid-19", labelLoc = "bottom", strokePattern = "solid") %>% ## Different type
  dyEvent("1999-01-01") %>%
  dyUnzoom() %>%
  dyCrosshair(direction = "vertical")



# Graph of CPI
cpi_ts %>%
  dygraph(main = "Consumer Price Index for All Urban Consumers: All Items in U.S. City Average (Monthly)", ylab = "Index 1982-1984 = 100") %>%
  dySeries("CPIAUCNS", label = "Index Value",
           strokeWidth = 3, strokeBorderWidth = 1, color = "black") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 1.5, labelsKMB = TRUE) %>%
  dyHighlight(highlightCircleSize = 3,
              highlightSeriesBackgroundAlpha = 0.75,
              hideOnMouseOut = TRUE) %>%
  dyShading(from= "2001-03-01", to = "2001-11-01", color = "lightgrey") %>%
  dyShading(from= "2007-12-01", to = "2009-06-01", color = "lightgrey") %>%
  dyShading(from= "2020-01-31", to = "2023-05-11", color = "lightgrey") %>%
  # dyRangeSelector(dateWindow = c("1999-01-01", "2024-06-01")) %>%
  dyEvent("2001-03-01", "Dot-Com Bubble", labelLoc = "bottom", strokePattern = "solid") %>% ##Different type
  dyEvent("2007-12-01", "Global Financial Crisis", labelLoc = "bottom", strokePattern = "solid") %>% ##Different type
  dyEvent("2020-01-31", "Covid-19", labelLoc = "bottom", strokePattern = "solid") %>% ## Different type
  dyEvent("1999-01-01") %>%
  dyUnzoom() %>%
  dyCrosshair(direction = "vertical")

#### Analysis ####

# Test if data is stationary or not


# Define the function to perform unit root tests and check for stationarity
unitroot_tests <- function(series) {
  if (!require("tseries")) install.packages("tseries", dependencies = TRUE)
  adf_test <- adf.test(series, alternative = "stationary")
  pp_test <- pp.test(series)
  kpss_test <- kpss.test(series, null = "Level", lshort = TRUE)
  
  data.frame(
    Test = c("ADF Test", "PP Test", "KPSS Test"),
    Lag_order = c(adf_test$parameter,
                  pp_test$parameter,
                  kpss_test$parameter),
    Statistic = c(adf_test$statistic,
                  pp_test$statistic,
                  kpss_test$statistic),
    Stationary_P_Value = c(adf_test$p.value, 
                           pp_test$p.value, 
                           kpss_test$p.value),
    Stationarity = c(ifelse(adf_test$p.value < 0.05, "Stationary", "Non-Stationary"),
                     ifelse(pp_test$p.value < 0.05, "Stationary", "Non-Stationary"),
                     ifelse(kpss_test$p.value > 0.05, "Stationary", "Non-Stationary")))
}


# Form 4473 Applications
unitroot_tests(data[,"Applications"])

# Data appears stationary except for KPSS test

# MN unemployment Rate
unitroot_tests(MNURN)

# MN unemployment data is non-stationary


# CPI
unitroot_tests(CPIAUCNS)

# CPI data is non-stationary

# Check if differncing data results in the data becoming stationary

unitroot_tests(diff(combined_ts[,"apps"]))

# Form 4473 data appears stationary 

unitroot_tests(diff(combined_ts[,"unemploy"]))

# MN unemployment data is stationary

unitroot_tests(diff(combined[,'cpi']))

# CPI appears to be stationary but fails KPSS test

# Given that all of the data exhibits stationary qualities we will now try modeling

# ARIMA Model

arima_model <- auto.arima(data_train[,"apps"], d = 1, D = 0, seasonal = TRUE)
summary(arima_model)

# auto regressive 1 and 2 along with seasonal auto regressive 1 are significant

arima_forecast <- forecast(arima_model, h = 6)


# Transfer Model
exog_train <- data_train[,2:3]
exog_test <- data_test[, 2:3]


transfer_model <- auto.arima(data_train[,"apps"], d = 1, max.D = 0, seasonal = TRUE, xreg = exog_train)
summary(transfer_model)

# auto regressive 1 and 2 along with seasonal auto regressive 1 are significant; exogenous variables are not significant

transfer_forecast <- forecast(transfer_model, h = 6, xreg = exog_test)

# VAR Model
lag_selection <- VARselect(data_train, lag.max = 24, type = "both")
optimal_lag <- lag_selection$selection["AIC(n)"]

var_model <- VAR(data_train, p = optimal_lag, type = "both")
var_forecast <- forecast(var_model, h = 6)

# Var Analysis
summary(var_model)
# Significant model that has an adjusted R-squared value of 0.9265; very few roots are close to 1.

irf_results <- irf(var_model, n.ahead = 6, boot = TRUE, runs=1000)
plot(irf_results)
# There appears to be a response, an increase, in the number of applications when a shock happens to unemployment in the second through fourth period. [t+2 - t+4]

# Granger causality CPI
causality(var_model, cause = "cpi")

# CPI does Granger cause apps and/or unemploy at 0.05 level
# CPI has no instantaneous causality

# Granger causality unemployment rate
causality(var_model, cause = "unemploy")

# unemploy does Granger cause apps and/or CPI at 0.001 level
# unemploy does have instantaneous causality at 0.05 level


# Granger causality form 4473 applications
causality(var_model, cause = "apps")

# apps does Granger cause CPI and/or unemploy at 0.001 level
# apps does have instantaneous causality at 0.05 level

# Accuracy

arima_accuracy <- accuracy(arima_forecast, data_test_y)
transfer_accuracy <- accuracy(transfer_forecast, data_test_y)
var_accuracy <- accuracy(var_forecast$forecast$apps, data_test_y)


accuracy_summary <- data.frame(
  Model = c("ARIMA", 
            "Transfer", 
            "VAR"),
  
  RMSE = c(arima_accuracy[1, "RMSE"], 
           transfer_accuracy[1, "RMSE"], 
           var_accuracy[1, "RMSE"]),
  
  MAE = c(arima_accuracy[1, "MAE"], 
          transfer_accuracy[1, "MAE"], 
          var_accuracy[1, "MAE"])
)
# Print the summary table using kableExtra
kable(accuracy_summary, 
      caption = "Forecast Accuracy Metrics") %>%
  kable_styling(bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed"), 
                full_width = F)

# VAR model appears to best mirror the data.


# Create data frames for each forecast
arima_forecast_df <- data.frame(Date = as.Date(time(arima_forecast$mean)), 
                                Forecast = as.numeric(arima_forecast$mean), 
                                Model = "ARIMA")
transfer_forecast_df <- data.frame(Date = as.Date(time(transfer_forecast$mean)), 
                                   Forecast = as.numeric(transfer_forecast$mean), 
                                   Model = "ARIMA with Exogenous")
var_forecast_df <- data.frame(Date = as.Date(time(var_forecast$forecast$apps$mean)),
                              Forecast = as.numeric(var_forecast$forecast$apps$mean), 
                              Model = "VAR")

# Combine all forecast data frames
combined_forecasts <- bind_rows(arima_forecast_df, transfer_forecast_df, var_forecast_df)

# Create actual values data frame
actual_values_df <- data.frame(Date = as.Date(time(data_test_y)), 
                               Actual = as.numeric(data_test_y))

# Plotting using ggplot2
ggplot() +
  geom_line(data = actual_values_df, 
            aes(x = Date, y = Actual), 
            color = "black", 
            size = 1) +
  geom_line(data = combined_forecasts, 
            aes(x = Date, y = Forecast, color = Model), 
            size = 1) +
  labs(title = "Comparison of Forecast Models for Form 4473 Applications in MN", 
       x = "Date", 
       y = "Monthy Applications") +
  scale_color_manual(values = c("ARIMA" = "blue", 
                                "ARIMA with Exogenous" = "red", 
                                "VAR" = "green")) +
  theme_minimal()


# MPLS PD Firearms data

mplspd <- read.csv("C:/Users/boss0/OneDrive/Documents/mplspd_firearms.csv")
mplspd$Date <- as.Date(with(mplspd, paste(Year, Month, 1, sep = "-")), "%Y-%m-%d")
mplspd_ts <- ts(mplspd, start = c(2019, 1), frequency = 12)
mplspd_ts <- mplspd_ts[,3]
mplspd <- mplspd[,3:4]

# Graph mpls PD


mplspd %>%
  dygraph( main = "Inicndents with Firearms in Minneapolis", ylab = "Number of Incidents") %>%
  dySeries("Observations", label = "Number of Incidents",
           strokeWidth = 2.5, strokeBorderWidth = 1, color = "black") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyOptions(drawPoints = TRUE, pointSize = 1.5, labelsKMB = TRUE) %>%
  dyHighlight(highlightCircleSize = 3,
              highlightSeriesBackgroundAlpha = 0.75,
              hideOnMouseOut = TRUE) %>%
  dyShading(from= "2020-01-31", to = "2023-05-11", color = "lightgrey") %>%
  dyEvent("2020-01-31", "Covid-19", labelLoc = "bottom", strokePattern = "solid") %>% ## Different type
  dyEvent("1999-01-01") %>%
  dyUnzoom() %>%
  dyCrosshair(direction = "vertical")


# Combine data

newcomb_ts <- cbind(data_ts, mnurn_ts, mplspd_ts)
newcomb_ts <- na.omit(newcomb_ts)
colnames(newcomb_ts) <- c("apps", "mnunemploy", "obs")


# ARIMA
narima_model <- auto.arima(newcomb_ts[,'apps'], d = 1, D = 0, seasonal = TRUE)
summary(narima_model)

narima_forecast <- forecast(narima_model, h = 6)


# Transfer Function
data_train <- window(newcomb_ts, end = c(2023, 12))
data_test <- window(newcomb_ts, start = c(2024, 1))
data_test_y <- data_test[,1]
exog_train <- data_train[,2:3]
exog_test <- data_test[,2:3]

ntransfer_model <- auto.arima(data_train[,"apps"], d = 1, max.D = 0, seasonal = TRUE, xreg = exog_train)
summary(ntransfer_model)

# seasonal auto regressive 1 is significant; MPLS PD incidents are significant

ntransfer_forecast <- forecast(ntransfer_model, h = 6, xreg = exog_test)

# VAR Model
lag_selection <- VARselect(data_train, lag.max = 24, type = "both")
optimal_lag <- lag_selection$selection["AIC(n)"]

nvar_model <- VAR(data_train, p = optimal_lag, type = "both")
nvar_forecast <- forecast(nvar_model, h = 6)

# Var Analysis
summary(nvar_model)
# Significant model that has an adjusted R-squared value of 0.871; very few roots are close to 1.

irf_results <- irf(nvar_model, n.ahead = 6, boot = TRUE, runs=1000)
plot(irf_results)

# Granger Causality
causality(nvar_model, cause = "obs")


# MPLS PD OBS has Granger Causalitty at the 0.1 level.

causality(nvar_model, cause = "mnunemploy")


# Unemployment rate has Granger causality at the 0.01 level

causality(nvar_model, cause = "apps")

# Applications has Granger causality at the 0.05 level.
