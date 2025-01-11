#Final Project
#topic:Global gold rate price
#Group:6

#STEP:1
#Load Necessary Libraries

install.packages("fabletools")
library(fabletools)
library(fpp3)

#Step-2
# The data consists of the  gold rate from 1979 to Present Date.  
# Load and Preprocess the Dataset

goldrate.df <- read.csv("C:/Users/HP/Desktop/goldrate.csv")

goldrate.df <- goldrate.df %>% 
  dplyr::mutate(Quarter = as.Date(Date, "%y")) %>% 
  dplyr::mutate(Quarter = yearquarter(Date)) %>%
  dplyr::select(Quarter, USD)

#STEP-3
#Convert Data to a Time Series Object
USD.tb <-  goldrate.df %>%
  as_tsibble(index = Quarter)

#STEP-4
#Split the Data into Training and Validation Sets

USD.tb.train <- USD.tb %>%
  filter(year(Quarter) <= 2017)

#STEP-5
#Visualize the Training Data
# Create a time series plot.
USD.tb.train %>% autoplot(USD)

# Create a seasonal plot.
USD.tb.train %>% gg_season(USD)

# Create a sub-series plot.
USD.tb.train %>% gg_subseries(USD)

## The time series indicates a clear long-term growth trend, 
#interrupted by periods of sharp volatility, likely reflecting macroeconomic events
# The seasonal plot does not reveal a strong, consistent seasonal component


## STEP-6
# Fit a Linear Regression Model

fit_USD <- USD.tb.train %>%
  model(TSLM(USD ~ trend() + season()))

fit_USD %>% report()

#STEP-7
# Visualize Regression Model Fit

fit_USD %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = USD, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

#STEP-8
#Forecast Using the Linear Regression Model
# which shows the forecast and prediction interval.
fc_USD <- fit_USD %>%
  forecast(h = 12)


fc_USD %>%
  autoplot(USD.tb.train)

#Generate and visualize forecasts for the next 12 quarters using
#the regression model

#STEP-9
## Fit ARIMA Models Iteratively
USD.tb.train %>% features(USD, unitroot_nsdiffs) 
USD.tb.train %>% features(USD, unitroot_ndiffs) 

#Check the level of differencing needed to make the data stationary.

# We start by fitting the model 

ARIMA0.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(0,1,0) + PDQ(0,0,0)))
report(ARIMA0.fit) # AIC=1727.62

augment(ARIMA0.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

#ARIMA1.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(0,0,0) + PDQ(0,1,0)))
#report(ARIMA1.fit) # AIC=1914.54

#augment(ARIMA1.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

ARIMA2.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(0,1,2) + PDQ(0,0,0)))
report(ARIMA2.fit) # AIC=1722.36
augment(ARIMA2.fit) %>% gg_tsdisplay(.resid, plot_type='partial')

ARIMA3.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(3,1,2) + PDQ(0,0,0)))
report(ARIMA3.fit) # AIC=1721.14
augment(ARIMA3.fit) %>% gg_tsdisplay(.resid, plot_type='partial')

ARIMA4.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(2,1,0) + PDQ(0,0,0)))
report(ARIMA4.fit) # AIC=1719.65
augment(ARIMA4.fit) %>% gg_tsdisplay(.resid, plot_type='partial')

ARIMA5.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(1,1,4) + PDQ(0,0,0)))
report(ARIMA5.fit) # AIC=1695.58
augment(ARIMA5.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

ARIMA6.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(2,1,4) + PDQ(0,1,0)))
report(ARIMA6.fit) # AIC=1690.96
augment(ARIMA6.fit) %>% gg_tsdisplay(.resid, plot_type='partial')

ARIMA7.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(2,1,0) + PDQ(1,0,0)))
report(ARIMA7.fit) # AIC=1719.75
augment(ARIMA7.fit) %>% gg_tsdisplay(.resid, plot_type='partial')

ARIMA8.fit <- USD.tb.train %>% model(ARIMA(USD ~ pdq(2,1,2) + PDQ(1,0,0)))
report(ARIMA8.fit) # AIC=1719.16
augment(ARIMA8.fit) %>% gg_tsdisplay(.resid, plot_type='partial')

# Model 6 has the lowest AIC
# Is there anything left in the residuals
augment(ARIMA6.fit) %>% gg_tsdisplay(.resid, plot_type='partial') 

#Compare ARIMA Model Fits
ARIMA6.fit %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = USD, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval.
fc_USD <- ARIMA6.fit %>%
  forecast(h = 12)

fc_USD %>%
  autoplot(USD.tb.train)

## Use auto.arima() 
ARIMAauto.fit <- USD.tb.train %>% model(ARIMA(USD))
report(ARIMAauto.fit) # AIC=1718.18

#STEP-10
# Create a plot to compare the fit to the training data.
ARIMAauto.fit %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = USD, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval.
fc_USD <- ARIMAauto.fit %>%
  forecast(h = 12)

fc_USD %>%
  autoplot(USD.tb.train)


#STEP-11
## Fit and Evaluate ETS Models 
# Fit your recommended ETS model(s).
ets_manual1 <- USD.tb.train %>% model(ETS(USD ~ error("A") + trend("A") + season("A")))
report(ets_manual1) # AIC = 2100.215

ets_manual2 <- USD.tb.train %>% model(ETS(USD ~ error("A") + trend("A") + season("M")))
report(ets_manual2) # AIC = 2104.076

ets_manual3 <- USD.tb.train %>% model(ETS(USD ~ error("M") + trend("A") + season("A")))
report(ets_manual3) # AIC = 1996.625

ets_manual4 <- USD.tb.train %>% model(ETS(USD ~ error("M") + trend("A") + season("M")))
report(ets_manual4) # AIC = 1996.003
# ETS model 4 has the lowest AIC


#Fitting Multiple Models to Training Data
ets_manual4 %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = USD, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))


# Create a plot to show the forecast and prediction interval.
fc_USD <- ets_manual4 %>%
  forecast(h = 12)

fc_USD %>%
  autoplot(USD.tb.train)

#STEP-12
## Fit ETS Models Automatically
ets_auto <- USD.tb.train %>% model(ETS(USD))
report(ets_auto) # AIC = 1973.621


# Create a plot to compare the fit to the training data. 
ets_auto %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = USD, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

# Create a plot to show the forecast and prediction interval.
fc_USD <- ets_auto %>%
  forecast(h = 12)

fc_USD %>%
  autoplot(USD.tb.train)

#STEP-13
## Assess the predictive accuracy of your five models in cross-validation 
all.models.fit <- USD.tb.train %>% 
  model(arima_manual = ARIMA(USD ~ pdq(2,1,4) + PDQ(0,1,0)),
        arima_auto = ARIMA(USD),
        ts_reg = TSLM(USD ~ trend() + season()),
        ets_manual = ETS(USD ~ error("A") + trend("A") + season("A")),
        ets_auto = ETS(USD),
        naive = NAIVE(USD),
        snaive = SNAIVE(USD))

all.models.pred <- all.models.fit %>% forecast(h = 12)
 
#STEP-14
#Evaluating Model Accuracy
all.models.pred %>% accuracy(USD.tb) %>% arrange(MAPE, decreasing = TRUE)


#STEP-15
#Ensemble the models 
all.models.fit <- USD.tb.train %>% 
  model(arima_manual = ARIMA(USD ~ pdq(2,1,4) + PDQ(0,1,0)),
        arima_auto = ARIMA(USD),
        ts_reg = TSLM(USD ~ trend() + season()),
        ets_manual = ETS(USD ~ error("A") + trend("A") + season("A")),
        ets_auto = ETS(USD),
        naive = NAIVE(USD),
        snaive = SNAIVE(USD),
        combination = combination_model(
        TSLM(USD ~ trend() + season()),
        ETS(USD ~ error("M") + trend("A") + season("N")),
        ARIMA(USD ~ pdq(1,1,0) + PDQ(1,0,0)),
        cmbn_args = list(weights = "equal"))
      )

all.models.pred <- all.models.fit %>% forecast(h = 12)

all.models.pred %>% accuracy(USD.tb) %>% arrange(MAPE, decreasing = TRUE)

#STEP-16
#  recommend Model

# ETS model with additive error, trend, and season
best_model <- USD.tb %>% model(ARIMA(USD ~ pdq(2,1,4) + PDQ(0,1,0)))

# Create a plot to compare the fit # Create a plot to compare the fit # Create a plot to compare the fit of your recommended model to the training and validation data. 
best_model %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = USD, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = "Series"))

#STEP-17
# Forecasting 3-Year Predictions
fc_USD <- best_model %>%
  forecast(h = 12)

fc_USD %>%
  autoplot(USD.tb)

library(fpp3)
#STEP-18
#Advance crossvalidation
# Stretch the tsibble for cross-validation
USD_crossval <- USD.tb %>%
  stretch_tsibble(.init = 40, .step = 1) %>%
  relocate(Quarter, .id)

# View the transformed data (optional)
View(USD_crossval)


# To assess the accuracy of the one-quarter ahead forecast
one_step_accuracy <- USD_crossval %>%
  model(
    ETS = ETS(USD),
    ARIMA = ARIMA(USD),
    TSLM = TSLM(USD ~ trend() + season())
  ) %>%
  forecast(h = 1) %>%
  accuracy(USD.tb)

# Print one-step ahead forecast accuracy
print(one_step_accuracy)

# To assess the accuracy over multiple forecast horizons (up to two years)
USD_fc <- USD_crossval %>%
  model(
    ETS = ETS(USD),
    ARIMA = ARIMA(USD),
    TSLM = TSLM(USD ~ trend() + season()),
    combination = combination_model(
      TSLM(USD ~ trend() + season()),
      ETS(USD),
      ARIMA(USD),
      cmbn_args = list(weights = "inv_var")
    )
  ) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%  # Assign forecast horizons
  ungroup() %>%
  as_fable(response = "USD", distribution = USD)

# Calculate accuracy for each model and horizon
multi_horizon_accuracy <- USD_fc %>%
  accuracy(USD.tb, by = c("h", ".model"))

# Plot RMSE by forecast horizon and model
multi_horizon_accuracy %>%
  ggplot(aes(x = h, y = RMSE, color = .model)) +
  geom_point() +
  geom_line() +
  labs(
    title = "RMSE by Forecast Horizon and Model",
    x = "Forecast Horizon (Quarters)",
    y = "RMSE",
    color = "Model"
  ) +
  theme_minimal()

# Return to one-step ahead forecast accuracy for final evaluation
one_step_accuracy_recheck <- USD_crossval %>%
  model(
    ETS = ETS(USD),
    ARIMA = ARIMA(USD),
    TSLM = TSLM(USD ~ trend() + season())
  ) %>%
  forecast(h = 1) %>%
  accuracy(USD.tb)

# Print rechecked one-step ahead accuracy
print(one_step_accuracy_recheck)






