## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/vinaymanchundiya/Desktop/MSBA/Time Series/Project/Final")

# Create data frame.
Chickenpox.data <- read.csv("Hungary_Chickenpox_Dataset.csv")

# See the first and last six records of the file.
head(Chickenpox.data)
tail(Chickenpox.data)

## CREATE TIME SERIES DATA SET.
hungary_chickenpox.ts <- ts(Chickenpox.data$Data, 
                   start = c(2005, 1), end = c(2014, 12), freq = 12)

hungary_chickenpox.ts

## PLOT TIME SERIES DATA. 
## Use plot() to plot time series data  
plot(hungary_chickenpox.ts, 
     xlab = "Time", ylab = "Number of Cases", 
     ylim = c(100, 9000), xaxt = 'n',
     main = "Hungary Chicken-Pox Cases")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2005, 2015, 1), labels = format(seq(2005, 2015, 1)))

# Use stl() function to plot times series components(patterns) of the original data. 
chickenpox.stl <- stl(hungary_chickenpox.ts, s.window = "periodic")
autoplot(chickenpox.stl, main = "Hungary Chicken-Pox Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation for different lags.
autocor <- Acf(hungary_chickenpox.ts, lag.max = 12, 
               main = "Autocorrelation for Hungary Chicken-Pox")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

## TEST PREDICTABILITY OF DATASET (Approach 1).

# Use Arima() function to fit AR(1) model for Chicken-Pox Cases.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
chickenpox.ar1<- Arima(hungary_chickenpox.ts, order = c(1,0,0))
summary(chickenpox.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.7802
s.e. <- 0.0572
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}


# Define the numbers of months in the training and validation sets, nTrain and nValid, respectively.
#Training Period from Jan 2005 - Dec 2012 = 96
#Validation Period from Jan 2013 to Dec 2014 = 24
nValid <- 24
nTrain <- length(hungary_chickenpox.ts) - nValid
train.ts <- window(hungary_chickenpox.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(hungary_chickenpox.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))
train.ts
valid.ts

#------------------------------------------------------------------

## Model 1: Regression Model with Seasonality
## FORECAST AND PLOT DATA

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Apply forecast() function to make predictions for ts with seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Number of Cases",
     ylim = c(100, 12500), bty = "l",
     xlim = c(2005, 2016), xaxt = "n",
     main = "Regression Model with Seasonality ", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2005,10500, legend = c("Hungary Chicken-Pox Time Series", "Seasonality Model for Training Data",
                             "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(0, 12000))
lines(c(2015, 2015), c(0, 12000))
text(2008, 11500, "Training")
text(2014, 11500, "Validation")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2012.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 11000, 2014.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2015.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the model with seasonality.
plot(train.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-3000, 3000), bty = "l",
     xlim = c(2005, 2016), xaxt = "n", 
     main = "Residuals for the Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(valid.ts - train.season.pred$mean, col = "brown", lty = 1, lwd=2)


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(-4000, 4000))
lines(c(2015, 2015), c(-4000, 4000))
text(2008, 3000, "Training")
text(2014, 3000, "Validation")
text(2015.5, 3000, "Future")
arrows(2005, 2500, 2012.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 2500, 2014.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 2500, 2015.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#------------------------------------------------------------------

## Model 2: Regression Model with Linear Trend and Seasonality

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Number of Cases", 
     ylim = c(100, 12500), bty = "l",
     xlim = c(2005, 2016), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(2005,10500, legend = c("Hungary Chicken-Pox Time Series", 
                             "Linear Trend and Seasonality Model for Training Data",
                             "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(0, 12000))
lines(c(2015, 2015), c(0, 12000))
text(2008, 11500, "Training")
text(2014, 11500, "Validation")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2012.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 11000, 2014.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2015.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of predictions with linear trend and seasonality.
plot(train.lin.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-3000, 3000), bty = "l",
     xlim = c(2005, 2016), xaxt = "n",
     main = "Residuals for Linear Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(valid.ts - train.lin.season.pred$mean, col = "brown", lty = 1, lwd=2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(-4000, 4000))
lines(c(2015, 2015), c(-4000, 4000))
text(2008, 3000, "Training")
text(2014, 3000, "Validation")
text(2015.5, 3000, "Future")
arrows(2005, 2500, 2012.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 2500, 2014.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 2500, 2015.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#------------------------------------------------------------------

## Model 3: Regression Model with Quadratic Trend and Seasonality

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Number of Cases", 
     ylim = c(100, 12500), bty = "l",
     xlim = c(2005, 2016), xaxt = "n", 
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(2005,10500, legend = c("Hungary Chicken-Pox Time Series", 
                             "Quadratic Trend and Seasonality Model for Training Data",
                             "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(0, 12000))
lines(c(2015, 2015), c(0, 12000))
text(2008, 11500, "Training")
text(2014, 11500, "Validation")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2012.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 11000, 2014.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2015.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of predictions with quadratic trend and seasonality.
plot(train.quad.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-3000, 3000), bty = "l",
     xlim = c(2005, 2016), xaxt = "n",
     main = "Residuals for Quadratic Trend and Seasonality Model", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(valid.ts - train.quad.season.pred$mean, col = "brown", lty = 1, lwd=2)

lines(c(2013, 2013), c(-4000, 4000))
lines(c(2015, 2015), c(-4000, 4000))
text(2008, 3000, "Training")
text(2014, 3000, "Validation")
text(2015.5, 3000, "Future")
arrows(2005, 2500, 2012.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 2500, 2014.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 2500, 2015.9, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
    
#------------------------------------------------------------------

# Model 4 AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Number of Cases", 
     ylim = c(100, 12500), xaxt = "n", 
     bty = "l", xlim = c(2005, 2016), 
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2005,10500, legend = c("Shipment Time Series", 
                              "Auto ARIMA Forecast for Training Period",
                              "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(0, 12000))
lines(c(2015, 2015), c(0, 12000))
text(2008, 11500, "Training")
text(2014, 11500, "Validation")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2012.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 11000, 2014.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2015.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#------------------------------------------------------------------

# Model 5 HOLTS WINTER EXPONENTIAL SMOOTHING.

## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.
### zzz will identify best options and best parameters

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Number of Cases", ylim = c(100, 12500), 
     bty = "l", xlim = c(2005, 2016), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(hungary_chickenpox.ts)
legend(2005,10500, 
       legend = c("Chicken-Pox Time Series", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2013, 2013), c(0, 12000))
lines(c(2015, 2015), c(0, 12000))
text(2008, 11500, "Training")
text(2014, 11500, "Validation")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2012.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2013.1, 11000, 2014.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2015.9, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#------------------------------------------------------------------

## Accuracy Measures
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


#------------------------------------------------------------------

## Entire Dataset
## Model 3: Regression Model with Quadratic Trend and Seasonality for Entire Dataset

# Use tslm() function to create model with quadratic trend and seasonality on the entire dataset.
chickenpox.quad.season <- tslm(hungary_chickenpox.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality equation and associated parameters of entire dataset.
summary(chickenpox.quad.season)

# Apply forecast() function to make predictions for ts with 
# quadratic trend and seasonality data 12 months in 2015.
chickenpox.quad.season.pred <- forecast(chickenpox.quad.season, h = 12, level = 0)
chickenpox.quad.season.pred

# Plot ts data, regression model with quadratic trend and seasonality data, 
# and predictions for 8 future periods i.e. 8 quarters in this case.
plot(chickenpox.quad.season.pred$mean, 
     xlab = "Time", ylab = "Number of Cases", 
     ylim = c(100, 12500), bty = "l",
     xlim = c(2005, 2016), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality and Forecast for Future Periods", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(chickenpox.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(hungary_chickenpox.ts)
legend(2005,10500, legend = c("Hungary Chicken-Pox Time Series", 
                              "Quadratic and Seasonal Trend Model for Entire Data",
                              "Quadratic and Seasonal Forecast for Future 12 months i.e, 2015"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2015, 2015), c(0, 12500))
text(2008, 11500, "Data Set")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2014.8, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2016, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#------------------------------------------------------------------

## Entire Dataset

## Model 4: AUTO ARIMA for Entire Dataset

auto.arima <- auto.arima(hungary_chickenpox.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with auto ARIMA model for the future 12 months. 
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals for entire dataset")

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(hungary_chickenpox.ts, 
     xlab = "Time", ylab = "Number of Cases", 
     ylim = c(100, 12500), xaxt = "n", 
     bty = "l", xlim = c(2005, 2016), lwd = 2,
     main = "Auto ARIMA Model for Entire Dataset") 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2005,10500, legend = c("Hungary Chicken-Pox Time Series", 
                              "Auto ARIMA Forecast", 
                              "Seasonal ARIMA Forecast for 12 Future months of 2015"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

 
# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2015, 2015), c(0, 12500))
text(2008, 11500, "Data Set")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2014.8, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2016, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#------------------------------------------------------------------
# Model 5 HOLTS WINTER EXPONENTIAL SMOOTHING.
## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Amtrak data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
# M,Ad, M represents m and  M is multiplicative error, multiplicative 
# Multiplicative trend and Ad is additive and d is damping factor
HW.ZZZ <- ets(hungary_chickenpox.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Number of Cases", ylim = c(100, 12500), 
     bty = "l", xlim = c(2005, 2016), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2005, 2016, 1), labels = format(seq(2005, 2016, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(hungary_chickenpox.ts)
legend(2005,10500, 
       legend = c("Hungary Chicken-Pox Time Series", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# entire data set and future prediction intervals.
lines(c(2015, 2015), c(0, 12500))
text(2008, 11500, "Data Set")
text(2015.5, 11500, "Future")
arrows(2005, 11000, 2014.8, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2015.1, 11000, 2016, 11000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#------------------------------------------------------------------

# Use accuracy() function to identify common accuracy measures
# (1) Quadratic Trend and Seasonality for entire dataset,
round(accuracy(chickenpox.quad.season.pred$fitted, hungary_chickenpox.ts),3)

# (2) Auto ARIMA Model,
round(accuracy(auto.arima.pred$fitted, hungary_chickenpox.ts), 3)

# (3) Holt's Winter Model,
round(accuracy(HW.ZZZ.pred$fitted, hungary_chickenpox.ts), 3)

# (4) Seasonal Naive,
round(accuracy((snaive(hungary_chickenpox.ts))$fitted,hungary_chickenpox.ts), 3)

# (5) Naive,
round(accuracy((naive(hungary_chickenpox.ts))$fitted,hungary_chickenpox.ts), 3)



