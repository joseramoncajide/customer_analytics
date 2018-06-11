##########################################################################
# Jose Cajide - @jrcajide
# Customer Analytics: Time series manipulation, analysis and forecasting
##########################################################################

rm(list=ls()) 
cat("\014")

list.of.packages <- c("tidyverse", "forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(forecast)

#----------------------------------------------------------------------------
# Load data
#----------------------------------------------------------------------------

customers.df <- read_csv('./series_temporales/customers_by_cohort.csv')

# customers.df <- tail(customers.df, nrow(customers.df)-1)
# 
# # customers.df <- head(customers.df, nrow(customers.df)-1)
# # 
# # customers.df %>% write_csv('./series_temporales/customers_by_cohort.csv')

(customers <- customers.df$client_id)


#----------------------------------------------------------------------------
# Time series with base R
#----------------------------------------------------------------------------

(customers.ts <- ts(data = customers,frequency = 12, start = c(2010, 09)))
start(customers.ts)
end(customers.ts)
frequency(customers.ts)

summary(customers.ts)

plot(customers.ts, main =  "customers")
autoplot(customers.ts) + labs(title = "customers")


# Differentiation
(customers_diff.ts <- diff(customers.ts,1))
plot(customers_diff.ts, main="customers")

(customers_diff.ts <- diff(customers.ts,12))
plot(customers_diff.ts, main="customers con una diferencia estacional")


# Average method
customers.mean <- meanf(customers.ts, h = 12)
summary(customers.mean)
plot(customers.mean)
autoplot(customers.mean)


# Naive
customers.naive <- naive(customers.ts, 12)
summary(customers.naive)
plot(customers.naive)
autoplot(customers.naive)


# Simple Moving Average
library(smooth)
# Predict customers would be for next 12 weeks based on the the last 12 weeks
# Order 2
fit2<-sma(customers.ts, order = 2, h = 12, holdout = T, level = .95)
round(fit2$forecast,0)
plot(forecast(fit2))

# Automatic
fitX<-sma(customers.ts, h = 12, holdout = T, level = .95, ic = 'AIC')
round(fitX$forecast,0)
plot(forecast(fitX))




# ts components -----------------------------------------------------------
# the trend is the long-term increase or decrease in the data
# the seasonal pattern occurs when a time series is affected by seasonal factors such as the time of the year or the day of the week
# the cycle occurs when the data exhibit rises and falls that are not of a fixed period
#----------------------------------------------------------------------------

customers.decomp <- decompose(customers.ts)

plot(customers.decomp)

ggseasonplot(customers.ts)

ggseasonplot(customers.ts , polar = TRUE)

ggsubseriesplot(customers.ts)

# Seasonally adjusted or deseasonalized data ------------------------------
# Obteining a seasonal stationary ts
customers_des.ts  <- customers.ts-customers.decomp$seasonal
plot(customers.ts,main="customers. Serie normal Vs desestacionalizada")
lines(customers_des.ts, col='red')



# Forecasting -------------------------------------------------------------

# Holt-Winters Forecasting or Triple Exponential Smoothing ----------------
# https://en.wikipedia.org/wiki/Exponential_smoothing#Triple_exponential_smoothing

fit_hw <- HoltWinters(customers.ts)
plot(fit_hw)

forecast_out_sample <- forecast(fit_hw, h=12)
round(print(forecast_out_sample),2)
plot(forecast_out_sample)

# More on TS --------------------------------------------------------------

monthplot(customers.ts, main="customers Monthly Patterns",
          xlab="Month", ylab="Count", col="darkblue", lwd=2, lty.base=2, lwd.base=1,
          col.base="gray40")

seasonplot(customers.ts, main="customers Seasonal Trends",
           col=rainbow(10), year.labels=TRUE, year.labels.left=TRUE, cex=0.7,
           cex.axis=0.8)



# Identifying possible breakpoints in a time series -----------------------
require(strucchange)
breakpoints(customers.ts ~ 1)
plot(customers.ts, main="customers breakpoints", ylab="customers", col="darkblue", lwd=1.5)
# Plot the line at the optimal breakpoint
lines(breakpoints(customers.ts ~ 1), col="darkgreen")
# Plot a 90% confidence interval
lines(confint(breakpoints(customers.ts ~ 1), level=0.90), col="darkgreen")

