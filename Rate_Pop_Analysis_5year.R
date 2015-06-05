# Compare US 10-year interest rates with population demographics

us_10year_rates <- read.csv("./us_10year_rates.csv")
us_pop_5year <- read.csv("./us_pop_5year.csv")

# round each year to the nearest 5 year mark
us_10year_rates$half_decade <- round(us_10year_rates$Year/5 + 0.01)*5

# reshape us_pop_5year to 'wide' format
library(reshape2)
us_pop_wide <- dcast(us_pop_5year, Year ~ Age, value.var="Value",fun.aggregate=sum,fill=0)

# calculate values for coarser age groups
under_20 <- us_pop_wide$" 0-4" + us_pop_wide$" 5-9" + us_pop_wide$" 10-14" + us_pop_wide$" 15-19"
X20_34 <- us_pop_wide$" 20-24" + us_pop_wide$" 25-29" + us_pop_wide$" 30-34"
X35_49 <- us_pop_wide$" 35-39" + us_pop_wide$" 40-44" + us_pop_wide$" 45-49"
X50_64 <- us_pop_wide$" 50-54" + us_pop_wide$" 55-59" + us_pop_wide$" 60-64"
over_64 <- us_pop_wide$" 65-69" + us_pop_wide$" 70-74" + us_pop_wide$" 75-79" + 
           us_pop_wide$" 80-84" + us_pop_wide$" 80+" + us_pop_wide$" 85-89" + 
           us_pop_wide$" 90-94" + us_pop_wide$" 95-99" + us_pop_wide$" 100+"
total <- under_20 + X20_34 + X35_49 + X50_64 + over_64

# construct dataframe for population buckets
us_age_props <- data.frame("Year"=us_pop_wide$Year, "under_20"=under_20/total, "X20_34"=X20_34/total, 
                           "X35_49"=X35_49/total, "X50_64"=X50_64/total, "over_64"=over_64/total)

# remove any N/A rows
us_age_props <- us_age_props[!is.na(us_age_props$Year),]

# merge the two datasets 
rates_pop_merged <- merge(us_10year_rates, us_age_props, by.x="half_decade", by.y="Year")

# compute a simple linear regression fit
linear_fit <- lm(interest_rate ~ X20_34 + X35_49 + X50_64 + over_64, data=rates_pop_merged)
summary(linear_fit)

# define smoothed log-transform and un-transform
log_transform <- function(x) {log(x+1)}
unlog_transform <- function(x) {exp(x)-1}

# compute smoothed log-transformed rates
rates_pop_merged$log_rate <- log_transform(rates_pop_merged$interest_rate)

# plot interest rate histograms with and without log-transform
hist(rates_pop_merged$interest_rate, main="Interest Rate Distribution",
     xlab="10-Year Treasury Rate", ylab="Probability",breaks=10,prob=TRUE)
curve(dnorm(x,mean=mean(rates_pop_merged$interest_rate),
            sd=sd(rates_pop_merged$interest_rate)),col="blue",add=TRUE)
hist(rates_pop_merged$log_rate, main="Log-Transformed Interest Rate Distribution",
     xlab="Log-Transformed 10-Year Treasury Rate", ylab="Probability",breaks=10,prob=TRUE)
curve(dnorm(x,mean=mean(rates_pop_merged$log_rate),
            sd=sd(rates_pop_merged$log_rate)),col="blue",add=TRUE)

# try with smoothed log-transformed interest rate var
log_fit <- lm(log_rate ~ X20_34 + X35_49 + X50_64 + over_64, data=rates_pop_merged)
summary(log_fit)

# historical predictions for linear fit model
preds <- predict(linear_fit, newdata=rates_pop_merged)
names(preds) <- rates_pop_merged$Year

# historical predictions for log-transformed model
preds_logmod <- unlog_transform(predict(log_fit, newdata=rates_pop_merged))
names(preds_logmod) <- rates_pop_merged$Year

# predict the future (linear fit)
future_age_props <- us_age_props[us_age_props$Year >= 2015,]
future_rate_preds <- predict(linear_fit, newdata=future_age_props)
names(future_rate_preds) <- future_age_props$Year

# predict the future (log-transformed model)
future_log_rate_preds <- predict(log_fit, newdata=future_age_props)
future_rate_preds_logmod <- unlog_transform(future_log_rate_preds)
names(future_rate_preds_logmod) <- future_age_props$Year

# plot the data and results
plot(x=rates_pop_merged$Year, y=rates_pop_merged$interest_rate, main="Interest Rates over Time",
     xlab="Year",ylab="10-Year Treasury Rate")

plot(x=rates_pop_merged$Year, y=rates_pop_merged$log_rate, main="Log-Transformed Rates over Time",
     xlab="Year",ylab="Log(10-Year Treasury Rate)")

# plot predictor variables over time
plot(x=rates_pop_merged$Year, y=rates_pop_merged$under_20,ylim=c(0.0,0.5),col="1",
     xlab="Year",ylab="Proportion")
points(x=rates_pop_merged$Year, y=rates_pop_merged$X20_34,col="2")
points(x=rates_pop_merged$Year, y=rates_pop_merged$X35_49,col="3")
points(x=rates_pop_merged$Year, y=rates_pop_merged$X50_64,col="4")
points(x=rates_pop_merged$Year, y=rates_pop_merged$over_64,col="5")
legend("topright", legend = c("under 20","20-34","35-49","50-64","65+"), pch=1, col=1:5,horiz=TRUE)

# plot predictions
plot(y=preds_logmod,x=rates_pop_merged$Year, main="Predicted Rates (Log Model)",
     xlab="Year",ylab="Predicted Rate")

plot(y=preds,x=rates_pop_merged$Year, main="Predicted Rates (Base Model)",
     xlab="Year",ylab="Predicted Rate")

# predictions, actuals vs time
plot(y=preds_logmod,x=rates_pop_merged$Year, main="Predicted and Actual Rates",
     xlab="Year",ylab="10-year Treasury Rate",col="red")
points(x=rates_pop_merged$Year, y=rates_pop_merged$interest_rate,col="blue")
legend("topright", legend = c("predicted","actual"), pch=1, col=c("red","blue"),horiz=FALSE)

plot(x=rates_pop_merged$interest_rate,y=preds_logmod, main="Predicted vs Actual (Log Model)",
     xlab="Actual 10-Year Treasury Rate", ylab="Predicted 10-Year Treasury Rate")

# plot residuals
plot(x=rates_pop_merged$Year, y=linear_fit$residuals, main="Base Fit Residuals",
     xlab="Year",ylab="Prediction Error")
plot(x=rates_pop_merged$Year, y=log_fit$residuals, main="Log-Transformed Fit Residuals",
     xlab="Year",ylab="Prediction Error")

#individual predictor variable correlations
cor(rates_pop_merged$interest_rate, rates_pop_merged$under_20)
cor(rates_pop_merged$interest_rate, rates_pop_merged$X20_34)
cor(rates_pop_merged$interest_rate, rates_pop_merged$X35_49)
cor(rates_pop_merged$interest_rate, rates_pop_merged$X50_64)
cor(rates_pop_merged$interest_rate, rates_pop_merged$over_64)

