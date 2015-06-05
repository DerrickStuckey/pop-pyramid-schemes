# Compare S&P 500 P/E Ratio with population demographics

sp_500_pe <- read.csv("./sp_500_pe.csv")
us_pop_5year <- read.csv("./us_pop_5year.csv")

# round each year to the nearest 5 year mark
sp_500_pe$half_decade <- round(sp_500_pe$Year/5 + 0.01)*5

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
pe_pop_merged <- merge(sp_500_pe, us_age_props, by.x="half_decade", by.y="Year")

# compute a simple linear regression fit
linear_fit <- lm(PE_Ratio ~ X20_34 + X35_49 + X50_64 + over_64, data=pe_pop_merged)
summary(linear_fit)

# define smoothed log-transform and un-transform
log_transform <- function(x) {log(x+1)}
unlog_transform <- function(x) {exp(x)-1}

# compute smoothed log-transformed P/E Ratios
pe_pop_merged$log_PE <- log_transform(pe_pop_merged$PE_Ratio)

# plot P/E Ratio histograms with and without log-transform
hist(pe_pop_merged$PE_Ratio, main="P/E Ratio Distribution",
     xlab="P/E Ratio", ylab="Probability",breaks=10,prob=TRUE)
curve(dnorm(x,mean=mean(pe_pop_merged$PE_Ratio),
            sd=sd(pe_pop_merged$PE_Ratio)),col="blue",add=TRUE)
hist(pe_pop_merged$log_PE, main="Log-Transformed P/E Ratio Distribution",
     xlab="Log-Transformed P/E Ratio", ylab="Probability",breaks=10,prob=TRUE)
curve(dnorm(x,mean=mean(pe_pop_merged$log_PE),
            sd=sd(pe_pop_merged$log_PE)),col="blue",add=TRUE)

# try with smoothed log-transformed P/E ratio var
log_fit <- lm(log_PE ~ X20_34 + X35_49 + X50_64 + over_64, data=pe_pop_merged)
summary(log_fit)

# historical predictions for linear fit model
preds <- predict(linear_fit, newdata=pe_pop_merged)
names(preds) <- pe_pop_merged$Year

# historical predictions for log-transformed model
preds_logmod <- unlog_transform(predict(log_fit, newdata=pe_pop_merged))
names(preds_logmod) <- pe_pop_merged$Year

# predict the future (linear fit)
future_age_props <- us_age_props[us_age_props$Year >= 2015,]
future_pe_preds <- predict(linear_fit, newdata=future_age_props)
names(future_pe_preds) <- future_age_props$Year
future_pe_preds

# predict the future (log-transformed model)
future_log_PE_preds <- predict(log_fit, newdata=future_age_props)
future_pe_preds_logmod <- unlog_transform(future_log_PE_preds)
names(future_pe_preds_logmod) <- future_age_props$Year
future_pe_preds_logmod

# plot the data and results
plot(x=pe_pop_merged$Year, y=pe_pop_merged$PE_Ratio, main="P/E Ratio over Time",
     xlab="Year",ylab="P/E Ratio")

plot(x=pe_pop_merged$Year, y=pe_pop_merged$log_PE, main="Log-Transformed P/E Ratio over Time",
     xlab="Year",ylab="Log(P/E Ratio)")

# plot predictor variables over time
plot(x=pe_pop_merged$Year, y=pe_pop_merged$under_20,ylim=c(0.0,0.5),col="1",
     xlab="Year",ylab="Proportion")
points(x=pe_pop_merged$Year, y=pe_pop_merged$X20_34,col="2")
points(x=pe_pop_merged$Year, y=pe_pop_merged$X35_49,col="3")
points(x=pe_pop_merged$Year, y=pe_pop_merged$X50_64,col="4")
points(x=pe_pop_merged$Year, y=pe_pop_merged$over_64,col="5")
legend("topright", legend = c("under 20","20-34","35-49","50-64","65+"), pch=1, col=1:5,horiz=TRUE)

# plot predictions
plot(y=preds_logmod,x=pe_pop_merged$Year, main="Predicted PE (Log Model)",
     xlab="Year",ylab="Predicted P/E Ratio")

plot(y=preds,x=pe_pop_merged$Year, main="Predicted PE (Base Model)",
     xlab="Year",ylab="Predicted P/E Ratio")

# predictions, actuals vs time
plot(y=preds_logmod,x=pe_pop_merged$Year, main="Predicted and Actual P/E Ratios",
     xlab="Year",ylab="P/E Ratio",col="red")
points(x=pe_pop_merged$Year, y=pe_pop_merged$PE_Ratio,col="blue")
legend("bottomright", legend = c("predicted","actual"), pch=1, col=c("red","blue"),horiz=FALSE)

plot(x=pe_pop_merged$PE_Ratio,y=preds_logmod, main="Predicted vs Actual (Log Model)",
     xlab="Actual P/E Ratio", ylab="Predicted P/E Ratio")

# plot residuals
plot(x=pe_pop_merged$Year, y=linear_fit$residuals, main="Base Fit Residuals",
     xlab="Year",ylab="Prediction Error")
plot(x=pe_pop_merged$Year, y=log_fit$residuals, main="Log-Transformed Fit Residuals",
     xlab="Year",ylab="Prediction Error")

# middle-age only model
midage_model <- lm(log_PE ~ X35_49, data=pe_pop_merged)
summary(midage_model)

# historical predictions for middle-age model
preds_midage <- unlog_transform(predict(midage_model, newdata=pe_pop_merged))
names(preds_midage) <- pe_pop_merged$Year

# predict the future (middle-age model)
future_midage_PE_preds <- predict(midage_model, newdata=future_age_props)
future_PE_preds_midage <- unlog_transform(future_midage_PE_preds)
names(future_PE_preds_midage) <- future_age_props$Year
future_PE_preds_midage

# middle-age model predictions, actuals vs time
plot(y=preds_midage,x=pe_pop_merged$Year, main="Predicted and Actual P/E Ratios (35-49 only)",
     xlab="Year",ylab="P/E Ratio",col="red")
points(x=pe_pop_merged$Year, y=pe_pop_merged$PE_Ratio,col="blue")
legend("bottomright", legend = c("predicted (35-49)","actual"), pch=1, col=c("red","blue"),horiz=FALSE)

#individual predictor variable correlations
cor(pe_pop_merged$PE_Ratio, pe_pop_merged$under_20)
cor(pe_pop_merged$PE_Ratio, pe_pop_merged$X20_34)
cor(pe_pop_merged$PE_Ratio, pe_pop_merged$X35_49)
cor(pe_pop_merged$PE_Ratio, pe_pop_merged$X50_64)
cor(pe_pop_merged$PE_Ratio, pe_pop_merged$over_64)


