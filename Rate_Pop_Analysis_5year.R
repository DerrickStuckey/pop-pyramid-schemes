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
                           "X35-49"=X35_49/total, "X50-64"=X50_64/total, "over_64"=over_64/total)

# remove any N/A rows
us_age_props <- us_age_props[!is.na(us_age_props$Year),]

# merge the two datasets 
rates_pop_merged <- merge(us_10year_rates, us_age_props, by.x="half_decade", by.y="Year")

# compute a simple linear regression fit
linear_fit <- lm(interest_rate ~ X20_34 + X35.49 + X50.64 + over_64, data=rates_pop_merged)
summary(linear_fit)

# define smoothed log-transform and un-transform
log_transform <- function(x) {log(x+1)}
unlog_transform <- function(x) {exp(x)-1}

# try with smoothed log-transformed interest rate var
rates_pop_merged$log_rate <- log_transform(rates_pop_merged$interest_rate)
log_fit <- lm(log_rate ~ X20_34 + X35.49 + X50.64 + over_64, data=rates_pop_merged)
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

# plot some stuff
plot(rates_pop_merged$interest_rate)
plot(preds_logmod)
plot(x=rates_pop_merged$interest_rate,y=preds_logmod)