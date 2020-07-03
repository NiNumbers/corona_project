# load libraries, functions
load("resplot.rda")

# load and prepare data
load("corona_app.RData")
str(app_data)

# ----- percentage of Users -----
# Assumptions: 
smartph <- 0.78  # 80% of all swiss residents use a smartphone 
  # (source: https://de.statista.com/statistik/daten/studie/819925/umfrage/prognose-zum-anteil-der-smartphone-nutzer-in-der-schweiz/)
os <- 0.9  # 90% of smartphones use a suitable operation system
wohnbev <- 8603900 
potential <- round((wohnbev * smartph * os), 0)

app_data$phones_with_app <- round((app_data$active_apps/potential), 3)

# ----- linear regression model -----
# change date format for lm function
model_data <- app_data
model_data$datum <- as.numeric(as.Date(model_data$datum, "%Y%m/%d"))
str(model_data)

# exclude inicial installations
model_data <- model_data[-c(1), -c(3)]

# fit simple
fit.app <- lm(active_apps ~ datum, data = model_data)
summary(fit.app)

par(mfrow = c(1,1))
plot(active_apps ~ datum, data = model_data)
abline(fit.app)
# resplot(fit.app, plots=1:3)

# prediction
ziel <- data.frame(datum = as.numeric(seq(as.Date("2020-06-30"), Sys.Date(), "days")))
pred <- data.frame(ziel, predict(fit.app, newdata = ziel, interval = "confidence", level = 0.95))
# result
as.Date(pred$datum, origin = "1970-01-01")
pred$fit

par(mfrow = c(1,1))
plot(x = as.Date(model_data$datum, origin="1970-01-01"),y = model_data$active_apps, 
     xlim = c(as.Date("2020-06-25"), as.Date("2020-07-10")), ylim = c(550000, 1500000),
     xlab = "Datum", ylab = "Active Apps", main = "standard model")
abline(fit.app)
points(pred$datum, pred$fit, col = "red")

# ----- fit log ----
model_data$log <- log(model_data$active_apps)

fit.applog <- lm(log ~datum, data = model_data)
summary(fit.applog)

plot(model_data$datum, model_data$log)
abline(fit.applog)

ziel <- ziel
pred <- data.frame(ziel, predict(fit.applog, newdata = ziel, interval = "confidence", level = 0.95))

par(mfrow = c(1,1))
plot(x = as.Date(model_data$datum, origin="1970-01-01"), y = model_data$log, 
     xlim = c(as.Date("2020-06-25"), as.Date("2020-07-10")), ylim = c(13, 14.5),
     xlab = "Datum", ylab = "Active Apps", main = "Log model")
abline(fit.applog)
points(pred$datum, pred$fit, col = "red")
