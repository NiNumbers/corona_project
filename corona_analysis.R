# Residuenanalyse
par(mfrow = c(2,1))
CHtaegl_rest <- CHtaegl - CHtaegl_loess
ts.CHtaegl_rest <- ts(CHtaegl_rest, start = c(2020, 02, 26), frequency = 7)

plot(ts.CHtaegl_rest)
plot(corona_taegl$CHtaeglich)
fit.CHtaegl <- ets(CHtaegl ~ datum + day, data = corona_taegl)
summary(fit.CHtaegl)
drop1(fit.CHtaegl, test = "F")
plot(fit.CHtaegl)

par(mfrow = c(1,1))
plot(log(ts.CHtaegl_rest+1000))
plot(stl(x = ts.CHtaegl_rest, s.window = 7))
# Autocorrelation

acf(ts.CHtaegl_rest)

