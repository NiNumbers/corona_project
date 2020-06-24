# libraries
library(zoo) # for rollmean()

load("corona_all_data.RData")

# ----------------- rolling average -------------
corona_rollmean <- data.frame(datum)
corona_rollmean$CHrollmean <- corona_rollmean$DErollmean <- corona_rollmean$ITrollmean <- corona_rollmean$USrollmean <- rep(0, dauer)
w <- 3
par(mfrow = c(1,w))
for (x in 1:w) {
  corona_rollmean$CHrollmean[x:dauer] <- rollmean(corona_taegl$CHtaeglich, x)
  corona_rollmean$DErollmean[x:dauer] <- rollmean(corona_taegl$DEtaeglich, x)
  corona_rollmean$ITrollmean[x:dauer] <- rollmean(corona_taegl$ITtaeglich, x)
  corona_rollmean$USrollmean[x:dauer] <- rollmean(corona_taegl$UStaeglich, x)
  if (x == 1) {
    plot(y = corona_rollmean$CHrollmean[17:dauer], x = corona_rollmean$datum[17:dauer], type = "b", col = "orange", ylab = "taegliche Fallzahlen", xlab = "Datum", main = "Tägl. Neuansteckungen",
         xlim=c(datum[17], max(datum)), ylim=c(0, max(corona_taegl$CHtaeglich, corona_taegl$DEtaeglich, corona_taegl$ITtaeglich))) 
    points(as.Date("2020/03/21"), 3000, type = "h",lwd = 1, lty = "dashed", col = "orange", pch = 15)
    text(as.Date("2020/03/21"), 1000, labels = "Ansammlungen über 5 Personen verboten", pos = 4, cex = 1, srt = 90, offset = -0.5)
    lines(DErollmean ~ datum, data = corona_rollmean, type = "b", col = "blue")
    lines(ITrollmean ~ datum, data = corona_rollmean, type = "b", col = "darkgreen")
    lines(USrollmean ~ datum, data = corona_rollmean, type = "b", col = "green")
    # ad smooth line
    lines(loess.smooth(y = corona_rollmean$CHrollmean[17:dauer], x = corona_rollmean$datum[17:dauer]), col = "orange", lty = 2)
    lines(loess.smooth(y = corona_rollmean$DErollmean[17:dauer], x = corona_rollmean$datum[17:dauer]), col = "blue", lty = 2)
    lines(loess.smooth(y = corona_rollmean$ITrollmean[17:dauer], x = corona_rollmean$datum[17:dauer]), col = "darkgreen", lty = 2)
    lines(loess.smooth(y = corona_rollmean$USrollmean[17:dauer], x = corona_rollmean$datum[17:dauer]), col = "green", lty = 2)
        }
  else {
    plot(CHrollmean[17:dauer] ~ datum[17:dauer], data = corona_rollmean, type = "b", col = "orange", ylab = paste(c("rolling mean, size of window:", x), collapse = " "), xlab = "Datum", main = "Tägl. Daten geglättet",
         xlim=c(datum[17], max(datum)), ylim=c(0, max(corona_taegl$CHtaeglich, corona_taegl$DEtaeglich, corona_taegl$ITtaeglich)))
    points(as.Date("2020/03/21"), 3000, type = "h",lwd = 1, lty = "dashed", col = "orange", pch = 15)
    text(as.Date("2020/03/21"), 1000, labels = "Ansammlungen über 5 Personen verboten", pos = 4, cex = 1, srt = 90, offset = -0.5)
        }
  lines(DErollmean ~ datum, data = corona_rollmean, type = "b", col = "blue")
  lines(ITrollmean ~ datum, data = corona_rollmean, type = "b", col = "darkgreen")
  lines(USrollmean ~ datum, data = corona_rollmean, type = "b", col = "green")
  legend(x = "topleft", legend = c("Schweiz", "Deutschland, Datum angepasst", "Italien, Datum angepasst", "USA, Datum angepasst"), 
         fill = c("orange", "blue", "darkgreen", "green"), bty = "n", cex = 1)
}

# View(corona_rollmean)

# --------- anzahl Verstorbene -------------
corona_taegl$verstorbenCH <- rep(0, dauer)
corona_taegl$verstorbenCH <- replace(corona_taegl$verstorbenCH, c(10, 13), 1)
corona_taegl$verstorbenCH[15:20] <- c(2, 3, 4, 2, 2, 4)
corona_kum$verstorbenCHkum <- cumsum(corona_taegl$verstorbenCH)
corona_kum$verstorbenCHkum[21:dauer] <- verstorbenCHkum
for (i in 2:dauer) {
  corona_taegl$verstorbenCH[i] = corona_kum$verstorbenCHkum[i] - corona_kum$verstorbenCHkum[i-1]
}

# View(corona_taegl)

# ---------- Trendlinien erstellen ------------------------
corona_kum$line50 <- rep(1, dauer)
for (j in 1:(dauer-1)) {
  corona_kum$line50[j+1] = corona_kum$line50[j]*1.5
}
corona_kum$line33 <- rep(13, dauer)
for (j in 1:(dauer-1)) {
  corona_kum$line33[j+1] = corona_kum$line33[j]*1.33
}

corona_kum$line25 <- rep(30, dauer)
for (j in 1:(dauer-1)) {
  corona_kum$line25[j+1] = corona_kum$line25[j]*1.25
}
corona_kum$line20 <- rep(71, dauer)
for (j in 1:(dauer-1)) {
  corona_kum$line20[j+1] = corona_kum$line20[j]*1.2
}
corona_kum$line10 <- rep(700, dauer)
for (j in 1:(dauer-1)) {
  corona_kum$line10[j+1] = corona_kum$line10[j]*1.1
}
# View(corona_kum)

# total Faelle
total <- sum(corona_taegl$CHtaeglich)
sterblichkeit <- sum(corona_taegl$verstorbenCH)/total


# ------------ plot cumulative CH data --------------
par(mfrow = c(1,2))
# pdf(file="COVID_19_CH_kummuliert.pdf")
plot(CHkum[17:dauer] ~ datum[17:dauer], data = corona_kum, type = "l", ylab = "Total Ansteckungen", xlab = "Datum", 
     main = "COVID-19 Ansteckungen (US off the chart)", col = "yellow",
     xlim=c(datum[17], max(datum)), ylim=c(0, max(CHkum)))
polygon(c(corona_kum$datum, corona_kum$datum[dauer]), c(corona_kum$CHkum, 0), col='yellow', lty = "blank")
points (CHkum ~ datum, data = corona_kum)
# plot daily infections
lines(corona_kum$datum, corona_taegl$CHtaeglich, type = "h", lwd = 4, lend = 1, col = "orange")

# plot further infos
points(as.Date("2020/03/10"), 485, type = "h",lwd = 1, lty = "dashed", col = "orange", pch = 15)
text(as.Date("2020/03/10"), 175, labels = "nur noch Risikopatienten getestet", pos = 4, cex = 1, srt = 90, offset = -0.5)
points(as.Date("2020/03/17"), 4000, type = "h",lwd = 1, lty = "dashed", col = "orange", pch = 15)
text(as.Date("2020/03/17"), 1000, labels = "Notrecht in Kraft", pos = 4, cex = 1, srt = 90, offset = -0.5)
points(as.Date("2020/03/21"), 6000, type = "h",lwd = 1, lty = "dashed", col = "orange", pch = 15)
text(as.Date("2020/03/21"), 1000, labels = "Ansammlungen über 5 Personen verboten", pos = 4, cex = 1, srt = 90, offset = -0.5)
points(as.Date("2020/04/03"), 18000, type = "h",lwd = 1, lty = "dashed", col = "orange", pch = 15)
text(as.Date("2020/04/03"), 1000, labels = "mehr Tests (auch symptomfreie Spitaleintritte und tw. Nichtrisikopatienten)", pos = 4, cex = 1, srt = 90, offset = -0.5)

# plot death
polygon(c(corona_kum$datum, corona_kum$datum[dauer]), c(corona_kum$verstorbenCHkum, 0), col = "red", lty = "blank")

# plot Germany and Italy
lines(DEkum ~ datum, data = corona_kum, type = "l", lty = 1, col = "blue", lwd = 2)
lines(ITkum ~ datum, data = corona_kum, type = "l", col = "darkgreen", lty = 1, lwd = 2)
lines(USkum ~ datum, data = corona_kum, type = "l", col = "green", lty = 1, lwd = 2)
# plot trendlinien
lines(line50 ~ datum, data = corona_kum, col = "skyblue", lty = 2)
lines(line33 ~ datum, data = corona_kum, col = "red", lty = 2)
lines(line25 ~ datum, data = corona_kum, col = "black", lty = 2)
lines(line20 ~ datum, data = corona_kum, col = "grey", lty = 2)
lines(line10[25:dauer] ~ datum[25:dauer], data = corona_kum, col = "lightgrey", lty = 2)

# grid(nx = 2, ny = NULL, col = "lightgray", lty = "dotted")
abline(v =  c(as.Date("2020/03/17"), as.Date("2020/03/22")), col = "gray", lty = "dotted")
legend(x="topleft", 
       legend = c("Schweiz, neu", "Schweiz, kummuliert", "Schweiz, verstorben", 
                  "Deutschland kummuliert, Datum angepasst", "Italien, kummuliert, Datum angepasst", "USA, kummuliert, Datum angepasst"), 
       fill = c("orange", "yellow", "red", "blue", "darkgreen", "green"), bty = "n")

legend(x="left", 
       legend = c("50% Wachstum", "33% Wachstum", "25% Wachstum", "20% Wachstum", "10% Wachstum"), 
       col = c("skyblue", "red", "black", "grey", "lightgrey"), bty = "n", lty=2, cex = 1)

# ---------- logarithmischer Plot hohe Fallzahlen ---------
plot(CHkum[17:dauer] ~ datum[17:dauer], data = corona_kum, type = "l", lwd = 2, ylab = "Total (log-Skala)", xlab = "Datum", main = "Daten kummuliert", col = "orange",
     log = "y", xlim=c(datum[17], max(datum)), ylim=c(1000, max(ITkum, DEkum, CHkum, USkum)))

lines(DEkum ~ datum, data = corona_kum, type = "l", col = "blue", lwd = 2)
lines(ITkum ~ datum, data = corona_kum, type = "l", col = "darkgreen", lwd = 2)
lines(USkum ~ datum, data = corona_kum, type = "l", col = "green", lwd = 2)
lines(line50 ~ datum, data = corona_kum, lty = 2, col = "skyblue")
lines(line33[10:dauer] ~ datum[10:dauer], data = corona_kum, lty = 2, col = "red")
lines(line25[16:dauer] ~ datum[16:dauer], data = corona_kum, lty = 2, col = "black")
lines(line20 ~ datum, data = corona_kum, col = "grey", lty = 2)
lines(line10[25:dauer] ~ datum[25:dauer], data = corona_kum, col = "lightgrey", lty = 2)

legend(x = "topleft", legend = c("Schweiz", "Deutschland, Datum angepasst", "Italien, Datum angepasst", "USA, kummuliert, Datum angepasst"),
       fill = c("orange", "blue", "darkgreen", "green"), bty = "n", cex = 1)
legend(x="bottomright",
       legend = c("50% Wachstum", "33% Wachstum", "25% Wachstum", "20% Wachstum", "10% Wachstum"),
       col = c("skyblue", "red", "black", "grey", "lightgrey"), bty = "n", lty=2, cex = 1)

# # ------------ Einzelne logar. Plots -----
# par(mfrow = c(1,3))
# plot(USkum, type = "l", lwd = 2, ylab = "Total (log-Skala)", xlab = "Datum", main = "US Daten kummuliert", col = "orange",
#      log = "y", ylim=c(10, max(ITkum, DEkum, CHkum, USkum)))
# lines(corona_kum$line33)
# legend(x="bottomright", legend = c("33% Wachstum"), col = c("black"), bty = "n", lty=1, cex = 1)
# plot(ITkum[9:length(ITkum)], type = "l", lwd = 2, ylab = "Total (log-Skala)", xlab = "Datum", main = "IT Daten kummuliert", col = "orange",
#      log = "y", ylim=c(10, max(ITkum, DEkum, CHkum, USkum)))
# lines(corona_kum$line33*5)
# legend(x="bottomright", legend = c("33% Wachstum"), col = c("black"), bty = "n", lty=1, cex = 1)
# plot(DEkum[9:length(DEkum)], type = "l", lwd = 2, ylab = "Total (log-Skala)", xlab = "Datum", main = "DE Daten kummuliert", col = "orange",
#      log = "y", ylim=c(10, max(ITkum, DEkum, CHkum, USkum)))
# lines(corona_kum$line33*10)
# legend(x="bottomright", legend = c("33% Wachstum"), col = c("black"), bty = "n", lty=1, cex = 1)
# #-------------- CH alleine ------------
# par(mfrow = c(1,2))
# plot(CHkum[1:dauer] ~ datum[1:dauer], data = corona_kum, type = "l", ylab = "Total Ansteckungen", xlab = "Datum", 
#      main = "COVID-19 Ansteckungen", col = "orange",
#      xlim=c(datum[1], max(datum)))
# 
# plot(CHkum[8:dauer] ~ datum[8:dauer], data = corona_kum, type = "l", lwd = 2, ylab = "Total (log-Skala)", xlab = "Datum", main = "Daten kummuliert", col = "orange",
#      log = "y")
# lines(line25[1:dauer] ~ datum[1:dauer], data = corona_kum, lty = 1, col = "grey")
# lines(line10[26:dauer] ~ datum[26:dauer], data = corona_kum, lty = 1, col = "green")
# 
# abline(v =  c(as.Date("2020/03/21"), as.Date("2020/04/04")), col = "orange", lty = "dotted")
# text(as.Date("2020/03/21"), 100, labels = "Ansammlungen über 5 Personen verboten", pos = 4, cex = 1, srt = 90, offset = -0.5)
# lines(y = c(21000, 21000), x = c(as.Date("2020/03/21"), as.Date("2020/04/04")), col = "orange", lty = "dotted")
# text(as.Date("2020/03/25"), 20000, labels = ("14 Tage"), pos = 3, col = "orange")
# 
# abline(v =  c(as.Date("2020/03/17"), as.Date("2020/03/31")), col = "darkgrey", lty = "dotted")
# text(as.Date("2020/03/17"), 100, labels = "Ausserordentliche Lage (Bars, Rest., etc zu)", pos = 4, cex = 1, srt = 90, offset = -0.5)
# lines(y = c(3000, 3000), x = c(as.Date("2020/03/17"), as.Date("2020/03/31")), col = "gray", lty = "dotted")
# text(as.Date("2020/03/22"), 2900, labels = ("14 Tage"), pos = 3, col = "grey")
# 
# legend(x="bottomright", legend = c("25% Wachstum", "10% Wachstumg"), col = c("grey", "green"), bty = "n", lty=1, cex = 1)

# --------------- tägliche Zahlen inkl. LOESS-Glättung ---------
par(mfrow = c(2,2))
# define parameter for LOESS function
smooth <- 0.5 # percentage of used Data in LOESS
t <- as.numeric(time(corona_taegl$datum))
CHtaegl_loess <- fitted(loess(corona_taegl$CHtaeglich ~ t, family = "symmetric", span = smooth))
CHtaegl_loess[CHtaegl_loess < 0] <- 0
DEtaegl_loess <- fitted(loess(DEtaegl[(length(DEtaegl)-length(corona_taegl$CHtaeglich)+1):length(DEtaegl)] ~ t, family = "symmetric", span = smooth))
DEtaegl_loess[DEtaegl_loess < 0] <- 0
ITtaegl_loess <- fitted(loess(ITtaegl[(length(ITtaegl)-length(corona_taegl$CHtaeglich)+1):length(ITtaegl)] ~ t, family = "symmetric", span = smooth))
ITtaegl_loess[ITtaegl_loess < 0] <- 0
UStaegl_loess <- fitted(loess(UStaegl[(length(UStaegl)-length(corona_taegl$CHtaeglich)+1):length(UStaegl)] ~ t, family = "symmetric", span = smooth))
UStaegl_loess[UStaegl_loess < 0] <- 0

plot(y = corona_taegl$CHtaeglich, x =corona_taegl$datum, main = "CH: tägliche Fallzahlen", xlab = "", ylab = "", type = "h",lwd = 3, lend = 1, col = "darkgrey")
lines(x = corona_taegl$datum, y = CHtaegl_loess, col = "red", lty = 2, lwd = 2)
abline(v =  c(as.Date("2020/04/15")), col = "red", lty = "dotted")
text(as.Date("2020/04/15"), 100, labels = "Achtung: 328 Fälle sind Nachmeldungen!", pos = 4, cex = 1, srt = 90, offset = -0.5)
abline(v =  c(as.Date("2020/04/29")), col = "red", lty = "dotted")
text(as.Date("2020/04/29"), 100, labels = "rund 1/5 mehr Tests", pos = 4, cex = 1, srt = 90, offset = -0.5)
abline(v =  c(as.Date("2020/06/21")), col = "red", lty = "dotted")
text(as.Date("2020/06/21"), 100, labels = "14 Nachmeldungen", pos = 4, cex = 1, srt = 90, offset = -0.5)


plot(x = corona_taegl$datum, y = DEtaegl[(length(DEtaegl)-length(corona_taegl$CHtaeglich)+1):length(DEtaegl)], main = "DE: tägliche Fallzahlen", type = "h", 
     lwd = 3, lend = 1, col = "darkgrey", xlab = "", ylab = "")
lines(x = corona_taegl$datum, y = DEtaegl_loess, col = "red", lty = 2, lwd = 2)
plot(x = corona_taegl$datum, y = ITtaegl[(length(ITtaegl)-length(corona_taegl$CHtaeglich)+1):length(ITtaegl)], main = "IT: tägliche Fallzahlen", type = "h", 
     lwd = 3, lend = 1, col = "darkgrey", xlab = "", ylab = "")
lines(x = corona_taegl$datum, y = ITtaegl_loess, col = "red", lty = 2, lwd = 2)
plot(x = corona_taegl$datum, y = UStaegl[(length(UStaegl)-length(corona_taegl$CHtaeglich)+1):length(UStaegl)], main = "US: tägliche Fallzahlen", type = "h", 
     lwd = 3, lend = 1, col = "darkgrey", xlab = "", ylab = "")
lines(x = corona_taegl$datum, y = UStaegl_loess, col = "red", lty = 2, lwd = 2)