# library laden
library(svDialogs) #fuer pop up message
library(readxl) # excel lesen

# ---------- mobile app dataframe  -----------
# Quelle: https://www.experimental.bfs.admin.ch/expstat/de/home/innovative-methoden/swisscovid-app-monitoring.html
# ----------- alte Datenerhebung (anzahl synch / 4) -------
active_apps_old <- c(566894, 746942, 806570, 855080, 915965, 928909, 970485, 991168, 1007199, 1015293, 1002682, 1019830,
                 1016889, 1017504, 1011066, 987424, 959815, 950288, 966781, 965832, 946368, 972034, 986117, 952076, 
                 937121, 945693, 947955)
app_data_old <- data.frame(datum = seq(as.Date("2020/06/25"), 
                                   (as.Date("2020/06/25")+(length(active_apps_old))-1), "days"), 
                       active_apps_old = active_apps_old)
# --------- neue Berechnungsmethode -----------
active_apps <- c(1150000, 1160000, 1200000, 1190000, 1200000, 1200000, 1200000, 1200000, 
                 1220000)
covidcodes = c(rep(0, 6), 5, 11, 14, 11, 2, 14, 13, 11, 16, 8, 8, 2, 5, 15, 15, 9, 8, 
               6, 4, 6, 18, 11, 15, 10, 8, 7, 12, 20, 17)

# --------- BAG Excel auslesen ------------
CH_data <- read_excel("200325_Datengrundlage_Grafiken_COVID-19-Bericht.xlsx", 
                      sheet = "COVID19 Zahlen", range = cell_limits(c(7, 1), c(NA, NA)),
                      col_names = TRUE)

# ---------- Fallzahlen dataframe -------------
corona_kum <- data.frame(datum = seq(as.Date("2020/02/24"), Sys.Date(), "days"))
dauer <- length(corona_kum$datum)
corona_kum$day <- rep(c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"), length.out = dauer)

verstorbenCHkum <- CH_data$`Todesfälle pro Tag, kumuliert`

# ---------- test if data is up to date --------
if (length(CH_data$Datum) != dauer){
  dlg_message("Data not up to date, code will not run properly, please download todays excel from 
              https://www.bag.admin.ch/bag/de/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/situation-schweiz-und-international.html", type = c("ok"),
              gui = .GUI)}

# Match mit Schweizer Daten, korrelierend auf 1000 Faelle. DE: 3 Tag verschoben, IT 12 d verschoben, US 4 d verschoben
# Quelle: JHU https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6
# -------- erste ca. 150 Tageszahlen fuer DE, IT, US --------------
DEkum_ <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 57, 150, 188, 240, 349, 534, 684, 847, 1150, 1565, 2369, 3062, 4838, 6012, 9352, 12327, 
           15320,	19848, 22213, 24904, 30150, 33593, 39502, 47278, 53340, 58247, 62435, 67051, 72383, 77981, 84794, 91159,
           96108, 100132, 103375, 107663, 113296, 118235, 122171, 125834, 127854, 130072, 132210, 134753, 138135, 141397, 
           143724, 145743, 147065, 148453, 150729, 153129, 155418, 156513, 157770, 158841, 159912, 161539, 163009, 164077, 
           164967, 165664, 166199, 167007, 168162, 169430, 170588, 171324, 171879, 172576, 173171, 174098, 174478, 175699, 
           176244, 176551, 177289, 177842, 178545, 179150, 179730, 179986, 180338, 180802, 181293, 181918, 182559, 183025,
           183302, 183500, 183771, 184097, 184427, round((184427+(185416-184427)/2),0), 185416, 185696, 185869, 186233,
           186522, 186525, 186883, 187263, 187267, 187682, 188213, 188449, 189445, 190203, 190670, 191225, 191657, 192127,
           192786, 193257, 193790, 194403, 194693, 194898, 195042, 195758, 196300, 196723, 197184, 197388, 197607, 198079, 
           198453, 198768, 199218, 199592, 199827, 199919, 200440, 200456, 201259, 201840, 202355, 202580, 202931, 203495, 
           203905, 204480, 205149, 205976, 206335)
ITkum_ <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 76, 124, 229, 374, 528, 650, 888, 1100, 1700, 2000, 2502, 3089, 3858, 4636,  5883, 
           7375, 9172, 10149, 12462, 15061, 17660, 21157, 24747, 31506, 35713, 41035, 47021, 53578, 59138, 63927, 69176, 
           74386, 80598, 86498, 92472, 97689, 101739, 105792,  110574, 115242, 119827, 124632, 128948, 132547, 135586, 
           139422, 143626, 147577, 152271, 156363, 159516, 162488, 165155, 168941, 172434, 175925, 178972, 181228, 183957,
           187327, 189973, 192994, 195351, 197675, 199590, 201505, 203591, 205463, 207428, 209328, 210717, 211938, 213013,
           214457, 215858, 217185, 218268, 219070, 219814, 221216, 222104, 223096, 223885, 224760, 225435, 225886, 226699,
           227364, 228006, 228658, 229327, 229858, 230158, 230555, 231139, 231732,232248, 232664, 232997, 233197, 233515,
           233836, round((233836+ (234531 -233836)/2), 0), 234531, 234801, 234998, 235278, 235561, 235763, 236142, 236305,
           236651, 236989, 237290, 237500, 237828, 237950, 238011, 238275, 238499, 238720, 238833, 239410, 239706, 239961,
           240136, 240310, 240436, 240578, 240760, 240961, 241184, 241419, 241611, 241819, 241956, 242149, 242363, 242639,
           242827, 243061, 243230, 243344, 243506, 243736, 243967, 244216, 244434, 244624, 244752, 245032, 245338, 245590,
           245864)
USkum_ <- c(0, 0, 15, 16, 51, 57, 58, 60, 68, 74, 98, 118, 149, 217, 262, 402, 518, 583, 959, 1300, 1700, 2200, 2700, 3500, 4600, 
           6400, 7800, 13700, 19100, 25500, 33300, 43800, 54935, 69197, 85996, 104837, 124686, 143055, 164610, 189633, 
           216772, 245573, 278458, 312245, 337646, 368449, 399929, 432438, 466299, 501615, 530830, 557590, 582594, 609685, 
           639664, 671425, 706779, 735287, 759786, 787960, 825306, 842624, 869172, 907096, 939249, 965933, 989258, 1012583,
           1040488, 1070032, 1104161, 1133069, 1158341, 1180634, 1204475, 1228609, 1256972, 1283929, 1309541, 1329799, 1347936,
           1369964, 1390764, 1417889, 1443397, 1467884, 1487447, 1508957, 1528661, 1551853, 1577758, 1601434, 1622670, 
           1643499, 1662768, 1681418, 1699933, 1721926, 1747087, 1770384, 1790191, 1811277, 1831821, 1851520, 
           round((1851520+ (1897838-1851520)/2), 0), 1897838, 1920061, 1942363, 1961185, 1979893, 2000464, 2023347, 2048986,
           2074526, 2094069, 2114026, 2137731, 2164071, 2191200, 2222576, 2255119, 2280969, 2312302, 2347102, 2381369,
           2422312, 2467837, 2510323, 2549069, 2590582, 2636538, 2686587, 2739879, 2795163, 2839917, 2888730, 2938624, 2996098,
           3055144, 3118168, 3184722, 3247782, 3304942, 3364547, 3431574, 3499398, 3576430, 3647715, 3712445, 3773260, 3830926,
           3902135, 3970908, 4038864, 4113224, 4178730) 
# --------- neueste Tageszahlen fuer DE, IT, US --------------- 
DEkum <- append(DEkum_, c(206751, 207382, 207960, 208819, 209736, 210676))
ITkum <- append(ITkum_, c(246118, 246286, 246488, 246776, 247158, 247537))
USkum <- append(USkum_, c(4234140, 4294770, 4352083, 4427493, 4495224, 4563262))

# ---------- Dataframe mit auf CH Fallzahlen normierte Werte generieren (fuer Vergleich erste Welle) --------
corona_kum$CHkum <- CH_data$`Fallzahlen pro Tag, kumuliert`
corona_kum$DEkum <- DEkum[0:dauer]
corona_kum$ITkum <- ITkum[0:dauer] 
corona_kum$USkum <- USkum[0:dauer] 

# ------------- Taegliche Fallzahlen, wiederum auf Schweiz normiert --------
corona_taegl <- corona_kum[ , c("datum", "day")]
corona_taegl$CHtaeglich <- rep(1, dauer)
corona_taegl$DEtaeglich <- rep(0, dauer)
corona_taegl$ITtaeglich <- rep(0, dauer)
corona_taegl$UStaeglich <- rep(0, dauer)

for (i in 2:dauer) {
  corona_taegl$CHtaeglich[i] = corona_kum$CHkum[i] - corona_kum$CHkum[i-1]
  corona_taegl$DEtaeglich[i] = corona_kum$DEkum[i] - corona_kum$DEkum[i-1]
  corona_taegl$ITtaeglich[i] = corona_kum$ITkum[i] - corona_kum$ITkum[i-1]
  corona_taegl$UStaeglich[i] = corona_kum$USkum[i] - corona_kum$USkum[i-1]
}
CHtaegl <- corona_taegl$CHtaeglich
DEtaegl <- DEkum
for (i in 2:length(DEkum)) {
  DEtaegl[i] = DEkum[i] - DEkum[i-1]
}
ITtaegl <- ITkum
for (i in 2:length(ITkum)) {
  ITtaegl[i] = ITkum[i] - ITkum[i-1]
}
UStaegl <- USkum
for (i in 2:length(USkum)) {
  UStaegl[i] = USkum[i] - USkum[i-1]
}
rm(i)
# -------------- Landesdataframes machen mit allen Daten
corona_CH <- corona_kum[ , c("datum", "CHkum")]
corona_CH$CHtaegl <- corona_taegl$CHtaeglich

corona_DE <- data.frame(datum = seq(as.Date("2020/03/02"), Sys.Date(), "days"))
corona_DE$DEkum <- DEkum[9:length(DEkum)]
corona_DE$DEtaegl <- DEtaegl[9:length(DEkum)]

corona_IT <- data.frame(datum = seq(as.Date("2020/02/21"), Sys.Date(), "days"))
corona_IT$ITkum <- ITkum[9:length(ITkum)]
corona_IT$ITtaegl <- ITtaegl[9:length(ITkum)]

corona_US <- data.frame(datum = seq(as.Date("2020/02/22"), Sys.Date(), "days"))
corona_US$USkum <- USkum
corona_US$UStaegl <- UStaegl

# ---------------- covid app dataframes erstellen ------------- 
app_data <- data.frame(datum = seq(as.Date("2020/07/22"), 
                                   (as.Date("2020/07/22")+(length(active_apps))-1), "days"), 
                       active_apps = active_apps)
covidcodes_data <- data.frame(datum = seq(as.Date("2020/06/25"), 
                                          (as.Date("2020/06/25")+(length(covidcodes))-1), "days"), 
                              covidcodes = covidcodes, 
                              faelle_CH = corona_CH$CHtaegl[121:(120+length(covidcodes))],
                              percentage = round(covidcodes/corona_CH$CHtaegl[121:(120+length(covidcodes))], 2))
# ---------------- save Data ---------
# for shiny app
save(corona_DE, corona_IT, corona_US, corona_CH, file = "corona_shiny.RData")
# for analysing app_data
save(app_data, covidcodes_data, file = "corona_app.RData")
# all data
save.image(file = "corona_all_data.RData")