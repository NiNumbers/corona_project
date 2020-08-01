load("corona_all_data.RData")

from <- 14 # number of days in the past
to <- 0 # if 0: today

# Mittelwert ueber die letzten Tage
mean(corona_CH$CHtaegl[c((length(corona_CH$datum)-from):(length(corona_CH$datum)-to))])

# Median
median(corona_CH$CHtaegl[c((length(corona_CH$datum)-from):(length(corona_CH$datum)-to))])
