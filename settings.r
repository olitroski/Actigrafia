## ----- Settings para dectección del día y la noche ---------------------------------- #
# SleepSearchTime (hora a partir del cual comienza a buscar noche)
# ininoc <- hm(ininoc)
# ininoc <- hour(ininoc) + minute(ininoc)/60
ininoc <- hm("20:00")
inidia <- hm("06:00")
     
# SleepDuration (tiempo minimo para determinar que comineza la noche)
dursleep <- 29
durawake <- 30


# SleepMaximumAwake (Tiempo máximo de vigilia dentro del SleepDuration)
# Como no se calcula sobre el estado actigrafico original sino el corregido esto
# No es necesario 
#         maxawake <- "00:05"
#         maxawake <- hm(maxawake)
#         maxawake <- period_to_seconds(maxawake)
# 
#         maxsleep <- "00:05"
#         maxsleep <- hm(maxsleep)
#         maxsleep <- period_to_seconds(maxsleep)

# StateFilterDuration (tiempo para cambio de estado)
statedur <- "00:05"
statedur <- hm(statedur)
statedur <- period_to_seconds(statedur)
