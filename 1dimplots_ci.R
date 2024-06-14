library(ggplot2)
library(dplyr)
library(SMPracticals)

#data_by_games====
statistics_by_games <- bind_rows(
  mean = data_by_games %>% summarize(across(where(is.numeric), mean)),
  sd = data_by_games %>% summarize(across(where(is.numeric), sd)),
  median = data_by_games %>% summarize(across(where(is.numeric), median)),
  .id = "statistic"
)

#gameDuration
ggplot(data_by_games, aes(x = gameDuration)) +
  geom_histogram(fill="purple") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_games[1, ]$a, 
                    xmax = ci_mean_by_games[1, ]$b ),
                color = "red",
                linewidth = 2,
                width = 150) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_gameDuration[1], 
                    xmax = ci_median_gameDuration[2]),
                color = "blue",
                linewidth = 2,
                width = 150) +
  labs(x = 'Тривалість гри, хв', y = 'Частота', title = 'Нормальний розподіл тривалості гри')

qqnorm(data_by_games$gameDuration)

#hasWon
ggplot(data_by_games, aes(x = hasWon)) +
  geom_bar(fill="red") +
  labs(x = 'Результат гри', y = 'Кількість', title = 'Однакова кількість виграних та програних матчів') +
  scale_x_discrete(labels = c('Програш', 'Виграш'))

#goldDiff
ggplot(data_by_games, aes(x = goldDiff)) +
  geom_histogram(bins = 60, fill="gold") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_games[2, ]$a, 
                    xmax = ci_mean_by_games[2, ]$b ),
                color = "red",
                linewidth = 2,
                width = 100) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_goldDiff[1], 
                    xmax = ci_median_goldDiff[2]),
                color = "blue",
                linewidth = 2,
                width = 100) +
  labs(x = 'Різниця золота', y = 'Частота', title = '"Бі-Нормальний розподіл" кінцевої різниці золота')

qqnorm(data_by_games$goldDiff)

#ExpDiff
ggplot(data_by_games, aes(x = expDiff)) +
  geom_histogram(bins = 60, fill="skyblue") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_games[3, ]$a, 
                    xmax = ci_mean_by_games[3, ]$b ),
                color = "red",
                linewidth = 2,
                width = 100) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_expDiff[1], 
                    xmax = ci_median_expDiff[2]),
                color = "blue",
                linewidth = 2,
                width = 100) +
  labs(x = 'Різниця золота', y = 'Частота', title = '"Бі-Нормальний розподіл" кінцевої різниці досвіду')

qqnorm(data_by_games$goldDiff)

#isFirstBlood
ggplot(data_by_games, aes(x = isFirstBlood)) +
  geom_bar(fill="purple") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Результат гри', y = 'Кількість', title = 'Значно більша кількість матчів з першим вбивством') +
  scale_x_discrete(labels = c('Немає першого вбиства', 'Є перше вбивство'))

#killedFireDrake
ggplot(data_by_games, aes(x = killedFireDrake)) +
  geom_bar(fill="red") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств дракона', y = 'Кількість ігор', title = 'Розподіл кількості вбитих вогнених драконів')

#killedWaterDrake
ggplot(data_by_games, aes(x = killedWaterDrake)) +
  geom_bar(fill="blue") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств дракона', y = 'Кількість ігор', title = 'Розподіл кількості вбитих водяних драконів')

#killedAirDrake
ggplot(data_by_games, aes(x = killedAirDrake)) +
  geom_bar(fill="yellow") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств дракона', y = 'Кількість ігор', title = 'Розподіл кількості вбитих повітряних драконів')

#killedEarthDrake
ggplot(data_by_games, aes(x = killedEarthDrake)) +
  geom_bar(fill="green") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств дракона', y = 'Кількість ігор', title = 'Розподіл кількості вбитих земляних драконів')

#killedElderDrake
ggplot(data_by_games, aes(x = killedElderDrake)) +
  geom_bar(fill="black") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств дракона', y = 'Кількість ігор', title = 'Розподіл кількості вбитих старших драконів')

#killedBaronNashor
ggplot(data_by_games, aes(x = killedBaronNashor)) +
  geom_bar(fill="purple") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств барона', y = 'Кількість ігор', title = 'Розподіл кількості вбитих баронів')

#killedRiftHerald
ggplot(data_by_games, aes(x = killedRiftHerald)) +
  geom_bar(fill="darkgreen") +
  labs(x = 'Вбивств Геральдів', y = 'Кількість ігор', title = 'Розподіл кількості вбитих Геральдів')

#destroyedTopInhibitor
ggplot(data_by_games, aes(x = destroyedTopInhibitor)) +
  geom_bar(fill="grey") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Зламаних інгібіторів', y = 'Кількість ігор', title = 'Розподіл кількості зламаних верхніх інгібіторів')

#destroyedMidInhibitor
ggplot(data_by_games, aes(x = destroyedMidInhibitor)) +
  geom_bar(fill="grey") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Зламаних інгібіторів', y = 'Кількість ігор', title = 'Розподіл кількості зламаних центральних інгібіторів')

#destroyedBotInhibitor
ggplot(data_by_games, aes(x = destroyedBotInhibitor)) +
  geom_bar(fill="grey") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Зламаних інгібіторів', y = 'Кількість ігор', title = 'Розподіл кількості зламаних нижніх інгібіторів')

#kills
ggplot(data_by_games, aes(x = kills)) +
  geom_bar(fill="purple") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_games[14, ]$a, 
                    xmax = ci_mean_by_games[14, ]$b ),
                color = "red",
                linewidth = 2,
                width = 100) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_kills[1], 
                    xmax = ci_median_kills[2]),
                color = "blue",
                linewidth = 2,
                width = 100) +
  labs(x = 'Вбивств', y = 'Частота', title = 'Нормальний розподіл кількості вбивств')

qqnorm(data_by_games$kills)

#deaths
ggplot(data_by_games, aes(x = deaths)) +
  geom_bar(fill="purple") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_games[15, ]$a, 
                    xmax = ci_mean_by_games[15, ]$b ),
                color = "red",
                linewidth = 2,
                width = 100) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_deaths[1], 
                    xmax = ci_median_deaths[2]),
                color = "blue",
                linewidth = 2,
                width = 100) +
  labs(x = 'Смертей', y = 'Частота', title = 'Нормальний розподіл кількості смертей')

qqnorm(data_by_games$deaths)

#data_by_frames====
statistics_by_frames <- bind_rows(
  mean = data_by_frames %>% summarize(across(where(is.numeric), mean)),
  sd = data_by_frames %>% summarize(across(where(is.numeric), sd)),
  median = data_by_frames %>% summarize(across(where(is.numeric), median)),
  .id = "statistic"
)

#frame
ggplot(data_by_frames, aes(x = frame)) +
  geom_bar(fill="purple") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_frames[1, ]$a, 
                    xmax = ci_mean_by_frames[1, ]$b ),
                color = "red",
                linewidth = 2,
                width = 1000) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_frame[1], 
                    xmax = ci_median_frame[2]),
                color = "blue",
                linewidth = 2,
                width = 1000) +
  labs(x = 'Поточна тривалість гри, хв', y = 'Частота', title = 'Розподіл поточних тривалостей ігор')

#goldDiff
ggplot(data_by_frames, aes(x = goldDiff)) +
  geom_histogram(bins = 30, fill="gold") +
  geom_errorbar(aes(y=0,
                    xmin = ci_mean_by_games[2, ]$a, 
                    xmax = ci_mean_by_games[2, ]$b ),
                color = "red",
                linewidth = 2,
                width = 1000) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_goldDiff2[1], 
                    xmax = ci_median_goldDiff2[2]),
                color = "blue",
                linewidth = 2,
                width = 1000) +
  labs(x = 'Різниця золота', y = 'Частота', title = 'Нормальний розподіл поточної різниці золота')

qqnorm(data_by_frames$goldDiff)

#killedBaronNashor
ggplot(data_by_frames, aes(x = killedBaronNashor)) +
  geom_bar(fill="purple") +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Вбивств барона', y = 'Кількість фреймів', title = 'Розподіл поточних кількостей вбитих баронів')

#kills
ggplot(data_by_frames, aes(x = kills)) +
  geom_histogram(bins = 60, fill="red") +
  labs(x = 'Кількість Вбивств', y = 'Частота', title = 'Розподіл поточних кількостей вбивств')

qqnorm(data_by_frames$kills)

#wardsPlaced
ggplot(data_by_frames, aes(x = wardsPlaced)) +
  geom_histogram(bins = 15, fill="skyblue") +
  
  scale_y_continuous(trans = "log10") +
  geom_errorbar(aes(y=1,
                    xmin = ci_mean_by_frames[5, ]$a, 
                    xmax = ci_mean_by_frames[5, ]$b ),
                color = "red",
                linewidth = 2,
                width = 1) +
  geom_errorbar(aes(y=0,
                    xmin = ci_median_wardsPlaced[1], 
                    xmax = ci_median_wardsPlaced[2]),
                color = "blue",
                linewidth = 2,
                width = 1) +
  
  labs(x = 'Кількість поставлених вардів', y = 'Частота', title = 'Розподіл поточної кількості розставлених вардів')

qqexp(data_by_frames$wardsPlaced)

#kd
ggplot(data_by_games, aes(x = kd)) +
  geom_histogram(bins = 20, fill="green") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10") +
  geom_errorbar(aes(y=1,
                    xmin = max(c(ci_mean_by_games[16, ]$a, 0)), 
                    xmax = ci_mean_by_games[16, ]$b ),
                color = "red",
                linewidth = 2,
                width = 0.5) +
  geom_errorbar(aes(y=1,
                    xmin = ci_median_kd[1], 
                    xmax = ci_median_kd[2]),
                color = "blue",
                linewidth = 2,
                width = 0.5) +
  labs(x = 'КД', y = 'Частота', title = 'Розподіл поточного КД')

qqnorm(data_by_frames$kd)

#towers
ggplot(data_by_frames, aes(x = towers)) +
  geom_bar(fill="orange") +
  geom_errorbar(aes(y=1,
                    xmin = ci_mean_by_games[6, ]$a, 
                    xmax = ci_mean_by_games[6, ]$b),
                color = "red",
                linewidth = 2,
                width = 5000) +
  labs(x = 'Кількість зламаних башт', y = 'Частота', title = 'Експоненційний розподіл поточної кількості зламаних башт')

