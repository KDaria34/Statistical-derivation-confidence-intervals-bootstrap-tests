library(ggplot2)
library(ggridges)
library(GGally)

# Як різниця в золоті/різниця в досвіді/кількість вбивств/смертей впливають на результат гри?====
ggplot(data_by_games, aes(x = goldDiff, fill = hasWon)) +
  geom_histogram(position = "dodge") +
  labs(x = 'Різниця в золоті', y = 'Частота', title = 'Розподіли різниці в золоті в заленості від результату гри', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = expDiff, fill = hasWon)) +
  geom_histogram(position = "dodge") +
  labs(x = 'Різниця в досвіді', y = 'Частота', title = 'Розподіли різниці в досвіді в заленості від результату гри', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = kills, y = deaths, colour = hasWon)) +
  geom_point(alpha = 1/10) +
  labs(x = 'Кількість вбивств', y = 'Кількість смертей', title = 'Виграним матчам відповідають більші значення вбивств та менші смертей', fill = 'Виграна гра?')


# Який тип дракону найбільше впливає на результат гри?====

dragon_kills <- data_by_games[c(2, 6:9)]
fire_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedFireDrake) %>% count()) /
  (dragon_kills %>% group_by(killedFireDrake) %>% count())
water_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedWaterDrake) %>% count()) /
  (dragon_kills %>% group_by(killedWaterDrake) %>% count())
air_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedAirDrake) %>% count()) /
  (dragon_kills %>% group_by(killedAirDrake) %>% count())
earth_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedEarthDrake) %>% count()) /
  (dragon_kills %>% group_by(killedEarthDrake) %>% count())
dragon_winrate <- data.frame(cbind(c(0, 1, 2, 3, 4), fire_dragon$n, water_dragon$n, air_dragon$n, earth_dragon$n))

ggplot(dragon_winrate) +
  geom_line(aes(x = X1, y = X2), colour = 'red', linewidth = 1) +
  geom_line(aes(x = X1, y = X3), colour = 'blue', linewidth = 1) +
  geom_line(aes(x = X1, y = X4), colour = 'yellow', linewidth = 1) +
  geom_line(aes(x = X1, y = X5), colour = 'green', linewidth = 1) +
  labs(x = 'Кількість вбитих драконів', y = 'Вінрейт', title = 'Залежності вінрейту від кількості й типу дракона') +
  geom_errorbar(aes(x=0.4, ymin = ci_mean_Earth0_Won[1], ymax = ci_mean_Earth0_Won[2]), color = "green", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.4, ymin = ci_mean_Earth1_Won[1], ymax = ci_mean_Earth1_Won[2]), color = "green", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.4, ymin = ci_mean_Earth2_Won[1], ymax = ci_mean_Earth2_Won[2]), color = "green", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.4, ymin = ci_mean_Earth3_Won[1], ymax = ci_mean_Earth3_Won[2]), color = "green", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=4.4, ymin = ci_mean_Earth4_Won[1], ymax = ci_mean_Earth4_Won[2]), color = "green", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=0.3, ymin = ci_mean_Air0_Won[1], ymax = ci_mean_Air0_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.3, ymin = ci_mean_Air1_Won[1], ymax = ci_mean_Air1_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.3, ymin = ci_mean_Air2_Won[1], ymax = ci_mean_Air2_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.3, ymin = ci_mean_Air3_Won[1], ymax = ci_mean_Air3_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=4.3, ymin = ci_mean_Air4_Won[1], ymax = ci_mean_Air4_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=0.2, ymin = ci_mean_Water0_Won[1], ymax = ci_mean_Water0_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.2, ymin = ci_mean_Water1_Won[1], ymax = ci_mean_Water1_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.2, ymin = ci_mean_Water2_Won[1], ymax = ci_mean_Water2_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.2, ymin = ci_mean_Water3_Won[1], ymax = ci_mean_Water3_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=4.2, ymin = ci_mean_Water4_Won[1], ymax = ci_mean_Water4_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=0.1, ymin = ci_mean_Fire0_Won[1], ymax = ci_mean_Fire0_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.1, ymin = ci_mean_Fire1_Won[1], ymax = ci_mean_Fire1_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.1, ymin = ci_mean_Fire2_Won[1], ymax = ci_mean_Fire2_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.1, ymin = ci_mean_Fire3_Won[1], ymax = ci_mean_Fire3_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=4.1, ymin = ci_mean_Fire4_Won[1], ymax = ci_mean_Fire4_Won[2]), color = "red", linewidth=1, width = 0.1) +
  
  annotate("text", x = 4, y = 1.02, label = 'Earth', colour = 'green') + 
  annotate("text", x = 4, y = 0.87, label = 'Air', colour = 'yellow') + 
  annotate("text", x = 4, y = 0.79, label = 'Fire', colour = 'red') + 
  annotate("text", x = 4, y = 0.71, label = 'Water', colour = 'blue')

# Руйнування якого інгібітору найбільше впливає на ймовірність перемоги? ====

inh_kills <- data_by_games[c(2, 13:15)]
top_inh <- (inh_kills[inh_kills$hasWon == 1, ] %>% group_by(destroyedTopInhibitor) %>% count()) /
  (inh_kills %>% group_by(destroyedTopInhibitor) %>% count())
mid_inh <- (inh_kills[inh_kills$hasWon == 1, ] %>% group_by(destroyedMidInhibitor) %>% count()) /
  (inh_kills %>% group_by(destroyedMidInhibitor) %>% count())
bot_inh <- (inh_kills[inh_kills$hasWon == 1, ] %>% group_by(destroyedBotInhibitor) %>% count()) /
  (inh_kills %>% group_by(destroyedBotInhibitor) %>% count())
inh_winrate <- data.frame(cbind(c(0, 1, 2, 3), bot_inh[c(1:4),]$n, mid_inh[]$n, top_inh[c(1:4),]$n))

ggplot(inh_winrate) +
  geom_line(aes(x = X1, y = X2), colour = 'red', linewidth = 1) +
  geom_line(aes(x = X1, y = X3), colour = 'blue', linewidth = 1) +
  geom_line(aes(x = X1, y = X4), colour = 'yellow', linewidth = 1) +
  geom_errorbar(aes(x=0.1, ymin = ci_mean_Bot0_Won[1], ymax = ci_mean_Bot0_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.1, ymin = ci_mean_Bot1_Won[1], ymax = ci_mean_Bot1_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.1, ymin = ci_mean_Bot2_Won[1], ymax = ci_mean_Bot2_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.1, ymin = ci_mean_Bot3_Won[1], ymax = ci_mean_Bot3_Won[2]), color = "red", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=0.2, ymin = ci_mean_Mid0_Won[1], ymax = ci_mean_Mid0_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.2, ymin = ci_mean_Mid1_Won[1], ymax = ci_mean_Mid1_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.2, ymin = ci_mean_Mid2_Won[1], ymax = ci_mean_Mid2_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.2, ymin = ci_mean_Mid3_Won[1], ymax = ci_mean_Mid3_Won[2]), color = "blue", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=0.3, ymin = ci_mean_Top0_Won[1], ymax = ci_mean_Top0_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=1.3, ymin = ci_mean_Top1_Won[1], ymax = ci_mean_Top1_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=2.3, ymin = ci_mean_Top2_Won[1], ymax = ci_mean_Top2_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  geom_errorbar(aes(x=3.3, ymin = ci_mean_Top3_Won[1], ymax = ci_mean_Top3_Won[2]), color = "yellow", linewidth=1, width = 0.1) +
  annotate("text", x = 3, y = 0.82, label = 'Top', colour = 'red') + 
  annotate("text", x = 3, y = 0.54, label = 'Top', colour = 'yellow') + 
  annotate("text", x = 3, y = 0.73, label = 'Mid', colour = 'blue') +
  labs(x = 'Різниця в золоті', y = 'Частота', title = 'Розподіли різниці в золоті в заленості від результату гри')

# Яким чином поточна тривалість гри впливає на зміну різниці в золоті?====

ggplot(data_by_frames, aes(x = goldDiff, y = as.factor(frame))) +
  geom_density_ridges_gradient(scale = 10, fill = "gold") +
  labs(x = 'Різниця в золоті', y = 'Поточна тривалість гри, хв', title = 'Збільшення відхилення різниці золота при\n більшій тривалості гри')
idx = seq(10,56,2)
ggplot() +
  xlim(-20000, 20000) +
  ylim(9, 57) +
  geom_errorbar(aes(y=idx[], xmin=as.numeric(ci_mean_goldDiff_frame[, 1]), xmax=as.numeric(ci_mean_goldDiff_frame[, 2])), color ="red", width = 0.5, linewidth=1) +
  labs(x = 'Різниця в золоті', y = 'Поточна тривалість гри, хв', title = 'Збільшення відхилення різниці золота при\n більшій тривалості гри')

# Чи існує зв'язок (і який?) між кількістю встановлених вардів/вбиствами====

ggplot(data_by_frames, aes(x = wardsPlaced, y = kills)) +
  geom_point(alpha = 0.03) +
  scale_x_continuous(trans = "log10") + 
  labs(x = 'Кількість встановлених вардів', y = 'Кількість вбивств', title = 'Більшій кількості встановених вардів відповідає\n більша кількість вбивств')

# Як КД впливає на тривалість гри?====
ggplot(data_by_games, aes(x = kd, y = gameDuration, colour = hasWon)) +
  geom_point(alpha = 0.05) +
  scale_x_continuous(trans = "log10") +
  labs(x = 'Кд', y = 'Тривалість гра, хв', title = 'Ближчому до 1 кд відповідають довші ігри')


# Як кількість зламаних башт впливає на різницю у золоті?====

ggplot(data_by_frames, aes(x = goldDiff, y = as.factor(towers))) +
  geom_boxplot(fill = "gold") +
  labs(x = 'Різниця в золоті', y = 'Кількість зламаних башт', title = 'Збільшення середньої різниці золота при\n більшій кількості зламаних башт')
ggplot() +
  geom_errorbar(aes(y=0, xmin=ci_median_tower_0[1], xmax=ci_median_tower_0[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=1, xmin=ci_median_tower_1[1], xmax=ci_median_tower_1[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=2, xmin=ci_median_tower_2[1], xmax=ci_median_tower_2[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=3, xmin=ci_median_tower_3[1], xmax=ci_median_tower_3[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=4, xmin=ci_median_tower_4[1], xmax=ci_median_tower_4[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=5, xmin=ci_median_tower_5[1], xmax=ci_median_tower_5[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=6, xmin=ci_median_tower_6[1], xmax=ci_median_tower_6[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=7, xmin=ci_median_tower_7[1], xmax=ci_median_tower_7[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=8, xmin=ci_median_tower_8[1], xmax=ci_median_tower_8[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=9, xmin=ci_median_tower_9[1], xmax=ci_median_tower_9[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=10, xmin=ci_median_tower_10[1], xmax=ci_median_tower_10[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=11, xmin=ci_median_tower_11[1], xmax=ci_median_tower_11[2]), color ="red", width = 0.5, linewidth=1) +
  xlim(-20000,20000) +
  ylim(-1, 12) + 
  labs(x = 'Різниця в золоті', y = 'Кількість зламаних башт', title = 'Збільшення медіани різниці золота при\n більшій кількості зламаних башт')

# Чи існує взаємозв'язок між вбивством Барона та руйнуванням башт?====

ggplot(data_by_frames, aes(x = towers, y = as.factor(killedBaronNashor))) +
  geom_boxplot(fill = 'purple') +
  labs(x = 'Кількість зламаних башт', y = 'Кількість вбитих Баронів', title = 'Збільшення середньої кількості зламаних башт при\n більшій кількості вбитих Баронів*')
ggplot() +
  geom_errorbar(aes(y=0, xmin=ci_median_baron_0[1], xmax=ci_median_baron_0[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=1, xmin=ci_median_baron_1[1], xmax=ci_median_baron_1[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=2, xmin=ci_median_baron_2[1], xmax=ci_median_baron_2[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=3, xmin=ci_median_baron_3[1], xmax=ci_median_baron_3[2]), color ="red", width = 0.5, linewidth=1) +
  geom_errorbar(aes(y=4, xmin=ci_median_baron_4[1], xmax=ci_median_baron_4[2]), color ="red", width = 0.5, linewidth=1) +
  xlim(0,12) +
  ylim(-1, 5) + 
  labs(x = 'Різниця в золоті', y = 'Кількість зламаних башт', title = 'Збільшення медіани різниці золота при\n більшій кількості зламаних башт')

# Як вбивство "ключових об'єктів" впливають на остаточний результат гри?====

ggplot(data_by_games, aes(x = killedBaronNashor, fill = hasWon)) +
  geom_bar(position = 'dodge') +
  scale_y_continuous(trans = "log10") +
  geom_errorbar(aes(x = -0.25, ymin = ci_mean_baron0_Lose[1], ymax = ci_mean_baron0_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 0.25, ymin = ci_mean_baron0_Won[1], ymax = ci_mean_baron0_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 0.75, ymin = ci_mean_baron1_Lose[1], ymax = ci_mean_baron1_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 1.25, ymin = ci_mean_baron1_Won[1], ymax = ci_mean_baron1_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 1.75, ymin = ci_mean_baron2_Lose[1], ymax = ci_mean_baron2_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 2.25, ymin = ci_mean_baron2_Won[1], ymax = ci_mean_baron2_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 2.75, ymin = ci_mean_baron3_Lose[1], ymax = ci_mean_baron3_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 3.25, ymin = ci_mean_baron3_Won[1], ymax = ci_mean_baron3_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 3.75, ymin = ci_mean_baron4_Lose[1], ymax = ci_mean_baron4_Lose[2]), width = 0.3) +
  labs(x = 'Кількість вбитих Баронів', y = 'Кількість ігор', title = 'Більша кількість перемог при більшій\n кількості вбитих Баронів', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = killedElderDrake, fill = hasWon)) +
  geom_bar(position = 'dodge') +
  scale_y_continuous(trans = "log10") +
  geom_errorbar(aes(x = -0.25, ymin = ci_mean_elder0_Lose[1], ymax = ci_mean_elder0_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 0.25, ymin = ci_mean_elder0_Won[1], ymax = ci_mean_elder0_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 0.75, ymin = ci_mean_elder1_Lose[1], ymax = ci_mean_elder1_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 1.25, ymin = ci_mean_elder1_Won[1], ymax = ci_mean_elder1_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 1.75, ymin = ci_mean_elder2_Lose[1], ymax = ci_mean_elder2_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 2.25, ymin = ci_mean_elder2_Won[1], ymax = ci_mean_elder2_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 2.75, ymin = ci_mean_elder3_Lose[1], ymax = ci_mean_elder3_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 3, ymin = ci_mean_elder3_Won[1], ymax = ci_mean_elder3_Won[2]), width = 0.3) +
  labs(x = 'Кількість вбитих старших драконів', y = 'Кількість ігор', title = 'Більша кількість перемог при більшій\n кількості вбитих старших драконів', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = killedRiftHerald, fill = hasWon)) +
  geom_bar(position = 'dodge') +
  geom_errorbar(aes(x = -0.25, ymin = ci_mean_herald0_Lose[1], ymax = ci_mean_herald0_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 0.25, ymin = ci_mean_herald0_Won[1], ymax = ci_mean_herald0_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 0.75, ymin = ci_mean_herald1_Lose[1], ymax = ci_mean_herald1_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 1.25, ymin = ci_mean_herald1_Won[1], ymax = ci_mean_herald1_Won[2]), width = 0.3) +
  geom_errorbar(aes(x = 1.75, ymin = ci_mean_herald2_Lose[1], ymax = ci_mean_herald2_Lose[2]), width = 0.3) +
  geom_errorbar(aes(x = 2.25, ymin = ci_mean_herald2_Won[1], ymax = ci_mean_herald2_Won[2]), width = 0.3) +
  labs(x = 'Кількість вбитих Геральдів', y = 'Кількість ігор', title = 'Більша кількість перемог при більшій\n кількості вбитих Геральдів', fill = 'Виграна гра?')

# Корелляційні коефіцієнти====

ggcorr(rev(data_by_games) %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 3)
ggcorr(rev(data_by_games) %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 3, method = c("pairwise", "spearman"))

ggcorr(data_by_frames %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 4)
ggcorr(data_by_frames %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 4, method = c("pairwise", "spearman"))
