library(dplyr)
library(boot)

#CI for mean ====
n_by_games = nrow(data_by_games)
ci_mean_by_games <- data.frame(t(statistics_by_games[]))
colnames(ci_mean_by_games) <- ci_mean_by_games[1,]
ci_mean_by_games <- ci_mean_by_games[-1, c(1,2)]

ci_mean_by_games <- ci_mean_by_games %>% mutate(a = as.numeric(mean) + qnorm(0.025)*as.numeric(sd) / sqrt(n_by_games))
ci_mean_by_games <- ci_mean_by_games %>% mutate(b = as.numeric(mean) + qnorm(0.975)*as.numeric(sd) / sqrt(n_by_games))

n_by_frames = nrow(data_by_frames)
ci_mean_by_frames <- data.frame(t(statistics_by_frames[]))
colnames(ci_mean_by_frames) <- ci_mean_by_frames[1,]
ci_mean_by_frames <- ci_mean_by_frames[-1, c(1,2)]

ci_mean_by_frames <- ci_mean_by_frames %>% mutate(a = as.numeric(mean) + qnorm(0.025)*as.numeric(sd) / sqrt(n_by_frames))
ci_mean_by_frames <- ci_mean_by_frames %>% mutate(b = as.numeric(mean) + qnorm(0.975)*as.numeric(sd) / sqrt(n_by_frames))

#CI for median ====
B <- 1000
set.seed(100)
bootmedian <- function(x, indices){
  return (median(x[indices]))
}
cimedian <- function(x){
  bootmedian_res = boot(x, statistic = bootmedian, R = B)
  
  return (c(median(x) + qnorm(0.025)*sd(bootmedian_res$t),
            median(x) + qnorm(0.975)*sd(bootmedian_res$t)))
}


ci_median_gameDuration <- cimedian(data_by_games$gameDuration)
ci_median_goldDiff <- cimedian(data_by_games$goldDiff)
ci_median_expDiff <- cimedian(data_by_games$expDiff)
ci_median_kills <- cimedian(data_by_games$kills)
ci_median_deaths <- cimedian(data_by_games$deaths)
ci_median_kd <- cimedian(data_by_games$kd)

ci_median_frame = cimedian(data_by_frames$frame)
ci_median_goldDiff2 = cimedian(data_by_frames$goldDiff)
ci_median_wardsPlaced = cimedian(data_by_frames$wardsPlaced)

cimean <- function(x){
  return (c(mean(x) + qnorm(0.025) *sd(x) / sqrt(length(x)),
            mean(x) + qnorm(0.975) *sd(x) / sqrt(length(x))))
}
#CIs for 1q====
ci_mean_goldDiff_Won <- cimean(data_by_games[data_by_games$hasWon == 1,]$goldDiff)
ci_mean_expDiff_Won <- cimean(data_by_games[data_by_games$hasWon == 1,]$expDiff)
ci_mean_kills_Won <- cimean(data_by_games[data_by_games$hasWon == 1,]$kills)
ci_mean_deaths_Won <- cimean(data_by_games[data_by_games$hasWon == 1,]$deaths)
#CIs for 2q====
ci_mean_Fire0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedFireDrake == 0,]$hasWon)-1)
ci_mean_Fire1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedFireDrake == 1,]$hasWon)-1)
ci_mean_Fire2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedFireDrake == 2,]$hasWon)-1)
ci_mean_Fire3_Won <- cimean(as.numeric(data_by_games[data_by_games$killedFireDrake == 3,]$hasWon)-1)
ci_mean_Fire4_Won <- cimean(as.numeric(data_by_games[data_by_games$killedFireDrake == 4,]$hasWon)-1)

ci_mean_Air0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedAirDrake == 0,]$hasWon)-1)
ci_mean_Air1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedAirDrake == 1,]$hasWon)-1)
ci_mean_Air2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedAirDrake == 2,]$hasWon)-1)
ci_mean_Air3_Won <- cimean(as.numeric(data_by_games[data_by_games$killedAirDrake == 3,]$hasWon)-1)
ci_mean_Air4_Won <- cimean(as.numeric(data_by_games[data_by_games$killedAirDrake == 4,]$hasWon)-1)

ci_mean_Water0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedWaterDrake == 0,]$hasWon)-1)
ci_mean_Water1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedWaterDrake == 1,]$hasWon)-1)
ci_mean_Water2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedWaterDrake == 2,]$hasWon)-1)
ci_mean_Water3_Won <- cimean(as.numeric(data_by_games[data_by_games$killedWaterDrake == 3,]$hasWon)-1)
ci_mean_Water4_Won <- cimean(as.numeric(data_by_games[data_by_games$killedWaterDrake == 4,]$hasWon)-1)

ci_mean_Earth0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedEarthDrake == 0,]$hasWon)-1)
ci_mean_Earth1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedEarthDrake == 1,]$hasWon)-1)
ci_mean_Earth2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedEarthDrake == 2,]$hasWon)-1)
ci_mean_Earth3_Won <- cimean(as.numeric(data_by_games[data_by_games$killedEarthDrake == 3,]$hasWon)-1)
ci_mean_Earth4_Won <- cimean(as.numeric(data_by_games[data_by_games$killedEarthDrake == 4,]$hasWon)-1)
#CIs for 3q====
ci_mean_Bot0_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 0,]$hasWon)-1)
ci_mean_Bot1_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 1,]$hasWon)-1)
ci_mean_Bot2_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 2,]$hasWon)-1)
ci_mean_Bot3_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 3,]$hasWon)-1)

ci_mean_Mid0_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 0,]$hasWon)-1)
ci_mean_Mid1_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 1,]$hasWon)-1)
ci_mean_Mid2_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 2,]$hasWon)-1)
ci_mean_Mid3_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 3,]$hasWon)-1)

ci_mean_Top0_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 0,]$hasWon)-1)
ci_mean_Top1_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 1,]$hasWon)-1)
ci_mean_Top2_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 2,]$hasWon)-1)
ci_mean_Top3_Won <- cimean(as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 3,]$hasWon)-1)
#CIs for 4q====
ci_mean_goldDiff_frame <- data.frame('a', 'b')
for (f in seq(10,56,2)){
  ci_mean_goldDiff_frame[(f-8)/2,] = cimean(data_by_frames[data_by_frames$frame == f,]$goldDiff)
}
#CIs for 7q====
ci_median_tower_0 <- cimedian(data_by_frames[data_by_frames$towers == 0,]$goldDiff)
ci_median_tower_1 <- cimedian(data_by_frames[data_by_frames$towers == 1,]$goldDiff)
ci_median_tower_2 <- cimedian(data_by_frames[data_by_frames$towers == 2,]$goldDiff)
ci_median_tower_3 <- cimedian(data_by_frames[data_by_frames$towers == 3,]$goldDiff)
ci_median_tower_4 <- cimedian(data_by_frames[data_by_frames$towers == 4,]$goldDiff)
ci_median_tower_5 <- cimedian(data_by_frames[data_by_frames$towers == 5,]$goldDiff)
ci_median_tower_6 <- cimedian(data_by_frames[data_by_frames$towers == 6,]$goldDiff)
ci_median_tower_7 <- cimedian(data_by_frames[data_by_frames$towers == 7,]$goldDiff)
ci_median_tower_8 <- cimedian(data_by_frames[data_by_frames$towers == 8,]$goldDiff)
ci_median_tower_9 <- cimedian(data_by_frames[data_by_frames$towers == 9,]$goldDiff)
ci_median_tower_10 <- cimedian(data_by_frames[data_by_frames$towers == 10,]$goldDiff)
ci_median_tower_11 <- cimedian(data_by_frames[data_by_frames$towers == 11,]$goldDiff)
#CIs for 8q====
ci_median_baron_0 <- cimedian(data_by_frames[data_by_frames$killedBaronNashor == 0,]$towers)
ci_median_baron_1 <- cimedian(data_by_frames[data_by_frames$killedBaronNashor == 1,]$towers)
ci_median_baron_2 <- cimedian(data_by_frames[data_by_frames$killedBaronNashor == 2,]$towers)
ci_median_baron_3 <- cimedian(data_by_frames[data_by_frames$killedBaronNashor == 3,]$towers)
ci_median_baron_4 <- cimedian(data_by_frames[data_by_frames$killedBaronNashor == 4,]$towers)

#CIs for 9q====
#baron
n_baron0 = nrow(data_by_games[data_by_games$killedBaronNashor == 0,])
ci_mean_baron0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 0,]$hasWon)-1) * n_baron0
ci_mean_baron0_Lose <- n_baron0-cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 0,]$hasWon)-1) * n_baron0
ci_mean_baron0_Lose <- rev(ci_mean_baron0_Lose)

n_baron1 = nrow(data_by_games[data_by_games$killedBaronNashor == 1,])
ci_mean_baron1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 1,]$hasWon)-1) * n_baron1
ci_mean_baron1_Lose <- n_baron1-cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 1,]$hasWon)-1) * n_baron1
ci_mean_baron1_Lose <- rev(ci_mean_baron1_Lose)

n_baron2 = nrow(data_by_games[data_by_games$killedBaronNashor == 2,])
ci_mean_baron2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 2,]$hasWon)-1) * n_baron2
ci_mean_baron2_Lose <- n_baron2-cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 2,]$hasWon)-1) * n_baron2
ci_mean_baron2_Lose <- rev(ci_mean_baron2_Lose)

n_baron3 = nrow(data_by_games[data_by_games$killedBaronNashor == 3,])
ci_mean_baron3_Won <- cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 3,]$hasWon)-1) * n_baron3
ci_mean_baron3_Lose <- n_baron3-cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 3,]$hasWon)-1) * n_baron3
ci_mean_baron3_Lose <- rev(ci_mean_baron3_Lose)

n_baron4 = nrow(data_by_games[data_by_games$killedBaronNashor == 4,])
ci_mean_baron4_Lose <- n_baron4-cimean(as.numeric(data_by_games[data_by_games$killedBaronNashor == 4,]$hasWon)-1) * n_baron4
ci_mean_baron4_Lose <- rev(ci_mean_baron4_Lose)
#elder
n_elder0 = nrow(data_by_games[data_by_games$killedElderDrake == 0,])
ci_mean_elder0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 0,]$hasWon)-1) * n_elder0
ci_mean_elder0_Lose <- n_elder0-cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 0,]$hasWon)-1) * n_elder0
ci_mean_elder0_Lose <- rev(ci_mean_elder0_Lose)

n_elder1 = nrow(data_by_games[data_by_games$killedElderDrake == 1,])
ci_mean_elder1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 1,]$hasWon)-1) * n_elder1
ci_mean_elder1_Lose <- n_elder1-cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 1,]$hasWon)-1) * n_elder1
ci_mean_elder1_Lose <- rev(ci_mean_elder1_Lose)

n_elder2 = nrow(data_by_games[data_by_games$killedElderDrake == 2,])
ci_mean_elder2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 2,]$hasWon)-1) * n_elder2
ci_mean_elder2_Lose <- n_elder2-cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 2,]$hasWon)-1) * n_elder2
ci_mean_elder2_Lose <- rev(ci_mean_elder2_Lose)

n_elder3 = nrow(data_by_games[data_by_games$killedElderDrake == 3,])
ci_mean_elder3_Won <- cimean(as.numeric(data_by_games[data_by_games$killedElderDrake == 3,]$hasWon)-1) * n_elder3
#gerald
n_herald0 = nrow(data_by_games[data_by_games$killedRiftHerald == 0,])
ci_mean_herald0_Won <- cimean(as.numeric(data_by_games[data_by_games$killedRiftHerald == 0,]$hasWon)-1) * n_herald0
ci_mean_herald0_Lose <- n_herald0-cimean(as.numeric(data_by_games[data_by_games$killedRiftHerald == 0,]$hasWon)-1) * n_herald0
ci_mean_herald0_Lose <- rev(ci_mean_herald0_Lose)

n_herald1 = nrow(data_by_games[data_by_games$killedRiftHerald == 1,])
ci_mean_herald1_Won <- cimean(as.numeric(data_by_games[data_by_games$killedRiftHerald == 1,]$hasWon)-1) * n_herald1
ci_mean_herald1_Lose <- n_herald1-cimean(as.numeric(data_by_games[data_by_games$killedRiftHerald == 1,]$hasWon)-1) * n_herald1
ci_mean_herald1_Lose <- rev(ci_mean_herald1_Lose)

n_herald2 = nrow(data_by_games[data_by_games$killedRiftHerald == 2,])
ci_mean_herald2_Won <- cimean(as.numeric(data_by_games[data_by_games$killedRiftHerald == 2,]$hasWon)-1) * n_herald2
ci_mean_herald2_Lose <- n_herald2-cimean(as.numeric(data_by_games[data_by_games$killedRiftHerald == 2,]$hasWon)-1) * n_herald2
ci_mean_herald2_Lose <- rev(ci_mean_herald2_Lose)


#CIs for corr====
boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 20, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}
B <- 200

Xq4 <- cbind(data_by_frames$frame, data_by_frames$goldDiff)
Xq5 <- cbind(data_by_frames$wardsPlaced, data_by_frames$kills)
Xq6 <- cbind(data_by_games$kd, data_by_games$gameDuration)
Xq7 <- cbind(data_by_frames$towers, data_by_frames$goldDiff)
Xq8 <- cbind(data_by_frames$towers, data_by_frames$killedBaronNashor)

boot_result_corq4 <- boot(Xq4, statistic = boot_cor_with_sd, R = B)
boot_result_corq5 <- boot(Xq5, statistic = boot_cor_with_sd, R = B)
boot_result_corq6 <- boot(Xq6, statistic = boot_cor_with_sd, R = B)
boot_result_corq7 <- boot(Xq7, statistic = boot_cor_with_sd, R = B)
boot_result_corq8 <- boot(Xq8, statistic = boot_cor_with_sd, R = B)

ci_cor_q4 <- boot.ci(boot_result_corq4, index = c(1, 2), type = c("norm", "basic", "perc", "stud"))
ci_cor_q5 <- boot.ci(boot_result_corq5, index = c(1, 2), type = c("norm", "basic", "perc", "stud"))
ci_cor_q6 <- boot.ci(boot_result_corq6, index = c(1, 2), type = c("norm", "basic", "perc", "stud"))
ci_cor_q7 <- boot.ci(boot_result_corq7, index = c(1, 2), type = c("norm", "basic", "perc", "stud"))
ci_cor_q8 <- boot.ci(boot_result_corq8, index = c(1, 2), type = c("norm", "basic", "perc", "stud"))



