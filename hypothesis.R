testhypo1 <- function(x, thetha){
  m = mean(x)
  se = sd(x) / sqrt(length(n))
  T <- (m - thetha) / sd(x)
  
  p_value <- pnorm(T, lower.tail = FALSE)
  conf.int <- c(mean(x) - thetha - qnorm(0.95)*se, Inf)
  print('p_value')
  print(p_value)
  print('conf int')
  print(conf.int)
}
testhypo2 <- function(x, y, a){
  mx = mean(x)
  my = mean(y)
  
  varx = var(x) / length(x)
  vary = var(y) / length(y)
  se = sqrt(varx+vary)
  
  T <- (mx - my) / se
  
  p_value <- pnorm(T, lower.tail = FALSE)
  conf.int <- c(mx - my - qnorm(1 - a)*se, Inf)
  print('p_value')
  print(p_value)
  print('conf int')
  print(conf.int)
}
testhypo3 <- function(x, y, a){
  mx = mean(x)
  my = mean(y)
  
  varx = var(x) / length(x)
  vary = var(y) / length(y)
  se = sqrt(varx+vary)
  
  T <- (mx - my) / se
  
  p_value <- pnorm(T)
  return (c(p_value, mx - my - qnorm(1 - a/2)*se, mx - my + qnorm(1 - a/2)*se))
}
bootmedian_diff <- function(x, indices){
  return (median(x[indices, 1]) - median(x[indices, 2]))
}
testhypo4 <- function(x, y, a){
  mx = median(x)
  my = median(y)
  
  bootmedian_diff_res = boot(cbind(x, y), statistic = bootmedian_diff, R = 200)
  se = sd(bootmedian_diff_res$t)
  
  T <- (mx - my) / se
  
  p_value <- pnorm(T, lower.tail = FALSE)
  return (c(p_value, mx - my - qnorm(1 - a)*se, Inf))
}
#q1====
# Нульова гіпотеза Н0: чатска виграних ігор де goldDiff > 0   <=0.5
testhypo1(as.numeric(data_by_games[data_by_games$goldDiff > 0,]$hasWon)-1, 0.5)
# Нульова гіпотеза Н0: чатска виграних ігор де expDiff > 0   <=0.5
testhypo1(as.numeric(data_by_games[data_by_games$expDiff > 0,]$hasWon)-1, 0.5)
# Нульова гіпотеза Н0: чатска виграних ігор де kills > deaths    <=0.5
testhypo1(as.numeric(data_by_games[data_by_games$kills > data_by_games$deaths,]$hasWon)-1, 0.5)

#q2====
# Нульова гіпотеза Н0: чатска виграних ігор з 1 вбитим WaterDrake <= аналогічної частки з ігшим драконом
testhypo2(as.numeric(data_by_games[data_by_games$killedWaterDrake == 1,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedFireDrake == 1,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedWaterDrake == 1,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedAirDrake == 1,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedWaterDrake == 1,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedEarthDrake == 1,]$hasWon)-1, 0.05/3)

# Нульова гіпотеза Н0: чатска виграних ігор з 2 вбитими FireDrake <= аналогічної частки з ігшим драконом
testhypo2(as.numeric(data_by_games[data_by_games$killedFireDrake == 2,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedWaterDrake == 2,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedFireDrake == 2,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedAirDrake == 2,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedFireDrake == 2,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedEarthDrake == 2,]$hasWon)-1, 0.05/3)

# Нульова гіпотеза Н0: чатска виграних ігор з 3 вбитими FireDrake <= аналогічної частки з ігшим драконом
testhypo2(as.numeric(data_by_games[data_by_games$killedFireDrake == 3,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedWaterDrake == 3,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedFireDrake == 3,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedAirDrake == 3,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedFireDrake == 3,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedEarthDrake == 3,]$hasWon)-1, 0.05/3)

# Нульова гіпотеза Н0: чатска виграних ігор з 4 вбитими EarthDrake <= аналогічної частки з ігшим драконом
testhypo2(as.numeric(data_by_games[data_by_games$killedEarthDrake == 4,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedWaterDrake == 4,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedEarthDrake == 4,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedAirDrake == 4,]$hasWon)-1, 0.05/3)
testhypo2(as.numeric(data_by_games[data_by_games$killedEarthDrake == 4,]$hasWon)-1, as.numeric(data_by_games[data_by_games$killedFireDrake == 4,]$hasWon)-1, 0.05/3)

#q3====
# Нульова гіпотеза Н0: чатска виграних ігор з 1 зруйнованим MidInh менша за аналогічні частки з іншими інгібіторами
testhypo2(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 1,]$hasWon)-1, as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 1,]$hasWon)-1, 0.05/2)
testhypo2(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 1,]$hasWon)-1, as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 1,]$hasWon)-1, 0.05/2)

# Нульова гіпотеза Н0: чатска виграних ігор з 2 зруйнованим TopInh менша за аналогічні частки з іншими інгібіторами
testhypo2(as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 2,]$hasWon)-1, as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 2,]$hasWon)-1, 0.05/2)
testhypo2(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 2,]$hasWon)-1, as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 2,]$hasWon)-1, 0.05/2)

# Нульова гіпотеза Н0: чатска виграних ігор з 3 зруйнованим TopInh менша за аналогічні частки з іншими інгібіторами
testhypo2(as.numeric(data_by_games[data_by_games$destroyedTopInhibitor == 3,]$hasWon)-1, as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 3,]$hasWon)-1, 0.05/2)
testhypo2(as.numeric(data_by_games[data_by_games$destroyedMidInhibitor == 3,]$hasWon)-1, as.numeric(data_by_games[data_by_games$destroyedBotInhibitor == 3,]$hasWon)-1, 0.05/2)

#q4====
hyp_mean_goldDiff_frame <- data.frame('frame', 'p_value', 'a', 'b')
for (f in seq(12,56,2)){
  hyp_mean_goldDiff_frame[(f-8)/2,] = c(f, testhypo3(data_by_frames[data_by_frames$frame == f,]$goldDiff, data_by_frames[data_by_frames$frame == f-2,]$goldDiff, 0.05/23))
}
#q6====
# Нульова гіпотеза Н0: чатска виграних ігор де kd > 1   <=0.5
testhypo1(as.numeric(data_by_games[data_by_games$kd > 1,]$hasWon)-1, 0.5)

#q7====
set.seed(100)
hyp_mean_goldDiff_tower <- data.frame('tower', 'p_value', 'a', 'b')
for (t in seq(1, 11, 1)){
  hyp_mean_goldDiff_tower[t+1,] = c(t, testhypo4(data_by_frames[data_by_frames$towers == t,]$goldDiff, data_by_frames[data_by_frames$towers == t-1,]$goldDiff, 0.05/10))
}
#q8====

hyp_mean_tower_baron <- data.frame('barons', 'p_value', 'a', 'b')
for (b in seq(1, 4, 1)){
  hyp_mean_tower_baron[b+1,] = c(b, testhypo4(data_by_frames[data_by_frames$killedBaronNashor == b,]$towers, data_by_frames[data_by_frames$killedBaronNashor == b-1,]$towers, 0.05/4))
}

