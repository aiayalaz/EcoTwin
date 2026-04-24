# Ana I. Ayala (isabel.ayala.zamora@ebc.uu.se)
# 2026-04-14

# performance metrics
statistical_performance_metrics <- function(obs, sim) {
  # mean
  mean_obs <- mean(obs, na.rm=TRUE)
  mean_sim <- mean(sim, na.rm=TRUE)
  # standard deviation
  sd_obs <- sd(obs, na.rm=TRUE)
  sd_sim <- sd(sim, na.rm=TRUE)
  #  bias
  bias <- mean((sim-obs), na.rm=TRUE)
  # root mean square error
  rmse <- sqrt(mean((obs-sim)^2, na.rm=TRUE))
  # correlation coef
  r <- sum((obs - mean(obs, na.rm=TRUE))*(sim - mean(sim, na.rm=TRUE)), na.rm=TRUE)/sqrt(sum((obs - mean(obs, na.rm=TRUE))^2, na.rm=TRUE)*sum((sim - mean(sim, na.rm=TRUE))^2, na.rm=TRUE))
  # nash sutcliff efficiency
  nse <- 1 - sum((obs-sim)^2, na.rm=TRUE)/sum((obs - mean(obs, na.rm=TRUE))^2, na.rm=TRUE)
  df <- data.frame(mean_obs=mean_obs, sd_obs=sd_obs, mean_sim=mean_sim, sd_sim=sd_sim, bias=bias, rmse=rmse, r=r, nse=nse, n=length(obs))
  return(df)
}
# figures
scatter_fig <- function(dt, var, cal_period, val_period) {
  obs_col <- paste0(var, "_obs")
  dis_col <- paste0(var, "_dis")
  
  dt_cal <- dt[datetime >= cal_period[1] & datetime <= cal_period[2], .SD, .SDcols = c(obs_col, dis_col)]
  dt_val <- dt[datetime >= val_period[1] & datetime <= val_period[2], .SD, .SDcols = c(obs_col, dis_col)]
  
  pm_cal <- statistical_performance_metrics(dt_cal[[obs_col]], dt_cal[[dis_col]])
  pm_val <- statistical_performance_metrics(dt_val[[obs_col]], dt_val[[dis_col]])
  
  par(mfrow = c(1, 2)) 
  # cal
  plot(dt_cal[[obs_col]], dt_cal[[dis_col]], pch = 19, col = "#003366", main = paste0(var, " (cal)"), xlab = "obs", ylab = "dis")
  abline(0, 1, lwd = 1)
  mtext(paste0("RMSE=", round(pm_cal$rmse, 2), ", ", "R=", round(pm_cal$r, 2), ", ", "NSE=", round(pm_cal$nse, 2)), 3, line=-1, col="black", cex=0.8)
  # val
  plot(dt_val[[obs_col]], dt_val[[dis_col]], pch = 19, col = "#990000", main = paste0(var, " (val)"), xlab = "obs", ylab = "dis")
  abline(0, 1, lwd = 1)
  mtext(paste0("RMSE=", round(pm_val$rmse, 2), ", ", "R=", round(pm_val$r, 2), ", ", "NSE=", round(pm_val$nse, 2)), 3, line=-1, col="black", cex=0.8)
}

plot_fig <- function(dt, var, period) {
  obs_col <- paste0(var, "_obs")
  dis_col <- paste0(var, "_dis")
  
  dt_period <- dt[datetime >= period[1] & datetime <= period[2], .SD, .SDcols = c("datetime", obs_col, dis_col)]
  
  par(mfrow = c(1, 1)) 
  plot(dt_period$datetime, dt_period[[obs_col]], type="l", col = "black", lwd=2,  xlab = "", ylab = var)
  points(dt_period$datetime, dt_period[[dis_col]], col = "#990000", pch=19)
  legend("topright", legend = c("dis", "obs"), col = c("#990000", "black"), lwd = c(NA, 2), pch = c(19, NA), bty = "n")
}
