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
scatter_fig <- function(dt, var) {
  obs_col <- paste0(var, "_obs")
  dis_col <- paste0(var, "_dis")
  
  pm <- statistical_performance_metrics(dt[[obs_col]], dt[[dis_col]])
  
  par(mfrow = c(1, 1)) 
  plot(dt[[obs_col]], dt[[dis_col]], pch = 19, col = "#990000", main = var, xlab = "obs", ylab = "dis")
  abline(0, 1, lwd = 1)
  mtext(paste0("RMSE=", round(pm$rmse, 2), ", ", "R=", round(pm$r, 2), ", ", "NSE=", round(pm$nse, 2)), 3, line=-1, col="black", cex=0.8)
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
