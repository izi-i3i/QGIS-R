#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : sex 30 mai 2025 16:15:17
# License :
# Updated :
#-------------------------------------------
get_stats <- function(x, dig = 4)
{ # get cross validation statistics

  pred = x$observed - x$residual

  # Sum of Squares Error (SSE)
  sserr = attr(var_model, "SSErr")
  # Mean error residual, ideally 0:
  mean_err <- mean(x$residual)
  # Mean Square Prediction Error (MSPE)
  mspe <- mean(x$residual^2)
  # Mean square normalized error
  msne <- mean(x$zscore^2)
  # Correlation observed and predicted
  cor_obs_pred <- cor(x$observed, pred)
  # Correlation predicted and residual 
  cor_pred_resid <- cor(pred, x$residual)
  # Root Mean Square Error (RMSE)
  rmse <- sqrt(sum(x$residual^2)/length(x$residual))

  Stat = c("Sum of Squares Error (SSE)",
           "Mean error residual",
           "Mean Square Prediction Error (MSPE)",
           "Mean square normalized error",
           "Correlation observed and predicted",
           "Correlation predicted and residual",
           "Root Mean Square Error (RMSE)")
  Observed = c(sserr, mean_err, mspe, msne, cor_obs_pred, cor_pred_resid, rmse)
  Ideally = c(0, 0, "small", "close to 1", 1, 0, "small")
  dfe = data.frame(Stat, Observed, Ideally)

  list(stats = round_df(dfe, dig = dig), mean_error_res = mean_err)
}
