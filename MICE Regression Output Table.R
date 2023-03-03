##############################################################
# MICE Regression Output Table Function                      #
# First Written Date: 3/2/23                                 #
# Last Edited: 3/2/23                                        #
# Author: Michael Becker                                     #
# Input: OLS model run via lm.mids that has been 'pooled'    #
##############################################################

library(flextable); library(tidyverse); library(mice)

mice_regression_table<-function(pooled_MICE_object){
  m<-pooled_MICE_object$m
  n<-mean(pooled_MICE_object$glanced$nobs)
  mean_r2<-mean(pooled_MICE_object$glanced$r.squared)
  mean_adj.r2<-mean(pooled_MICE_object$glanced$adj.r.squared)
  mean_model_p<-mean(pooled_MICE_object$glanced$p.value)
  predictors<-pooled_MICE_object$pooled$term
  coefficients<-pooled_MICE_object$pooled$estimate
  df<-pooled_MICE_object$pooled$df
  std_coefficient<-pooled_MICE_object$pooled$b
  t_stat<-summary(pooled_MICE_object)$statistic
  fmi<-pooled_MICE_object$pooled$fmi
  mean_log_likelihood<-mean(pooled_MICE_object$glanced$logLik)
  mean_deviance<-mean(pooled_MICE_object$glanced$deviance)
  coef_std_error<-summary(pooled_MICE_object)$std.error
  coef_p_value<-summary(pooled_MICE_object)$p.value
  attributes<-list("Predictor"=predictors,
                   "Coef." = coefficients,
                   "Std.Error" = coef_std_error,
                   "t.value"=t_stat,
                   "p.value" = coef_p_value,
                   "Standardized Coef."=std_coefficient,
                   "df" = df,
                   "FMI"=fmi,
                   "m"= m,
                   "N"=n,
                   "LogLik"=mean_log_likelihood,
                   "Deviance"=mean_deviance,
                   "R2"=mean_r2,
                   "Adj. R2"=mean_adj.r2)
  df<-as.data.frame(attributes[1:8])
  mice_regression_details<-list(df, attributes[9:14])
  mice_regression_table<-flextable(df[c(2:nrow(df),1),1:5],
                                   col_keys = c("Predictor",
                                                "Coef.",
                                                "sig",
                                                "Std.Error"))%>%
    width(j = "sig",
          width = 0.2)%>%
    colformat_double(digits = 3)%>%
    append_chunks(
      i = ~ p.value < 0.01, 
      j = "sig",
      as_chunk("*"))%>%
    append_chunks(
      i = ~ p.value < 0.05, 
      j = "sig",
      as_chunk("*"))%>%
    theme_vanilla()%>%
    add_footer_lines(c(paste("N: ",
                             attributes$N,
                             "; Mean R-Squared: ",
                             round(attributes$R2,
                                   3),
                             "; p < 0.05 *; p < 0.01 **"),
                       paste("Model estimated with m =",
                             attributes$m,
                             " imputed datasets")))%>%
    set_caption(caption = "Multiply Imputed Ordinary Least Squares Model")%>%
    autofit()
  return(mice_regression_table)
}
