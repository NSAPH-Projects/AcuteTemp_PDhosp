library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)
library(ggrepel)
library(cowplot)

dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)

#Remove rows with HI values below 0 
data <- data[data$HI_lag_0 >= 0, ]

#----- clean up race variable for stratification
# remove missing race
data <- data[race != 0,]

# make a new column with race name (group Native American and other)
race_labels <- c("1" = "White", 
                 "2" = "Black", 
                 "3" = "Other",
                 "4" = "Asian",
                 "5" = "Hispanic",
                 "6" = "Other")
data[, race_name := race_labels[as.character(race)]]

# names to loop through
race_names <- unique(race_labels)


# Climate type models -----------------------

# loop through each racial group
for(i in 1:length(race_names)){
  
  # restrict to a single racial group
  cat("Race: ", race_names[i])
  data_onerace <- data[race_name == race_names[i]]
  
  #initial models, no lag
  ##By climate region 
  model_temp <- clogit(case ~ HI_lag_0 + strata(bene_id), data_onerace[Koppen_s =="Temperate"])
  model_trop <- clogit(case ~ HI_lag_0 + strata(bene_id), data_onerace[Koppen_s =="Tropical"])
  model_arid <- clogit(case ~ HI_lag_0 + strata(bene_id), data_onerace[Koppen_s =="Arid"])
  model_cont <- clogit(case ~ HI_lag_0 + strata(bene_id), data_onerace[Koppen_s =="Continental"])
  
  summary(model_temp)
  summary(model_trop)
  summary(model_arid)
  summary(model_cont)
  
  ##US Overall 
  model_lin <- clogit(case ~ HI_lag_0 + strata(bene_id), data_onerace)
  summary(model_lin)
  
  cp <- list()
  for (c in unique(data_onerace[!is.na(Koppen_s), Koppen_s])) {
    cat("\n", c)}
  
  
  #Temperate model
  c <- "Temperate"
  
  lag_dat_perc <- matrix(0.0, data_onerace[Koppen_s == c, .N], 15)
  for (st in unique(data_onerace[Koppen_s == c, Koppen])) {
    ec <- ecdf(data_onerace[Koppen == st]$HI_lag_0)
    lag_dat_perc[which(data_onerace[Koppen_s == c, Koppen] == st), ] <-
      sapply(data_onerace[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
  }
  
  aic <- Inf
  for (df_knot in c(.7, .75, .8, .85, .9)) {
    for (df_lag in 3:5) {
      cat("\n", df_knot, df_lag)
      cb <- crossbasis(lag_dat_perc,
                       lag = 14, 
                       argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                       arglag = list(fun = "cr", 
                                     knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
      model_temperate <- clogit(case ~ cb + strata(bene_id), 
                                  data_onerace[Koppen_s == c])
      
      if (AIC(model_temperate) < aic) {
        cat(" *")
        aic <- AIC(model_temperate)
        cp[[c]] <- crosspred(cb, model_temperate, at = 1:999/1000, cen = 0.5, cumul = TRUE)
        cp[[c]]$df_knot <- df_knot
        cp[[c]]$df_lag <- df_lag
        best_model <- model_temperate
        best_df_knot <- df_knot
        best_df_lag <- df_lag
      }
    }
  }
  summary(model_temperate)
  
  cat("\nknots:", best_df_knot, "\n")
  cat("chosen lag df:", best_df_lag, "\n")
  summary(best_model)
  
  
  
  #Tropical model
  c <- "Tropical"
  
  lag_dat_perc <- matrix(0.0, data_onerace[Koppen_s == c, .N], 15)
  for (st in unique(data_onerace[Koppen_s == c, Koppen])) {
    ec <- ecdf(data_onerace[Koppen == st]$HI_lag_0)
    lag_dat_perc[which(data_onerace[Koppen_s == c, Koppen] == st), ] <-
      sapply(data_onerace[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
  }
  
  aic <- Inf
  for (df_knot in c(.7, .75, .8, .85, .9)) {
    for (df_lag in 3:5) {
      cat("\n", df_knot, df_lag)
      cb <- crossbasis(lag_dat_perc,
                       lag = 14, 
                       argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                       arglag = list(fun = "cr", 
                                     knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
      model_tropical <- clogit(case ~ cb + strata(bene_id), 
                                 data_onerace[Koppen_s == c])
      
      if (AIC(model_tropical) < aic) {
        cat(" *")
        aic <- AIC(model_tropical)
        cp[[c]] <- crosspred(cb, model_tropical, at = 1:999/1000, cen = 0.5, cumul = TRUE)
        cp[[c]]$df_knot <- df_knot
        cp[[c]]$df_lag <- df_lag
        best_model <- model_tropical
        best_df_knot <- df_knot
        best_df_lag <- df_lag
      }
    }
  }
  summary(model_tropical)
  
  cat("\nknots:", best_df_knot, "\n")
  cat("chosen lag df:", best_df_lag, "\n")
  summary(best_model)
  
  
  #Arid model
  c <- "Arid"
  
  lag_dat_perc <- matrix(0.0, data_onerace[Koppen_s == c, .N], 15)
  for (st in unique(data_onerace[Koppen_s == c, Koppen])) {
    ec <- ecdf(data_onerace[Koppen == st]$HI_lag_0)
    lag_dat_perc[which(data_onerace[Koppen_s == c, Koppen] == st), ] <-
      sapply(data_onerace[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
  }
  
  aic <- Inf
  for (df_knot in c(.7, .75, .8, .85, .9)) {
    for (df_lag in 3:5) {
      cat("\n", df_knot, df_lag)
      cb <- crossbasis(lag_dat_perc,
                       lag = 14, 
                       argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                       arglag = list(fun = "cr", 
                                     knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
      model_Arid <- clogit(case ~ cb + strata(bene_id), 
                             data_onerace[Koppen_s == c])
      
      if (AIC(model_Arid) < aic) {
        cat(" *")
        aic <- AIC(model_Arid)
        cp[[c]] <- crosspred(cb, model_Arid, at = 1:999/1000, cen = 0.5, cumul = TRUE)
        cp[[c]]$df_knot <- df_knot
        cp[[c]]$df_lag <- df_lag
        best_model <- model_Arid
        best_df_knot <- df_knot
        best_df_lag <- df_lag
      }
    }
  }
  summary(model_Arid)
  
  cat("\nknots:", best_df_knot, "\n")
  cat("chosen lag df:", best_df_lag, "\n")
  summary(best_model)
  
  
  
  #Continental model
  c <- "Continental"
  
  lag_dat_perc <- matrix(0.0, data_onerace[Koppen_s == c, .N], 15)
  for (st in unique(data_onerace[Koppen_s == c, Koppen])) {
    ec <- ecdf(data_onerace[Koppen == st]$HI_lag_0)
    lag_dat_perc[which(data_onerace[Koppen_s == c, Koppen] == st), ] <-
      sapply(data_onerace[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
  }
  
  aic <- Inf
  for (df_knot in c(.7, .75, .8, .85, .9)) {
    for (df_lag in 3:5) {
      cat("\n", df_knot, df_lag)
      cb <- crossbasis(lag_dat_perc,
                       lag = 14, 
                       argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                       arglag = list(fun = "cr", 
                                     knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
      model_Continental  <- clogit(case ~ cb + strata(bene_id), 
                                     data_onerace[Koppen_s == c])
      
      if (AIC(model_Continental) < aic) {
        cat(" *")
        aic <- AIC(model_Continental)
        cp[[c]] <- crosspred(cb, model_Continental, at = 1:999/1000, cen = 0.5, cumul = TRUE)
        cp[[c]]$df_knot <- df_knot
        cp[[c]]$df_lag <- df_lag
        best_model <- model_Continental
        best_df_knot <- df_knot
        best_df_lag <- df_lag
      }
    }
  }
  summary(model_Continental)
  
  
  cat("\nknots:", best_df_knot, "\n")
  cat("chosen lag df:", best_df_lag, "\n")
  summary(best_model)
  
  
  # Nationwide: all climate types
  lag_dat_perc2 <- matrix(0.0, data_onerace[, .N], 15)
  for (st in unique(data_onerace[!is.na(Koppen), Koppen])) {
    ec <- ecdf(data_onerace[Koppen == st]$HI_lag_0)
    lag_dat_perc2[which(data_onerace[, Koppen] == st), ] <-
      sapply(data_onerace[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
  }
  c <- c("Temperate", "Tropical", "Arid", "Continental")
  
  aic <- Inf
  cp_all <- list()
  for (df_knot in c(.7, .75, .8, .85, .9)) {
    for (df_lag in 3:5) {
      cat("\n", df_knot, df_lag)
      cb <- crossbasis(lag_dat_perc2,
                       lag = 14, 
                       argvar = list(fun = "cr", knots = c(0, df_knot, 1)),
                       arglag = list(fun = "cr", 
                                     knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
      model_nat <- clogit(case ~ cb + strata(bene_id), data_onerace)
      
      if (AIC(model_nat) < aic) {
        cat(" *")
        aic <- AIC(model_nat)
        cp_all <- crosspred(cb, model_nat, at = 1:999/1000, cen = 0.5, cumul = TRUE)
        cp_all$df_lag <- df_lag
        cp_all$df_knot <- df_knot
        best_model <- model_nat
        best_df_knot <- df_knot
        best_df_lag <- df_lag
      }
    }
  }
  summary(model_nat)
  
  cat("\nknots:", best_df_knot, "\n")
  cat("chosen lag df:", best_df_lag, "\n")
  summary(best_model)
  
  save(cp, cp_all, file = paste0("data/aic_models_", race_names[i], ".rda"))
  
  gc()
}


