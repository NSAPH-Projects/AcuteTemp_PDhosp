#necessary libraries
library(dplyr)
library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)
library(ggrepel)
library(cowplot)

dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)

#Edit the dataset to remove rows with HI values below 0 
data <- data[data$HI_lag_0 >= 0, ]

model_cont_air <- clogit(case ~ HI_lag_0 + pm25 + no2 + ozone + strata(bene_id), data[Koppen_s =="Continental"])
model_temp_air <- clogit(case ~ HI_lag_0 + pm25 + no2 + ozone + strata(bene_id), data[Koppen_s =="Temperate"])
model_trop_air <- clogit(case ~ HI_lag_0 + pm25 + no2 + ozone + strata(bene_id), data[Koppen_s =="Tropical"])
model_arid_air <- clogit(case ~ HI_lag_0 + pm25 + no2 + ozone + strata(bene_id), data[Koppen_s =="Arid"])

summary(model_temp_air)
summary(model_trop_air)
summary(model_arid_air)
summary(model_cont_air)

##US overall with AQI predictors 

model_air <- clogit(case ~ HI_lag_0 + pm25 + no2 + ozone + strata(bene_id), data)

cp_air <- list()
for (c in unique(data[!is.na(Koppen_s), Koppen_s])) {
  cat("\n", c)}

#Temperate model
c <- "Temperate"

lag_dat_perc <- matrix(0.0, data[Koppen_s == c, .N], 15)
for (st in unique(data[Koppen_s == c, Koppen])) {
  ec <- ecdf(data[Koppen == st]$HI_lag_0)
  lag_dat_perc[which(data[Koppen_s == c, Koppen] == st), ] <-
    sapply(data[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
}

df_knot <- 0.9
df_lag <- 3

cb <- crossbasis(lag_dat_perc,
                 lag = 14, 
                 argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                 arglag = list(fun = "cr", 
                               knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
model_temperate_air <- clogit(case ~ cb + pm25 + no2 + ozone + strata(bene_id), 
                            data[Koppen_s == c])
cp_air[[c]] <- crosspred(cb, model_temperate_air, at = 1:999/1000, cen = 0.5, cumul = TRUE)
cp_air[[c]]$df_knot <- df_knot
cp_air[[c]]$df_lag <- df_lag
summary(model_temperate_air)



#Tropical model
c <- "Tropical"
lag_dat_perc <- matrix(0.0, data[Koppen_s == c, .N], 15)
for (st in unique(data[Koppen_s == c, Koppen])) {
  ec <- ecdf(data[Koppen == st]$HI_lag_0)
  lag_dat_perc[which(data[Koppen_s == c, Koppen] == st), ] <-
    sapply(data[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
}

df_knot <- 0.7
df_lag <- 3

cb <- crossbasis(lag_dat_perc,
                 lag = 14, 
                 argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                 arglag = list(fun = "cr", 
                               knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
model_tropical_air <- clogit(case ~ cb + pm25 + no2 + ozone + strata(bene_id), 
                          data[Koppen_s == c])
cp_air[[c]] <- crosspred(cb, model_tropical_air, at = 1:999/1000, cen = 0.5, cumul = TRUE)
cp_air[[c]]$df_knot <- df_knot
cp_air[[c]]$df_lag <- df_lag

summary(model_tropical_air)


#Arid model
c <- "Arid"

lag_dat_perc <- matrix(0.0, data[Koppen_s == c, .N], 15)
for (st in unique(data[Koppen_s == c, Koppen])) {
  ec <- ecdf(data[Koppen == st]$HI_lag_0)
  lag_dat_perc[which(data[Koppen_s == c, Koppen] == st), ] <-
    sapply(data[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
}

df_knot <- 0.9
df_lag <- 3

cb <- crossbasis(lag_dat_perc,
                 lag = 14, 
                 argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                 arglag = list(fun = "cr", 
                               knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
model_Arid_air <- clogit(case ~ cb + pm25 + no2 + ozone + strata(bene_id), 
                          data[Koppen_s == c])
cp_air[[c]] <- crosspred(cb, model_Arid_air, at = 1:999/1000, cen = 0.5, cumul = TRUE)
cp_air[[c]]$df_knot <- df_knot
cp_air[[c]]$df_lag <- df_lag
summary(model_Arid_air)


#Continental model
c <- "Continental"

lag_dat_perc <- matrix(0.0, data[Koppen_s == c, .N], 15)
for (st in unique(data[Koppen_s == c, Koppen])) {
  ec <- ecdf(data[Koppen == st]$HI_lag_0)
  lag_dat_perc[which(data[Koppen_s == c, Koppen] == st), ] <-
    sapply(data[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
}

df_knot <- 0.7
df_lag <- 4

cb <- crossbasis(lag_dat_perc,
                 lag = 14, 
                 argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                 arglag = list(fun = "cr", 
                               knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
model_continental_air <- clogit(case ~ cb + pm25 + no2 + ozone + strata(bene_id), 
                          data[Koppen_s == c])
cp_air[[c]] <- crosspred(cb, model_continental_air, at = 1:999/1000, cen = 0.5, cumul = TRUE)
cp_air[[c]]$df_knot <- df_knot
cp_air[[c]]$df_lag <- df_lag
summary(model_continental_air)

#National Model

lag_dat_perc2 <- matrix(0.0, data[, .N], 15)
for (st in unique(data[!is.na(Koppen), Koppen])) {
  ec <- ecdf(data[Koppen == st]$HI_lag_0)
  lag_dat_perc2[which(data[, Koppen] == st), ] <-
    sapply(data[Koppen == st, .SD, .SDcols = paste0("HI_lag_", 0:14)], ec)
}
c <- c("Temperate", "Tropical", "Arid", "Continental")


df_knot <- 0.7
df_lag <- 3

cp_all_air <- list()

cb <- crossbasis(lag_dat_perc2,
                 lag = 14, 
                 argvar = list(fun = "cr", knots = c(0, df_knot, 1)), 
                 arglag = list(fun = "cr", 
                               knots = (exp(seq(0, log(15), length.out = df_lag)) - 1)))
model_nat_air <- clogit(case ~ cb + pm25 + no2 + ozone + strata(bene_id), 
                                data[data$Koppen_s %in% c, ])
cp_all_air <- crosspred(cb, model_nat_air, at = 1:999/1000, cen = 0.5, cumul = TRUE)
cp_all_air$df_knot <- df_knot
cp_all_air$df_lag <- df_lag
summary(model_nat_air)

save(cp_air, cp_all_air, file = "data/aic_models_secondary.rda")
