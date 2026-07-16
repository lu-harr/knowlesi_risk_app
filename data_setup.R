# =============================================================================
# data_setup.R  (based on lu-harr/knowlesi_risk_app, with small robustness fixes)
#
# Builds the two data files the app needs, from the raw Tobin et al. surfaces:
#     data/risk_masked.tif      (2-layer: prediction_mean, prediction_variance)
#     data/idn_districts.shp    (Indonesia admin-2 boundaries)
#
# PREREQUISITES — put these three files in data/ first (download from OSF k5bsa):
#     data/prediction_mean.tif
#     data/prediction_variance.tif
#     data/prediction_masking.tif
#
# Run once:  source("data_setup.R")
# =============================================================================

library(malariaAtlas)   
library(sf)
library(terra)
library(dplyr)          

if (!dir.exists("data")) dir.create("data")

# ---- Indonesia admin-2 (district/regency) boundaries ------------------------
idn_shp <- malariaAtlas::getShp(ISO = "IDN", admin_level = "admin2")
sf::st_write(idn_shp, "data/idn_districts.shp", delete_dsn = TRUE)

# ---- Tobin et al. surfaces --------------------------------------------------
risk      <- rast("data/prediction_mean.tif")
risk_var  <- rast("data/prediction_variance.tif")
risk_mask <- rast("data/prediction_masking.tif")

# masking layer: keep 1s, set everything else to NA
values(risk_mask) <- ifelse(values(risk_mask) == 1, 1, NA)

risk_stack <- c(risk, risk_var) %>%
  mask(risk_mask, updatevalue = 0) %>%
  mask(risk)

# guarantee the layer names the app checks for
names(risk_stack) <- c("prediction_mean", "prediction_variance")

writeRaster(risk_stack, "data/risk_masked.tif", overwrite = TRUE)

message("Done: wrote data/risk_masked.tif and data/idn_districts.shp")
