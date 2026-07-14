library(malariaAtlas)
library(sf)
library(terra)

idn_shp <- malariaAtlas::getShp(ISO = "IDN", admin_level = "admin2") %>%
  st_write("data/idn_districts.shp")

# SHEARER ET AL SURFACE
# tmp <- malariaAtlas::listRaster()
# tmp %>% filter(str_detect(dataset_id, "Pv"))
# 
# risk <- malariaAtlas::getRaster(dataset_id = "Explorer__2016_Pk_SEAsia_masked",
#                                   shp = idn_shp)
# 
# writeRaster(risk, "data/risk_map.tif")

# TOBIN ET AL SURFACES
risk <- rast("data/prediction_mean.tif")
risk_var <- rast("data/prediction_variance.tif")
risk_mask <- rast("data/prediction_masking.tif")
values(risk_mask) <- ifelse(values(risk_mask) == 1, 1, NA)

risk_stack <- c(risk, risk_var) %>%
  mask(risk_mask, updatevalue = 0) %>%
  mask(risk)

writeRaster(risk_stack, "data/risk_masked.tif")

