library(SPEI)
library(raster)
library(ncdf4)
library(terra)
library(chron)  # for the datestring  

SPI_calculator <- function(filename, timescale, rcp, ts){
# set path and filename
ncpath <- "R:\\02_PROJECTS\\01_P_330001\\127_UNCHAIN\\03__Work\\WP4\\03_Casestudy\\02_Data\\01_ZAMG_SPARTACUS_Monthly\\"
ncname <- filename  
ncfname <- paste0(ncpath, ncname, ".nc")
dname <- "pr" #pr is the variable that holds the precipitation values

ncin <- nc_open(ncfname)

lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")

# Reading the precipitation data for the vegetation months.
# We have 575 x/lon and 297 y/lat values. 1572 is the original number of timesteps (including the whole year).
prec_array <- ncvar_get(ncin, "pr", c(1,1,1,1), c(575,297,1572,1))
nc_close(ncin)

# Creating an empty brick with 297 rows, 575 columns and 786 layers (6 vegetation months á 131 years)
br <- brick(nrows=297, ncols=575, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), nl=1572, crs=4326)

# Here, the empty brick is populated with the precipitation data
br_val <- setValues (br, prec_array)

# For some reason the values were flipped, so here we flip it back
prec_brick_flip <- flip(br_val,2)


# Calculates SPI for the whole of Austria
SPI_Austria <- calc(prec_brick_flip, fun = function(x, scale = timescale, na.rm = TRUE) {
  xts <- ts (x, start=c(1970,1), frequency=12)
  as.numeric((SPEI::spi(xts, scale = scale, na.rm = na.rm, ref.start = c(1981, 1), ref.end = c(2010, 12)))$fitted)
})

# To write the rasterbrick to disk
writeRaster(SPI_Austria, filename=paste0('SPI_Austria', rcp, ts, '.grd'), format="raster", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

}

spi_rcp45_1 <- SPI_calculator("oeks-rcp45-pr-austria-MS", 1, "RCP_45", "ts1")
spi_rcp45_3 <- SPI_calculator("oeks-rcp45-pr-austria-MS", 3, "RCP_45", "ts3")
spi_rcp45_6 <- SPI_calculator("oeks-rcp45-pr-austria-MS", 6, "RCP_45", "ts6")

spi_rcp85_1 <- SPI_calculator("oeks-rcp85-pr-austria-MS", 1, "RCP_85", "ts1")
spi_rcp85_3 <- SPI_calculator("oeks-rcp85-pr-austria-MS", 3, "RCP_85", "ts3")
spi_rcp85_6 <- SPI_calculator("oeks-rcp85-pr-austria-MS", 6, "RCP_85", "ts6")

