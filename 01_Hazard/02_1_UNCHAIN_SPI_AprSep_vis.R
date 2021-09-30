
library(raster)
library(rasterVis)

#### We start with RCP 8.5####

# To open the rasterbrick from disk
SPI_Austria_45 <- brick("R:\\02_PROJECTS\\01_P_330001\\127_UNCHAIN\\03__Work\\WP4\\03_Casestudy\\02_Data\\01_ZAMG_SPARTACUS_Monthly\\Output\\Data\\SPI_Austria_RCP45.grd") 

# Here we select three time slices of 30 years each.
# Reference period: 1981 - 2010
# First projection: 2021 - 2050
# Second projection: 2071 - 2100

# Define which months are the summer months
month <- 4:9
summer_months <- rep ((12 * 0:130), each = length(month)) + month
SPI_Austria_summer <- subset(SPI_Austria_45, summer_months)

# To test whether the average over the reference period is 0
test <- subset(SPI_Austria_45, 133:492)
test_mean <- stackApply(test, indices =  rep(1,nlayers(test)), fun = "mean", na.rm = T)

# SPI_2021_2050 <- subset(SPI_Austria, 613:972)
# SPI_2071_2100 <- subset(SPI_Austria, 1212:1572)

SPI_ref_period_45 <- subset(SPI_Austria_summer, 67:246)

# To test whether the average over the whole time period is still ~0
summer_test_mean <- stackApply(SPI_ref_period_45, indices =  rep(1,nlayers(SPI_ref_period_45)), fun = "mean", na.rm = T)

SPI_2021_2050_45 <- subset(SPI_Austria_summer, 307:484)
SPI_2071_2100_45 <- subset(SPI_Austria_summer, 607:786)

# Count the number of cells with a value <=-1 in each cell stack
count_1981_2010_45 <- calc(SPI_ref_period_45, fun=function(x){
  sum(x <= -1)
})
count_1981_2010_45 <- count_1981_2010_45/30


# For middle of the century
count_2021_2050_45 <- calc(SPI_2021_2050_45, fun=function(x){
  sum(x <= -1)
})
count_2021_2050_45 <- count_2021_2050_45/30


# For end of the century
count_2071_2100_45 <- calc(SPI_2071_2100_45, fun=function(x){
  sum(x <= -1)
})
count_2071_2100_45 <- count_2071_2100_45/30


# Calculate delta 
delta_2021_2050_45 <- count_2021_2050_45 - count_1981_2010_45

delta_2071_2100_45 <- count_2071_2100_45 - count_1981_2010_45


# Plot the three time slices
rasterNames <- c("1981-2010", "2021-2050", "2071-2100")
prec_count_45 <- brick(count_1981_2010_45, count_2010_2021_45, count_2071_2100_45)
rasterVis::levelplot(prec_count_45, main="Development of the average number \nof summermonths with an \nSPI of less than -1 \nfor RCP 4.5 [Apr-Sep]",
                     names.attr=rasterNames, 
                     scales=list(draw= FALSE),
                     par.settings = YlOrRdTheme())
# Plot delta
rasterNames <- c("2021-2050", "2071-2100")
prec_delta_45 <- brick(delta_2021_2050_45, delta_2071_2100_45)
rasterVis::levelplot(prec_delta_45, main="Delta of the development of the average number \nof summermonths with an \nSPI of less than -1 \nfor RCP 4.5 [Apr-Sep]",
                     names.attr=rasterNames, 
                     scales=list(draw= FALSE),
                     layout=c(1,2), 
                     par.settings = BuRdTheme())

# writeRaster(count_1981_2010_45, "SPI_count_1981_2010_45_RCP45.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(count_2010_2021_45, "SPI_count_2010_2021_45_RCP45.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(count_2071_2100_45, "SPI_count_2070_2100_RCP45.tif", format = "GTiff", overwrite = TRUE)
# 
# writeRaster(delta_2021_2050_45, "SPI_delta_2021_2050_RCP45.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(delta_2071_2100_45, "SPI_delta_2070_2100_RCP45.tif", format = "GTiff", overwrite = TRUE)


#### We continue with RCP 8.5 ####

# To open the rasterbrick from disk
SPI_Austria_85 <- brick("R:\\02_PROJECTS\\01_P_330001\\127_UNCHAIN\\03__Work\\WP4\\03_Casestudy\\02_Data\\01_ZAMG_SPARTACUS_Monthly\\Output\\Data\\SPI_Austria_RCP85.grd") 

#__________Select the vegetation months of the three time slices of interest______________

# Select three time slices of 30 years each
# Reference period: 1981 - 2010
# First projection: 2021 - 2050
# Second projection: 2071 - 2100

# Define and subset which months are the vegetation months
SPI_Austria_summer_85 <- subset(SPI_Austria_85, summer_months)

# After removing the winter months from the stack, cut the three 30-year timeslices from the stack
SPI_ref_period_85 <- subset(SPI_Austria_summer_85, 67:246)
SPI_2021_2050_85 <- subset(SPI_Austria_summer_85, 307:484)
SPI_2071_2100_85 <- subset(SPI_Austria_summer_85, 607:786)

#________Calculate the number of times and SPI <-1 occurrs within each 30 year time slice_____
# Count the number of cells with a value <=-1 in each cell stack

# For the reference period
count_1981_2010_85 <- calc(SPI_ref_period_85, fun=function(x){
  sum(x <= -1)
})
count_1981_2010_85 <- count_1981_2010_85/30


# For middle of the century
count_2021_2050_85 <- calc(SPI_2021_2050_85, fun=function(x){
  sum(x <= -1)
})
count_2021_2050_85 <- count_2021_2050_85/30


# For end of the century
count_2071_2100_85 <- calc(SPI_2071_2100_85, fun=function(x){
  sum(x <= -1)
})
count_2071_2100_85 <- count_2071_2100_85/30


#_____Calculate the difference between current and future conditions_____________
delta_2021_2050_85 <- count_2021_2050_85 - count_1981_2010_85
delta_2071_2100_85 <- count_2071_2100_85 - count_1981_2010_85



# ___Plot the results___________
rasterNames <- c("1981-2010", "2021-2050", "2071-2100")
prec_count_85 <- brick(count_1981_2010_85, count_2021_2050_85, count_2071_2100_85)
rasterVis::levelplot(prec_count_85, main="Development of the average number of \nsummermonths with an SPI of less than -1 for RCP 8.5 [Apr-Sep]",
                     names.attr=rasterNames, 
                     scales=list(draw= FALSE),
                     par.settings = YlOrRdTheme(),
                     layout=c(1,3))

rasterNames <- c("2021-2050", "2071-2100")
prec_delta_85 <- brick(delta_2021_2050_85, delta_2071_2100_85)
rasterVis::levelplot(prec_delta_85, main="Delta of the development of the average number of \nsummermonths with an SPI of less than -1 for RCP 8.5 [Apr-Sep]",
                     names.attr=rasterNames, 
                     scales=list(draw= FALSE),
                     par.settings = BuRdTheme(),
                     layout=c(1,2))

names <- c("RCP 4.5 1981-2010","RCP 8.5 1981-2010", "2021-2050","2021-2050", "2071-2100", "2071-2100")

yy <- brick(count_1981_2010_45, count_1981_2010_85, count_2010_2021_45, count_2021_2050_85, count_2071_2100_45, count_2071_2100_85)
levelplot(yy,
          names.attr=names,
          main = "Average number of vegetation months [Apr-Sep] with SPI <= -1",
          scales = list(draw = FALSE),
          par.settings = YlOrRdTheme())

names <- c("RCP 4.5 2021-2050","RCP 8.5 2021-2050", "2071-2100", "2071-2100")
prec_delta <- brick(delta_2021_2050_45, delta_2021_2050_85, delta_2071_2100_85, delta_2071_2100_85)
levelplot(prec_delta,
          names.attr=names,
          main = "Delta of average number of vegetation months [Apr-Sep] with SPI <= -1",
          scales = list(draw = FALSE),
          par.settings = BuRdTheme())
#_____Write the results to disk________________  
#  writeRaster(count_1981_2010_85, "SPI_count_1981_2010_85_RCP85.tif", format = "GTiff", overwrite = TRUE)
#  writeRaster(count_2021_2050_85, "SPI_count_2021_2050_85_RCP85.tif", format = "GTiff", overwrite = TRUE)
#  writeRaster(count_2071_2100_85, "SPI_count_2070_2100_RCP85.tif", format = "GTiff", overwrite = TRUE)
# 
# writeRaster(delta_2021_2050_85, "SPI_delta_2021_2050_RCP85.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(delta_2071_2100_85, "SPI_delta_2070_2100_RCP85.tif", format = "GTiff", overwrite = TRUE)
