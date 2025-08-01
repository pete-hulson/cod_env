
#from CEFI data portal query generator
#citation
# For "northeast_pacific/full_domain/hindcast/monthly/regrid/r20241015"
# 
# Data DOI:10.5281/zenodo.13936240
# Paper DOI:10.5194/gmd-2024-195

library("ncdf4")
library(tidync)
library(akgfmaps)
library(tidyverse)

#REGRID - OLD - IGNORE =======
#download monthly bottom temp on regrid (not raw grid)

# Specify the OPeNDAP server URL (using regular grid output)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/regrid/r20241015/btm_temp.nep.full.hcast.monthly.regrid.r20241015.199301-201912.nc"

# Open a NetCDF file lazily and remotely
ncopendap <- nc_open(url)

#saveRDS(ncopendap, file = "data/MOM6_pull_btm_temp.rds", compress = TRUE) #saving b/c can't access portal don't want to lose

tidydap <- tidync(url) %>% hyper_tibble()

# Read the coordinate into memory
lon <- ncvar_get(ncopendap, "lon")   #
lat <- ncvar_get(ncopendap, "lat")
time <- ncvar_get(ncopendap, "time",start = c(1), count = c(1))

# Read a slice of the data into memory
btm_temp <- ncvar_get(ncopendap, "btm_temp", start = c(1, 1, 1), count = c(-1, -1, 1))


#RAW-----

library("ncdf4")

# Specify the OPeNDAP server URL (using regular grid output)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/raw/r20250509/tob.nep.full.hcast.monthly.raw.r20250509.199301-202412.nc"

# Open a NetCDF file lazily and remotely
ncopendap <- nc_open(url)

# Read the coordinate into memory
x <- ncvar_get(ncopendap, "xh")
y <- ncvar_get(ncopendap, "yh")
url_static <-"http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/raw/r20250509/ocean_static.nc"
ncopendap_static <- nc_open(url_static)
lon <- ncvar_get(ncopendap_static, "geolon")
lat <- ncvar_get(ncopendap_static, "geolat")
time <- ncvar_get(ncopendap, "time",start = c(1), count = c(1))

# Read a slice of the data into memory
tob <- ncvar_get(ncopendap, "tob", start = c(1, 1, 1), count = c(-1, -1, 1))


#EDITS TO GET EXPAND GRID TO WORK

lon <- lon[which(lon<181 & lon>125)]
lat <- lat[which(lat<70 & lat>50 )]

#this fixes expand grid issue but next line doesn't work. Try breaking the array into a matrix for each month/yr combo?

#-------
#updates needed from here 12/Jun/2025

#try ALL slices
all_btm_temp <- ncvar_get(ncopendap, "tob", start = c(1, 1, 1), count = c(-1, -1, -1))
#this array is 342 816 384, 3rd dimension is time (384 unique times)


# Get the units
tunits <- ncatt_get(ncopendap, "time", "units")
datesince <- tunits$value
datesince <- substr(datesince, nchar(datesince)-18, nchar(datesince)-9) #I am editting here compared to cookbook, mine seems to have time in addition to date
datesince

# convert the number to datetime (input should be in second while the time is in unit of days)
datetime_var <- as.POSIXct(time*86400, origin=datesince, tz="UTC")
datetime_var

#quick view
#filled.contour(lon, lat, btm_temp, main = paste("Bottom temp at ", datetime_var), xlab = "Longitude", ylab = "Latitude", levels = pretty(c(-10,10), 20))

df <- expand.grid(X = lon, Y = lat)
data <- as.vector(t(all_btm_temp))
df$Data <- data
names(df) <- c("lon", "lat", "btm_temp")

saveRDS(df, file = "data/MOM6_raw_btm_temp.rds", compress = TRUE)

#EDITING above to loop through slices ie month/year combos
time_vec <- ncvar_get(ncopendap, "time",start = c(1), count = c(-1)) #look at all values
datetime_var_vec <- as.POSIXct(time_vec*86400, origin=datesince, tz="UTC")
datetime_var_vec

#now loop
outdf <- data.frame(matrix(0, nrow = 0, ncol = 4))
names(outdf) <- c("lon", "lat", "btm_temp", "date")
i<-1
for(i in 1:length(datetime_var_vec)){

df <- expand.grid(X = lon, Y = lat)
data <- as.vector(t(all_btm_temp[,,i]))
df$Data <- data #size mismatch here :(
names(df) <- c("lon", "lat", "btm_temp", "date")
df$date <- datetime_var_vec[i]

}

#can I index all_btm_temp with lat long? like all_btm_temp[which(lat>45),]???


#trying to update date time in tidync 
tidydap <- tidync(url) %>% hyper_tibble()

tidydap$time_tidy <- as.POSIXct(tidydap$time*86400, origin=datesince, tz="UTC")
#get one date per month in middle of month, 1993-2019

saveRDS(tidydap, file = "data/MOM6_tidy_raw_btm_temp.rds", compress = TRUE)

#next step, summarize to regions

# Get GOA stat areas to use for a mask ('auto' returns Alaska Albers)
goa <- akgfmaps::get_nmfs_areas(set.crs = 'auto') %>% 
  filter(REP_AREA > 609) 

tidydap <- tidydap[which(tidydap$lat<70 & tidydap$lat>40),]
tidydap <- tidydap[which(tidydap$lon<230),]

tidy_sf <- st_as_sf(tidydap, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
  st_transform(st_crs(goa)) %>% # projects to Alaska Albers
  st_intersection(goa) # clips to GOA area, and takes a few minutes

range(tidydap$lat)

saveRDS(tidy_sf, file = "data/MOM6_GOA_clip_btm_temp.rds", compress = TRUE) #saving b/c can't access portal don't want to lose

just_sf <- st_as_sf(tidydap, coords = c("lon", "lat"), crs = "EPSG:4326") #quick


ggplot(just_sf, aes(long, lat)) + geom_point()

 sebs_base <- get_base_layers(select.region = "sebs", set.crs = 'auto')
# sebs <- get_base_layers(select.region = "sebs", set.crs = "EPSG:4326")
 
 ggplot() +
   geom_sf(data = sebs_base$akland) +
   geom_sf(data = sebs_base$survey.strata,
           fill = NA,
           mapping = aes(color = "Survey strata")) +
   geom_sf(data = sebs_base$survey.grid,
           fill = NA,
           mapping = aes(color = "Station grid")) +
   geom_sf(data = sebs_base$survey.area,
           fill = NA,
           mapping = aes(color = "Survey area")) +
   geom_sf(data = sebs_base$graticule, alpha = 0.3, linewidth = 0.5) +
   scale_x_continuous(limits = sebs_base$plot.boundary$x,
                      breaks = sebs_base$lon.breaks) +
   scale_y_continuous(limits = sebs_base$plot.boundary$y,
                      breaks = sebs_base$lat.breaks) +
   theme_bw()
 
 

ggplot() +
   geom_sf(data = nmfs_areas) +
 geom_sf_text(data = nmfs_areas_centroid,
 mapping = aes(label = REP_AREA))

sebs <- akgfmaps::get_nmfs_areas(set.crs = 'auto') %>% 
  filter(REP_AREA == 521|
           REP_AREA == 524|
           REP_AREA == 514|
           REP_AREA == 513|
           REP_AREA == 517|
           REP_AREA == 509|
           REP_AREA == 516|
           REP_AREA == 512|
           REP_AREA == 508) 

sebs_test <- st_as_sf(tidydap, coords = c("lon", "lat"), crs = "EPSG:4326") %>%  
  st_transform(st_crs(sebs)) %>% # projects to Alaska Albers
  st_intersection(sebs) # clips to ebs area, and takes a few minutes

saveRDS(sebs_test, file = "data/MOM6_SEBS_clip_btm_temp.rds", compress = TRUE) #saving b/c can't access portal don't want to lose

sebs_temp <- sebs_test %>%
  mutate(year = lubridate::year(time_tidy), 
                month = lubridate::month(time_tidy), 
                day = lubridate::day(time_tidy))


megatest <- sebs_temp %>% 
  st_transform(st_crs(sebs_base$survey.area)) %>%
  st_intersection(sebs_base$survey.area)

saveRDS(megatest, file = "data/MOM6_SEBS_clipped_to_SURVEY_btm_temp.rds", compress = TRUE) #saving b/c can't access portal don't want to lose

ggplot(megatest[which(megatest$year==2020),], aes(geometry)) + geom_point()

plot(megatest$geometry[which(megatest$year==2020)], col=megatest$btm_temp[which(megatest$year==2020)])


#look at one year
m2019 <- megatest[which(megatest$year==2019),]

ggplot() +
 # geom_sf(data = megatest) +
  geom_sf(data = m2019,
          #fill = btm_temp,
          mapping = aes(color = btm_temp)) + facet_wrap(~month)
  

#look at mays
mmay<- megatest[which(megatest$month==5),]

ggplot() +
  geom_sf(data = mmay,
          #fill = btm_temp,
          mapping = aes(color = btm_temp)) + facet_wrap(~year)

#get monthly means===========================

sebs_monthly_means_surv_area <- megatest %>% group_by(year, month) %>%
  summarise(mean_btm_temp=mean(btm_temp, na.rm=TRUE))

ggplot(sebs_monthly_means_surv_area, aes(year, mean_btm_temp)) + geom_point() +
  geom_line() + facet_wrap(~month)

wd <- getwd()
write_csv(sebs_monthly_means_surv_area, file=paste0(wd,"/data/sebs_surveyarea_monthly_means_MOM6.csv"))

