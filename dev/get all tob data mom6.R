# download MOM6 output

#Based on code kindly shared by Jenny Bigman, UDel

  library(tidyverse)
  library(ncdf4)
  library(reshape2)
  library(data.table)
  
  # specify the OPeNDAP server URL (using regular grid output)
  url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/raw/r20250509/tob.nep.full.hcast.monthly.raw.r20250509.199301-202412.nc"
  url_static <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal/northeast_pacific/full_domain/hindcast/monthly/raw/r20250509/ocean_static.nc"

  # open a NetCDF file lazily and remotely
  ncopendap <- nc_open(url)
  ncopendap_static <- nc_open(url_static)
  
  # read the coordinates into memory
  
  # from file with environmental vars
  x <- ncvar_get(ncopendap, "xh")
  y <- ncvar_get(ncopendap, "yh")

  # from static file
  lon <- ncvar_get(ncopendap_static, "geolon")
  lat <- ncvar_get(ncopendap_static, "geolat")
  
  # set number of time_steps
  all_time <- ncvar_get(ncopendap, "time")
  n_steps <- 1:length(all_time)
  
  
  # download and concatenate 
  tob_all <- purrr::map_dfr(n_steps, \(temp){ #replaced t with temp to troubleshoot
    
    # read time steps
    time <- ncvar_get(ncopendap, "time", start = c(temp), count = c(1))
    
    # read a slice of the data into memory
    tob_slice <- ncvar_get(ncopendap, "tob", start = c(1, 1, temp), count = c(-1, -1, 1))
  
    # Get the units
    tunits <- ncatt_get(ncopendap, "time", "units")
    datesince <- tunits$value
    datesince <- substr(datesince, nchar(datesince)-18, nchar(datesince)-8)
    datesince
  
    # convert the number to datetime (input should be in second while the time is in unit of days)
    datetime_var <- as.POSIXct(time*86400, origin=datesince, tz="UTC")
  
    X <- as.vector(t(lon))
    Y <- as.vector(t(lat))
    data <- as.vector(t(tob_slice))
  
    df <- data.frame(
      "lon" = X,
      "lat" = Y,
      "tob" = data,
      "date" = datetime_var
    )
  
    df
    
    
  })
  

  #loop alternative added by Krista
  
  # download and concatenate 
#  tob_all <- purrr::map_dfr(n_steps, \(temp){ #replaced t with temp to troubleshoot
  

  #chunk this up for speed  
  k<-1
  
  for(k in 1:3){
    
    start <- (k*100)-99
    stop <- k*100
    
    df_out <- data.frame(matrix(ncol=4, nrow=0))
    colnames(df_out) <- c("lon",  "lat",  "tob",  "date")
    df_out$lon <- as.numeric(df_out$lon)
    df_out$lat <- as.numeric(df_out$lat)
    df_out$tob <- as.numeric(df_out$tob)
    df_out$date <- as.Date(df_out$date)
  
    for(i in start:stop){

    # read time steps
    time <- ncvar_get(ncopendap, "time", start = c(i), count = c(1))
    
    # read a slice of the data into memory
    tob_slice <- ncvar_get(ncopendap, "tob", start = c(1, 1, i), count = c(-1, -1, 1))
    
    # Get the units
    tunits <- ncatt_get(ncopendap, "time", "units")
    datesince <- tunits$value
    datesince <- substr(datesince, nchar(datesince)-18, nchar(datesince)-8)
    datesince
    
    # convert the number to datetime (input should be in second while the time is in unit of days)
    datetime_var <- as.POSIXct(time*86400, origin=datesince, tz="UTC")
    
    X <- as.vector(t(lon))
    Y <- as.vector(t(lat))
    data <- as.vector(t(tob_slice))
    
    df <- data.frame(
      "lon" = X,
      "lat" = Y,
      "tob" = data,
      "date" = datetime_var
    )
    
    df_out <- as.data.frame(bind_rows(df_out, df))
    
    print(i)
    
  }
  
    saveRDS(df_out, file=paste0("data/MOM6_tob_file_", k ,".rds"), compress = TRUE)
}
  

  
  #300-384 breaks at 384 (most recent), so last chunk here
  
  df_out <- data.frame(matrix(ncol=4, nrow=0))
  colnames(df_out) <- c("lon",  "lat",  "tob",  "date")
  df_out$lon <- as.numeric(df_out$lon)
  df_out$lat <- as.numeric(df_out$lat)
  df_out$tob <- as.numeric(df_out$tob)
  df_out$date <- as.Date(df_out$date)
  
  for(i in 301:383){
    
    # read time steps
    time <- ncvar_get(ncopendap, "time", start = c(i), count = c(1))
    
    # read a slice of the data into memory
    tob_slice <- ncvar_get(ncopendap, "tob", start = c(1, 1, i), count = c(-1, -1, 1))
    
    # Get the units
    tunits <- ncatt_get(ncopendap, "time", "units")
    datesince <- tunits$value
    datesince <- substr(datesince, nchar(datesince)-18, nchar(datesince)-8)
    datesince
    
    # convert the number to datetime (input should be in second while the time is in unit of days)
    datetime_var <- as.POSIXct(time*86400, origin=datesince, tz="UTC")
    
    X <- as.vector(t(lon))
    Y <- as.vector(t(lat))
    data <- as.vector(t(tob_slice))
    
    df <- data.frame(
      "lon" = X,
      "lat" = Y,
      "tob" = data,
      "date" = datetime_var
    )
    
    df_out <- as.data.frame(bind_rows(df_out, df))
    
    print(i)
    
  }
  
  saveRDS(df_out, file=paste0("data/MOM6_tob_file_", 4 ,".rds"), compress = TRUE)
  
#loop through files and rbind, will be slow  
  
  filelist<-list.files("data", pattern="MOM6_tob_file_", full.names=TRUE)
  
  in_dat <- readRDS(file=filelist[1])[1,]
  in_dat[1,] <- NA
  
  i<-1
  for(i in 1:length(filelist)){
    temp_dat <- readRDS(file=filelist[i])
    
  
    m_list <- list(in_dat, temp_dat)
   in_dat <- data.table::rbindlist(m_list)
    print(i)
  }
  
  saveRDS(in_dat, file="data/MOM6_tob_all_data.rds", compress = TRUE)

  tob <- readRDS(file="data/MOM6_tob_all_data.rds")  
  
  tob <- tob[-1,]
  
  tob <- as.data.frame(tob)
  
  tob <- tob[which(tob$lat>48&tob$lat<72),]
  tob <- tob[which(tob$lon>175),]
  
  tob <- tob %>%
    mutate(year = lubridate::year(date), 
           month = lubridate::month(date), 
           day = lubridate::day(date))
  


  ggplot(tob[which(tob$year=="1993" & tob$month=="1"),], aes(lon, lat, col=tob))  + 
    geom_point()
  

  
  #CLIP======
  
  library(akgfmaps)
  
  #GOA
  
  # Get GOA stat areas to use for a mask ('auto' returns Alaska Albers)
  goa <- akgfmaps::get_nmfs_areas(set.crs = 'auto') %>% 
    filter(REP_AREA > 609) 
  
  goa_tob <- st_as_sf(tob, coords = c("lon", "lat"), crs = "EPSG:4326") %>% # Sets to WGS 1984
    st_transform(st_crs(goa)) %>% # projects to Alaska Albers
    st_intersection(goa) # clips to GOA area, and takes a few minutes
  
  saveRDS(goa_tob, file="data/MOM6_tob_GOA.rds", compress = TRUE)
  
  # ggplot(goa_tob[which(goa_tob$year=="1993" & goa_tob$month=="1"),], aes(lon, lat, col=goa_tob$tob))  + 
  #   geom_point()
  
  ggplot() +
    geom_sf(data = goa_tob,
            #fill = btm_temp,
            mapping = aes(color = tob)) #LOOKS good
  
  #further reduce by clipping to SURVEY area
  
  goa_base <- get_base_layers(select.region = "goa", set.crs = 'auto')
  
  ggplot() +
    geom_sf(data = goa_base$akland) +
    geom_sf(data = goa_base$survey.strata,
            fill = NA,
            mapping = aes(color = "Survey strata")) +
    geom_sf(data = goa_base$survey.grid,
            fill = NA,
            mapping = aes(color = "Station grid")) +
    geom_sf(data = goa_base$survey.area,
            fill = NA,
            mapping = aes(color = "Survey area")) +
    geom_sf(data = goa_base$graticule, alpha = 0.3, linewidth = 0.5) +
    scale_x_continuous(limits = goa_base$plot.boundary$x,
                       breaks = goa_base$lon.breaks) +
    scale_y_continuous(limits = goa_base$plot.boundary$y,
                       breaks = goa_base$lat.breaks) +
    theme_bw()
  
  goa_surv_tob <- goa_tob %>% 
    st_transform(st_crs(goa_base$survey.area)) %>%
    st_intersection(goa_base$survey.area)
  
  saveRDS(goa_surv_tob, file="data/MOM6_tob_GOA_surv_area.rds", compress = TRUE)
  
  
  #get monthly means===========================
  
  goa_monthly_means_surv_area <- goa_surv_tob %>% group_by(year, month) %>%
    summarise(mean_tob=mean(tob, na.rm=TRUE))
  
  ggplot(goa_monthly_means_surv_area, aes(year, mean_tob)) + geom_point() +
    geom_line() + facet_wrap(~month)
  
  wd <- getwd()
  write_csv(goa_monthly_means_surv_area, file=paste0(wd,"/data/goa_tob_surveyarea_monthly_means_MOM6.csv"))
  
  
  
  
  #Bering
  
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
  
  sebs_tob <- st_as_sf(tob, coords = c("lon", "lat"), crs = "EPSG:4326") %>%  
    st_transform(st_crs(sebs)) %>% # projects to Alaska Albers
    st_intersection(sebs) # clips to ebs area, and takes a few minutes
  
  saveRDS(sebs_tob, file = "data/MOM6_tob_SEBS.rds", compress = TRUE) #saving b/c can't access portal don't want to lose
  
  ggplot() +
    geom_sf(data = sebs_tob,
            #fill = btm_temp,
            mapping = aes(color = tob)) #looks good
  
  
  #further reduce by clipping to SURVEY area
  
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
  
  sebs_surv_tob <- sebs_tob %>% 
    st_transform(st_crs(sebs_base$survey.area)) %>%
    st_intersection(sebs_base$survey.area)
  
  saveRDS(sebs_surv_tob, file="data/MOM6_tob_SEBS_surv_area.rds", compress = TRUE)
  
  #get monthly means===========================
  
  sebs_monthly_means_surv_area <- sebs_surv_tob %>% group_by(year, month) %>%
    summarise(mean_tob=mean(tob, na.rm=TRUE))
  
  ggplot(sebs_monthly_means_surv_area, aes(year, mean_tob)) + geom_point() +
    geom_line() + facet_wrap(~month)
  
  wd <- getwd()
  write_csv(sebs_monthly_means_surv_area, file=paste0(wd,"/data/sebs_tob_surveyarea_monthly_means_MOM6.csv"))
  
  
  
  
  
  
  
  
  
  
  
  
   