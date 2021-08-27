### Load / Install Package Dependencies
packages = c(
  'suncalc', # getSunlightTimes()
  'lubridate', # with_tz()
  'readxl', # read_excel()
  'ggplot2', # ggplot()
  'data.table', # uniqueN()
  'reshape2',
  'ggmap', # get_map(), ggmap()
  'dplyr', # inner_join
  'lubridate' # format
)

# loop to check R library for each dependent package and install if they're not currently in library
for (package in packages){
  if (!require(package, character.only = TRUE)){
    install.packages(package)
  }
  ## Load each package
  library(package, character.only = TRUE)
}


### Load Utility Functions
load_vemco_data = function(filename, format = '%Y-%m-%d %H:%M:%S', tz = 'HST'){
  #### Loads in a csv datafile exported from VUE and cleans up file for further analysis
  ### Loading in data file
  vue_df = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column names
  colnames(vue_df)[1]  <- 'datetime'
  colnames(vue_df)[2]  <- 'receiver'
  colnames(vue_df)[3]  <- 'tag_id'
  colnames(vue_df)[4]  <- 'name'
  colnames(vue_df)[5]  <- 'tag_serial'
  colnames(vue_df)[6]  <- 'depth'
  colnames(vue_df)[7]  <- 'sensor.unit'
  colnames(vue_df)[8]  <- 'station'
  colnames(vue_df)[9]  <- 'lat'
  colnames(vue_df)[10] <- 'lon'
  ### Converting datetime to POSIXct Object
  vue_df$datetime = as.POSIXct(vue_df$datetime, format = format, tz = 'UTC')
  ## Convert datetime from GMT to HST
  vue_df$datetime = with_tz(vue_df$datetime, tz) # Note: HST is 10 Hours behind GMT
  ## Pulling out just date and time components
  vue_df$date = as.Date(vue_df$datetime)
  vue_df$time = strftime(vue_df$datetime, format="%H:%M:%S", tz = tz)
  ### Cleaning up tag ids - removing the 'A69-####-' prefix
  vue_df$full_tag_id = vue_df$tag_id
  vue_df$tag_id = substring(vue_df$tag_id, 10)
  ### Cleaning up receiver - remvoing the 'VR2W-' Prefix
  if(substr(vue_df$receiver[1], 1, 5) == 'VR2W-'){
    vue_df$receiver = substring(vue_df$receiver, 6)
  }
  vue_df$receiver = abs(as.numeric(vue_df$receiver))
  return (vue_df)
}

load_fdf_report = function(filename = '/Users/stephenscherrer/Desktop/FDF.csv', tag_specs = NULL){
  fdf_report = read.csv(filename, stringsAsFactors = FALSE)
  colnames(fdf_report) = c('tag_id', 'receiver', 'detections', 'min_interval', 'short_intervals', 'long_intervals', 'first_detected', 'last_detected', 'acceptance')
  ## Reclassing date objects
  fdf_report$first_detected = as.POSIXct(fdf_report$first_detected, tz = 'UTC')  
  fdf_report$last_detected = as.POSIXct(fdf_report$last_detected, tz = 'UTC') 
  # Converting to local time
  fdf_report$first_detected = with_tz(fdf_report$first_detected, 'HST')
  fdf_report$last_detected = with_tz(fdf_report$last_detected, 'HST')
  ## Removing any tags that aren't ours.
  if(!is.null(tag_specs)){
    fdf_report = fdf_report[fdf_report$tag_id %in% tag_specs$vue_tag_id, ]
  }
  ## Changing tag format
  for(i in 1:length(fdf_report$tag_id)){
    fdf_report$tag_id[i] = strsplit(fdf_report$tag_id[i], split = "-")[[1]][3]
  }
  ## Changing reciever format
  fdf_report$receiver = gsub(pattern = "VR2AR-", replacement = "", x = fdf_report$receiver)
  fdf_report$receiver = gsub(pattern = "VR2W-", replacement = "", x = fdf_report$receiver)
  ## Changing datetime to local time (HST)
  fdf_report$first_detected
  return(fdf_report)
}

load_receiver_data = function(filename, format = '%m/%d/%y %H:%M', tz = 'HST'){
  #### Loads in .csv file containing receiver deployment and recovery data and cleans up file as appropriate
  ### Loading in datafile
  receiver_df = read.csv(filename, stringsAsFactors = FALSE)
  ### Adjusting Column Names
  colnames(receiver_df)[1] = 'station_name'
  colnames(receiver_df)[2] = 'consecutive_deployment_number'
  colnames(receiver_df)[3] = 'deployment_date'
  colnames(receiver_df)[4] = 'recovery_date'
  colnames(receiver_df)[5] = 'recovered'
  colnames(receiver_df)[6] = 'lat_deg'
  colnames(receiver_df)[7] = 'lat_min'
  colnames(receiver_df)[8] = 'lon_deg'
  colnames(receiver_df)[9] = 'lon_min'
  colnames(receiver_df)[10] = 'depth'
  colnames(receiver_df)[11] = 'vr2w_serial'
  colnames(receiver_df)[12] = 'acoustic_release_serial'
  colnames(receiver_df)[13] = 'acoustic_release_battery_life'
  colnames(receiver_df)[14] = 'acoustic_release_voltage_at_deployment'
  colnames(receiver_df)[15] = 'acoustic_release_serial_code'
  colnames(receiver_df)[16] = 'temperature_logger_serial'
  colnames(receiver_df)[17] = 'deployed_by'
  colnames(receiver_df)[18] = 'recovered_by'
  colnames(receiver_df)[19] = 'comments_deployment'
  colnames(receiver_df)[20] = 'comments_recovery'
  ### Converting deployment and recovery dates to POSIX objects
  receiver_df$deployment_date = as.POSIXct(receiver_df$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_df$recovery_date = as.POSIXct(receiver_df$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  ## Converting latitude and longitude from degree minutes to decimal degrees
  receiver_df$lat = convert_lat_lon(receiver_df$lat_deg, receiver_df$lat_min)
  receiver_df$lon = convert_lat_lon(receiver_df$lon_deg, receiver_df$lon_min)
  ## Converting station depth recorded in fathoms to m. 1 fathom = 1.8288 m
  station_fath_to_m = as.numeric(sapply(strsplit(receiver_df$depth, " "), "[", 1)) * 1.8288
  fath_indicies = which(tolower(sapply(strsplit(receiver_df$depth, " "), "[", 2)) %in% c('fathoms', 'fath'))
  receiver_df$depth[fath_indicies] = station_fath_to_m[fath_indicies]
  receiver_df$depth = as.numeric(gsub(pattern = " m", replacement = "", x = receiver_df$depth))
  return (receiver_df)
}

load_tagging_data = function(filename){
  #### Loads in .csv file containing fish tagging data and cleans it up as appropriate
  tagging_df = read_excel(filename)
  ### Adjusting Column names
  colnames(tagging_df)[1]  <- 'species'
  colnames(tagging_df)[2]  <- 'fork_length'
  colnames(tagging_df)[3]  <- 'condition'
  colnames(tagging_df)[4]  <- 'tag_id'
  colnames(tagging_df)[5]  <- 'tag_type'
  colnames(tagging_df)[6]  <- 'tagging_date'
  colnames(tagging_df)[7]  <- 'time'
  colnames(tagging_df)[8] <- 'dart_tag_id'
  colnames(tagging_df)[9] <- 'lon_deg'
  colnames(tagging_df)[10] <- 'lon_min'
  colnames(tagging_df)[11] <- 'lat_deg'
  colnames(tagging_df)[12] <- 'lat_min'
  colnames(tagging_df)[13] <- 'comments'
  
  #### Converting tagging date to POSIX object
  # tagging_df$datetime = as.POSIXct(paste(tagging_df$tagging_date, tagging_df$time), format = '%Y-%m-%d %H:%M', tz = 'HST')
  
  #### Converting lat lon data from degree minutes to decimal degrees
  tagging_df$lat = convert_lat_lon(tagging_df$lat_deg, tagging_df$lat_min)
  tagging_df$lon = convert_lat_lon(tagging_df$lon_deg, tagging_df$lon_min)
  
  return (tagging_df)
}

load_vessel_data = function(filename){
  ## Read in datafile
  vessel_df = read.csv(filename)
  colnames(vessel_df) = c('permit_number', 'vessel_name', 'permittee_name', 'date', 'start_time', 'end_time', 'stay_time', 'buoy', 'scuba', 'snuba', 'snorkel', 'other', 'activity_type', 'same_group', 'comment', 'number_of_buoys', 'x')
  
  ## Reformat date and time components
  for (i in 1:nrow(vessel_df)){
    vessel_df$start_time[i] = as.POSIXct(format(strptime(paste(vessel_df$date[i], vessel_df$start_time[i]), "%m/%d/%y %I:%M %p"), "%Y-%m-%d %H:%M"), tz = 'HST')
    vessel_df$end_time[i] = as.POSIXct(format(strptime(paste(vessel_df$date[i], vessel_df$end_time[i]), "%m/%d/%y %I:%M %p"), "%Y-%m-%d %H:%M"), tz = 'HST')
    vessel_df$date[i] = as.POSIXct(format(strptime(vessel_df$date[i], "%m/%d/%y"), "%Y-%m-%d"), tz = 'HST')
  }
  
  return(vessel_df)
}


calculate_spatial_evenness = function(vue_df, receiver_df){
  ### function to calculate spacitail eveness based on Pielou 1966 from TinHan 2014
  ## outputs a dataframe first column is tag id, second column is spatial evenness index
  vue_df = vue_df[vue_df$station != 'Tagging Location', ]
  spatial_evenness_df = as.data.frame(matrix(data = 0, nrow = length(sort(unique(vue_df$tag_id))), ncol = 2))
  colnames(spatial_evenness_df) = c('tag_id', 'spatial_evenness_metric')
  spatial_evenness_df$tag_id = sort(unique(vue_df$tag_id))
  R = length(unique(receiver_df$station_name)) #could replaces "station_name" with "zone"
  for(i in 1:length(sort(unique(vue_df$tag_id)))){
    indv_data = vue_df[which(vue_df$tag_id == sort(unique(vue_df$tag_id))[i]), ]
    spatial_sum = c()
    for(a in 1:length(unique(receiver_df$station_name))){
      rho_i = length(indv_data$datetime[which(as.character(indv_data$station) == as.character(receiver_df$station_name[a]))]) / length(indv_data$station)
      spatial_sum = c(spatial_sum, (rho_i * log(rho_i)))
    }
    #print(spatial_sum)
    spatial_evenness_df[i, 2] = (-1 * sum((spatial_sum[spatial_sum != 'NaN']))) / log(R)
  }
  return(spatial_evenness_df)
}




plot_day_night = function(vue_df, receiver_df = NULL, color_palette = NULL, plot_title = NULL){
  ### Function to produce day night plots modified from code provided by Alex Filous with individual days on X axis and time of day on Y axis. Shading represents night/day
  
  datetime_to_decimal_hr = function(datetime){
    ## Helper function to strip time component from a datetime object and return it as hr.(min/60)
    if (length(datetime) == 1){
      return(as.numeric(format(as.POSIXlt(datetime, format= "%H:%M:%S"),'%H')) + as.numeric(format(as.POSIXlt(datetime, format= "%H:%M:%S"),'%M'))/60 + as.numeric(format(as.POSIXlt(datetime, format= "%H:%M:%S"), '%S')) / 3600)
    } else if ( length(datetime) > 1 ){
      return(as.numeric(format(as.POSIXlt(datetime, format= "%H:%M:%S"),'%H')) + as.numeric(format(as.POSIXlt(datetime, format= "%H:%M:%S"),'%M'))/60 + as.numeric(format(as.POSIXlt(datetime, format= "%H:%M:%S"), '%S')) / 3600)
    } else {
      print('datetime arguement must be of length >= 1')
    }
  }
  
  ### If receiver_df argument is present, pull relevant dates for plotting vetical bars
  if(is.null(receiver_df) == FALSE){
    ### Getting dates when receivers in the study were rearranged.
    receiver_df_subset = receiver_df[receiver_df$station_name %in% unique(vue_df$station), ] # First subset only stations where a fish was detected
    receiver_dates = unique(c(trunc(receiver_df_subset$deployment_date, 'day'), trunc(receiver_df_subset$recovery_date, 'day'))) # Then pull out unique dates on which those stations were serviced
    receiver_dates = receiver_dates[which(receiver_dates >= floor_date(min(vue_df$datetime)) & receiver_dates <= ceiling_date(max(vue_df$datetime)))]
  }
  
  if(is.null(color_palette)){
    vue_df$plot_color = "black"
  }else if(class(color_palette$colors) == "character"){
    station_colors = as.data.frame(cbind(color_palette$colors, color_palette$station))
    colnames(station_colors) = c('plot_color', 'station')
    vue_df = merge(vue_df, station_colors)
  }
  
  # Creating separate date (x axis) and time (y axis) from POSIX datetime
  vue_df$plot_date  = as.POSIXlt(as.character(vue_df$datetime),format = "%Y-%m-%d")
  vue_df$plot_time = datetime_to_decimal_hr(vue_df$datetime)
    
  ### Setup Plot
  ## Make empty plot without x axis labels
  plot(vue_df$plot_date, vue_df$plot_time, main=plot_title, col=vue_df$plot_color, pch=19,cex=1,ylim=c(0,24),type="n", xaxt = 'n',xaxs="i",yaxs="i",xlab="", ylab="Time of Day (HST - 24 hr.)")
    
  ## Determine frequency to add 10 x-axis labels to plot from data and then add them
  date_labels = as.character(strptime(seq(min(vue_df$datetime), max(vue_df$datetime), length.out = 10), format = '%Y-%m-%d'))
  axis(side = 1, at = seq(min(vue_df$plot_date), max(vue_df$plot_date), length.out = 10), labels = date_labels, las = 2, cex = .25)

  ## Fetch sunrise and sunset times (And other solar event times as well) using time zone from vue_data
  sundate<-as.POSIXct(seq.Date(from = as.Date(min(vue_df$plot_date)), to = as.Date(max(vue_df$plot_date)), by = 'day'))
  solar_events = getSunlightTimes(date = as.Date(sundate), lat = mean(unique(vue_df$lat)), lon = mean(unique(vue_df$lon)), tz = tz(vue_df$datetime))
  
  ## Add shaded polygons to plot
  lines(x = sundate, y = datetime_to_decimal_hr(solar_events$sunrise), col="lightgrey")
  cord.x <- c(min(sundate),sundate,max(sundate)) 
  cord.y <- c(24, datetime_to_decimal_hr(solar_events$sunset),24)
  polygon(cord.x,cord.y,col='lightgrey')

  lines(x = sundate,y = datetime_to_decimal_hr(solar_events$sunset), col="lightgrey") 
  cord.x <- c(min(sundate),sundate,max(sundate)) 
  cord.y <- c(0, datetime_to_decimal_hr(solar_events$sunrise),0)
  polygon(cord.x,cord.y,col='lightgrey')
  
  ## Add detections to plot
  with(vue_df, points(plot_date, plot_time, col = plot_color, pch = 19, cex = 1))
  if(is.null(receiver_df) == FALSE){
    abline(v = as.numeric(receiver_dates), col = 'blue')
  }
}

convert_lat_lon = function(ll_deg, ll_min = FALSE){
  #### Converts latitude and longitude between ll minutes and ll decimal degrees
  ### 2 usages:
  ## 1. Convert decimal degrees to degree minutes
  # 1 argument
  # ll_pref is a single argument of latitude or longitude in decimal degrees
  # Returns a prefix and decimal for that argument
  ## 2. Convert degree minutes to decimal degrees
  # 2 arguments
  # ll_pref is the latitude or longitude's degree
  # ll_min is the degree minutes
  # returns a single float of ll in decimal degrees
  ## Use Case 1.
  if (ll_min[1] == FALSE){
    ll_deg = as.numeric(as.character(ll_deg))
    ll_bin = matrix(0, length(ll_deg), 2)
    for (r in 1:length(ll_deg)){
      if (isTRUE(ll_deg[r] >= 0)){
        ll_dec = ll_deg[r] - floor(ll_deg[r])
        ll_bin[r, ] = c(floor(ll_deg[r]), (ll_dec)*60)
      } else {
        ll_dec = (ll_deg[r] - ceiling(ll_deg[r]))*-1
        ll_bin[r, ] = c(ceiling(ll_deg[r]), (ll_dec)*60)
      }
    }
    ## Use Case 2.
  }else{
    ll_deg = as.numeric(as.character(ll_deg))
    ll_min = as.numeric(as.character(ll_min))
    ll_bin = matrix(0, length(ll_deg), 1)
    for (r in 1:length(ll_deg)){
      ll_dec_deg = abs(ll_deg[r]) + (abs(ll_min[r])/60)
      if (isTRUE(ll_deg[r] < 0)){
        ll_dec_deg = ll_dec_deg*(-1)
      }
      ll_bin[r] = ll_dec_deg
    }
  }
  return (ll_bin)
}

get_time_of_day = function(vue_df){
  ## Calculating sunrise, sunset and dawn for each detection using mean lat lon coordinates of from vue file
  
  ## Get mean lat lon from unique receiver positions 
  lat = mean(unique(vue_df$lat))
  lon = mean(unique(vue_df$lon))

  ## Get time of "sunrise", "sunset", "nauticalDawn", "nauticalDusk"
  sun_times = getSunlightTimes(date = seq.Date(from = min(vue_df$date) - 1, to = max(vue_df$date) + 1, by = 1) , lat = lat, lon = lon, keep = c("sunrise", "sunset", "nauticalDawn", "nauticalDusk"), tz = "HST")
  sun_times$previous_sunset = c(sun_times$sunset[1], sun_times$sunset[1:(length(sun_times$sunset)-1)])
  sun_times$next_sunrise = c(sun_times$sunrise[2:length(sun_times$sunrise)], sun_times$sunrise[length(sun_times$sunrise)])
  sun_times$date = as.Date(sun_times$date, tz = "HST")
  
  ## Assigning time of day to each detection
  sun_times_by_vue_df = sun_times[match(vue_df$date, sun_times$date), c("nauticalDawn", "sunrise", "sunset", "nauticalDusk")]
  sun_times_by_vue_df$datetime = vue_df$datetime
  suntimes =  as.matrix(sun_times_by_vue_df)
  
  ## Assigning each detection a time of day by comparing the datetime element to all the other elements. This returns an integer (1-5) which corrosponds to time of day
  # index = apply(suntimes, MARGIN = 1, function(x) length(which(x["datetime"] >= x)))
  vue_df$time_of_day = c('night', 'dawn', 'day', 'dusk', 'night')[apply(suntimes, MARGIN = 1, function(x) length(which(x["datetime"] >= x)))]
  return(vue_df)
}

file.path(data_directory, '20200524_Molokini_VR2_deployments.xlsx')

load_receiver_data = function(filename, format = '%m/%d/%y %H:%M', tz = 'HST'){
  #### Loads in .csv file containing receiver deployment and recovery data and cleans up file as appropriate
  ### Loading in datafile
  receiver_df = read_excel(file.path(data_directory, '20200524_Molokini_VR2_deployments.xlsx'))

  ### Adjusting Column Names
  colnames(receiver_df)[1] = 'station_name'
  colnames(receiver_df)[2] = 'consecutive_deployment_number'
  colnames(receiver_df)[3] = 'deployment_date'
  colnames(receiver_df)[4] = 'recovery_date'
  colnames(receiver_df)[5] = 'recovered'
  colnames(receiver_df)[6] = 'lat_deg'
  colnames(receiver_df)[7] = 'lat_min'
  colnames(receiver_df)[8] = 'lon_deg'
  colnames(receiver_df)[9] = 'lon_min'
  colnames(receiver_df)[10] = 'depth'
  colnames(receiver_df)[11] = 'vr2w_serial'
  colnames(receiver_df)[12] = 'acoustic_release_serial'
  colnames(receiver_df)[13] = 'acoustic_release_battery_life'
  colnames(receiver_df)[14] = 'acoustic_release_voltage_at_deployment'
  colnames(receiver_df)[15] = 'acoustic_release_serial_code'
  colnames(receiver_df)[16] = 'temperature_logger_serial'
  colnames(receiver_df)[17] = 'deployed_by'
  colnames(receiver_df)[18] = 'recovered_by'
  colnames(receiver_df)[19] = 'comments_deployment'
  colnames(receiver_df)[20] = 'comments_recovery'
  ### Converting deployment and recovery dates to POSIX objects
  receiver_df$deployment_date = as.POSIXct(receiver_df$deployment_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  receiver_df$recovery_date = as.POSIXct(receiver_df$recovery_date, format = '%m/%d/%y %H:%M', tz = 'HST')
  ## Converting latitude and longitude from degree minutes to decimal degrees
  receiver_df$lat = convert_lat_lon(receiver_df$lat_deg, receiver_df$lat_min)
  receiver_df$lon = convert_lat_lon(receiver_df$lon_deg, receiver_df$lon_min)
  ## Converting station depth recorded in fathoms to m. 1 fathom = 1.8288 m
  station_fath_to_m = as.numeric(sapply(strsplit(receiver_df$depth, " "), "[", 1)) * 1.8288
  fath_indicies = which(tolower(sapply(strsplit(receiver_df$depth, " "), "[", 2)) %in% c('fathoms', 'fath'))
  receiver_df$depth[fath_indicies] = station_fath_to_m[fath_indicies]
  receiver_df$depth = as.numeric(gsub(pattern = " m", replacement = "", x = receiver_df$depth))
  return (receiver_df)
}

count_detections_per_date = function(vue_df){
  ## Create column in tagging data  corrosponding to date
  vue_df$date = as.Date(vue_df$datetime)
  
  ## Count detections per fish by day
  daily_detections_per_tag = aggregate(datetime ~ tag_id + date, data = vue_df, FUN = length)
  
  ## Create a dataframe of all study dates
  detections_per_date_df = data.frame('date' = seq.Date(from = min(as.Date(vue_df$date)) - 1, to = max(as.Date(vue_df$datetime)), by = 'day'))

  ## Loop through tag ides
  for (tag_id in unique(vue_df$tag_id)){
    
    ## Subset daily_detections_per_tag to a single fish
    daily_tag_detections = daily_detections_per_tag[daily_detections_per_tag$tag_id == tag_id, ]
    colnames(daily_tag_detections)[colnames(daily_tag_detections) == 'datetime'] = 'n_detections'
    
    ## Count daily detections 
    daily_detections = c()
    ## Detection Vector 
    detection_vector = rep(0, length(detections_per_date_df$date))
    for (i in 1:length(detections_per_date_df$date)){
      if (detections_per_date_df$date[i] %in% daily_tag_detections$date){
        detection_vector[i] = daily_tag_detections$n_detections[daily_tag_detections$date == detections_per_date_df$date[i]]
      }
    }
    
    ## Create a new column with a column name corrosponding to tag id
    detections_per_date_df = cbind(detections_per_date_df, detection_vector)
    colnames(detections_per_date_df)[ncol(detections_per_date_df)] =  as.character(tag_id)
  }
  
  date_col_names = detections_per_date_df[ ,1]
  detections_per_date_df_t = as.data.frame((t(detections_per_date_df[, -1])))
  colnames(detections_per_date_df_t) = date_col_names
  
  return(detections_per_date_df_t)
}

calculate_time_at_liberty = function(vue_df){
  #### Calculating the number of days between the first and last detection for each tag in the data set
  ## Aggregating to get min and max dates
  time_at_liberty = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = min)
  colnames(time_at_liberty) = c("tag_id", "min_date")
  time_at_liberty$max_date = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = max)$'x'
  ## Calculating difference between them. Add 1 to this to get difference inclusive of all dates. ex: Fish tagged 1/1/18 last detection 1/1/18 should have time at liberty = 1 not 0
  time_at_liberty$days_at_liberty = as.numeric(difftime(time_at_liberty$max_date, time_at_liberty$min_date, units = "days") + 1)
  return(time_at_liberty)
}
  
calculate_days_detected = function(vue_df){
  #### Calculating the number of unique days a tag was detected
  days_detected = aggregate(vue_df$date, by = list(vue_df$tag_id), FUN = uniqueN)
  colnames(days_detected) = c("tag_id", "unique_days")
  return(days_detected)
}
  

    
  
  

