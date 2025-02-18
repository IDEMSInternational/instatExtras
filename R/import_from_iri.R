#' Import Data from IRI (International Research Institute for Climate and Society)
#' 
#' @description
#' This function imports data from various sources at the International Research Institute for Climate and Society (IRI) based on the specified parameters.
#'
#' @param download_from The source from which to download the data. Supported values are "CHIRPS_V2P0", "TAMSAT", "NOAA_ARC2", "NOAA_RFE2", "NOAA_CMORPH_DAILY", "NOAA_CMORPH_3HOURLY", "NOAA_CMORPH_DAILY_CALCULATED", "NASA_TRMM_3B42".
#' @param data_file The specific data file to download from the selected source.
#' @param path The directory path where the downloaded file will be saved. If empty, the current working directory is used.
#' @param X1 The starting longitude or X-coordinate of the desired data range.
#' @param X2 The ending longitude or X-coordinate of the desired data range.
#' @param Y1 The starting latitude or Y-coordinate of the desired data range.
#' @param Y2 The ending latitude or Y-coordinate of the desired data range.
#' @param get_area_point Specifies whether the data should be downloaded for an "area" or a single "point". 
#'
#' @return A list containing two elements:
#' - The imported data as a data frame.
#' - A data frame containing unique latitude and longitude values from the imported data.
#' 
#' @export
#'
#' @examples
#' # Import area data from CHIRPS_V2P0 source
#' # import_from_iri(download_from = "CHIRPS_V2P0", data_file = "daily_0p05",
#' #                 path = "data", X1 = -10, X2 = 10, Y1 = -10, Y2 = 10,
#' #                 get_area_point = "area")
#'
#' # Import point data from TAMSAT source
#' # import_from_iri(download_from = "TAMSAT", data_file = "rainfall_estimates",
#' #                 path = "", X1 = -1, Y1 = 50, get_area_point = "point")
#' 
import_from_iri <- function(download_from, data_file, path, X1, X2,Y1,Y2, get_area_point){
  if(path == ""){
    gaugelocdir = getwd()
  }
  else {
    if(!dir.exists(path)){
      dir.create(path)
    }
    gaugelocdir = path
  }
  
  if(download_from == "CHIRPS_V2P0"){
    prexyaddress <- "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0"
    if(data_file == "daily_0p05") {
      extension <- ".daily/.global/.0p05/.prcp"
    }
    else if(data_file == "daily_0p25") {
      extension <- ".daily/.global/.0p25/.prcp"
    }
    else if(data_file == "daily_improved_0p05") {
      extension <- ".daily-improved/.global/.0p05/.prcp"
    }
    else if(data_file == "daily_improved_0p25") {
      extension <- ".daily-improved/.global/.0p25/.prcp"
    }
    else if(data_file == "dekad") {
      extension <- ".dekad/.prcp"
    }
    else if(data_file == "monthly_c8113") {
      extension <- ".monthly/.global/.c8113/.precipitation"
    }
    else if(data_file == "monthly_deg1p0") {
      extension <- ".monthly/.global/.deg1p0/.precipitation"
    }
    else if(data_file == "monthly_NMME_deg1p0") {
      extension <- ".monthly/.global/.NMME_deg1p0/.precipitation"
    }
    else if(data_file == "monthly_prcp") {
      extension <- ".monthly/.global/.precipitation"
    }
    
    else stop("Data file does not exist for CHIRPS V2P0 data")
    #Annual and 2Monthly and 3monthly does not exist for CHIRPS_V2P0
  }
  else if(download_from == "TAMSAT") {
    prexyaddress <- "http://iridl.ldeo.columbia.edu/home/.remic/.Reading/.Meteorology/.TAMSAT"
    if(data_file == "rainfall_estimates") {
      extension <- ".TAMSAT-RFE/.rfe"
    }
    else if(data_file == "reconstructed_rainfall_anomaly") {
      extension <- ".TAMSAT-RFE/.rfediff"
    }
    else if(data_file == "sahel_dry_mask") {
      extension <- ".TAMSAT-RFE/.sahel_drymask"
    }
    else if(data_file == "SPI_1_dekad") {
      extension <- ".TAMSAT-RFE/.SPI-rfe_1-dekad_Sahel"
    }
    #monthly,climatology and TAMSAT RFE 0p1 are yet to be implemented.
    else stop("Data file does not exist for TAMSAT data")
  }
  else if(download_from=="NOAA_ARC2") {
    prexyaddress<-paste("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2")
    if(data_file == "daily_estimated_prcp") {
      extension <- ".daily/.est_prcp"
    }
    else if(data_file == "monthly_average_estimated_prcp") {
      extension <- ".monthly/.est_prcp"
    }
    else stop("Data file does not exist for NOAA ARC2 data")
  }
  else if(download_from=="NOAA_RFE2") {
    prexyaddress <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa"
    if(data_file == "daily_estimated_prcp"){
      extension <- ".DAILY/.RFEv2/.est_prcp"
    }
    else stop("Data file does not exist for NOAA RFE2 data")
  }
  else if(download_from=="NOAA_CMORPH_DAILY" || download_from=="NOAA_CMORPH_3HOURLY" || download_from=="NOAA_CMORPH_DAILY_CALCULATED") {
    if(download_from=="NOAA_CMORPH_DAILY") {
      prexyaddress <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.daily"
    }
    else if(download_from == "NOAA_CMORPH_3HOURLY") {
      prexyaddress <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly"
    }
    if(download_from == "NOAA_CMORPH_DAILY_CALCULATED") {
      prexyaddress <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.daily_calculated"
    }
    
    if(data_file == "mean_microwave_only_est_prcp") {
      extension <- ".mean/.microwave-only/.comb"
    }
    else if(data_file == "mean_morphed_est_prcp") {
      extension <- ".mean/.morphed/.cmorph"
    }
    if(data_file == "orignames_mean_microwave_only_est_prcp") {
      extension <- ".orignames/.mean/.microwave-only/.comb"
    }
    else if(data_file == "orignames_mean_morphed_est_prcp") {
      extension <- ".orignames/.mean/.morphed/.cmorph"
    }
    if(data_file == "renamed102015_mean_microwave_only_est_prcp") {
      extension <- ".renamed102015/.mean/.microwave-only/.comb"
    }
    else if(data_file == "renamed102015_mean_morphed_est_prcp") {
      extension <- ".renamed102015/.mean/.morphed/.cmorph"
    }
    else stop("Data file does not exist for NOAA CMORPH data")
  }
  else if(download_from=="NASA_TRMM_3B42") {
    prexyaddress <- "https://iridl.ldeo.columbia.edu/SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7"
    if(data_file == "daily_estimated_prcp") {
      extension <- ".daily/.precipitation"
    }
    else if(data_file == "3_hourly_estimated_prcp") {
      extension <- ".three-hourly/.precipitation"
    }
    else if(data_file == "3_hourly_pre_gauge_adjusted_infrared_est_prcp") {
      extension <- ".three-hourly/.IRprecipitation"
    }
    else if(data_file == "3_hourly_pre_gauge_adjusted_microwave_est_prcp") {
      extension <- ".three-hourly/.HQprecipitation"
    }
    else stop("Data file does not exist for NASA TRMM 3B42 data")
  }
  else{
    stop("Source not specified correctly.")
  }
  
  prexyaddress = paste(prexyaddress, extension, sep="/")
  #we need to add time range to get the data
  if(get_area_point == "area") {
    xystuff <- paste("X", X1, X2, "RANGEEDGES/Y", Y1, Y2, "RANGEEDGES", sep = "/")
    postxyaddress <- "ngridtable+table-+skipanyNaN+4+-table+.csv" 
  }
  else if(get_area_point == "point") {
    xystuff <- paste("X", X1, "VALUES/Y", Y1, "VALUES", sep = "/")
    postxyaddress <- "T+exch+table-+text+text+skipanyNaN+-table+.csv" 
  }
  else stop("Unrecognised download type.")
  
  address <- paste(prexyaddress,xystuff,postxyaddress,sep="/")
  
  file.name <- paste(gaugelocdir,"tmp_iri.csv", sep="/")
  utils::download.file(address, file.name, quiet=FALSE)
  dataout <- utils::read.table(paste(gaugelocdir, "tmp_iri.csv", sep="/"), sep = ",", header = TRUE)
  if(nrow(dataout) == 0) stop("There is no data for the selected point/area.")
  
  if(get_area_point == "point") {
    Longitude <- rep(X1, nrow(dataout))
    Latitude = rep(Y1, nrow(dataout))
    dataout = cbind(Longitude, Latitude, dataout)
  }
  
  lat_lon_dataframe = unique(dataout[,c(1,2)])
  
  file.remove(paste(gaugelocdir,"tmp_iri.csv",sep="/"))
  return(list(dataout,lat_lon_dataframe))
}