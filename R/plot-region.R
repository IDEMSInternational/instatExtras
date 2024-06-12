#' Generate a map of the selected product
#' 
#' @description This function generates a map of the selected product. It can plot either a certain year/month from a data file with multiple time steps or a single 2D field. The function requires the data to be prepared using the R script "Prep.Data.R" or "Apply.Function.R".
#' 
#' @param lon Numeric vector representing the longitudes.
#' @param lat Numeric vector representing the latitudes.
#' @param product The data product to be plotted.
#' @param time A vector representing the time steps of the data.
#' @param time_point The specific time point to be plotted. Default is "2002-01-01".
#' @param add2title Additional text to be added to the plot title. Default is "CM SAF, ".
#' @param lonmin The minimum longitude value for the plot. Default is NA.
#' @param lonmax The maximum longitude value for the plot. Default is NA.
#' @param latmin The minimum latitude value for the plot. Default is NA.
#' @param latmax The maximum latitude value for the plot. Default is NA.
#' @param height The height of the plot in pixels. Default is 600.
#' @param width The width of the plot in pixels. Default is 600.
#' @param plot.ano If TRUE, plot the anomalies. Default is FALSE.
#' @param set.col.breaks If TRUE, set custom color breaks. Default is FALSE.
#' @param brk.set A numeric vector representing custom color breaks. Default is seq(240,310,5).
#' @param colmin0 The minimum color value for the plot. Default is NA.
#' @param colmax0 The maximum color value for the plot. Default is NA.
#' @param ncol The number of colors in the color scale. Default is 14.
#' @param plotHighRes If TRUE, plot the map in high resolution. Default is FALSE.
#' @param plotCoastline If TRUE, plot the coastline. Default is TRUE.
#' @param plotCountries If TRUE, plot the country borders. Default is TRUE.
#' @param plotRivers If TRUE, plot the rivers. Default is FALSE.
#' @param contour.thick The thickness of the contour lines. Default is 2.
#' @param plotCities If TRUE, plot the cities. Default is TRUE.
#' @param pch.cities The symbol type for plotting cities. Default is 2.
#' @param cex.cities The size of the city symbols. Default is 1.
#' @param label.cities If TRUE, label the cities. Default is TRUE.
#' @param plotCapitals The type of capitals to plot. Default is 1.
#' @param cex.label.cities The size of the city labels. Default is 0.5.
#' @param dlat The latitude spacing for plotting. Default is 0.25.
#' @param plotOwnLocations If TRUE, plot user-defined locations. Default is FALSE.
#' @param loc_lon Numeric vector representing the longitudes of the user-defined locations. Default is c().
#' @param loc_lat Numeric vector representing the latitudes of the user-defined locations. Default is c().
#' @param loc_name Character vector representing the names of the user-defined locations. Default is c("").
#' @param label_pos The position of the city labels. Default is 1.
#' @param variable The variable name. Default is "Tm".
#' @param level The level of the data. Default is 5.
#' @param CTY.type The data type for the "CTY" variable. Default is 4.
#'
#' @export
#' @usage plot.region(...)
#'
#' @examples
#' # Example usage
#' # lon <- read.csv("lon.csv")
#' # lat <- read.csv("lat.csv")
#' # product <- read.csv("product.csv")
#' # time <- read.csv("time.csv")
#' # plot.region(lon, lat, product, time)
#'
#' # Example usage with additional parameters
#' # plot.region(lon, lat, product, time, time_point = as.Date("2002-01-01"), add2title = "CM SAF, ", lonmin = -10, lonmax = 10, latmin = 35, latmax = 45, height = 800, width = 800, plot.ano = TRUE, set.col.breaks = TRUE, brk.set = seq(240,310,10), colmin0 = 240, colmax0 = 310, ncol = 10, plotHighRes = TRUE, plotCoastline = FALSE, plotCountries = FALSE, plotRivers = TRUE, contour.thick = 3, plotCities = FALSE, pch.cities = 16, cex.cities = 1.5, label.cities = FALSE, plotCapitals = 2, cex.label.cities = 0.8, dlat = 0.5, plotOwnLocations = TRUE, loc_lon = c(0, 2, 4), loc_lat = c(40, 42, 44), loc_name = c("Location A", "Location B", "Location C"), label_pos = 3, variable = "Precipitation", level = 2, CTY.type = 2)

plot.region <- function(lon, lat, product, time, time_point = as.Date("2002-01-01"), add2title = "CM SAF, ", lonmin = NA, lonmax = NA, latmin = NA, latmax = NA, height = 600, width = 600, plot.ano = FALSE, set.col.breaks = FALSE, brk.set = seq(240,310,5), colmin0 = NA, colmax0 = NA, ncol = 14, plotHighRes = FALSE, plotCoastline = TRUE, plotCountries = TRUE, plotRivers = FALSE, contour.thick = 2, plotCities = TRUE, pch.cities = 2, cex.cities = 1, label.cities = TRUE, plotCapitals = 1, cex.label.cities = 0.5, dlat = 0.25, plotOwnLocations = FALSE, loc_lon = c(), loc_lat = c(), loc_name = c(""), label_pos = 1, variable = "Tm", level = 5, CTY.type = 4) {
  
  # Set the variable name
  #varname <- nc$var[[datalev]]$name
  varname <- attr(product, "name")
  if(is.null(varname)) varname <- attr(product, "longname")
  if(is.null(varname)) varname <- ""
  
  # In HLW and HSH multiple variables are stored in each file
  if (varname == "HLW" || varname =="HSH") {
    varname <- paste(varname, "_", variable, sep="")
  }
  
  # Retrieve the unit, the missing_data-value, and the title of the data
  # unit <- ncatt_get(nc, varname,"units")$value
  # missval <- ncatt_get(nc,varname,"_FillValue")$value
  unit <- attr(product, "units")
  missval <- attr(product, "_FillValue")
  
  # if (ncatt_get(nc,varname,"title")$hasatt==TRUE) {
  #   name <- ncatt_get(nc,varname,"title")$value 
  # } else {
  #   name <- varname
  # }
  name <- attr(product, "title")
  if(is.null(name)) name <- varname
  
  # The offset and the scalefactor is required because
  # the Fill_Value attribute is not applied by the ncdf-package
  # The offset and scalefactor is automatically applied to all data
  # if (ncatt_get(nc,varname,"add_offset")$hasatt==TRUE) {
  #   offset.value <- ncatt_get(nc,varname,"add_offset")$value }
  # if (ncatt_get(nc,varname,"scale_factor")$hasatt==TRUE) {
  #   scale.factor <- ncatt_get(nc,varname,"scale_factor")$value }
  offset.value <- attr(product, "add_offset")
  if(is.null(offset.value)) offset.value <- 0
  scale.factor <- attr(product, "scale_factor")
  if(is.null(scale.factor)) scale.factor <- 1
  
  time.unit <- attr(time, "units")
  
  time_ind <- which(time == time_point)
  if(length(time_ind) == 0) stop("time_point of ", time_point, " not within range of data.")
  lon <- lon[time_ind]
  lon <- unique(lon)
  lat <- lat[time_ind]
  lat <- unique(lat)
  product <- product[time_ind]
  if(missing(lonmin)) lonmin <- min(lon, na.rm = TRUE)
  if(missing(lonmax)) lonmax <- max(lon, na.rm = TRUE)
  if(missing(latmin)) latmin <- min(lat, na.rm = TRUE)
  if(missing(latmax)) latmax <- max(lat, na.rm = TRUE)
  
  nx <- length(lon)
  ny <- length(lat)
  nt <- length(time)
  
  field <- matrix(product, nrow = nx, ncol = ny)
  
  na.ind <- which(field == (missval * scale.factor + offset.value))
  field[na.ind] <- NA
  
  z <- field
  zdate <- time_point
  
  # Set the plot ranges
  lonplot=c(lonmin,lonmax)
  latplot=c(latmin,latmax)
  
  # Define the HOAPS2011 data set
  HOAPS2011 <- c("PRE","EMP","EVA","LHF","NSH","SWS")
  
  # Retrieve the name of the variable and the data
  datalev <- 1
  if (varname %in% HOAPS2011) datalev <- 2
  if (varname == "CTY") datalev <- CTY.type
  
  #--------------------------------------------------#
  
  # Invert the latitude dimension if necessary
  if (lat[ny] < lat[1]) {
    sort.out <- sort(lat,index.return=TRUE)
    lat <- sort.out$x
    z <- z[,sort.out$ix]
  }
  
  # Calulate the mean, min, max for the selected region only
  lon.reg <- which(lon >= lonmin & lon <= lonmax)
  lat.reg <- which(lat >= latmin & lat <= latmax)
  z.reg <- z[lon.reg,lat.reg]
  
  # Set the title of the plot
  title <- paste(name," (",unit,"), ",sep="")
  if (varname == "HLW" || varname =="HSH") {
    title <- paste(varname," (",unit,"), level: ",level.out,", ",sep="")
  }
  #----------------------------------------------------------
  # Set the number of rows and columns of the plot
  graphics::par(mfrow = c(1,1))
  
  colmin <- colmin0
  colmax <- colmax0
  
  if (is.na(colmin) && is.na(colmax)) {
    colmin <- min(z.reg,na.rm=TRUE)
    colmax <- max(z.reg,na.rm=TRUE)
  }
  
  if (set.col.breaks) {
    brk <- brk.set
  } else {
    brk <- seq(colmin,colmax,length.out=ncol+1)
  } 
  
  # Set the colors and the color bar for the Difference plots
  col.breaks <- brk
  ncolor <- length(col.breaks)
  at.ticks <- seq(1,ncolor)
  names.ticks <- round(col.breaks[at.ticks])
  zlim <- c(1,ncolor)
  
  colors <- colorRamps::matlab.like(ncolor-1)
  if (plot.ano) colors[as.integer(ncolor/2)] <- grDevices::rgb(1,1,1)
  
  # Generate the field to be plotted
  field.plot <- matrix(ncol=ny,nrow=nx)
  for (l in seq(1,ncolor-1) ) {
    idx <- which(z >= col.breaks[l] &
                   z < col.breaks[l+1],arr.ind=TRUE)
    field.plot[idx] <- l + 0.1
  }
  
  if (plotCoastline && plotCountries) (plotCoastline=FALSE)
  
  # Make the plot including color bar
  fields::image.plot(lon,lat,field.plot,xlab="longitude, deg E",ylab="latitude, deg N",
                     main=paste(title,add2title,zdate,sep=""),
                     legend.mar = 4, xlim=lonplot, ylim=latplot, zlim=zlim,
                     nlevel=(ncolor-1), col=colors,lab.breaks=names.ticks)
  
  if (plotHighRes){  
    
    # TODO: For sets of data, set up a package to use lazy-loading of data.
    utils::data("worldMapEnv", package = "maps")
    utils::data("worldHiresMapEnv", package = "mapdata")
    utils::data("countriesHigh", package = "rworldxtra")
    world <- methods::as(countriesHigh,"SpatialLines")
    
    # add rivers
    if (plotRivers) {
      maps::map('rivers', add=TRUE, col="blue")
    }
    
    # add coastline
    if (plotCoastline) {
      maps::map('worldHires', add=TRUE, interior=F)
    }
    
    # add country borders
    if (plotCountries) {
      plot(world,add=TRUE)
    }
    
    # add cities
    if (plotCities) {    
      if (label.cities) { 
        maps::map.cities(pch=pch.cities,cex=cex.cities,capitals=plotCapitals,label=TRUE)
      }else{maps::map.cities(pch=pch.cities,cex=cex.cities,capitals=plotCapitals,label=FALSE)}
    }
    
    # add own locations
    if (plotOwnLocations){
      if (length(loc_lon)==length(loc_lat)&length(loc_lon)==length(loc_name)){
        for (i in 1:length(loc_lon)){
          graphics::points(loc_lon[i],loc_lat[i],pch=pch.cities)
          text(loc_lon[i],loc_lat[i],loc_name[i], pos=label_pos)
        }
      }
    }
    
  }else{
    
    utils::data("worldMapEnv", package = "maps")
    
    # add rivers
    if (plotRivers) {
      maps::map('rivers', add=TRUE, col="blue")
    }
    
    # add coastline
    if (plotCoastline) {
      maps::map('world', add=TRUE, interior=F)
    }
    
    # add country borders
    if (plotCountries) {
      utils::data("countriesLow", package = "rworldmap")
      world <- methods::as(countriesLow,"SpatialLines")
      plot(world,add=TRUE)
    }
    
    # add cities
    if (plotCities) {    
      if (label.cities) { 
        maps::map.cities(pch=pch.cities,cex=cex.cities,capitals=plotCapitals,label=TRUE)
      }
      else {
        maps::map.cities(pch=pch.cities,cex=cex.cities,capitals=plotCapitals,label=FALSE)
      }
    }
    
    # add own locations
    if (plotOwnLocations){
      if (length(loc_lon)==length(loc_lat)&length(loc_lon)==length(loc_name)){
        for (i in 1:length(loc_lon)){
          graphics::points(loc_lon[i],loc_lat[i],pch=pch.cities)
          text(loc_lon[i],loc_lat[i],loc_name[i], pos=label_pos)
        }
      }
    }
  }
  
  # Draw lines around the plot
  graphics::axis(1,lwd=1,at=c(lonmin,lonmax),tick=TRUE,lwd.ticks=0,labels=FALSE)
  graphics::axis(2,lwd=1,at=c(latmin,latmax), tick=TRUE,lwd.ticks=0,labels=FALSE)
  graphics::axis(3,lwd=1,at=c(lonmin,lonmax),tick=TRUE,lwd.ticks=0,labels=FALSE)
  graphics::axis(4,lwd=1,at=c(latmin,latmax), tick=TRUE,lwd.ticks=0,labels=FALSE)
}