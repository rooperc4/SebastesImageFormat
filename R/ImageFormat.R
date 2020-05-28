#' Function to produce summarized fishing effort data by gear type and year group while accounting for confidentiality concerns
#'
#' This function takes a raw fishing effort data frame and summarizes it for mapping, creating a footprint_data_object. The raw fishing effort data should
#' include rows for each individual fishing event (set), such as a trawl haul or longline set. For each set the year, latitude, longitude, gear type and a
#' vessel identifier must be included. A minimum number of vessels can be specified so that the output does not include grid cells where less than this
#' number of vessels fished a given gear type in a given year group. The default value is 3 (consistent with Canadian requirements for confidentiality),
#' The starting year of interest (usually the earliest year in the data) and how many years should be combined should be specified. Additionally,
#' the spatial resolution that is required and the units of that spatial resolution (defaults are 0.5 decimal degrees of latitude and longitude) need to
#' be specified. A map projection (in proj4 format) should also be given.
#'
#' The function outputs a list of eight data objects that include a data frame of raw fishing effort data (fishing_effort_data) summarized to the spatial
#' resolution #' provided for each fishing event with an additional column for the year group to which the event belongs. There should be one record in this
#' table for each of the fishing events that were input. This table may not be appropriate to share with the NPFC, as it will contain records for grid cells
#' where fewer than the minimum number of vessels fished. The second data object output by the function (fishing_footprint_table) is a table that summarizes
#' the fishing events by year group, latitude, longitude and gear type. This table produces the number of sets that occurred in each year by each gear type
#' at a grid cell with the spatial resolution specified and the center position given by the latitude and longitude where it was grouped. In addition, a column
#' specifies the number of vessels that are included at each grid cell. The table will not include grid cells where there were fewer than the minimum number of
#' vessels fishing. This table may be appropriate for sharing with the wider group, as it will meet confidentiality rules. The third item is a table of the number
#' of grid cells by gear type and year group where the minimum number of vessels was not met (not_enough_vessels). This table indicates how many grid cells for which data exists, but
#' cannot be reported. The fourth item is the year groups identified from the inputs (years) and the fifth item is the gear types represented in the data (gear_
#' types). The last three items are the spatial resolution and units of the data summary and the map projection of the data (map_projection). These last items
#' are for tracking inputs and outputs and are needed for creating the figures using the footprint_map function.
#'
#' @param Year The year in which the fishing event occurred
#' @param Longitude The longitude of the fishing event in decimal degrees
#' @param Latitude The latitude of the fishing event in decimal degrees
#' @param Gear_type The gear type of the fishing event (e.g. trawl, trap, longline, gillnet)
#' @param Vessel_ID The vessel identifier for the set
#' @param Minimum_vessels The minimum number of vessels to fish in in a grid cell in order to satisfy confidentiality rules (default = 3)
#' @param Start_year The year to start summmarizing data
#' @param Years_to_combine The number of years to combine in the summary of data
#' @param Spatial_resolution The spatial resolution of the output grid (default = 0.5). If using "ISEA_12" or "ISEA_13" this value should be NA.
#' @param Units The units of the spatial resolution, can be "dd" for decimal degrees, "m" for projected data, "ISEA_12" or "ISEA_13"
#' @param map_projection The map projection should be specified in proj4 format (e.g. "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" for decimal degrees or "+proj=utm +zone=59 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" for UTM
#'
#'
#' @keywords NPFC, fishing effort, fishing footprint
#' @export
#' @examples
#' Spatial_resolution_dd<-data.frame(Spatial_resolution=.5,Units="dd")
#' Start_year<-1991
#' Years_to_combine<-5
#' Minimum_vessels<-3
#' map_projection_dd<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' example_dd<-footprint_data(effort_data$Year,effort_data$Longitude,effort_data$Latitude,effort_data$Gear_type,effort_data$Vessel,Minimum_vessels,Start_year,
#' Years_to_combine,Spatial_resolution_dd$Spatial_resolution,Spatial_resolution_dd$Units, map_projection=map_projection_dd)


ImageFormat<-function(pathFrom=NA,pathTo=NA){
 require(RSQLite)
  require(magick)
  #con1<-dbConnect(drv=SQLite(),dbname=system.file("extdata","Template.db3",package="WeeViewImageFormat"))

  if(is.na(pathFrom)){pathFrom<-choose.dir(default = "", caption = "Select raw image folder")}
  filesFrom<-list.files(path=pathFrom,pattern="*.JPG",full.names=TRUE)
  print(filesFrom)
  if(is.na(pathTo)){pathTo<-choose.dir(default = "", caption = "Select processed image folder")}
  setwd(pathTo)
  dir.create("./images")
  dir.create("./images/WeeViewLeft")
  dir.create("./images/WeeViewRight")
  dir.create("./logs")
  dir.create("./settings")
  fileConn<-file("./logs/CamTrawlAcquisition.log")
  writeLines(c(format(Sys.time(),"%H:%M:%S"),as.character(Sys.Date()),"WeeViewCamera","Image Splitting"), fileConn)
  close(fileConn)
  fileConn<-file("./logs/ImageLogger2.log")
  writeLines(c("WeeViewCamera",paste("Number of images = ",length(filesFrom),sep="")), fileConn)
  close(fileConn)

  con1<-dbConnect(RSQLite::SQLite(),system.file("extdata","Template.db3",package="WeeViewImageFormat"))
  myConn<-dbConnect(drv=SQLite(),dbname="./logs/CamTrawlMetadata.db3")
  sqliteCopyDatabase(con1,myConn)
  dbDisconnect(con1)

  async_data<-data.frame(time=character(),sensor_id=character(),header=character(),data=character(),stringsAsFactors=FALSE)
  cameras<-data.frame(camera=character(),mac_address=character(),model=character(),label=character(),rotation=character(),stringsAsFactors=FALSE)
  cameras[1:2,1]<-c("WeeViewRight","WeeViewLeft")
  cameras[1:2,3]<-c("WeeView","WeeView")
  cameras[1:2,4]<-c("right","left")
  cameras[1:2,5]<-"none"
  deployment_data<-data.frame(deployment_parameter="image_extension",parameter_value="jpg",stringsAsFactors=FALSE)
  dropped<-data.frame(number=integer(),camera=character(),time=character(),stringsAsFactors = FALSE)
  images<-data.frame(number=integer(),camera=character(),time=character(),name=character(),exposure_us=integer(),stringsAsFactors = FALSE)
  sensor_data<-data.frame(number=integer(),sensor_id=character(),header=character(),data=character(),stringsAsFactors = FALSE)

 k<-1
  for(i in 1:length(filesFrom)){
 frink<-image_read(filesFrom[i])
    frinkL<-image_crop(frink,"4032x4032")
    frinkR<-image_crop(frink,"8064x4032+4032")
    image_write(frinkR, path = paste(pathTo,"/images/WeeViewRight/",formatC(i, width = 4, format = "d", flag = "0"),"_Right.jpg",sep=""), format = "jpg")
    images[k,1]<-i
    images[k,2]<-cameras$camera[cameras$label=="right"]
    images[k,3]<-Sys.time()
    images[k,4]<-paste(formatC(i, width = 4, format = "d", flag = "0"),"_Right.jpg",sep="")
    images[k,5]<-2500
    k<-k+1
    image_write(frinkL, path = paste(pathTo,"/images/WeeViewLeft/",formatC(i, width = 4, format = "d", flag = "0"),"_Left.jpg",sep=""), format = "jpg")
    images[k,1]<-i
    images[k,2]<-cameras$camera[cameras$label=="left"]
    images[k,3]<-Sys.time()
    images[k,4]<-paste(formatC(i, width = 4, format = "d", flag = "0"),"_Left.jpg",sep="")
    images[k,5]<-2500
    k<-k+1

    async_data[i,1]<-Sys.time()
    async_data[i,2]<-"Camera"
    async_data[i,3]<-"$CTCS"
    async_data[i,4]<-"$CTCS,WeeViewCamera"

    sensor_data[i,1]<-i
    sensor_data[i,2]<-"CTControl"
    sensor_data[i,3]<-"$OHPR"
    sensor_data[i,4]<-"$OHPR,,,,,,,,"

  }

 dbWriteTable(myConn, "async_data", async_data,append=TRUE)
 dbWriteTable(myConn, "cameras",cameras,append=TRUE)
 dbWriteTable(myConn, "deployment_data",deployment_data,append=TRUE)
 dbWriteTable(myConn,"dropped",dropped,append=TRUE)
 dbWriteTable(myConn, "images",images,append=TRUE)
 dbWriteTable(myConn, "sensor_data",sensor_data,append=TRUE)
 dbDisconnect(myConn)

}
