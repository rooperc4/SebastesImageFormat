#' Function to format Stereo Camera images for use in SEBASTES stereo-analysis software
#'
#' This function converts images collected by a stereo camera (stereopi v2 or Weeview 3d camera) to
#' paired images and metadata that can be used for stereo-image analysis in SEBASTES software. The
#' function prompts the user to point to a folder where the photos are located that need to be converted.
#' Then it will print the list of images in that folder. Next it will prompt the user for a location to place
#' the converted images and the metadata required by Sebastes. It will then do the image conversion and
#' produce the required metadata. It is important to note that the metadata generated by this function
#' is not informative about the images. The function can also be run from the command line using the
#' second example where the from and to file folders are specified.
#'
#' @param pathFrom This is the folder location where the photos to be converted are located
#' @param pathTo This is the folder location where you want the formated images and metadata to go
#' @param iformat This is the extension (e.g. "jpg" (default), "JPG", "tiff", etc.) of the images to format

#'
#' @keywords SEBASTES, Stereo imagery, Weeview 3d camera, StereoPi V2
#' @export
#' @examples
#' Example 1: User is prompted for file locations
#' ImageFormat()
#'
#' Example 2: Command line specification of file locations
#' ImageFormat("D:/Weeview/PHOTO","C:/Users/rooperc/Desktop/WeeViewTest")

ImageFormat<-function (pathFrom = NULL, pathTo = NULL, iformat = "jpg")
{
  require(RSQLite)
  require(magick)

  if (is.null(pathFrom)) {
    pathFrom <- choose.dir(default = "", caption = "Select raw image folder")
  }
  filesFrom <- list.files(path = pathFrom, pattern = paste0("*.",iformat),
                          full.names = TRUE)
  frametimes<-list.files(path = pathFrom, pattern = paste0("*.",iformat))
  t1<-which(file.size(filesFrom)==0|grepl("-preview",filesFrom))
  if(length(t1>0)){
  filesFrom<-filesFrom[-t1]
  frametimes<-frametimes[-t1]}

  frametimes<-sapply(strsplit(frametimes,paste0(".",iformat)),"[",1)
  frametimes<-as.POSIXct(frametimes,tz="UTC",format="%Y-%m-%d_%H-%M-%S")
  frametimes<-data.frame(FRAME_NUMBER=seq(1,length(frametimes),1),Time=frametimes)

  i<-1
  width<-image_info(image_read(filesFrom[i]))$width
  height<-image_info(image_read(filesFrom[i]))$height

  if(iformat!="Video"){
    isizeL <- (paste0(width/2,"x",height))
    isizeR <- (paste0(width,"x",height,"+",width/2))
  }



  print(filesFrom)
  if (is.null(pathTo)) {
    pathTo <- choose.dir(default = "", caption = "Select processed image folder")
  }
  setwd(pathTo)
  dir.create("./images")
  dir.create("./images/Left")
  dir.create("./images/Right")
  dir.create("./logs")
  dir.create("./settings")
  fileConn <- file("./logs/CamTrawlAcquisition.log")
  writeLines(c(format(Sys.time(), "%H:%M:%S"), as.character(Sys.Date()),
               "StereoCamera", "Image Splitting"), fileConn)
  close(fileConn)
  fileConn <- file("./logs/ImageLogger2.log")
  writeLines(c("StereoCamera", paste("Number of images = ",
                                      length(filesFrom), sep = "")), fileConn)
  close(fileConn)
  con1 <- dbConnect(RSQLite::SQLite(), system.file("extdata",
                                                   "Template.db3", package = "SebastesImageFormat"))
  myConn <- dbConnect(drv = SQLite(), dbname = "./logs/CamTrawlMetadata.db3")
  sqliteCopyDatabase(con1, myConn)
  dbDisconnect(con1)
  async_data <- data.frame(time = character(), sensor_id = character(),
                           header = character(), data = character(), stringsAsFactors = FALSE)
  cameras <- data.frame(camera = character(), mac_address = character(),
                        model = character(), label = character(), rotation = character(),
                        stringsAsFactors = FALSE)
  cameras[1:2, 1] <- c("Right", "Left")
  cameras[1:2, 3] <- c("Stereo", "Stereo")
  cameras[1:2, 4] <- c("right", "left")
  cameras[1:2, 5] <- "none"
  deployment_data <- data.frame(deployment_parameter = "image_extension",
                                parameter_value = iformat, stringsAsFactors = FALSE)
  dropped <- data.frame(number = integer(), camera = character(),
                        time = character(), stringsAsFactors = FALSE)
  images <- data.frame(number = integer(), camera = character(),
                       time = character(), name = character(), exposure_us = integer(),
                       stringsAsFactors = FALSE)
  sensor_data <- data.frame(number = integer(), sensor_id = character(),
                            header = character(), data = character(), stringsAsFactors = FALSE)
  k <- 1
  for (i in 1:length(filesFrom)) {
    frink <- image_read(filesFrom[i])
    frinkL <- image_crop(frink, isizeL)
    frinkR <- image_crop(frink, isizeR)
    image_write(frinkR, path = paste(pathTo, "/images/Right/",
                                     formatC(i, width = 4, format = "d", flag = "0"),
                                     "_Right.jpg", sep = ""), format = "jpg")
    images[k, 1] <- i
    images[k, 2] <- cameras$camera[cameras$label == "right"]
    images[k, 3] <- as.character(strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"))
    images[k, 4] <- paste(formatC(i, width = 4, format = "d",
                                  flag = "0"), "_Right.jpg", sep = "")
    images[k, 5] <- 2500
    k <- k + 1
    image_write(frinkL, path = paste(pathTo, "/images/Left/",
                                     formatC(i, width = 4, format = "d", flag = "0"),
                                     "_Left.jpg", sep = ""), format = "jpg")
    images[k, 1] <- i
    images[k, 2] <- cameras$camera[cameras$label == "left"]
    images[k, 3] <- as.character(strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%OS3"))
    images[k, 4] <- paste(formatC(i, width = 4, format = "d",
                                  flag = "0"), "_Left.jpg", sep = "")
    images[k, 5] <- 2500
    k <- k + 1
    async_data[i, 1] <- as.character(strftime(Sys.time(),
                                              format = "%Y-%m-%d %H:%M:%OS3"))
    async_data[i, 2] <- "Camera"
    async_data[i, 3] <- "$CTCS"
    async_data[i, 4] <- "$CTCS,StereoCamera"
    sensor_data[i, 1] <- i
    sensor_data[i, 2] <- "CTControl"
    sensor_data[i, 3] <- "$OHPR"
    sensor_data[i, 4] <- "$OHPR,,,,,,,,"
  }
  dbWriteTable(myConn, "async_data", async_data, append = TRUE)
  dbWriteTable(myConn, "cameras", cameras, append = TRUE)
  dbWriteTable(myConn, "deployment_data", deployment_data,
               append = TRUE)
  dbWriteTable(myConn, "dropped", dropped, append = TRUE)
  dbWriteTable(myConn, "images", images, append = TRUE)
  dbWriteTable(myConn, "sensor_data", sensor_data, append = TRUE)
  dbDisconnect(myConn)

  write.csv(frametimes,paste0(pathTo,"/image_times.csv"),row.names=FALSE)
}




#' Function to format Weeview 3d video images for use in SEBASTES stereo-analysis software
#'
#' This function converts video collected by a Weeview 3d camera to paired images and metadata that can be used for stereo-image analysis
#' in SEBASTES software. The function prompts the user to point to a folder where the Weeview movies are located that need
#' to be converted. Then it will print the list of movies in that folder. Next it will prompt the user for a location to place
#' the converted images and the metadata required by Sebastes. It will then sample the movie for individual frames at 1 second intervals,
#' do the image conversion and produce the required metadata. #' It is important to note that the metadata generated by this function is
#' not informative about the images. The function can also #' be run from the command line using the second example where the from and to
#' file folders are specified.
#'
#' @param pathFrom This is the folder location where the movies to be converted are located
#' @param pathTo This is the folder location where you want the formated images and metadata to go

#'
#' @keywords SEBASTES, Stereo imagery, Weeview 3d camera
#' @export
#' @examples
#' Example 1: User is prompted for file locations
#' ImageExtract()
#'
#' Example 2: Command line specification of file locations
#' ImageExtract("D:/Weeview/MOVIE","C:/Users/rooperc/Desktop/WeeViewTest2")

ImageExtract<-function(pathFrom=NA,pathTo=NA){
  require(RSQLite)
  require(magick)
  require(Rvision)
#  https://swarm-lab.github.io/ROpenCVLite/articles/install.html
#  install.packages("devtools")
#  devtools::install_github("swarm-lab/Rvision")

  if(is.na(pathFrom)){pathFrom<-choose.dir(default = "", caption = "Select video folder")}
  filesFrom<-list.files(path=pathFrom,pattern="*.MP4",full.names=TRUE)
  print(filesFrom)
  if(is.na(pathTo)){pathTo<-choose.dir(default = "", caption = "Select processed image folder")}
  setwd(pathTo)

    for(m in 1:length(filesFrom)){
    the_vid = Rvision::video(filesFrom[m])
    nf1<-nframes(the_vid)
    nf1<-seq(30,nf1,30)

  dir.create(paste0("./Video_",m,"/WeeViewVideoFrames"),recursive=TRUE)
  setwd(paste0("./Video_",m))
  for(i in nf1){
    frink<-readFrame(the_vid,i)
    write.Image(frink,paste0("./WeeViewVideoFrames/Image_",i,".JPG"))}

    }
  release(the_vid)

  ImageFormat(paste0("./WeeViewVideoFrames"),paste0(pathTo,"/Video_",m),"JPG")
 setwd(pathTo)
  }






