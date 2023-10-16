#' Collar Video File Reorganization
#'
#' This package and the functions within were developed to assist collaborators on the Sonora Mexico desert bighorn sheep project reorganize video files (.mp4) collected from GPS collars into new subdirectories based on fix success and coordinate acquisition. Multiple options were added to allow the user to visualize the GPS data and paired video acquisition locations interactively. These options allow the user to specify the inclusion or exclusion of mortality data and/or video location data. Interactive mapping is specific to an individual, which the user can specify within the function. The collaborator also has the option to select between a range of dates or view all the GPS data for the individual. All dates are in UTC+0. Lastly, the user can specify to have a copy of the video metadata exported and the video files copied and reorganized in folders based on fix success and coordinate acquisition. It is imperative that the user select a choice for each option in the function.
#' @param video                logical. If TRUE, then the paired video locations and GPS fixes will both be projected on an interactive map. If FALSE, only the GPS data will be projected. The user must specify TRUE or FALSE.
#' @param mortdata             logical. If TRUE, mortality data will be displayed and color coded on an interactive map. If FALSE, mortality data will not be color coded. Only specify TRUE if mortality data is available for the individual. The user must specify TRUE or FALSE.
#' @param id                   numeric. GPS data will be mapped for the individual. Only one individual may be selected. The user must specify an individual.
#' @param selection            logical. If TRUE, mapping will range from the date time specified between "begin" and "end". If FALSE, all GPS data will be included in the interactive map. The user must specify TRUE or FALSE. 
#' @param begin                POSIXct object. If selection is TRUE, mapping will start at the date time specified. Date time objects must be in "YYYY-MM-DD HH:MM:SS" format. The time stamp object must contain parentheses (e.g.,"2023-01-01 00:00:01"), and all date time objects are subset based on UTC+0. The user must specify a date time if selection == TRUE. 
#' @param end                  POSIXct object. If selection is TRUE, mapping will end at the date time specified. Date time objects must be in "YYYY-MM-DD HH:MM:SS" format. The time stamp object must contain parentheses (e.g.,"2023-01-01 00:00:01"), and all date time objects are subset based on UTC+0. The user must specify a date time if selection == TRUE.          
#' @param videofolderpath      File path to the locally stored collar video file containing folders for each individual. Video files for each individual must be in id_collarid format. The user must specify a file path.
#' @param veckeypath           File path to the locally stored Vectronic collar keys. Folder should only contain keys of deployed collars. The user must specify a file path.
#' @param metadatafilepath     File path to the locally stored metadata. User should verify that the metadata is up-to-date. The user must specify a file path.
#' @param savevideometadata    logical. If selection is TRUE, a copy of the video metadata will be exported to the video folder. If FALSE, the metadata will not be exported. The user must specify TRUE or FALSE.
#' @param subdivide            logical. If selection is TRUE, video files will be copied into new folders based on id, video status, and coordinate acquisition success. The user must specify TRUE or FALSE.
#' @examples 
#' DBSvideo(video = TRUE,
#'          mortdata = TRUE,
#'          id = 39971,
#'          selection = FALSE,
#'          begin = "2023-02-01 00:00:01",
#'          end = "2023-02-20 00:00:01",
#'          videofolderpath = "E:\\Texas A&M\\DBS Mexico\\Video",
#'          veckeypath = "E:\\Texas A&M\\DBS Mexico\\Vectronic Key Files",
#'          metadatafilepath = "E:\\Texas A&M\\DBS Mexico\\metadata files\\metadata.xlsx",
#'          savevideometadata = TRUE,
#'          subdivide = TRUE)
#' @author Dylan Stewart, Ph.D. student, Texas A&M University, dylan.stewart@tamu.edu
#' @export
DBSvideo <- function(video, mortdata, id, selection, begin, end, videofolderpath, veckeypath, metadatafilepath, savevideometadata, subdivide){
list.of.packages <- c("remotes","epitools",'googledrive',"googlesheets4","lutz","MODISTools",'mapview',"move","raster","readxl",'terra',"tidyverse",'utils','sf')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
suppressWarnings({update.packages(oldPkgs = list.of.packages, ask = FALSE)})
if(suppressWarnings({collarinstall = require("collar")}) == FALSE){
  remotes::install_github("Huh/CollaR")
library(collar)
}else{
library(collar)
}
required_packages <- c("epitools",'googledrive',"googlesheets4","lutz","MODISTools",'mapview',"move","raster","readxl",'terra',"tidyverse",'utils','sf')
lapply(required_packages, require, character.only = TRUE)
if(subdivide == TRUE){
  metadata = read_excel(metadatafilepath) %>% filter(!animalid %in% NA) %>% mutate(collarid = as.numeric(collarid))
  metadata[which(metadata$lastfixoutsidemortclust_UTC %in% NA),"lastfixoutsidemortclust_UTC"] <- as.Date(Sys.Date()) + 1
  keypath = get_paths(file.path(veckeypath))
  DBSdata = suppressMessages(fetch_vectronics(keypath, start_date = "2000-01-01T00:00:01", which_date = "acquisition"))
  if(video == FALSE){
    rawdata = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
      left_join(metadata %>% dplyr::select(collarid, starttestingdate, lastfixoutsidemortclust_UTC), by = c('idcollar' = 'collarid')) %>%
      filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% rename(CollarID = idcollar) %>%
      filter(!longitude > 5000) %>% arrange(CollarID, acquisitiontime) %>% filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
    if(mortdata == TRUE){
      if(selection == TRUE){
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
          mapview(postmortmovementpath, color = 'red') +
          mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6)
      }else{
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
          mapview(postmortmovementpath, color = 'red') +
          mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6)
      }
    }else{
      if(selection == TRUE){
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6)
      }else{
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6)
      }
    }
  }else{
    DBSdata[which(DBSdata$longitude %in% NA),"longitude"] <- as.numeric('7777')
    DBSdata[which(DBSdata$latitude %in% NA),"latitude"] <- as.numeric('7777')
    dirnames = data.frame(directoryname = list.dirs(path = videofolderpath, full.names = TRUE)) %>%
      filter(!directoryname %in% videofolderpath)
    datum = tibble(AnimalID = character(0), CollarID = character(0), TimeStamp_UTC = POSIXct(0), Year = numeric(0),
                   Month = numeric(0), Day = numeric(0), Hour = numeric(0), Minute = numeric(0), Second = numeric(0), directoryname = character(0))
    for(i in 1:nrow(dirnames)){
      df = data.frame(filename = list.files((dirnames[i,1]), pattern='mp4$', all.files=TRUE, full.names=FALSE)) %>%
        mutate(filename2 = filename) %>% separate(filename2, c('Year', 'Month', 'Day', 'Time','FileFormat')) %>%
        separate(Time, c("Hour", "MinuteSecond"), sep = 2, remove = F) %>%
        separate(MinuteSecond, c("Minute", "Second"), sep = 2, remove = F) %>%
        mutate_at(c('Year','Month','Day','Hour','Minute','Second'), as.numeric) %>%
        mutate(TimeStamp_UTC = make_datetime(year = Year, month = Month, day = Day, hour = Hour, min = Minute, sec = Second, tz = 'UTC')) %>%
        add_column(dirname = paste0(dirnames[i,1])) %>% mutate(dir2 = gsub(paste0(str_sub(videofolderpath, end = -1),"/"),'', dirname, fixed = TRUE)) %>%
        separate(dir2, c('AnimalID', 'CollarID')) %>%
        mutate(directoryname = paste0(dirname,"/", filename)) %>%
        dplyr::select(AnimalID, CollarID, TimeStamp_UTC, Year, Month, Day, Hour, Minute, Second, directoryname)
      datum = suppressMessages(full_join(datum, df))
    }
    videodatum = datum %>% left_join(metadata %>% mutate_at(c('animalid','collarid'), as.character) %>% dplyr::select(animalid, collarid, sex, lastfixoutsidemortclust_UTC,
                                                                                                                      starttestingdate, starthandlingpens, startsoftreleasepens,
                                                                                                                      firstfixoutsidepen_UTC), by = c('AnimalID' = 'animalid', 'CollarID' = 'collarid')) %>%
      mutate(livevideoprerelease = ifelse(TimeStamp_UTC < firstfixoutsidepen_UTC, 'Yes', 'No')) %>%
      mutate(livevideopostrelease = ifelse(TimeStamp_UTC >= firstfixoutsidepen_UTC & TimeStamp_UTC <= lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(mortvideo = ifelse(TimeStamp_UTC > lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(Date = as.Date(TimeStamp_UTC)) %>%
      left_join(DBSdata %>% dplyr::select(idcollar, acquisitiontime, latitude, longitude, height, temperature) %>%
                  mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
                  mutate(Date = as.Date(acquisitiontime), Hour = hour(acquisitiontime)) %>%
                  mutate_at(c('idcollar'), as.character), by = c('CollarID' = 'idcollar','Date','Hour')) %>%
      mutate(status = ifelse(longitude > 5000 | latitude > 5000, 'Successful', '')) %>%
      mutate(status = ifelse(longitude <= 5000 | latitude <= 5000, 'Successful', status)) %>%
      mutate(status = ifelse(longitude %in% NA & Hour %in% 0, 'Unsuccessful', status)) %>%
      mutate(status = ifelse(longitude %in% NA & Hour %in% 15, 'Unsuccessful', status)) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude <= 5000, 'Exact', '')) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude > 5000, 'Missing', coordinates)) %>%
      mutate(coordinates = ifelse(status %in% 'Unsuccessful', 'None', coordinates)) %>%
      mutate(longitude = ifelse(longitude > 5000, 'NA', longitude)) %>%
      mutate(latitude = ifelse(latitude > 5000, 'NA', latitude)) %>%
      group_by(AnimalID, CollarID, Date, Hour) %>% filter(acquisitiontime == min(acquisitiontime) | acquisitiontime %in% NA) %>% ungroup() %>%
      dplyr::select(AnimalID, CollarID, sex, livevideoprerelease, livevideopostrelease, mortvideo, status, coordinates,
                    TimeStamp_UTC, Year, Month, Day, Hour, Minute, Second, starttestingdate, starthandlingpens,
                    startsoftreleasepens, firstfixoutsidepen_UTC, lastfixoutsidemortclust_UTC, acquisitiontime,
                    latitude, longitude, height, temperature, directoryname)
    videodatum2 = datum %>% anti_join(videodatum %>% filter(!status %in% NA), by = c('AnimalID','TimeStamp_UTC')) %>%
      mutate(Hour2 = ifelse(Hour %in% 19 & Minute > 45, 21, Hour)) %>%
      mutate(Hour2 = ifelse(Hour %in% 20, 21, Hour2)) %>%
      left_join(metadata %>% mutate_at(c('animalid','collarid'), as.character) %>% dplyr::select(animalid, collarid, sex, lastfixoutsidemortclust_UTC,
                                                                                                 starttestingdate, starthandlingpens, startsoftreleasepens,
                                                                                                 firstfixoutsidepen_UTC), by = c('AnimalID' = 'animalid', 'CollarID' = 'collarid')) %>%
      mutate(livevideoprerelease = ifelse(TimeStamp_UTC < firstfixoutsidepen_UTC, 'Yes', 'No')) %>%
      mutate(livevideopostrelease = ifelse(TimeStamp_UTC >= firstfixoutsidepen_UTC & TimeStamp_UTC <= lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(mortvideo = ifelse(TimeStamp_UTC > lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(Date = as.Date(TimeStamp_UTC)) %>%
      left_join(DBSdata %>% dplyr::select(idcollar, acquisitiontime, latitude, longitude, height, temperature) %>%
                  mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
                  mutate(Date = as.Date(acquisitiontime), Hour = hour(acquisitiontime)) %>%
                  mutate_at(c('idcollar'), as.character) %>% filter(Hour %in% 21), by = c('CollarID' = 'idcollar','Date','Hour2' = 'Hour')) %>%
      mutate(status = ifelse(longitude > 5000 | latitude > 5000, 'Successful', '')) %>%
      mutate(status = ifelse(longitude <= 5000 | latitude <= 5000, 'Successful', status)) %>%
      mutate(status = ifelse(longitude %in% NA & Hour2 %in% 21, 'Unsuccessful', status)) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude <= 5000, 'Approximate', NA)) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude > 5000, 'ApproxMissing', coordinates)) %>%
      mutate(coordinates = ifelse(status %in% 'Unsuccessful', 'None', coordinates)) %>%
      mutate(longitude = ifelse(longitude > 5000, 'NA', longitude)) %>%
      mutate(latitude = ifelse(latitude > 5000, 'NA', latitude)) %>%
      group_by(AnimalID, CollarID, Date, Hour) %>% filter(acquisitiontime == min(acquisitiontime) | acquisitiontime %in% NA) %>% ungroup() %>%
      dplyr::select(AnimalID, CollarID, sex, livevideoprerelease, livevideopostrelease, mortvideo, status, coordinates,
                    TimeStamp_UTC, Year, Month, Day, Hour, Minute, Second, starttestingdate, starthandlingpens,
                    startsoftreleasepens, firstfixoutsidepen_UTC, lastfixoutsidemortclust_UTC, acquisitiontime,
                    latitude, longitude, height, temperature, directoryname)
    videodatum3 = rbind(videodatum %>% filter(!status %in% NA), videodatum2) %>% mutate(bindingcolumn = paste0(CollarID, status, coordinates))
    if(savevideometadata == TRUE){
      write.csv(videodatum3 %>% dplyr::select(-bindingcolumn),paste0(videofolderpath,"\\","DBS video spreadsheet.csv"), row.names = FALSE)
      rawdata = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
        left_join(metadata %>% dplyr::select(collarid, starttestingdate, lastfixoutsidemortclust_UTC), by = c('idcollar' = 'collarid')) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% rename(CollarID = idcollar) %>%
        filter(!longitude > 5000) %>% arrange(CollarID, acquisitiontime) %>% filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      videodatum4 = videodatum3 %>% arrange(CollarID, acquisitiontime) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      if(mortdata == TRUE){
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }else{
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)

        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }
    }else{
      rawdata = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
        left_join(metadata %>% dplyr::select(collarid, starttestingdate, lastfixoutsidemortclust_UTC), by = c('idcollar' = 'collarid')) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% rename(CollarID = idcollar) %>%
        filter(!longitude > 5000) %>% arrange(CollarID, acquisitiontime) %>% filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      videodatum4 = videodatum3 %>% arrange(CollarID, acquisitiontime) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      if(mortdata == TRUE){
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }else{
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)

        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }
    }
  }
  dataframe = videodatum3 %>% distinct(CollarID, status, coordinates)
  for(i in 1:nrow(dataframe)){
    dir.create(path = paste0(videofolderpath,"/","groupedvideos" ,"/" ,dataframe[i,1], "/", dataframe[i,2],"/", dataframe[i,3]), recursive = TRUE)
  }
  filename = data.frame(subfoldername = list.dirs(path = paste0(videofolderpath,"/groupedvideos"), full.names = TRUE)) %>%
    mutate(dirname = gsub(paste0(videofolderpath,"/groupedvideos"),'', subfoldername, fixed = TRUE)) %>%
    filter(!dirname %in% '') %>% mutate(dirname = gsub('/','', dirname, fixed = TRUE)) %>% mutate(subfoldername = gsub('\\','/', subfoldername, fixed = TRUE))
  videodatum3 = videodatum3 %>% left_join(filename, by = c("bindingcolumn" = "dirname")) %>% dplyr::select(-bindingcolumn) %>%
    mutate(directoryname = gsub('\\','/', directoryname, fixed = TRUE))
  pb = txtProgressBar(title = "Video Files Subdivision Progress",min = 0, max = nrow(videodatum3), style = 3, width = 100, char = "=")
  for(i in 1:nrow(videodatum3)){
    file.copy(from = paste0(videodatum3[i,26]), to = paste0(videodatum3[i,27]))
    setTxtProgressBar(pb, i)
  }
}else{
  metadata = read_excel(metadatafilepath) %>% filter(!animalid %in% NA) %>% mutate(collarid = as.numeric(collarid))
  metadata[which(metadata$lastfixoutsidemortclust_UTC %in% NA),"lastfixoutsidemortclust_UTC"] <- as.Date(Sys.Date()) + 1
  keypath = get_paths(file.path(veckeypath))
  DBSdata = suppressMessages(fetch_vectronics(keypath, start_date = "2000-01-01T00:00:01", which_date = "acquisition"))
  if(video == FALSE){
    rawdata = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
      left_join(metadata %>% dplyr::select(collarid, starttestingdate, lastfixoutsidemortclust_UTC), by = c('idcollar' = 'collarid')) %>%
      filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% rename(CollarID = idcollar) %>%
      filter(!longitude > 5000) %>% arrange(CollarID, acquisitiontime) %>% filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
    if(mortdata == TRUE){
      if(selection == TRUE){
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
          mapview(postmortmovementpath, color = 'red') +
          mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6)
      }else{
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
          mapview(postmortmovementpath, color = 'red') +
          mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6)
      }
    }else{
      if(selection == TRUE){
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6)
      }else{
        premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
        premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
          summarise(do_union = FALSE) %>% st_cast("LINESTRING")
        mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6)
      }
    }
  }else{
    DBSdata[which(DBSdata$longitude %in% NA),"longitude"] <- as.numeric('7777')
    DBSdata[which(DBSdata$latitude %in% NA),"latitude"] <- as.numeric('7777')
    dirnames = data.frame(directoryname = list.dirs(path = videofolderpath, full.names = TRUE)) %>%
      filter(!directoryname %in% videofolderpath)
    datum = tibble(AnimalID = character(0), CollarID = character(0), TimeStamp_UTC = POSIXct(0), Year = numeric(0),
                   Month = numeric(0), Day = numeric(0), Hour = numeric(0), Minute = numeric(0), Second = numeric(0), directoryname = character(0))
    for(i in 1:nrow(dirnames)){
      df = data.frame(filename = list.files((dirnames[i,1]), pattern='mp4$', all.files=TRUE, full.names=FALSE)) %>%
        mutate(filename2 = filename) %>% separate(filename2, c('Year', 'Month', 'Day', 'Time','FileFormat')) %>%
        separate(Time, c("Hour", "MinuteSecond"), sep = 2, remove = F) %>%
        separate(MinuteSecond, c("Minute", "Second"), sep = 2, remove = F) %>%
        mutate_at(c('Year','Month','Day','Hour','Minute','Second'), as.numeric) %>%
        mutate(TimeStamp_UTC = make_datetime(year = Year, month = Month, day = Day, hour = Hour, min = Minute, sec = Second, tz = 'UTC')) %>%
        add_column(dirname = paste0(dirnames[i,1])) %>% mutate(dir2 = gsub(paste0(str_sub(videofolderpath, end = -1),"/"),'', dirname, fixed = TRUE)) %>%
        separate(dir2, c('AnimalID', 'CollarID')) %>%
        mutate(directoryname = paste0(dirname,"/", filename)) %>%
        dplyr::select(AnimalID, CollarID, TimeStamp_UTC, Year, Month, Day, Hour, Minute, Second, directoryname)
      datum = suppressMessages(full_join(datum, df))
    }
    videodatum = datum %>% left_join(metadata %>% mutate_at(c('animalid','collarid'), as.character) %>% dplyr::select(animalid, collarid, sex, lastfixoutsidemortclust_UTC,
                                                                                                                      starttestingdate, starthandlingpens, startsoftreleasepens,
                                                                                                                      firstfixoutsidepen_UTC), by = c('AnimalID' = 'animalid', 'CollarID' = 'collarid')) %>%
      mutate(livevideoprerelease = ifelse(TimeStamp_UTC < firstfixoutsidepen_UTC, 'Yes', 'No')) %>%
      mutate(livevideopostrelease = ifelse(TimeStamp_UTC >= firstfixoutsidepen_UTC & TimeStamp_UTC <= lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(mortvideo = ifelse(TimeStamp_UTC > lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(Date = as.Date(TimeStamp_UTC)) %>%
      left_join(DBSdata %>% dplyr::select(idcollar, acquisitiontime, latitude, longitude, height, temperature) %>%
                  mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
                  mutate(Date = as.Date(acquisitiontime), Hour = hour(acquisitiontime)) %>%
                  mutate_at(c('idcollar'), as.character), by = c('CollarID' = 'idcollar','Date','Hour')) %>%
      mutate(status = ifelse(longitude > 5000 | latitude > 5000, 'Successful', '')) %>%
      mutate(status = ifelse(longitude <= 5000 | latitude <= 5000, 'Successful', status)) %>%
      mutate(status = ifelse(longitude %in% NA & Hour %in% 0, 'Unsuccessful', status)) %>%
      mutate(status = ifelse(longitude %in% NA & Hour %in% 15, 'Unsuccessful', status)) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude <= 5000, 'Exact', '')) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude > 5000, 'Missing', coordinates)) %>%
      mutate(coordinates = ifelse(status %in% 'Unsuccessful', 'None', coordinates)) %>%
      mutate(longitude = ifelse(longitude > 5000, 'NA', longitude)) %>%
      mutate(latitude = ifelse(latitude > 5000, 'NA', latitude)) %>%
      group_by(AnimalID, CollarID, Date, Hour) %>% filter(acquisitiontime == min(acquisitiontime) | acquisitiontime %in% NA) %>% ungroup() %>%
      dplyr::select(AnimalID, CollarID, sex, livevideoprerelease, livevideopostrelease, mortvideo, status, coordinates,
                    TimeStamp_UTC, Year, Month, Day, Hour, Minute, Second, starttestingdate, starthandlingpens,
                    startsoftreleasepens, firstfixoutsidepen_UTC, lastfixoutsidemortclust_UTC, acquisitiontime,
                    latitude, longitude, height, temperature, directoryname)
    videodatum2 = datum %>% anti_join(videodatum %>% filter(!status %in% NA), by = c('AnimalID','TimeStamp_UTC')) %>%
      mutate(Hour2 = ifelse(Hour %in% 19 & Minute > 45, 21, Hour)) %>%
      mutate(Hour2 = ifelse(Hour %in% 20, 21, Hour2)) %>%
      left_join(metadata %>% mutate_at(c('animalid','collarid'), as.character) %>% dplyr::select(animalid, collarid, sex, lastfixoutsidemortclust_UTC,
                                                                                                 starttestingdate, starthandlingpens, startsoftreleasepens,
                                                                                                 firstfixoutsidepen_UTC), by = c('AnimalID' = 'animalid', 'CollarID' = 'collarid')) %>%
      mutate(livevideoprerelease = ifelse(TimeStamp_UTC < firstfixoutsidepen_UTC, 'Yes', 'No')) %>%
      mutate(livevideopostrelease = ifelse(TimeStamp_UTC >= firstfixoutsidepen_UTC & TimeStamp_UTC <= lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(mortvideo = ifelse(TimeStamp_UTC > lastfixoutsidemortclust_UTC, 'Yes', 'No')) %>%
      mutate(Date = as.Date(TimeStamp_UTC)) %>%
      left_join(DBSdata %>% dplyr::select(idcollar, acquisitiontime, latitude, longitude, height, temperature) %>%
                  mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
                  mutate(Date = as.Date(acquisitiontime), Hour = hour(acquisitiontime)) %>%
                  mutate_at(c('idcollar'), as.character) %>% filter(Hour %in% 21), by = c('CollarID' = 'idcollar','Date','Hour2' = 'Hour')) %>%
      mutate(status = ifelse(longitude > 5000 | latitude > 5000, 'Successful', '')) %>%
      mutate(status = ifelse(longitude <= 5000 | latitude <= 5000, 'Successful', status)) %>%
      mutate(status = ifelse(longitude %in% NA & Hour2 %in% 21, 'Unsuccessful', status)) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude <= 5000, 'Approximate', NA)) %>%
      mutate(coordinates = ifelse(status %in% 'Successful' & longitude > 5000, 'ApproxMissing', coordinates)) %>%
      mutate(coordinates = ifelse(status %in% 'Unsuccessful', 'None', coordinates)) %>%
      mutate(longitude = ifelse(longitude > 5000, 'NA', longitude)) %>%
      mutate(latitude = ifelse(latitude > 5000, 'NA', latitude)) %>%
      group_by(AnimalID, CollarID, Date, Hour) %>% filter(acquisitiontime == min(acquisitiontime) | acquisitiontime %in% NA) %>% ungroup() %>%
      dplyr::select(AnimalID, CollarID, sex, livevideoprerelease, livevideopostrelease, mortvideo, status, coordinates,
                    TimeStamp_UTC, Year, Month, Day, Hour, Minute, Second, starttestingdate, starthandlingpens,
                    startsoftreleasepens, firstfixoutsidepen_UTC, lastfixoutsidemortclust_UTC, acquisitiontime,
                    latitude, longitude, height, temperature, directoryname)
    videodatum3 = rbind(videodatum %>% filter(!status %in% NA), videodatum2) %>% mutate(bindingcolumn = paste0(CollarID, status, coordinates))
    if(savevideometadata == TRUE){
      write.csv(videodatum3 %>% dplyr::select(-bindingcolumn),paste0(videofolderpath,"\\","DBS video spreadsheet.csv"), row.names = FALSE)
      rawdata = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
        left_join(metadata %>% dplyr::select(collarid, starttestingdate, lastfixoutsidemortclust_UTC), by = c('idcollar' = 'collarid')) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% rename(CollarID = idcollar) %>%
        filter(!longitude > 5000) %>% arrange(CollarID, acquisitiontime) %>% filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      videodatum4 = videodatum3 %>% arrange(CollarID, acquisitiontime) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      if(mortdata == TRUE){
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }else{
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)

        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }
    }else{
      rawdata = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
        left_join(metadata %>% dplyr::select(collarid, starttestingdate, lastfixoutsidemortclust_UTC), by = c('idcollar' = 'collarid')) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% rename(CollarID = idcollar) %>%
        filter(!longitude > 5000) %>% arrange(CollarID, acquisitiontime) %>% filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      videodatum4 = videodatum3 %>% arrange(CollarID, acquisitiontime) %>%
        filter(acquisitiontime >= as.POSIXct('2022-11-12T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
      if(mortdata == TRUE){
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          postmortfixes = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          postmortmovementpath = rawdata %>% filter(acquisitiontime > lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(postmortmovementpath, color = 'red') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(postmortfixes, alpha.regions = 4, col.regions = "red", color = "black", cex = 6) + mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
        }
      }else{
        if(selection == TRUE){
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            filter(acquisitiontime >= as.POSIXct(begin, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC') & acquisitiontime <= as.POSIXct(end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)

        }else{
          premortfixes = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          premortmovementpath = rawdata %>% filter(acquisitiontime <= lastfixoutsidemortclust_UTC) %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs") %>%
            summarise(do_union = FALSE) %>% st_cast("LINESTRING")
          successapprox = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Approximate') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          successexact = videodatum4 %>% filter(status %in% 'Successful' & coordinates %in% 'Exact') %>% filter(CollarID %in% id) %>% filter(!longitude %in% NA | !latitude %in% NA) %>%
            st_as_sf(coords = c("longitude", "latitude"), crs="+proj=longlat +datum=NAD83 +units=m +no_defs")
          mapview(premortmovementpath, color = 'darkgreen') + mapview(premortfixes, alpha.regions = 4, col.regions = "darkgreen", color = "black", cex = 6) +
            mapview(successapprox, alpha.regions = 4, col.regions = "orange", color = "black", cex = 6) +
            mapview(successexact, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6)
     }
    }
   }
  }
 }
}