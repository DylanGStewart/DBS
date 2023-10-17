#' Mortality Date Estimation
#'
#' This package and the functions within were developed to assist collaborators on the Sonora Mexico desert bighorn sheep project identify defunct clusters and accurately predict mortality and/or dropped collar dates. The package relies on functions in the amt package that can be adjusted to better identify defunct clusters. The user can also specify if sections of location data should be ignored. For example, collars are often recovered and powered off hours to weeks post-recovery. This can create secondary clusters that traditional tools are unable to account for. This package allows the user to filter out these clusters to better identify true defunct clusters. The user can select the option to have the updated metadata file output in CSV format. A new folder will be created in the directory that contains the original metadata. Additionally, the user will select prompts within the code to better tailor the tool to the specific project. Lastly, an interactive map is displayed with live fixes, defunct cluster fixes, movement path, and a predicted final live fix to allow the user to confirm the validity of the predicted dates before data is extracted. The function is specifically designed to pull location data from the API using Vectronic collar keys. As such, all time stamps are in UTC+0.
#' @param id                   numeric. A specific collar id number. The user must select an individual.
#' @param date                 POSIXct object. The beginning date time in which to focus the search for defunct clusters. A date time must be listed in "YYYY-MM-DDTHH:MM:SS" format. Dates are downloaded in UTC+0.
#' @param removed              POSIXct object. The last date time in which to consider when searching for defunct clusters. The date should be moved forward if collars were moved from the study site and a secondary cluster was created. A date time must be listed in "YYYY-MM-DDTHH:MM:SS" format. Dates are downloaded in UTC+0.
#' @param zeta                 numeric. Threshold (determined by the CRS, typically meters) for determining non-movement and the formation of a defunct cluster. For example, if 10 is specified, fixes less than or equal to 10 meters would be considered non-movement. See ?flag_defunct_clusters for more information.
#' @param eta                  numeric. The minimum number of steps to be considered non-movement. For example, 2 would mean the animal must persist in a state of non-movement for a minimum of two success fixes to create a defunct cluster. See ?flag_defunct_clusters for more information.
#' @param theta                numeric. The minimum number of hours in which a cluster must persist in a non-movement state. For example, 8 would mean the animal must meet the aforementioned criteria for a minimum of 8 hours to create a defunct cluster. See ?flag_defunct_clusters for more information.
#' @param filepath             File path to the locally stored metadata. The user must specify a file path.
#' @param keypath              File path to the locally stored Vectronic collar keys. Folder should only contain keys of deployed collars. The user must specify a file path.
#' @examples 
#' DBSmort(id = 39971,
#'          date = '2023-02-10T00:00:01',
#'          removed = '2023-02-15T21:01:39',
#'          zeta = 30,
#'          eta = 2,
#'          theta = 6,
#'          reason = "cougar",
#'          filepath = "E:\\Texas A&M\\DBS Mexico\\metadata files\\metadata.xlsx",
#'          keypath = "E:\\Texas A&M\\DBS Mexico\\Vectronic Key Files")  #' theta is in hours!
#' @author Dylan Stewart, Ph.D. student, Texas A&M University, dylan.stewart@tamu.edu
#' @export
DBSmort <- function(id, date, removed, zeta, eta, theta, reason, filepath, keypath){ 
  #' packages
  required_packages <- c("adehabitatLT","amt","mapview",'openxlsx',"readxl","remotes","sf","tidyverse") 
  lapply(required_packages, require, character.only = TRUE)
  list.of.packages <- c("adehabitatLT","amt","mapview",'openxlsx',"readxl","remotes","sf","tidyverse")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  suppressWarnings({update.packages(oldPkgs = list.of.packages, ask = FALSE)})
  if(suppressWarnings({collarinstall = require("collar")}) == FALSE){
    remotes::install_github("Huh/CollaR")
    library(collar)
  }else{
    library(collar)
  }
  required_packages <- c("adehabitatLT","amt","mapview",'openxlsx',"readxl","remotes","sf","tidyverse")
  lapply(required_packages, require, character.only = TRUE)
  metadata = suppressWarnings(suppressMessages(read_excel(filepath) %>% mutate(collarid = as.numeric(collarid))))
  keyfile = get_paths(file.path(keypath))
  DBSdata = suppressMessages(fetch_vectronics(keyfile, start_date = "2000-01-01T00:00:01", which_date = "acquisition")) %>% mutate(idcollar = as.numeric(idcollar))
  individual = DBSdata %>% mutate(acquisitiontime = as.POSIXct(acquisitiontime, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>%
    filter(idcollar %in% id & acquisitiontime >= as.POSIXct(date, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% arrange(acquisitiontime) %>%
    filter(!acquisitiontime >= as.POSIXct('2025-01-01T00:00:01', format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')) %>% filter(!longitude %in% NA | !latitude %in% NA)
    message = readline(prompt="Do you plan to remove fixes following mortality [yes] if not type [no] to see all data")
  message = as.character(message)
  if(message == "[yes]"){
   individual = individual %>% filter(!acquisitiontime >= as.POSIXct(removed, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC'))
  }else{}
  coordinates(individual) = c("longitude", "latitude")
  proj4string(individual) = CRS("+proj=longlat +datum=NAD83 +units=m +no_defs") 
  individual = as.data.frame(spTransform(individual, CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs"))) %>%
    rename(longitude = coords.x1, latitude = coords.x2)
  track = make_track(individual, longitude, latitude, acquisitiontime) 
  check = flag_defunct_clusters(track, zeta, eta, (lubridate::period(as.numeric(theta),"hours")))
  lastlivefix = as.data.frame(as.data.frame(check) %>% filter(defunct_cluster_ %in% FALSE) %>%
                                group_by(defunct_cluster_) %>% filter(t_ != max(t_)) %>% arrange(desc(t_)) %>% slice(1))
  points = check %>% rename (zerolength = defunct_cluster_) %>%
    st_as_sf(coords = c("x_", "y_"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs") %>%
    mapview(zcol = "zerolength", alpha.regions = 4, col.regions = c("darkgreen","red"), color = "black", cex = 6)
  deermovementpath = check %>% filter(defunct_cluster_ %in% FALSE) %>%
    st_as_sf(coords = c("x_", "y_"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs") %>%
    summarise(do_union = FALSE) %>% st_cast("LINESTRING") 
  postmortemcluster = check %>% filter(defunct_cluster_ %in% TRUE) %>%
    st_as_sf(coords = c("x_", "y_"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs") %>%
    summarise(do_union = FALSE) %>% st_cast("LINESTRING") 
  if(lastlivefix[1,3] %in% NA){
    map = points + mapview(deermovementpath, color = "darkgreen") + mapview(postmortemcluster, color = "red")
    print(map)
  }else{
    lastpoint = individual %>% filter(acquisitiontime %in% lastlivefix[1,3]) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs") 
    map = points + mapview(lastpoint, alpha.regions = 4, col.regions = "purple", color = "black", cex = 6) + mapview(deermovementpath, color = "darkgreen") + mapview(postmortemcluster, color = "red")
    print(map)
  }
  message = readline(prompt="Check and see if the map looks accurate if so type [accurate] if not type [notaccurate] to exit")
  message = as.character(message)
  if(message == "[accurate]"){
    metadata = metadata %>% mutate(lastfixoutsidemortclust_UTC = as.character(lastfixoutsidemortclust_UTC)) %>% mutate(lastfixoutsidemortclust_UTC = ifelse(collarid == id, as.character(lastlivefix[1,3]),lastfixoutsidemortclust_UTC)) %>%
      mutate(fate = ifelse(collarid == id, 1,fate)) %>%
      mutate(zeta_m = ifelse(collarid == id, zeta ,zeta_m)) %>%
      mutate(eta_fixes = ifelse(collarid == id, eta ,eta_fixes)) %>%
      mutate(theta_hr = ifelse(collarid == id, theta ,theta_hr)) %>%
      mutate(cause = ifelse(collarid == id, reason ,cause)) %>%
      mutate(lastfixoutsidemortclust_UTC = as.POSIXct(lastfixoutsidemortclust_UTC, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC'))
    message = readline(prompt="Did you remove fixes following mortality and need to document them [yes] or [no]")
    message = as.character(message)
    if(message == "[yes]"){
    metadata = metadata %>% mutate(beginremoved_UTC = as.character(beginremoved_UTC)) %>% mutate(beginremoved_UTC = ifelse(collarid == id, as.character(gsub("T", " ",removed)),beginremoved_UTC)) %>%  
      mutate(beginremoved_UTC = as.POSIXct(beginremoved_UTC, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
      mutate_at(c('DOB','starttestingdate','starthandlingpens','startsoftreleasepens'), as.Date)
      dir.create(path = paste0(gsub("metadata.xlsx", "new folder",filepath)))
    openxlsx::write.xlsx(metadata,paste0(gsub("metadata.xlsx", "new folder",filepath), '\\metadata.xlsx'), keepNA = FALSE)
    }else{
      metadata = metadata %>% mutate_at(c('DOB','starttestingdate','starthandlingpens','startsoftreleasepens'), as.Date)
      dir.create(path = paste0(gsub("metadata.xlsx", "new folder",filepath)))
      openxlsx::write.xlsx(metadata,paste0(gsub("metadata.xlsx", "new folder",filepath), '\\metadata.xlsx'), keepNA = FALSE)    
    }
  }else{
    print("rerun")
  }
}
