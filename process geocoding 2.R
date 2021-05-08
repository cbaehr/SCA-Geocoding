

# country <- c("Bangladesh", "Pakistan", "Turkmenistan", "Uzbekistan", "Afghanistan", "India", "Kazakhstan", "Kyrgyzstan",
#              "Sri Lanka", "Tajikistan")
# abbrev <- c("BGD", "PAK", "TKM", "UZB", "AFG", "IND", "KAZ", "KGZ", "LKA", "TJK")

country <- c("Kazakhstan", "Kyrgyzstan",
             "Sri Lanka", "Tajikistan", "Turkmenistan")
abbrev <- c("KAZ", "KGZ", "LKA", "TJK", "TKM")

library(sf)

for (j in 1:length(country)) {
  
  rm(list = setdiff(ls(), c("country", "abbrev", "j")))
  
  a <- country[j]
  b <- abbrev[j]
  
  
  dat_path <- paste0("/Users/christianbaehr/GitHub/SCA Geocoding/", a, "/", a, "_master.csv")
  dat <- read.csv(dat_path, stringsAsFactors = F)
  dat_new <- dat[1,]
  dat_new$geometry <- NA
  dat_new <- dat_new[0,]
  
  
  for(i in c(1:nrow(dat))) {
    
    dat_path <- paste0("/Users/christianbaehr/GitHub/SCA Geocoding/", a, "/", a, "_master.csv")
    dat <- read.csv(dat_path, stringsAsFactors = F)
    
    dat_new <- dat[1,]
    dat_new$geometry <- NA
    dat_new <- dat_new[0,]
    
    
    if(dat$christian_geojson[i] %in% c("", "good code up", "drop")) {
      new_dat <- dat[i, (names(dat)!="christian_geojson")]
      new_dat[, c("X", "Y")] <- NA
    } else {
      geo <- st_read(dat$christian_geojson[i])
      if(dat$level=="province" & nrow(geo)>1) {stop("more than one province in row")}
      points_shp <- st_centroid(geo$geometry)
      points <- st_coordinates(points_shp)
      new_dat <- dat[i, (names(dat)!="christian_geojson")]
      new_dat[1:nrow(points), ] <- new_dat[1, ]
      new_dat <- cbind(new_dat, points)
    }
    
    if (i==1) {
      dat_out <- new_dat
    } else {
      dat_out <- rbind(dat_out, new_dat)
    }
    print(i)
    
  }
  
  
  
  dat_out_new <- dat_out
  
  dat_out_new[, c("X", "Y")] <- round(data.frame(dat_out_new)[, c("X", "Y")], digits=2)
  dat_out_new$merge_id <- paste0(dat_out_new$X, ", ", dat_out_new$Y)
  
  gadm2_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_", b, "_shp/gadm36_", b, "_2.shp")
  
  gadm2 <- st_read(gadm2_path, stringsAsFactors=F)
  gadm2$centroids <- st_centroid(gadm2$geometry)
  gadm2[, c("X", "Y")] <- st_coordinates(gadm2$centroids)
  gadm2[, c("X", "Y")] <- round(data.frame(gadm2)[, c("X", "Y")], digits=2)
  gadm2$merge_id <- paste0(gadm2$X, ", ", gadm2$Y)
  
  names(gadm2)[names(gadm2)=="ENGTYPE_2"] <- "type"
  names(gadm2)[names(gadm2)=="NAME_2"] <- "name"
  names(gadm2)[names(gadm2)=="GID_2"] <- "GID"
  
  ###
  
  sum(dat_out_new$merge_id %in% gadm2$merge_id)
  sum(dat_out_new$merge_id=="NA, NA")
  
  mask <- !((dat_out_new$merge_id %in% gadm2$merge_id) | (dat_out_new$merge_id=="NA, NA"))
  
  unique(dat_out_new$level)
  dat_out_new$level[dat_out_new$level=="actual province"] <- "province"
  dat_out_new$level[dat_out_new$level==""] <- "country"
  
  dat_out_new_district <- dat_out_new[dat_out_new$level=="district", ]
  dat_out_new_district_merge <- merge(dat_out_new_district, gadm2[, c("merge_id", "name", "type", "GID", "geometry")], by="merge_id")
  sum(duplicated(gadm2$merge_id))
  
  gadm1_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_", b, "_shp/gadm36_", b, "_1.shp")
  gadm1 <- st_read(gadm1_path, stringsAsFactors=F)
  gadm1$centroids <- st_centroid(gadm1$geometry)
  gadm1[, c("X", "Y")] <- st_coordinates(gadm1$centroids)
  gadm1[, c("X", "Y")] <- round(data.frame(gadm1)[, c("X", "Y")], digits=2)
  gadm1$merge_id <- paste0(gadm1$X, ", ", gadm1$Y)
  
  names(gadm1)[names(gadm1)=="ENGTYPE_1"] <- "type"
  names(gadm1)[names(gadm1)=="NAME_1"] <- "name"
  names(gadm1)[names(gadm1)=="GID_1"] <- "GID"
  names(gadm1)[names(gadm1)=="NAME_1"] <- "name"
  
  dat_out_new_province <- dat_out_new[dat_out_new$level=="province", ]
  dat_out_new_province_merge <- merge(dat_out_new_province, gadm1[, c("merge_id", "name", "type", "GID", "geometry")], by="merge_id")
  
  ###
  
  gadm0_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_", b, "_shp/gadm36_", b, "_0.shp")
  gadm0 <- st_read(gadm0_path, stringsAsFactors=F)
  
  dat_out_new_merge_countrylevel <- dat_out_new[dat_out_new$level=="country", ]
  
  dat_out_new_merge_countrylevel$geometry <- gadm0$geometry
  dat_out_new_merge_countrylevel$name <- a
  dat_out_new_merge_countrylevel$type <- "Country"
  dat_out_new_merge_countrylevel$GID <- "Country"
  
  dat_order <- names(dat_out_new_district_merge)
  
  out <- rbind(dat_out_new_district_merge, dat_out_new_province_merge[, dat_order], dat_out_new_merge_countrylevel[, dat_order])
  out$geometry[out$level=="country"] <- NULL
  out <- out[, !(names(out) %in% c("merge_id", "GEO_codedUp", "location_details", "GEO_geojsonLink", "level"))]
  out$country <- "Sri Lanka"
  names(out)[names(out)=="type"] <- "name_type"
  
  out_file <- paste0("/Users/christianbaehr/Downloads/", a, "_2000-17_geocoded_", as.character(Sys.Date()), ".geojson")
  out_file_csv <- paste0("/Users/christianbaehr/Downloads/", a, "_2000-17_geocoded_", as.character(Sys.Date()), ".csv")
  
  write_sf(out, out_file, driver="GeoJSON", delete_dsn=T)
  write.csv(data.frame(out)[names(out)!="geometry"], out_file_csv, row.names = F)
  
  print(a)
  
}

