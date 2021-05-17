
my_c <- c("Bangladesh", "Pakistan", "Uzbekistan", "Afghanistan", "India", "Kazakhstan", "Kyrgyzstan",
          "Tajikistan", "Sri Lanka")
my_a <- c("BGD", "PAK", "UZB", "AFG", "IND", "KAZ", "KGZ", "TJK", "LKA")

my_c <- c("Nepal")
my_a <- c("NPL")


library(rgeos); library(sf)

for (l in 1:length(my_c)) {
  
  rm(list = setdiff(ls(), c("my_c", "my_a", "l")))
  
  country <- my_c[l]
  abb <- my_a[l]
  
  ##########
  
  dat_path <- paste0("/Users/christianbaehr/GitHub/SCA Geocoding/", country, "/", country, "_master.csv")
  dat <- read.csv(dat_path, stringsAsFactors = F)
  dat <- dat[!is.na(dat$project_id), ]
  dat_new <- dat[1,]
  dat_new$geometry <- NA
  dat_new <- dat_new[0,]
  
  ###
  
  for(i in c(1:nrow(dat))) {
    
    
    if(dat$christian_geojson[i] %in% c("", "good code up", "drop")) {
      new_dat <- dat[i, (names(dat)!="christian_geojson")]
      new_dat[, c("X", "Y")] <- NA
    } else {
      geo <- st_read(dat$christian_geojson[i])
      #if(dat$level[i]=="province" & nrow(geo)>1) {stop("more than one province in row")}
      
      if(dat$level[i]=="province" & nrow(geo)>1) {
        a <- as(geo$geometry, "Spatial")
        b <- gUnaryUnion(a)
        c <- st_as_sf(b)
        geo <- geo[1, ]
        geo$geometry[1] <- c
        geo$ENGTYPE_1 <- "Province"
      }
      
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
  
  ###
  
  gadm2_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_", abb, "_shp/gadm36_", abb, "_2.shp")
  
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
  
  gadm1_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_", abb, "_shp/gadm36_", abb, "_1.shp")
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
  
  
  gadm0_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_", abb, "_shp/gadm36_", abb, "_0.shp")
  gadm0 <- st_read(gadm0_path, stringsAsFactors=F)
  
  dat_out_new_merge_countrylevel <- dat_out_new[dat_out_new$level=="country", ]
  
  dat_out_new_merge_countrylevel$geometry <- gadm0$geometry
  dat_out_new_merge_countrylevel$name <- country
  dat_out_new_merge_countrylevel$type <- "Country"
  dat_out_new_merge_countrylevel$GID <- "Country"
  
  dat_order <- names(dat_out_new_district_merge)
  
  out <- rbind(dat_out_new_district_merge, dat_out_new_province_merge[, dat_order], dat_out_new_merge_countrylevel[, dat_order])
  out$geometry[out$level=="country"] <- NULL
  out <- out[, !(names(out) %in% c("merge_id", "GEO_codedUp", "location_details", "GEO_geojsonLink"))]
  out$country <- country
  names(out)[names(out)=="type"] <- "name_type"
  
  out_file <- paste0("/Users/christianbaehr/Downloads/", country, "_2000-17_geocoded_", as.character(Sys.Date()), ".geojson")
  out_file_csv <- paste0("/Users/christianbaehr/Downloads/", country, "_2000-17_geocoded_", as.character(Sys.Date()), ".csv")
  
  write_sf(out, out_file, driver="GeoJSON", delete_dsn=T)
  write.csv(data.frame(out)[names(out)!="geometry"], out_file_csv, row.names = F)
  
  
  
}

######################

tkm <- read.csv("/Users/christianbaehr/Github/SCA Geocoding/turkmenistan/turkmenistan_master.csv", 
                stringsAsFactors=F)

gadm1 <- st_read("/Users/christianbaehr/Desktop/shapefiles/gadm36_TKM_shp/gadm36_TKM_2.shp", stringsAsFactors=F)
gadm1$centroids <- st_centroid(gadm1$geometry)
gadm1[, c("X", "Y")] <- st_coordinates(gadm1$centroids)
gadm1[, c("X", "Y")] <- round(data.frame(gadm1)[, c("X", "Y")], digits=2)
gadm1$merge_id <- paste0(gadm1$X, ", ", gadm1$Y)

names(gadm1)[names(gadm1)=="ENGTYPE_1"] <- "type"
names(gadm1)[names(gadm1)=="NAME_1"] <- "name"
names(gadm1)[names(gadm1)=="GID_1"] <- "GID"
names(gadm1)[names(gadm1)=="NAME_1"] <- "name"

dat_out_new_province <- dat_out_new[dat_out_new$level=="province", ]

#tkm[, c("X", "Y", "name", "name_type", "GID")] <- NA
tkm[, c("X", "Y")] <- NA

for (i in 1:nrow(tkm)) {
  if(tkm$level[i]=="country") {
    
  } else {
    geo <- st_read(tkm$christian_geojson[i])
    cent <- st_coordinates(st_centroid(geo$geometry))
    tkm$X[i] <- cent[1]
    tkm$Y[i] <- cent[2]
  }
}


dat_out_new <- tkm

dat_out_new[, c("X", "Y")] <- round(data.frame(dat_out_new)[, c("X", "Y")], digits=2)
dat_out_new$merge_id <- paste0(dat_out_new$X, ", ", dat_out_new$Y)

dat_out_new_province_merge <- merge(dat_out_new, gadm1[, c("merge_id", "name", "type", "GID", "geometry")], by="merge_id")

tkm_country <- tkm[tkm$level=="country",]
tkm_country$name <- "Turkmenistan"
tkm_country$type <- "Country"
tkm_country$GID <- "Country"
tkm_country$merge_id <- NA

gadm0_path <- paste0("/Users/christianbaehr/Desktop/shapefiles/gadm36_TKM_shp/gadm36_TKM_0.shp")
gadm0 <- st_read(gadm0_path, stringsAsFactors=F)

tkm_country$geometry <- gadm0$geometry

dat_order <- names(dat_out_new_province_merge)


tkm_out <- rbind(dat_out_new_province_merge, tkm_country[, dat_order])

###

out=tkm_out

out$geometry[out$level=="country"] <- NULL
out <- out[, !(names(out) %in% c("merge_id", "GEO_codedUp", "location_details", "GEO_geojsonLink"))]
out$country <- "Turkmenistan"
names(out)[names(out)=="type"] <- "name_type"

out_file <- paste0("/Users/christianbaehr/Downloads/Turkmenistan_2000-17_geocoded_", as.character(Sys.Date()), ".geojson")
out_file_csv <- paste0("/Users/christianbaehr/Downloads/Turkmenistan_2000-17_geocoded_", as.character(Sys.Date()), ".csv")

write_sf(out, out_file, driver="GeoJSON", delete_dsn=T)
write.csv(data.frame(out)[names(out)!="geometry"], out_file_csv, row.names = F)
















