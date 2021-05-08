
library(sf)

dat <- read.csv("/Users/christianbaehr/Desktop/SCA Geocoding/Sri Lanka/srilanka_master.csv",
                stringsAsFactors = F)

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
  
}

##########

dat_out_new <- dat_out

dat_out_new[, c("X", "Y")] <- round(data.frame(dat_out_new)[, c("X", "Y")], digits=2)
dat_out_new$merge_id <- paste0(dat_out_new$X, ", ", dat_out_new$Y)

gadm2 <- st_read("/Users/christianbaehr/Desktop/SCA Geocoding/Sri Lanka/gadm36_LKA_2.geojson", stringsAsFactors=F)
gadm2$centroids <- st_centroid(gadm2$geometry)
gadm2[, c("X", "Y")] <- st_coordinates(gadm2$centroids)
gadm2[, c("X", "Y")] <- round(data.frame(gadm2)[, c("X", "Y")], digits=2)
gadm2$merge_id <- paste0(gadm2$X, ", ", gadm2$Y)

names(gadm2)[names(gadm2)=="ENGTYPE_2"] <- "type"
names(gadm2)[names(gadm2)=="NAME_2"] <- "name"
names(gadm2)[names(gadm2)=="GID_2"] <- "GID"


sum(dat_out_new$merge_id %in% gadm2$merge_id)
sum(dat_out_new$merge_id=="NA, NA")

mask <- !((dat_out_new$merge_id %in% gadm2$merge_id) | (dat_out_new$merge_id=="NA, NA"))

unique(dat_out_new$level)
dat_out_new$level[dat_out_new$level=="actual province"] <- "province"
dat_out_new$level[dat_out_new$level==""] <- "code up"



#View(dat_out_new[mask, ])

###

dat_out_new_district <- dat_out_new[dat_out_new$level=="district", ]

dat_out_new_district_merge <- merge(dat_out_new_district, gadm2[, c("merge_id", "name", "type", "GID", "geometry")], by="merge_id")

sum(duplicated(gadm2$merge_id))

###


gadm1 <- st_read("/Users/christianbaehr/Desktop/SCA Geocoding/Sri Lanka/gadm36_LKA_1.geojson", stringsAsFactors=F)
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

gadm0 <- st_read("/Users/christianbaehr/Desktop/SCA Geocoding/Sri Lanka/gadm36_LKA_0.geojson", stringsAsFactors=F)

dat_out_new_merge_countrylevel <- dat_out_new[dat_out_new$level=="code up", ]

dat_out_new_merge_countrylevel$geometry <- gadm0$geometry
dat_out_new_merge_countrylevel$name <- "Sri Lanka"
dat_out_new_merge_countrylevel$type <- "Country"
dat_out_new_merge_countrylevel$GID <- "Country"


###

dat_order <- names(dat_out_new_district_merge)

out <- rbind(dat_out_new_district_merge, dat_out_new_province_merge[, dat_order], dat_out_new_merge_countrylevel[, dat_order])

out$geometry[out$level=="code up"] <- NULL

out <- out[, !(names(out) %in% c("merge_id", "GEO_codedUp", "location_details", "GEO_geojsonLink", "level"))]
out$country <- "Sri Lanka"
names(out)[names(out)=="type"] <- "name_type"

out_file <- paste0("/Users/christianbaehr/Downloads/SriLanka_2000-17_geocoded_", as.character(Sys.Date()), ".geojson")
out_file_csv <- paste0("/Users/christianbaehr/Downloads/SriLanka_2000-17_geocoded_", as.character(Sys.Date()), ".csv")

write_sf(out, out_file, driver="GeoJSON", delete_dsn=T)

write.csv(data.frame(out)[names(out)!="geometry"], out_file_csv, row.names = F)










