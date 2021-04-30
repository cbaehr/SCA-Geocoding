
library(sf)

countries <- c("AFG", "IND", "KAZ", "KGZ", "LKA", "NPL", "TJK", "UZB", "PAK", "MDV", "BGD", "TKM")

for(i in countries) {
  file <- paste0("/Users/christianbaehr/Github/SCA Geocoding/shapefiles/gadm36_", i, "_shp/gadm36_", i, "_2.shp")
  dat <- st_read(file, stringsAsFactors=F)
  if(i=="AFG") {out <- dat}
  else if (i=="MDV") {dat[, names(out)[!(names(out) %in% names(dat))]] <- NA}
  else if (i=="TKM") {dat[, names(out)[!(names(out) %in% names(dat))]] <- NA}
  dat <- dat[, (names(dat) %in% names(out))]
  out <- rbind(out, dat)
}

write_sf(out, "/Users/christianbaehr/Github/SCA Geocoding/shapefiles/all.geojson", driver="GeoJSON", delete_dsn=T)



