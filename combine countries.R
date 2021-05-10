
library(sf)

###



###

files <- paste0("/Users/christianbaehr/Downloads/country_files/",
                grep(".csv", dir("/Users/christianbaehr/Downloads/country_files"), value = T))

temp <- names(read.csv(grep("Pakistan", files, value=T)))

for (i in 1:length(files)) {
  
  if(i==1) {
    dat <- read.csv(files[i], stringsAsFactors = F)[, temp]
  } else {
    dat <- rbind(dat, read.csv(files[i], stringsAsFactors = F)[, temp])
  }
  
}

### MALDIVES

mal <- read.csv("/Users/christianbaehr/Github/SCA Geocoding/Maldives/maldives_master.csv",
                stringsAsFactors = F)

mal[, c("level")] <- NA

mal <- mal[, names(dat)]

dat <- rbind(dat, mal)


###

table(dat$level)
dat$level[(dat$level=="country" | dat$name_type=="Country")] <- "ADM0"
dat$level[dat$level=="province"] <- "ADM1"
dat$level[dat$level=="district"] <- "ADM2"

dat$name_type[dat$GID=="Country"] <- "Country"
dat$name_type[(is.na(dat$name_type) & dat$level=="ADM2")] <- "District"

dat$name_type[dat$name_type=="Distict"] <- "District"
dat$name_type[dat$name_type=="Autononous Region"] <- "Autonomous Region"



table(dat$country)

table(dat$name_type)
table(dat$level)


View(dat)

dat <- dat[, !(names(dat) %in% c("sources", "GEO_notes", "Christian.Notes"))]

dat$GID[(dat$level=="ADM0" & dat$country=="Afghanistan")] <- "AFG"
dat$GID[(dat$level=="ADM0" & dat$country=="Bangladesh")] <- "BGD"
dat$GID[(dat$level=="ADM0" & dat$country=="India")] <- "IND"
dat$GID[(dat$level=="ADM0" & dat$country=="Kazakhstan")] <- "KAZ"
dat$GID[(dat$level=="ADM0" & dat$country=="Kyrgyzstan")] <- "KGZ"
dat$GID[(dat$level=="ADM0" & dat$country=="Pakistan")] <- "PAK"
dat$GID[(dat$level=="ADM0" & dat$country=="Sri Lanka")] <- "LKA"
dat$GID[(dat$level=="ADM0" & dat$country=="Tajikistan")] <- "TJK"
dat$GID[(dat$level=="ADM0" & dat$country=="Turkmenistan")] <- "TKM"
dat$GID[(dat$level=="ADM0" & dat$country=="Uzbekistan")] <- "UZB"
dat$GID[(dat$level=="ADM0" & dat$country=="Maldives")] <- "MDV"

dat <- dat[order(dat$GID), ]

###


###


write.csv(dat, paste0("/Users/christianbaehr/Downloads/china-sca_2000-17_geocoded_", as.character(Sys.Date()), ".csv"))

