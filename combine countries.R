
files <- ls()

for (i in 1:length(files)) {
  if(i==1) {
    dat <- read.csv(files[i], stringsAsFactors = F)
  } else {
    dat <- rbind(dat, read.csv(files[i], stringsAsFactors = F))
  }
  
}

write.csv(dat, paste0("/Users/christianbaehr/Downloads/china-sca_2000-17_geocoded_", as.character(Sys.Date()), ".csv"))

