
###### DESIGNATE A COUNTRY ######

country <- "Maldives"

###### DESIGNATE A COUNTRY ######

###

set.seed(123)

library(readxl)

dat <- read_excel("/Users/christianbaehr/Downloads/SCA2000-2017_Mar82021_fixed.xlsx")
#dat <- read_excel("/Users/christianbaehr/Downloads/SCA2000-2017_Feb92021_fixed.xlsx")

#unique(dat$recommended_for_aggregates)
dat2 <- dat[dat$recommended_for_aggregates==1, ]

table(dat2$all_recipients)

dat3 <- dat2[dat2$all_recipients==country, ]
dat3$sample <- runif(nrow(dat3))

dat4 <- dat3[order(dat3$sample), ]

chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))

n <- c(1:nrow(dat4))
mysplit <- chunk(n, 4)

juliana <- dat4[mysplit[1][[1]], ]
abby <- dat4[mysplit[2][[1]], ]
camila <- dat4[mysplit[3][[1]], ]
sarah <- dat4[mysplit[4][[1]], ]

juliana <- juliana[, c("project_id", "title", "description", "crs_sector_name", "sources", "factiva_sources", "location_details")]
juliana[, c("GEO_codedUp", "GEO_geojsonLink", "GEO_sources", "GEO_notes")] <- ""

abby <- abby[, c("project_id", "title", "description", "crs_sector_name", "sources", "factiva_sources", "location_details")]
abby[, c("GEO_codedUp", "GEO_geojsonLink", "GEO_sources", "GEO_notes")] <- ""

camila <- camila[, c("project_id", "title", "description", "crs_sector_name", "sources", "factiva_sources", "location_details")]
camila[, c("GEO_codedUp", "GEO_geojsonLink", "GEO_sources", "GEO_notes")] <- ""

sarah <- sarah[, c("project_id", "title", "description", "crs_sector_name", "sources", "factiva_sources", "location_details")]
sarah[, c("GEO_codedUp", "GEO_geojsonLink", "GEO_sources", "GEO_notes")] <- ""

write.csv(juliana, paste0("/Users/christianbaehr/Desktop/juliana_", country, ".csv"), row.names = F)
write.csv(abby, paste0("/Users/christianbaehr/Desktop/abby_", country, ".csv"), row.names = F)
write.csv(camila, paste0("/Users/christianbaehr/Desktop/camila_", country, ".csv"), row.names = F)
write.csv(sarah, paste0("/Users/christianbaehr/Desktop/sarah_", country, ".csv"), row.names = F)


