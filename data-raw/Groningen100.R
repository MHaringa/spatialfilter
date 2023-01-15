## code to prepare `Groningen100` dataset goes here

library(spatialrisk)
create_data <- function(n = 100){
  data.frame(lon = sample(spatialrisk::Groningen$lon, n),
             lat = sample(spatialrisk::Groningen$lat, n),
             som = sample(spatialrisk::Groningen$amount, n),
             naam = sample(LETTERS[1:4], n, replace = TRUE),
             categorie = sample(LETTERS[1:3], n, replace = TRUE))
}

Groningen100 <- create_data()

usethis::use_data(Groningen100, overwrite = TRUE)
