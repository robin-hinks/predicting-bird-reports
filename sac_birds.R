#initialize packages
library(aod)
library(ggplot2)

#import CSV data for birds and all census tracts

sac_dem_data <- read.csv('SAC_ACS_DATA.csv')
sac_bird_data <- read.csv('sac_bird_data.csv')
sac_mos_tracts <- read.csv('sac_mos_tracts.csv')

yolo_dem_data <- read.csv('yolo_acs_data.csv')
yolo_bird_data <- read.csv('yolo_bird_data.csv')
yolo_mos_tracts <- read.csv('yolo_mos_tracts.csv')

la_dem_data <- read.csv('la_acs_data.csv')
la_bird_data <- read.csv('la_bird_data.csv')
la_mos_tracts <- read.csv('la_mos_tracts.csv')

setClass("county_data", slots=list(dems="list", birds = "list", mos = "list", regression = "list"))

sac_data <- new("county_data", dems = sac_dem_data, birds = sac_bird_data, mos = sac_mos_tracts, regression = list(NULL))
yolo_data <- new("county_data", dems = yolo_dem_data, birds = yolo_bird_data, mos = yolo_mos_tracts, regression = list(NULL))
la_data <- new("county_data", dems = la_dem_data, birds = la_bird_data, mos = la_mos_tracts, regression = list(NULL))

append_bird_data <- function(county) {
  dem_data <- county@dems
  bird_data <- county@birds
  
  #find out which census tracts have birds and which don't, and put them in a vector
  all_tracts <- dem_data[1]
  tracts_with_birds <- bird_data[4]
  
  birds_present <- c(integer(length(all_tracts[[1]])))
  
  for (i in 1:length(all_tracts[[1]])) {
    for (bird_tract in tracts_with_birds[[1]]) {
      if (all_tracts[[1]][i] == (bird_tract)) {
        birds_present[i] <- 1 
      }
    }
  }
  
  return(birds_present)
}

append_mosquito_data <- function(county) {
  #adds a column for whether there have been positive mosquitoes
  dem_data <- county@dems
  mos_tracts <- county@mos
  
  all_tracts <- dem_data[1]
  tracts_with_mosquitoes <- mos_tracts[1]
  
  mos_present <- c(integer(length(all_tracts[[1]])))
  
  for (i in 1:length(all_tracts[[1]])) {
    for (mos_tract in tracts_with_mosquitoes[[1]]) {
      if (all_tracts[[1]][i] == (mos_tract)) {
        mos_present[i] <- 1 
      }
    }
  }
  
  return(mos_present)
}

#add columns to sac data
bird_col <- append_bird_data(sac_data)
mos_col <- append_mosquito_data(sac_data)
sac_dem_data <- cbind(sac_dem_data, bird_col, mos_col)

#run sac regression
sac_logit_regression <- glm(bird_col ~ total_population + over_65 + median_income + housing_density, data = sac_dem_data, family = "binomial")
summary(sac_logit_regression)

ggplot(sac_dem_data, aes(x=median_income, y=bird_col)) + geom_point() +
  stat_smooth(method="glm", color="turquoise", se=FALSE,
              method.args = list(family=binomial) + geom_point(aes(x = total_population, y = bird_col)))


#add columns to yolo data
bird_col <- append_bird_data(yolo_data)
mos_col <- append_mosquito_data(yolo_data)
yolo_dem_data <- cbind(yolo_dem_data, bird_col, mos_col)
  
#run yolo regression
yolo_logit_regression <- glm(bird_col ~ total_population + over_65 + median_income + housing_density + percent_vacant_units + percent_white + percent_asian + percent_hispanic, data = yolo_dem_data, family = "binomial")
summary(yolo_logit_regression)

#add columns to la data
bird_col <- append_bird_data(la_data)
mos_col <- append_mosquito_data(la_data)
la_dem_data <- cbind(la_dem_data, bird_col, mos_col)

#run la regression
la_logit_regression <- glm(bird_col ~ total_population + over_65 + median_income + housing_density + percent_vacant_units + percent_white + percent_hispanic + other_language_spoken_at_home, data = la_dem_data, family = "binomial")
summary(la_logit_regression)

#confint(logit_regression)
#exp(coef(logit_regression))
