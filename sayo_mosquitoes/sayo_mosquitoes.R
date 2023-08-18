#import csv data
all_sayo_tracts_csv <- read.csv('SAYO_POS_MOSQUITOES_GEOID.csv')
pos_sites_csv <- read.csv('POS_MOSQUITOES_CSV.csv')

pos_sites <- pos_sites_csv[1:57, 2] #not sure why but when i include all rows it gives a bunch of NA's at the end
all_sayo_tracts <- all_sayo_tracts_csv[1:1687, ]

#initialize empty array that will be filled in for loop
has_mosquitoes <- integer(length(all_sayo_tracts[, c(1)]))

#loop through all positive site codes and find matches in all sites sheet
site_counter <- 1
for (site in all_sayo_tracts[, c(1)]) {
  for (s in pos_sites) {
    if (!is.na(site) && site == s) {
      has_mosquitoes[site_counter] <- 1
    }
  }
  site_counter <- site_counter + 1
}

sayo_tracts_with_mos <- cbind(all_sayo_tracts, has_mosquitoes)

write.csv(sayo_tracts_with_mos, file='sayo_mos_tracts.csv')
