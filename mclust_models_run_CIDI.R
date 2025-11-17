library(haven)
library(dplyr)
library(mclust)

ud_all = readRDS("./synthetic_cidi.rds")

Gmax = 10

men_big_file_root <- "./2022_fitted_models_v3/mclust.men.2022."
women_big_file_root <- "./2022_fitted_models_v3/mclust.women.2022."
all_big_file_root <- "./2022_fitted_models_v3/mclust.all.2022."

for (i in seq(Gmax)) {
  mclust.men <- Mclust(ud_men,G=i)
  mclust.women <- Mclust(ud_women,G=i)
  mclust.all <- Mclust(ud_all,G=i)
  
  saveRDS(mclust.men, file = paste(men_big_file_root,i,".rds",sep=""))
  saveRDS(mclust.women, file = paste(women_big_file_root,i,".rds",sep=""))
  saveRDS(mclust.all, file = paste(all_big_file_root,i,".rds",sep=""))
  
}



