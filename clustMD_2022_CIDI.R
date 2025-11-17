library(haven)
library(dplyr)
library(clustMD)

ud_all = readRDS("./synthetic_cidi.rds")

G_max <- 10
G_seq <- seq(G_max)

for(G_here in G_seq){
  print(G_here)
  m.clustMD.all.2022.G_here <- clustMD(as.matrix(ud_all), G = G_here, CnsInd = 2, OrdIndx = 26, Nnorms = 20000, MaxIter=500, model = "VVI")
  saveRDS(m.clustMD.all.2022.G_here, file = paste("./2022_fitted_models_v3/m.clustMD.all.2022.",G_here,".rds",sep=""))
  rm(m.clustMD.all.2022.G_here)
  gc()
}




