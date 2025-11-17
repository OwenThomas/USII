library(haven)
library(dplyr)

ud_all = readRDS("./synthetic_cidi.rds")

library(mclust)

G_max = 4

mclust_times <- vector("list", G_max + 1)
mclust_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  mclust.all <- Mclust(ud_all,G=i)
  mclust_times[[i + 1]] <- Sys.time()
}
rm(mclust.all)

library(fpc)

fpc_times <- vector("list", G_max + 1)
fpc_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  fr.all <- flexmixedruns(ud_all, continuous=2,discrete=24,simruns=2,n.cluster=i,allout=TRUE)
  fpc_times[[i + 1]] <- Sys.time()
}
rm(fr.all)
library(clustMD)

clustmd_times <- vector("list", G_max + 1)
clustmd_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  m.clustMD.all.2022.G_here <- clustMD(as.matrix(ud_all), G = i, CnsInd = 2, OrdIndx = 26, Nnorms = 20000, MaxIter=500, model = "VVI")
  clustmd_times[[i + 1]] <- Sys.time()
  rm(m.clustMD.all.2022.G_here)
  gc()
}

saveRDS(mclust_times, "CIDI_mclust_times")
saveRDS(fpc_times, "CIDI_fpc_times")
saveRDS(clustmd_times, "CIDI_clustmd_times")


