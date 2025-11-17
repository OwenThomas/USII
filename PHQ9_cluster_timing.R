setwd("~/R_scripts")

bd_PHQ9 = readRDS("./synthetic_cidi.rds")

n.obs <- dim(bd_PHQ9_normed)[1]

G_max = 4

library(mclust)

mclust_times <- vector("list", G_max + 1)
mclust_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  mclust.all <- Mclust(bd_PHQ9,G=i)
  mclust_times[[i + 1]] <- Sys.time()
}
rm(mclust.all)

library(fpc)

fpc_times <- vector("list", G_max + 1)
fpc_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  fr.all <- flexmixedruns(bd_PHQ9, continuous=0,discrete=9,simruns=2,n.cluster=i,allout=TRUE)
  fpc_times[[i + 1]] <- Sys.time()
}
rm(fr.all)
gc()

library(clustMD)

clustmd_times <- vector("list", G_max + 1)
clustmd_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  m.clustMD.all.PHQ9.G_here <- clustMD(as.matrix(bd_PHQ9), G = i, CnsInd = 0, OrdIndx = 9, Nnorms = 20000, MaxIter=500, model = "VVI", startCL = "kmeans")
  clustmd_times[[i + 1]] <- Sys.time()
  rm(m.clustMD.all.PHQ9.G_here)
  gc()
}

saveRDS(mclust_times, "PHQ9_mclust_times")
saveRDS(fpc_times, "PHQ9_fpc_times")
saveRDS(clustmd_times, "PHQ9_clustmd_times")
