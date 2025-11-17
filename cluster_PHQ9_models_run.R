bd_PHQ9 = readRDS("./synthetic_cidi.rds")

n.obs <- dim(bd_PHQ9)[1]

G_max = 10
G_seq <- seq(G_max)

library(fpc)

fr.all <- flexmixedruns(bd_PHQ9, continuous=0,discrete=9,simruns=2,n.cluster=1:G_max,allout=TRUE)

saveRDS(fr.all, file = paste("./PHQ9_fitted_models/m.fpc.all.PHQ9.rds",sep=""))

library(mclust)

all_big_file_root <- "./PHQ9_fitted_models/mclust.all.PHQ9."

for (i in seq(G_max)) {
  mclust.all <- Mclust(bd_PHQ9,G=i)
  saveRDS(mclust.all, file = paste(all_big_file_root,i,".rds",sep=""))
}

library(clustMD)

for(G_here in G_seq){
  m.clustMD.all.PHQ9.G_here <- clustMD(as.matrix(bd_PHQ9), G = G_here, CnsInd = 0, OrdIndx = 9, Nnorms = 20000, MaxIter=500, model = "VVI", startCL = "kmeans")
  saveRDS(m.clustMD.all.PHQ9.G_here, file = paste("./PHQ9_fitted_models/m.clustMD.all.PHQ9.",G_here,".rds",sep=""))
}

for(G_here in G_seq){
  m.kmeans.all.PHQ9.G_here <- kmeans(bd_PHQ9, G_here)
  saveRDS(m.kmeans.all.PHQ9.G_here, file = paste("./PHQ9_fitted_models/m.kmeans.all.PHQ9.",G_here,".rds",sep=""))
}

