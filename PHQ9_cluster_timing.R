setwd("~/R_scripts")

bd_raw <- read.table("/ess/p2756/data/durable/original-files/data-download/ukb675054.tab", header=TRUE, sep="\t")

PHQ9.indices <- c("f.20514.0.0","f.20510.0.0","f.20517.0.0","f.20519.0.0","f.20511.0.0","f.20507.0.0", "f.20508.0.0","f.20518.0.0", "f.20513.0.0")

bd_PHQ9 <- bd_raw[,PHQ9.indices]

for (i in seq(1,9)) {
  bd_PHQ9[which(bd_PHQ9[,i]<0),i] <- NA
}

bd_PHQ9 <- bd_PHQ9[complete.cases(bd_PHQ9),]

bd_PHQ9_normed <- bd_PHQ9/rowSums(bd_PHQ9)

bd_normed_unique <- unique(as.vector(as.matrix(bd_PHQ9_normed)))
bd_normed_ranks <- rank(bd_normed_unique)

bd_PHQ9_normed_ranked <- bd_PHQ9_normed

for(i in seq(length(bd_normed_unique))){
  bd_PHQ9_normed_ranked[bd_PHQ9_normed==bd_normed_unique[i]] <- bd_normed_ranks[i]
}

n.obs <- dim(bd_PHQ9_normed)[1]

G_max = 4

library(mclust)

mclust_times <- vector("list", G_max + 1)
mclust_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  mclust.all <- Mclust(bd_PHQ9_normed_ranked,G=i)
  mclust_times[[i + 1]] <- Sys.time()
}
rm(mclust.all)

library(fpc)

fpc_times <- vector("list", G_max + 1)
fpc_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  fr.all <- flexmixedruns(bd_PHQ9_normed_ranked, continuous=0,discrete=9,simruns=2,n.cluster=i,allout=TRUE)
  fpc_times[[i + 1]] <- Sys.time()
}
rm(fr.all)
gc()

library(clustMD)

clustmd_times <- vector("list", G_max + 1)
clustmd_times[[1]] <- Sys.time()

for (i in seq(G_max)) {
  m.clustMD.all.PHQ9.G_here <- clustMD(as.matrix(bd_PHQ9_normed_ranked), G = i, CnsInd = 0, OrdIndx = 9, Nnorms = 20000, MaxIter=500, model = "VVI", startCL = "kmeans")
  clustmd_times[[i + 1]] <- Sys.time()
  rm(m.clustMD.all.PHQ9.G_here)
  gc()
}

saveRDS(mclust_times, "PHQ9_mclust_times")
saveRDS(fpc_times, "PHQ9_fpc_times")
saveRDS(clustmd_times, "PHQ9_clustmd_times")

CIDI_mclust_times = readRDS("~/CIDI_mclust_times")
CIDI_fpc_times = readRDS("~/CIDI_fpc_times")
CIDI_clustmd_times = readRDS("~/CIDI_clustmd_times")
