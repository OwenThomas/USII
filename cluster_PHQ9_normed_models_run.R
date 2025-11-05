setwd("~/R_scripts")

bd_raw <- read.table("/ess/p2756/data/durable/original-files/data-download/ukb675054.tab", header=TRUE, sep="\t")

PHQ9.indices <- c("f.20514.0.0","f.20510.0.0","f.20517.0.0","f.20519.0.0","f.20511.0.0","f.20507.0.0", "f.20508.0.0","f.20518.0.0", "f.20513.0.0")

bd_PHQ9 <- bd_raw[,PHQ9.indices]

for (i in seq(1,9)) {
  bd_PHQ9[which(bd_PHQ9[,i]<0),i] <- NA
  print(table(bd_PHQ9[,i]))
}

bd_PHQ9 <- bd_PHQ9[complete.cases(bd_PHQ9),]

bd_PHQ9_normed <- bd_PHQ9/rowSums(bd_PHQ9)

bd_normed_unique <- unique(as.vector(as.matrix(bd_PHQ9_normed)))
bd_normed_ranks <- rank(bd_normed_unique)

bd_PHQ9_normed_ranked <- bd_PHQ9_normed

for(i in seq(length(bd_normed_unique))){
  bd_PHQ9_normed_ranked[bd_PHQ9_normed==bd_normed_unique[i]] <- bd_normed_ranks[i]
}

for (i in seq(1,9)) {
  print(table(bd_PHQ9_normed_ranked[,i]))
}


n.obs <- dim(bd_PHQ9_normed)[1]

G_max = 10
G_seq <- seq(G_max)

for(G_here in G_seq){
  m.kmeans.all.PHQ9.G_here <- kmeans(bd_PHQ9_normed_ranked, G_here)
  saveRDS(m.kmeans.all.PHQ9.G_here, file = paste("./PHQ9_normed_fitted_models/m.kmeans.all.PHQ9.",G_here,".rds",sep=""))
}

library(fpc)

fr.all <- flexmixedruns(bd_PHQ9_normed_ranked, continuous=0,discrete=9,simruns=2,n.cluster=1:G_max,allout=TRUE)

saveRDS(fr.all, file = paste("./PHQ9_normed_fitted_models/m.fpc.all.PHQ9.rds",sep=""))

library(mclust)

all_big_file_root <- "./PHQ9_normed_fitted_models/mclust.all.PHQ9."

for (i in seq(G_max)) {
  mclust.all <- Mclust(bd_PHQ9_normed_ranked,G=i)
  saveRDS(mclust.all, file = paste(all_big_file_root,i,".rds",sep=""))
}

library(clustMD)

for(G_here in G_seq){
  m.clustMD.all.PHQ9.G_here <- clustMD(as.matrix(bd_PHQ9_normed_ranked), G = G_here, CnsInd = 0, OrdIndx = 9, Nnorms = 20000, MaxIter=500, model = "VVI", startCL = "kmeans")
  saveRDS(m.clustMD.all.PHQ9.G_here, file = paste("./PHQ9_normed_fitted_models/m.clustMD.all.PHQ9.",G_here,".rds",sep=""))
  rm(m.clustMD.all.PHQ9.G_here)
  gc()
}


