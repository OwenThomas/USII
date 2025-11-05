setwd("~/R_scripts")

bd_raw <- read.table("/ess/p2756/data/durable/original-files/data-download/ukb675054.tab", header=TRUE, sep="\t")

PHQ9.indices <- c("f.20514.0.0","f.20510.0.0","f.20517.0.0","f.20519.0.0","f.20511.0.0","f.20507.0.0", "f.20508.0.0","f.20518.0.0", "f.20513.0.0")

bd_PHQ9 <- bd_raw[,PHQ9.indices]

for (i in seq(1,9)) {
  bd_PHQ9[which(bd_PHQ9[,i]<0),i] <- NA
  print(table(bd_PHQ9[,i]))
}

bd_PHQ9 <- bd_PHQ9[complete.cases(bd_PHQ9),]

n.obs <- dim(bd_PHQ9)[1]

library(psych)
library(qgraph)

mixed.cor.all <- cor_auto(data = bd_PHQ9)
pearson.cor.all <- cor(bd_PHQ9, method="pearson")
kendall.cor.all <- cor(bd_PHQ9, method="kendall")
spearman.cor.all <- cor(bd_PHQ9, method="spearman")

saveRDS(mixed.cor.all, file = paste("./PHQ9_fitted_models/mixed.cor.all.rds",sep=""))
saveRDS(pearson.cor.all, file = paste("./PHQ9_fitted_models/pearson.cor.all.rds",sep=""))
saveRDS(kendall.cor.all, file = paste("./PHQ9_fitted_models/kendall.cor.all.rds",sep=""))
saveRDS(spearman.cor.all, file = paste("./PHQ9_fitted_models/spearman.cor.all.rds",sep=""))


mixed.cor.all <- readRDS(file = "./PHQ9_fitted_models/mixed.cor.all.rds")
pearson.cor.all <- readRDS(file = "./PHQ9_fitted_models/pearson.cor.all.rds")
kendall.cor.all <- readRDS(file = "./PHQ9_fitted_models/kendall.cor.all.rds")
spearman.cor.all <- readRDS(file = "./PHQ9_fitted_models/spearman.cor.all.rds")

library(psych)

G_max <- 10
G_seq <- seq(G_max)

rotation = "none"

all.mixed.list <- list()
all.pearson.list <- list()
all.kendall.list <- list()
all.spearman.list <- list()

for(G_here in G_seq){
  m.fa.all.PHQ9.mixed.G_here <- fa(mixed.cor.all, nfactor=G_here, rotate = rotation, n.obs = n.obs)
  saveRDS(m.fa.all.PHQ9.mixed.G_here, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.mixed.",G_here,".rds",sep=""))
  
  m.fa.all.PHQ9.pearson.G_here <- fa(pearson.cor.all, nfactor=G_here, rotate = rotation, n.obs = n.obs)
  saveRDS(m.fa.all.PHQ9.pearson.G_here, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.pearson.",G_here,".rds",sep=""))
  
  m.fa.all.PHQ9.kendall.G_here <- fa(kendall.cor.all, nfactor=G_here, rotate = rotation, n.obs = n.obs)
  saveRDS(m.fa.all.PHQ9.kendall.G_here, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.kendall.",G_here,".rds",sep=""))
  
  m.fa.all.PHQ9.spearman.G_here <- fa(spearman.cor.all, nfactor=G_here, rotate = rotation, n.obs = n.obs)
  saveRDS(m.fa.all.PHQ9.spearman.G_here, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.spearman.",G_here,".rds",sep=""))
  
  all.mixed.list[[G_here]] <- m.fa.all.PHQ9.mixed.G_here
  all.pearson.list[[G_here]] <- m.fa.all.PHQ9.pearson.G_here
  all.kendall.list[[G_here]] <- m.fa.all.PHQ9.kendall.G_here
  all.spearman.list[[G_here]] <- m.fa.all.PHQ9.spearman.G_here
  
  print(G_here)
}

saveRDS(all.mixed.list, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.mixed.list.rds",sep=""))
saveRDS(all.pearson.list, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.pearson.list.rds",sep=""))
saveRDS(all.kendall.list, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.kendall.list.rds",sep=""))
saveRDS(all.spearman.list, file = paste("./PHQ9_fitted_models/m.fa.all.PHQ9.spearman.list.rds",sep=""))

