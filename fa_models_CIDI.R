library(haven)
library(dplyr)
library(qgraph)
library(psych)

ud_all = readRDS("./synthetic_cidi.rds")

G_max <- 10
G_seq <- seq(G_max)

c_indices <- c()
p_indices <- c()
d_indices <- c()

for(i in seq(1:26)){
  print(i)
  print(names(ud_all)[i])
  print(table(ud_all[,i]))
  n_unique <- length(unique(ud_all[,i]))
  if(n_unique == 2){
    d_indices <- c(d_indices, i)
  }else if(n_unique > 10){
    c_indices <-c(c_indices, i)
  }else{
    p_indices <- c(p_indices,i)
  }
}


mixed.cor.all <- cor_auto(data = ud_all )
pearson.cor.all <- cor(ud_all, method="pearson")
kendall.cor.all <- cor(ud_all, method="kendall")
spearman.cor.all <- cor(ud_all, method="spearman")

saveRDS(mixed.cor.all, file = paste("./2022_fitted_models_v3/mixed.cor.all.rds",sep=""))
saveRDS(pearson.cor.all, file = paste("./2022_fitted_models_v3/pearson.cor.all.rds",sep=""))
saveRDS(kendall.cor.all, file = paste("./2022_fitted_models_v3/kendall.cor.all.rds",sep=""))
saveRDS(spearman.cor.all, file = paste("./2022_fitted_models_v3/spearman.cor.all.rds",sep=""))

rotation = "none"

all.mixed.list <- list()
all.pearson.list <- list()
all.kendall.list <- list()
all.spearman.list <- list()


for(G_here in G_seq){
  m.fa.all.2022.mixed.G_here <- fa(mixed.cor.all, nfactor=G_here, rotate = rotation, n.obs = dim(ud_all)[1])
  saveRDS(m.fa.all.2022.mixed.G_here, file = paste("./2022_fitted_models_v3/m.fa.all.2022.mixed.",G_here,".rds",sep=""))
  
  m.fa.all.2022.pearson.G_here <- fa(pearson.cor.all, nfactor=G_here, rotate = rotation, n.obs = dim(ud_all)[1])
  saveRDS(m.fa.all.2022.pearson.G_here, file = paste("./2022_fitted_models_v3/m.fa.all.2022.pearson.",G_here,".rds",sep=""))
  
  m.fa.all.2022.kendall.G_here <- fa(kendall.cor.all, nfactor=G_here, rotate = rotation, n.obs = dim(ud_all)[1])
  saveRDS(m.fa.all.2022.kendall.G_here, file = paste("./2022_fitted_models_v3/m.fa.all.2022.kendall.",G_here,".rds",sep=""))
  
  m.fa.all.2022.spearman.G_here <- fa(spearman.cor.all, nfactor=G_here, rotate = rotation, n.obs = dim(ud_all)[1])
  saveRDS(m.fa.all.2022.spearman.G_here, file = paste("./2022_fitted_models_v3/m.fa.all.2022.spearman.",G_here,".rds",sep=""))
  
  all.mixed.list[[G_here]] <- m.fa.all.2022.mixed.G_here
  all.pearson.list[[G_here]] <- m.fa.all.2022.pearson.G_here
  all.kendall.list[[G_here]] <- m.fa.all.2022.kendall.G_here
  all.spearman.list[[G_here]] <- m.fa.all.2022.spearman.G_here
  
  print(G_here)
}

saveRDS(all.mixed.list, file = paste("./2022_fitted_models_v3/m.fa.all.2022.mixed.list.rds",sep=""))
saveRDS(all.pearson.list, file = paste("./2022_fitted_models_v3/m.fa.all.2022.pearson.list.rds",sep=""))
saveRDS(all.kendall.list, file = paste("./2022_fitted_models_v3/m.fa.all.2022.kendall.list.rds",sep=""))
saveRDS(all.spearman.list, file = paste("./2022_fitted_models_v3/m.fa.all.2022.spearman.list.rds",sep=""))


