library(haven)
library(dplyr)
library(qgraph)

setwd("~/R_scripts")

depression_2022 <- read_dta("/ess/p2756/data/durable/original-files/Working files/Owen/depression_2022.dta")
#View(depression_2022)

include = c("n_eid","n_29011_0_0", "n_29012_0_0", "n_29013_0_0", "n_29014_0_0", "n_29015_0_0", "n_29016_0_0", "n_29017_0_0", "n_29018_0_0", "n_29019_0_0", "n_29020_0_0", "n_29021_0_0", "n_29022_0_0", "n_29023_0_0", "n_29024_0_0", "n_29025_0_0", "n_29026_0_0", "n_29027_0_0", "n_29028_0_0", "n_29029_0_0", "n_29030_0_0", "n_29031_0_0", "n_29032_0_0", "n_29033_0_0", "n_29034_0_0", "n_29036_0_0")

disinclude = c("n_29002_0_0","n_29003_0_0", "n_29004_0_0", "n_29005_0_0","n_29006_0_0", "n_29007_0_0", "n_29008_0_0", "n_29009_0_0", "n_29010_0_0", "n_29035_0_0", "n_29037_0_0","n_29038_0_0", "n_29038_0_1", "n_29038_0_2", "n_29039_0_0", "n_29039_0_1", "n_29039_0_2", "n_29039_0_3", "n_29039_0_4", "n_29039_0_5", "n_29039_0_6", "n_29040_0_0", "n_29041_0_0", "n_29042_0_0", "n_29043_0_0", "n_29044_0_0", "n_29045_0_0", "n_29046_0_0", "n_29047_0_0", "n_29047_0_1", "n_29048_0_0", "ts_29185_0_0", "ts_29198_0_0"  )

bd_raw <- read.table("/ess/p2756/data/durable/original-files/data-download/ukb675054.tab", header=TRUE, sep="\t")

bd_gender <- bd_raw[,1:2]

remove(bd_raw)

names(bd_gender)[1] <- "eid"

use_depression <- depression_2022[,include]

names(use_depression)[1] <-"eid"

use_depression <- merge(bd_gender, use_depression, by = "eid")

remove_negative_names <- c("n_29011_0_0", "n_29012_0_0", "n_29013_0_0", "n_29014_0_0", "n_29015_0_0", "n_29016_0_0", "n_29017_0_0", "n_29018_0_0", "n_29019_0_0", "n_29020_0_0", "n_29021_0_0", "n_29022_0_0", "n_29026_0_0", "n_29027_0_0", "n_29028_0_0", "n_29029_0_0", "n_29030_0_0", "n_29031_0_0", "n_29032_0_0"  )

for( vn in remove_negative_names){
  i_vn <- which(vn == names(use_depression))
  use_depression[which(use_depression[,i_vn]<0),i_vn] <- NA
}

max_n_depressed_periods <- 7

use_depression[which(use_depression$n_29033_0_0>(max_n_depressed_periods - 1)),]$n_29033_0_0 <- max_n_depressed_periods

use_depression[which(use_depression$n_29033_0_0==-4),]$n_29033_0_0 <- max_n_depressed_periods

use_depression[which(use_depression$n_29033_0_0<0),]$n_29033_0_0 <- NA

use_depression$n_29021_0_0_weightgain <- as.integer((use_depression$n_29021_0_0==0) | (use_depression$n_29021_0_0==2))
use_depression$n_29021_0_0_weightloss <- as.integer((use_depression$n_29021_0_0==1) | (use_depression$n_29021_0_0==2))
use_depression <- use_depression[,-which(names(use_depression)=="n_29021_0_0")]
#how to encode weight changes? field n_29021_0_0

use_depression$n_29020_0_0 <- case_match(use_depression$n_29020_0_0, 0 ~ 2, 1 ~ 3, 2 ~ 1)
use_depression$n_29017_0_0 <- case_match(use_depression$n_29017_0_0, 0 ~ 1, 1 ~ 3, 2 ~ 2)


#use_depression$n_29020_0_0_moreappetite <- as.integer(use_depression$n_29020_0_0==1)
#use_depression$n_29020_0_0_lessappetite <- as.integer(use_depression$n_29020_0_0==2)
#use_depression <- use_depression[,-which(names(use_depression)=="n_29020_0_0")]

use_depression[is.na(use_depression$n_29023_0_0),]$n_29023_0_0 <- 0
use_depression[is.na(use_depression$n_29024_0_0),]$n_29024_0_0 <- 0
use_depression[is.na(use_depression$n_29025_0_0),]$n_29025_0_0 <- 0

hist(colSums(!is.na(use_depression[5:28])))

ud_cc <- use_depression[complete.cases(use_depression),]
ud_cc[,apply(ud_cc, 2, min)==0] <- ud_cc[,apply(ud_cc, 2, min)==0] + 1
ud_cc <- ud_cc[,names(ud_cc)[c(1,2,25,26,seq(3,24),27,28)]]

for(i in seq(5:28)){
  print(i)
  print(table(ud_cc[,i]))
}

#ud_cc <- ud_cc[,-which(names(ud_cc)=="n_29022_0_0")]

#ud_men <- ud_cc[ud_cc[,2]==2,]
#ud_men <- ud_men[,-c(1,2)]

#ud_women <- ud_cc[ud_cc[,2]==1,]
#ud_women <- ud_women[,-c(1,2)]

ud_all <- ud_cc[,-c(1,2)]

#ud_men$n_29034_0_0 <- scale(ud_men$n_29034_0_0)
#ud_men$n_29036_0_0 <- scale(ud_men$n_29036_0_0)

#ud_women$n_29034_0_0 <- scale(ud_women$n_29034_0_0)
#ud_women$n_29036_0_0 <- scale(ud_women$n_29036_0_0)

ud_all$n_29034_0_0 <- scale(ud_all$n_29034_0_0)
ud_all$n_29036_0_0 <- scale(ud_all$n_29036_0_0)

library(psych)

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


#mixed.cor.men <- mixedCor(data = ud_men, c = c_indices, d= d_indices, p= p_indices)
#mixed.cor.women <- mixedCor(data = ud_women, c = c_indices, d= d_indices, p= p_indices)
#mixed.cor.all <- mixedCor(data = ud_all, c = c_indices, d= d_indices, p= p_indices)

#mixed.cor.men <- mixedCor(data = ud_men)
#mixed.cor.women <- mixedCor(data = ud_women)
#options("mc.cores=num_cores") 
#mixed.cor.all <- mixedCor(data = ud_all )
mixed.cor.all <- cor_auto(data = ud_all )

#pearson.cor.men <- cor(ud_men, method="pearson")
#pearson.cor.women <- cor(ud_women, method="pearson")
pearson.cor.all <- cor(ud_all, method="pearson")

#kendall.cor.men <- cor(ud_men, method="kendall")
#kendall.cor.women <- cor(ud_women, method="kendall")
kendall.cor.all <- cor(ud_all, method="kendall")

#spearman.cor.men <- cor(ud_men, method="spearman")
#spearman.cor.women <- cor(ud_women, method="spearman")
spearman.cor.all <- cor(ud_all, method="spearman")

#saveRDS(mixed.cor.men, file = paste("./2022_fitted_models_v3/mixed.cor.men.rds",sep=""))
#saveRDS(mixed.cor.women, file = paste("./2022_fitted_models_v3/mixed.cor.women.rds",sep=""))
saveRDS(mixed.cor.all, file = paste("./2022_fitted_models_v3/mixed.cor.all.rds",sep=""))

#saveRDS(pearson.cor.men, file = paste("./2022_fitted_models_v3/pearson.cor.men.rds",sep=""))
#saveRDS(pearson.cor.women, file = paste("./2022_fitted_models_v3/pearson.cor.women.rds",sep=""))
saveRDS(pearson.cor.all, file = paste("./2022_fitted_models_v3/pearson.cor.all.rds",sep=""))

#saveRDS(kendall.cor.men, file = paste("./2022_fitted_models_v3/kendall.cor.men.rds",sep=""))
#saveRDS(kendall.cor.women, file = paste("./2022_fitted_models_v3/kendall.cor.women.rds",sep=""))
saveRDS(kendall.cor.all, file = paste("./2022_fitted_models_v3/kendall.cor.all.rds",sep=""))

#saveRDS(spearman.cor.men, file = paste("./2022_fitted_models_v3/spearman.cor.men.rds",sep=""))
#saveRDS(spearman.cor.women, file = paste("./2022_fitted_models_v3/spearman.cor.women.rds",sep=""))
saveRDS(spearman.cor.all, file = paste("./2022_fitted_models_v3/spearman.cor.all.rds",sep=""))

#mixed.cor.men <- readRDS(file = paste("./2022_fitted_models_v3/mixed.cor.men.rds",sep=""))
#mixed.cor.women <- readRDS(file = paste("./2022_fitted_models_v3/mixed.cor.all.rds",sep=""))
mixed.cor.all <- readRDS(file = paste("./2022_fitted_models_v3/mixed.cor.all.rds",sep=""))

#pearson.cor.men <- readRDS(file = paste("./2022_fitted_models_v3/pearson.cor.men.rds",sep=""))
#pearson.cor.women <- readRDS(file = paste("./2022_fitted_models_v3/pearson.cor.women.rds",sep=""))
pearson.cor.all <- readRDS(file = paste("./2022_fitted_models_v3/pearson.cor.all.rds",sep=""))

#kendall.cor.men <- readRDS(file = paste("./2022_fitted_models_v3/kendall.cor.men.rds",sep=""))
#kendall.cor.women <- readRDS(file = paste("./2022_fitted_models_v3/kendall.cor.women.rds",sep=""))
kendall.cor.all <- readRDS(file = paste("./2022_fitted_models_v3/kendall.cor.all.rds",sep=""))

#spearman.cor.men <- readRDS(file = paste("./2022_fitted_models_v3/spearman.cor.men.rds",sep=""))
#spearman.cor.women <- readRDS(file = paste("./2022_fitted_models_v3/spearman.cor.women.rds",sep=""))
spearman.cor.all <- readRDS(file = paste("./2022_fitted_models_v3/spearman.cor.all.rds",sep=""))

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


