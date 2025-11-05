library(haven)
library(dplyr)
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

use_depression[which(use_depression$n_29033_0_0>9),]$n_29033_0_0 <- 10

use_depression[which(use_depression$n_29033_0_0==-4),]$n_29033_0_0 <- 10

use_depression[which(use_depression$n_29033_0_0<0),]$n_29033_0_0 <- NA

use_depression$n_29021_0_0_weightgain <- as.integer((use_depression$n_29021_0_0==0) | (use_depression$n_29021_0_0==2))
use_depression$n_29021_0_0_weightloss <- as.integer((use_depression$n_29021_0_0==1) | (use_depression$n_29021_0_0==2))
use_depression <- use_depression[,-which(names(use_depression)=="n_29021_0_0")]
#how to encode weight changes? field n_29021_0_0

use_depression$n_29020_0_0 <- case_match(use_depression$n_29020_0_0, 0 ~ 2, 1 ~ 3, 2 ~ 1)
use_depression$n_29017_0_0 <- case_match(use_depression$n_29017_0_0, 0 ~ 1, 1 ~ 3, 2 ~ 2)

use_depression[is.na(use_depression$n_29023_0_0),]$n_29023_0_0 <- 0
use_depression[is.na(use_depression$n_29024_0_0),]$n_29024_0_0 <- 0
use_depression[is.na(use_depression$n_29025_0_0),]$n_29025_0_0 <- 0

hist(colSums(!is.na(use_depression[5:28])))

ud_cc <- use_depression[complete.cases(use_depression),]
ud_cc[,apply(ud_cc, 2, min)==0] <- ud_cc[,apply(ud_cc, 2, min)==0] + 1
ud_cc <- ud_cc[,names(ud_cc)[c(1,2,25,26,seq(3,24),27,28)]]

ud_all <- ud_cc[,-c(1,2)]

ud_all$n_29034_0_0 <- scale(ud_all$n_29034_0_0)
ud_all$n_29036_0_0 <- scale(ud_all$n_29036_0_0)

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


