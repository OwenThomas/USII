library(haven)
library(dplyr)
library(fpc)

ud_all = readRDS("./synthetic_cidi.rds")

fr.men <- flexmixedruns(ud_men, continuous=2,discrete=24,simruns=2,n.cluster=1:10,allout=TRUE)
fr.women <- flexmixedruns(ud_women, continuous=2,discrete=24,simruns=2,n.cluster=1:10,allout=TRUE)
fr.all <- flexmixedruns(ud_all, continuous=2,discrete=24,simruns=2,n.cluster=1:10,allout=TRUE)

saveRDS(fr.men, file = paste("./2022_fitted_models_v3/m.fpc.men.2022.rds",sep=""))
saveRDS(fr.women, file = paste("./2022_fitted_models_v3/m.fpc.women.2022.rds",sep=""))
saveRDS(fr.all, file = paste("./2022_fitted_models_v3/m.fpc.all.2022.rds",sep=""))

