#Use this to check if the station list is correct...

q.sta <- paste("select unique(STRATUM) from SVDBS.union_fscs_svsta  order by stratum;", sep = '')

options(stringsAsFactors = F)
allStrata=(sqlQuery(oc,q.sta))
tmp=lapply(allStrata$STRATUM,FUN=function(x) substr(x,1,1))
tmp2=lapply(allStrata$STRATUM,FUN=function(x) substr(x,5,5))
gfishstrata=sort(allStrata$STRATUM[which(tmp=='0' & tmp2=='0') ])

strata.list <- read.csv('files/StrataList.csv')
strata.list$AllStrata <- ifelse(nchar(strata.list$AllStrata)<5,paste0('0', strata.list$AllStrata),strata.list$AllStrata)

strata.list$AllStrata[which(!paste(strata.list$AllStrata)%in%gfishstrata)]
