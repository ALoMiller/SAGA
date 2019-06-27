#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analytical function to generate survey indices
#__________________________________________________________________________________________________________________________
get.survey.stratum.estimates.2.fn <- function(spp=NULL, 
                                              survey = NULL, 
                                              oc = sole, 
                                              strata = NULL,
                                              lengths = 1:34, 
                                              do.length = TRUE, 
                                              do.age = FALSE, 
                                              gcf = 1,  #gear conversion factor
                                              dcf = 1,  #door conversion factor
                                              vcf = 1,  #vessel conversion factor
                                              tow_swept_area = 0.01)
  {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # *********** sql queries ***************#
  #Stratum information required to stratify (stratum area, etc)
  strata <- paste0('0', strata)
  stratum.sizes.q <- paste("select stratum, stratum_area, strgrp_desc, stratum_name from svmstrata where STRATUM IN ('", 
                           paste(strata, collapse = "','"), "')"," order by stratum", sep = '')
  str.size <- sqlQuery(oc,stratum.sizes.q)
  print(str.size)
  str.size$NTOWS <- str.size$STRATUM_AREA/tow_swept_area

  #STATION location data 
  q.sta <- paste("select cruise6, stratum, tow, station, shg, svvessel, svgear, est_year, est_month, est_day, ",
                 "substr(est_time,1,2) || substr(est_time,4,2) as time, towdur, dopdistb, dopdistw, avgdepth, ",
                 "area, bottemp, beglat, beglon from union_fscs_svsta ",
                  "where cruise6 = ", survey
                 #this would only get one year of survey data! option to fix below
                 #"where cruise6 in ('", paste(survey, collapse = "','"), "')"
                 , " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                 " and shg<= '136' order by cruise6, stratum, tow, station", sep = '')
  sta.view <- sqlQuery(oc,q.sta) 
  temp <- str.size[match(sta.view$STRATUM, str.size$STRATUM),]
  sta.view <- cbind(sta.view, temp[,-1])
  
  #CATCH data
  q.cat <- paste("select cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from union_fscs_svcat ",
                 "where cruise6 = ", survey
                 #this would only get one year of survey data! option to fix below
                 #"where cruise6 in ('", paste(survey, collapse = "','"), "')"
                 , " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                 " and svspp = ", spp, " order by cruise6, stratum, tow, station", sep = '')
  #print(q.cat)
  cat.view <- sqlQuery(oc,q.cat)
  #merge catch data and station location information
  catch.data <- merge(sta.view, cat.view, by = c('CRUISE6','STRATUM','TOW','STATION'),  all.x = T, all.y=F)
  #convert NA to 0 in catch number and weight
  catch.data$EXPCATCHNUM[is.na(catch.data$EXPCATCHNUM)] <- 0
  catch.data$EXPCATCHWT[is.na(catch.data$EXPCATCHWT)] <- 0
  
  #gear conversion - expand catch using a particular gear by the gear conversion factor.
  catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))] <- 
    gcf * catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))]
  catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))] <- 
    gcf * catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))]

  #door conversion 
  catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)] <- 
    dcf * catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)]
  catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)] <- 
    dcf * catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)]

  #vessel conversion
  catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')] <- 
    vcf * catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')]
  catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')] <- 
    vcf * catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')]
  
  #Extract the number of stations in each selected stratum in the selected years
  m <- sapply(str.size$STRATUM, function(x) sum(sta.view$STRATUM == x))
  M <- str.size$NTOWS #This is the proportional relationship between the area of the stratum and area of a tow...
  #not sure what this is doing as towarea is set to .01
  
  #This is a sum of the catch over each stratum
  samp.tot.n.w <- t(sapply(str.size$STRATUM, 
                           function(x) apply(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')],2,sum)))
  #variance covariance matrix of each catch variable (why do we need covariance?)
  S.n.w.stratum <- t(sapply(str.size$STRATUM, 
                            function(x) var(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')])))
  #weighting factor for each stratum (area of stratum/area sampled)
  N.W.hat.stratum <- M * samp.tot.n.w/m
  Vhat.N.W.hat.stratum <- M^2 * (1 - m/M) * S.n.w.stratum/m
  n.strata <- length(M)
  print(4)
  
  if(do.length){
    #LENGTH
    q.len <- paste("select  cruise6, stratum, tow, station, catchsex, length, expnumlen from union_fscs_svlen " ,
                   "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
                   "and svspp = ", spp, " order by cruise6, stratum, tow, station, svspp, catchsex", sep = '')
    len.view <- sqlQuery(oc,q.len)
    len.data <- merge(catch.data, len.view, by = c('CRUISE6','STRATUM','TOW','STATION','CATCHSEX'),  all.x=T, all.y = F)
    len.data$EXPNUMLEN[is.na(len.data$EXPNUMLEN)] <- 0
    if(max(len.data$LENGTH, na.rm= T) > max(lengths)) warning(paste('max of lengths in length data = ', max(len.data$LENGTH, na.rm= T), ' whereas max of lengths given is ', max(lengths), sep = ''))
    
    samp.tot.nal <- sapply(lengths, function(x) sapply(str.size$STRATUM, function(y) sum(len.data$EXPNUMLEN[len.data$STRATUM == y & len.data$LENGTH == x],
                                                                                         na.rm = TRUE)))
    Nal.hat.stratum <- M * samp.tot.nal/m
    #		print(Nal.hat.stratum)
    #		print(cbind(apply(Nal.hat.stratum,1,sum), N.W.hat.stratum))
    S.nal.stratum <- t(sapply(str.size$STRATUM, function(x){
      x.dat <- len.data[which(len.data$STRATUM == x),]
      if(dim(x.dat)[1]){
        nal.by.tow <- t(sapply(unique(x.dat$TOW), function(y){
          if(sum(x.dat$TOW == y)){
            nal.tow <- sapply(lengths, function(z) sum(x.dat$EXPNUMLEN[which(x.dat$LENGTH == z & x.dat$TOW == y)], na.rm = TRUE))
          }
          else nal.tow <- rep(0,length(lengths))
          return(nal.tow)
        }))
        cov.nal <- cov(nal.by.tow)
        return(cov.nal)
      }
      else return(matrix(NA,length(lengths),length(lengths)))
    }))
    Vhat.Nal.stratum <- M^2 * (1 - m/M) * S.nal.stratum/m
    print(5)
    
    
    if(do.age){
      #AGE
      q.age <- paste("select  cruise6, stratum, tow, station, sex, length, age, indwt, maturity from union_fscs_svbio ",
                     "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
                     "and svspp = ", spp, " and age is not null order by cruise6, stratum, tow, station", sep = '')
      age.view <- sqlQuery(oc,q.age)
      age.data <- merge(len.data, age.view, by = c('CRUISE6','STRATUM','TOW','STATION','LENGTH'),  all.x = T, all.y=F)
    }
  }
  
  
  
  out <- cbind(stratum = str.size$STRATUM, M = M, m = m, mean.n.w.per.tow = N.W.hat.stratum/M, V.mean.n.w.per.tow = Vhat.N.W.hat.stratum/(M^2))
  out <- list(out = out)
  out$stratum.size <- str.size
  if(do.length) {
    out$Nal.hat.stratum = Nal.hat.stratum
    out$V.Nal.stratum = Vhat.Nal.stratum
  }
  if(do.age) out$age.data <- age.data
  return(out)
}