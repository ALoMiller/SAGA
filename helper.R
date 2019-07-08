#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analytical function to generate survey indices
#__________________________________________________________________________________________________________________________
get.survey.stratum.estimates.2.fn <- function(spp=NULL, 
                                              survey = NULL, 
                                              oc = sole, 
                                              strata = NULL,
                                              lengths = NULL, 
                                              do.length = TRUE, 
                                              do.age = FALSE, 
                                              gcf = 1,  #gear conversion factor
                                              dcf = 1,  #door conversion factor
                                              vcf = 1,  #vessel conversion factor
                                              do.Albatross = FALSE,
                                              tow_swept_area = 0.01,
                                              S=1,
                                              H=3,
                                              G=6
                                              )
  {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # *********** sql queries ***************#
  #Stratum information required to stratify (stratum area, etc)
  strata <- ifelse(nchar(strata)<5,paste0('0', strata),strata) #fix for when this is run twice - it adds another 0 and causes many errors
  stratum.sizes.q <- paste("select stratum, stratum_area, strgrp_desc, stratum_name from svdbs.svmstrata where STRATUM IN ('", 
                           paste(strata, collapse = "','"), "')"," order by stratum", sep = '')
  str.size <- sqlQuery(oc,stratum.sizes.q)
  #print(str.size)
  str.size$ExpAreas <- str.size$STRATUM_AREA/tow_swept_area

  #STATION location data 
  q.sta <- paste("select cruise6, stratum, tow, station, shg, svvessel, svgear, est_year, est_month, est_day, ",
                 "substr(est_time,1,2) || substr(est_time,4,2) as time, towdur, dopdistb, dopdistw, avgdepth, ",
                 " statype, haul, gearcond, ",
                 "area, bottemp, beglat, beglon from svdbs.union_fscs_svsta ",
                  "where cruise6 = ", survey
                 #this would only get one year of survey data! option to fix below
                 #"where cruise6 in ('", paste(survey, collapse = "','"), "')"
                 , " and STRATUM IN ('", paste(strata, collapse = "','"), "')"
                 , " and STATYPE <= ",S," and HAUL <= ",H," and GEARCOND <= ", G #changing this to allow user specified SHG choices
                 #" and shg<= '136' order by cruise6, stratum, tow, station", sep = '')
                 , " order by cruise6, stratum, tow, station", sep = '')
  sta.view <- sqlQuery(oc,q.sta) 
  temp <- str.size[match(sta.view$STRATUM, str.size$STRATUM),]
  sta.view <- cbind(sta.view, temp[,-1])
  
  #CATCH data
  q.cat <- paste("select cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from svdbs.union_fscs_svcat ",
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
  if(any(catch.data$SVGEAR %in% c(41,45))) { #This is an error trap for no gear of this type being in catch data
    catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))] <- 
      gcf * catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))]
    catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))] <- 
      gcf * catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))]
  }
  #door conversion 
  if(any(catch.data$YEAR< 1985)) { #This is an error trap for no years < 1985
    catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)] <- 
      dcf * catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)]
    catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)] <- 
      dcf * catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)]
  }
  #vessel conversion
  if(any(catch.data$SVVESSEL == 'DE')) { #This is an error trap for no DE vessel observations in catch data
    catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')] <- 
      vcf * catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')]
    catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')] <- 
      vcf * catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')]
  }
  #Bigelow conversion to Albatross series
  if(do.Albatross){
    if(species$BIGELOWCALTYPE[species$SVSPP==spp] != 'NONE'){
        if(catch.data$CRUISE6[1] %in% fall.cruises){
          catch.data$EXPCATCHNUM[catch.data$EST_YEAR>2008] <- catch.data$EXPCATCHNUM[catch.data$EST_YEAR>2008]/species$FALLNUM[species$SVSPP==spp]
          catch.data$EXPCATCHWT[catch.data$EST_YEAR>2008] <- catch.data$EXPCATCHWT[catch.data$EST_YEAR>2008]/species$FALLWT[species$SVSPP==spp]
        }
        if(catch.data$CRUISE6[1] %in% spring.cruises){
          catch.data$EXPCATCHNUM[catch.data$EST_YEAR>2008] <- catch.data$EXPCATCHNUM[catch.data$EST_YEAR>2008]/species$SPRNUM[species$SVSPP==spp]
          catch.data$EXPCATCHWT[catch.data$EST_YEAR>2008] <- catch.data$EXPCATCHWT[catch.data$EST_YEAR>2008]/species$SPRWT[species$SVSPP==spp]
        }
    }
  }  
  #Extract the number of stations in each selected stratum in the selected years
  m <- sapply(str.size$STRATUM, function(x) sum(sta.view$STRATUM == x))
  M <- str.size$ExpArea #This is the proportional relationship between the area of the stratum and area of a tow...
  #This is a sum of the catch over each stratum
  
  #print(catch.data[which(catch.data$STRATUM%in%str.size$STRATUM),c('EXPCATCHNUM','EXPCATCHWT')])
  samp.tot.n.w <- t(sapply(str.size$STRATUM, 
                           function(x) apply(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')],2,sum)))
  #variance covariance matrix of each catch variable (why do we need covariance?)
  S.n.w.stratum <- t(sapply(str.size$STRATUM, 
                            function(x) var(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')])))
  #We only need the variances so drop the covariance cols
  S.n.w.stratum <- S.n.w.stratum[,c(1,4)] #NEED TO CHECK THESE AGAINST KNOWN VARIANCES!!!
  
  
  #weighting factor for each stratum (area of stratum/area sampled) * effort
  N.W.hat.stratum <- M * samp.tot.n.w/m
  #variance weighting factor
  Vhat.N.W.hat.stratum <- M^2 * (1 - m/M) * S.n.w.stratum/m
  n.strata <- length(M)
  
  if(do.length){
    #LENGTH from sql
    q.len <- paste("select  cruise6, stratum, tow, station, catchsex, length, expnumlen from svdbs.union_fscs_svlen " ,
                   "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
                   "and svspp = ", spp, " order by cruise6, stratum, tow, station, svspp, catchsex", sep = '')
    len.view <- sqlQuery(oc,q.len)
    len.data <- merge(catch.data, len.view, by = c('CRUISE6','STRATUM','TOW','STATION','CATCHSEX'),  all.x=T, all.y = F)
    len.data$EXPNUMLEN[is.na(len.data$EXPNUMLEN)] <- 0
    #check to see if the entire length comp is is sample per user bounds
    if(max(len.data$LENGTH, na.rm= T) > max(lengths)) warning(paste('max of lengths in length data = ', max(len.data$LENGTH, na.rm= T), ' whereas max of lengths given is ', max(lengths), sep = ''))
    #nested sapply! This sums the numbers at length in each stratum over each of the user specified lengths
    #result is a matrix with a row for each stratum and a col for each length
    samp.tot.nal <- sapply(lengths, function(x) sapply(str.size$STRATUM
                    , function(y) sum(len.data$EXPNUMLEN[len.data$STRATUM == y & len.data$LENGTH == x],na.rm = TRUE)))
    rownames(samp.tot.nal) <- as.character(strata)
    colnames(samp.tot.nal) <- as.character(lengths)
    #print(head(len.data))
    #print(samp.tot.nal)
    #Apply the stratum weights to these
    Nal.hat.stratum <- M * samp.tot.nal/m
    #Remove NA
    Nal.hat.stratum[is.na(Nal.hat.stratum)] <- 0 
    #Now we need to get the variances - triple nested sapply!
    #This will produce nonsense if there are strata selected with no observations!!
    S.nal.stratum <- t(sapply(str.size$STRATUM, function(x){ #The function below is applied over each strata
      x.dat <- len.data[which(len.data$STRATUM == x & !is.na(len.data$LENGTH)),] #len.data is the merged catch data (has location, etc) and the length data
      if(dim(x.dat)[1]){ #if there are observations in len.data that match the stratum being tested this will be T 
        nal.by.tow <- t(sapply(unique(x.dat$TOW), function(y){ #for each unique tow...
          if(sum(x.dat$TOW == y)){ #I think this has to always be T, unless x.dat$TOW is a factor or something? 
            #generate a sum of the number of fish at each length from the user specified lengths this will also generate 0s for 
            #empty lengths
            nal.tow <- sapply(lengths, function(z) sum(x.dat$EXPNUMLEN[which(x.dat$LENGTH == z & x.dat$TOW == y)], na.rm = TRUE))
          }
          else nal.tow <- rep(0,length(lengths)) #fill with zeroes
          return(nal.tow)
        }))
        cov.nal <- diag(cov(nal.by.tow)) #since we have all the zeroes filled in we can generate a covariance, but we only want
        #the main diagonal of this matrix (I think) #NEED TO CHECK THESE VARIANCE TERMS AGAINST A KNOWN SOLUTION!!!
        #cov.nal[is.na(cov.nal)]= 0
        #cov.nal=diag(cov.nal)
        return(cov.nal)
      }
      #else return(matrix(0,length(lengths),length(lengths))) 
      else return(matrix(NA,1,length(lengths))) #only returning the main diagonal so this should be 1 row
    }))
    print(Nal.hat.stratum)
    print(S.nal.stratum)
    #Replace NA with zero top match SAGA
    S.nal.stratum=ifelse(is.na(S.nal.stratum),0,S.nal.stratum)
    Vhat.Nal.stratum <- M^2 * (1 - m/M) * S.nal.stratum/m #Apply the weighting factors to the variances
    
    
    if(do.age){
      #AGE
      q.age <- paste("select  cruise6, stratum, tow, station, sex, length, age, indwt, maturity from svdbs.union_fscs_svbio ",
                     "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
                     "and svspp = ", spp, " and age is not null order by cruise6, stratum, tow, station", sep = '')
      age.view <- sqlQuery(oc,q.age)
      age.data <- merge(len.data, age.view, by = c('CRUISE6','STRATUM','TOW','STATION','LENGTH'),  all.x = T, all.y=F)
      #This is incomplete - all it does is collect the age data and merge it with the station data....
    }
  }
  
  #data.frame of stratified indices and variances 
  out <- cbind(stratum = str.size$STRATUM, M = M, m = m, mean.n.w.per.tow = N.W.hat.stratum/M
               , V.mean.n.w.per.tow = Vhat.N.W.hat.stratum/(M^2))
  out <- list(out = out)
  out$stratum.size <- str.size
  if(do.length) {
    out$Nal.hat.stratum = Nal.hat.stratum #Indices by length
    out$V.Nal.stratum = Vhat.Nal.stratum #variances by length
  }
  if(do.age) out$age.data <- age.data
  return(out)
}