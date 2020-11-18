#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analytical function to generate survey indices
#__________________________________________________________________________________________________________________________
get.survey.stratum.estimates.2.fn <- function(spp=NULL, 
                                              survey = NULL, 
                                              oc = sole, 
                                              strata = NULL,
                                              lengths = NULL, 
                                              ages=NULL,
                                              do.length = TRUE, 
                                              do.age = FALSE,
                                              gcf.n = 1,  #gear number conversion factor
                                              dcf.n = 1,  #door numberconversion factor
                                              vcf.n = 1,  #vessel number conversion factor
                                              gcf.w = 1,  #gear number conversion factor
                                              dcf.w = 1,  #door numberconversion factor
                                              vcf.w = 1,  #vessel number conversion factor
                                              do.Albatross = FALSE,
                                              do.Bigelow = FALSE,
                                              tow_swept_area = 0.01,
                                              S=1,
                                              H=3,
                                              G=6,
                                              Type=1,
                                              Operation=3,
                                              Gear=2,
                                              Acquisition="X",
                                              species=NULL,
                                              spring.cruises=NULL,
                                              fall.cruises=NULL,
                                              do.BigLen=F,
                                              do.AlbLen=F,
                                              big.len.calib=NULL,
                                              boot=F,
                                              swept_area = TRUE
)
{
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  options(stringsAsFactors = F)
  # *********** sql queries ***************#
  #Stratum information required to stratify (stratum area, etc)
  strata <- ifelse(nchar(strata)<5,paste0('0', strata),strata) #fix for when this is run twice - it adds another 0 and causes many errors
  stratum.sizes.q <- paste("select stratum, stratum_area, strgrp_desc, stratum_name from svdbs.svmstrata where STRATUM IN ('", 
                           paste(strata, collapse = "','"), "')"," order by stratum", sep = '')
  str.size <- sqlQuery(oc,stratum.sizes.q)
  #print(survey)
  
  #if(as.integer(substr(paste(survey),1,4))<2009) { #TOGA is not needed for pre Bigelow years
  #  TowCoding=paste0( " and STATYPE <= ",S," and HAUL <= ",H," and GEARCOND <= ", G) 
  #} else TowCoding=paste0(" and TYPE_CODE <= ", Type, " and OPERATION_CODE <= ", Operation, " and GEAR_CODE <= ", Gear) 
  
  #STATION location data 
  q.sta <- paste("select cruise6, stratum, tow, station, shg, svvessel, svgear, est_year, est_month, est_day, ",
                 "substr(est_time,1,2) || substr(est_time,4,2) as time, towdur, dopdistb, dopdistw, avgdepth, ",
                 " statype, haul, gearcond, type_code, operation_code, gear_code, acquisition_code,",
                 "area, bottemp, beglat, beglon from svdbs.union_fscs_svsta ",
                 "where cruise6 = ", paste(survey)
                 #this would only get one year of survey data! option to fix below
                 #"where cruise6 in ('", paste(survey, collapse = "','"), "')"
                 , " and STRATUM IN ('", paste(strata, collapse = "','"), "')"
                 #, TowCoding, 
                 ," order by cruise6, stratum, tow, station", sep = '')
  
  sta.view <- sqlQuery(oc,q.sta) 
  #print(head(sta.view))
  shg = cbind.data.frame(S= as.integer(substr(sta.view$SHG,1,1)),H = as.integer(substr(sta.view$SHG,2,2)),G = as.integer(substr(sta.view$SHG,3,3)))
  toga = cbind.data.frame(T = sta.view$TYPE_CODE, O = sta.view$OPERATION_CODE, G = sta.view$GEAR_CODE, A = sta.view$ACQUISITION_CODE)
  #SHG OR TOGA filter below
  if(as.integer(substr(paste(survey),1,4))<2009) sta.view = sta.view[shg$S <= S & shg$H <= H & shg$G <= G,]
  else sta.view = sta.view[toga$T <= Type & toga$O <= Operation & toga$G <= Gear,] #A not used
  #print("sta.view")
  #print(head(sta.view))
  temp <- str.size[match(sta.view$STRATUM, str.size$STRATUM),]
  sta.view <- cbind(sta.view, temp[,-1])
  
  #Add station specific swept area
  
  q.sta.sweptarea <- paste0("select cruise6, station, area_swept_wings_mean_km2 from svdbs.tow_evaluation ",
                            "where cruise6 = ", survey)
  sta.sweptarea <- sqlQuery(oc,q.sta.sweptarea)
  temp2 <- sta.sweptarea[match(sta.view$STATION,sta.sweptarea$STATION),]
  tow_swept_area = ifelse(sta.view$SVVESSEL[1]=='HB', 0.007, 0.01) #changes for smaller swept area during Bigelow years
  sta.view <- cbind(sta.view, AREA_SWEPT_WINGS_MEAN_KM2 = temp2[,c(-1,-2)])
  #Early years don't seem to have this variable!
  sta.view$AREA_SWEPT_WINGS_MEAN_NM2=ifelse(!is.na(sta.view$AREA_SWEPT_WINGS_MEAN_KM2)
                                            ,sta.view$AREA_SWEPT_WINGS_MEAN_KM2*0.539957^2,NA) #change from km to nm
  sta.view$AREA_SWEPT_WINGS_MEAN_NM2[which(is.na(sta.view$AREA_SWEPT_WINGS_MEAN_NM2))] <- tow_swept_area #if no tow eval data, uses default
  sta.view$SWEPT_AREA_RATIO <- tow_swept_area/sta.view$AREA_SWEPT_WINGS_MEAN_NM2 #proportion of default swept area for that particular tow
  str.size$ExpArea <- str.size$STRATUM_AREA/tow_swept_area #effectively the total number of possible tows in a stratum
  
  #CATCH data
  q.cat <- paste("select cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from svdbs.union_fscs_svcat ",
                 "where cruise6 = ", survey
                 #this would only get one year of survey data! option to fix below
                 #"where cruise6 in ('", paste(survey, collapse = "','"), "')"
                 , " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                 " and svspp = ", spp, " order by cruise6, stratum, tow, station", sep = '')
  cat.view <- sqlQuery(oc,q.cat)
  #merge catch data and station location information
  catch.data <- merge(sta.view, cat.view, by = c('CRUISE6','STRATUM','TOW','STATION'),  all.x = T, all.y=F)
  #convert NA to 0 in catch number and weight
  catch.data$EXPCATCHNUM=ifelse(is.na(catch.data$EXPCATCHNUM),0,catch.data$EXPCATCHNUM)
  catch.data$EXPCATCHWT=ifelse(is.na(catch.data$EXPCATCHWT),0,catch.data$EXPCATCHWT)
  
  #gear conversion - expand catch using a particular gear by the gear conversion factor.
  if(any(catch.data$SVGEAR %in% c(41,45))) { #This is an error trap for no gear of this type being in catch data
    catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))] <- 
      as.numeric(gcf.n) * catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))]
    catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))] <- 
      as.numeric(gcf.w) * catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))]
  }
  #door conversion 
  if(any(catch.data$YEAR< 1985)) { #This is an error trap for no years < 1985
    catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)] <- 
      as.numeric(dcf.n) * catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)]
    catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)] <- 
      as.numeric(dcf.w) * catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)]
  }
  #vessel conversion
  if(any(catch.data$SVVESSEL[which(!is.na(catch.data$SVVESSEL))] == 'DE')) { #This is an error trap for no DE vessel observations in catch data
    catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')] <- 
      as.numeric(vcf.n) * catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')]
    catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')] <- 
      as.numeric(vcf.w) * catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')]
  }
  #Bigelow conversion to Albatross series
  if(do.Albatross & !do.AlbLen){
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
  #Albatross conversion to Bigelow series
  if(do.Bigelow){
    if(species$BIGELOWCALTYPE[species$SVSPP==spp] != 'NONE'){
      if(catch.data$CRUISE6[1] %in% fall.cruises){
        catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009]*species$FALLNUM[species$SVSPP==spp]
        catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009]*species$FALLWT[species$SVSPP==spp]
      }
      if(catch.data$CRUISE6[1] %in% spring.cruises){
        catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009]*species$SPRNUM[species$SVSPP==spp]
        catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009]*species$SPRWT[species$SVSPP==spp]
      }
    }
  } 
  #Albatross conversion to Bigelow series
  if(do.Bigelow & !do.BigLen){
    if(species$BIGELOWCALTYPE[species$SVSPP==spp] != 'NONE'){
      if(catch.data$CRUISE6[1] %in% fall.cruises){
        catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009]*species$FALLNUM[species$SVSPP==spp]
        catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009]*species$FALLWT[species$SVSPP==spp]
      }
      if(catch.data$CRUISE6[1] %in% spring.cruises){
        catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHNUM[catch.data$EST_YEAR<2009]*species$SPRNUM[species$SVSPP==spp]
        catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009] <- catch.data$EXPCATCHWT[catch.data$EST_YEAR<2009]*species$SPRWT[species$SVSPP==spp]
      }
    }
  }
  #Account for new swept area conversions
  if(swept_area){  
    catch.data$EXPCATCHNUM = catch.data$EXPCATCHNUM * catch.data$SWEPT_AREA_RATIO
    catch.data$EXPCATCHWT = catch.data$EXPCATCHWT * catch.data$SWEPT_AREA_RATIO
  }
  #Extract the number of stations in each selected stratum in the selected years
  m <- sapply(str.size$STRATUM, function(x) sum(sta.view$STRATUM == x))
  M <- str.size$ExpArea #This is the proportional relationship between the area of the stratum and area of a tow...
  #This is a sum of the catch over each stratum
  
  samp.tot.n.w <- t(sapply(str.size$STRATUM, 
                           function(x) apply(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')],2,sum)))
  #variance covariance matrix of each catch variable (why do we need covariance?)
  S.n.w.stratum <- t(sapply(str.size$STRATUM, 
                            function(x) var(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')])))
  #We only need the variances so drop the covariance cols
  S.n.w.stratum <- S.n.w.stratum[,c(1,4)] 
  
  #weighting factor for each stratum (area of stratum/area sampled) * effort
  N.W.hat.stratum <- M * samp.tot.n.w/m
  #variance weighting factor
  Vhat.N.W.hat.stratum <- matrix((M^2 * (1 - m/M) * S.n.w.stratum/m),ncol=2)
  n.strata <- length(M)
  
  if(do.length){
    #LENGTH from sql
    q.len <- paste("select  cruise6, stratum, tow, station, catchsex, length, expnumlen from svdbs.union_fscs_svlen " ,
                   "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
                   "and svspp = ", spp, " order by cruise6, stratum, tow, station, svspp, catchsex", sep = '')
    len.view <- sqlQuery(oc,q.len)
    len.data <- merge(catch.data, len.view, by = c('CRUISE6','STRATUM','TOW','STATION','CATCHSEX'),  all.x=T, all.y = F)
    len.data$EXPNUMLEN=ifelse(is.na(len.data$EXPNUMLEN),0,len.data$EXPNUMLEN)
    
    if(swept_area) len.data$EXPNUMLEN_ADJ = len.data$EXPNUMLEN * len.data$SWEPT_AREA_RATIO
    
    #cal.Nal.hat.stratum = Nal.hat.stratum
    for(i in 1:length(lengths))
    {
      if(do.Bigelow & do.BigLen & catch.data$EST_YEAR[1]<2009){ 
        len.data$EXPNUMLEN[which(len.data$LENGTH == lengths[i])] <- len.data$EXPNUMLEN[which(len.data$LENGTH == lengths[i])] * big.len.calib$CALIBRATION_FACTOR[big.len.calib$SVSPP == spp & big.len.calib$LENGTH == lengths[i]]
      }
      if(do.Albatross & do.AlbLen & catch.data$EST_YEAR[1]>2008){ 
        len.data$EXPNUMLEN[which(len.data$LENGTH == lengths[i])] <- len.data$EXPNUMLEN[which(len.data$LENGTH == lengths[i])] / big.len.calib$CALIBRATION_FACTOR[big.len.calib$SVSPP == spp & big.len.calib$LENGTH == lengths[i]]
      }
    }
    
    #Build in a place holder for bootstrapping length data
    if(boot){
      boot.lendat = boot.lendat.fn(len.data)
      len.data=boot.lendata
    }
    
    #check to see if the entire length comp is is sample per user bounds
    #if(max(len.data$LENGTH, na.rm= T) > max(lengths)) warning(paste('max of lengths in length data = ', max(len.data$LENGTH, na.rm= T), ' whereas max of lengths given is ', max(lengths), sep = ''))
    #Changing this so the warning message is reported through the app rather than the console
    
    Lrange=range(len.data$LENGTH,na.rm = TRUE) #will report this back to the app and then determine if the user has missed some lengths
    
    #nested sapply! This sums the numbers at length in each stratum over each of the user specified lengths
    #result is a matrix with a row for each stratum and a col for each length
    samp.tot.nal <- sapply(lengths, function(x) sapply(str.size$STRATUM
                                                       , function(y) sum(len.data$EXPNUMLEN[len.data$STRATUM == y & len.data$LENGTH == x],na.rm = TRUE)))
    samp.tot.nal<-matrix(samp.tot.nal,nrow=length(strata))
    rownames(samp.tot.nal) <- as.character(strata)
    colnames(samp.tot.nal) <- as.character(lengths)
    #print(head(len.data))
    #print(samp.tot.nal)
    #Apply the stratum weights to these
    Nal.hat.stratum <- M * samp.tot.nal/m
    #Remove NA
    Nal.hat.stratum=ifelse(is.na(Nal.hat.stratum),0,Nal.hat.stratum) 
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
    #print(Nal.hat.stratum)
    #print(S.nal.stratum)
    #Replace NA with zero top match SAGA
    S.nal.stratum=ifelse(is.na(S.nal.stratum),0,S.nal.stratum)
    Vhat.Nal.stratum <- M^2 * (1 - m/M) * S.nal.stratum/m #Apply the weighting factors to the variances
    
    if(do.age){
      #AGE
      species <- read.csv('files/speciesTable.csv')
      BigRange=(species[which(species$SVSPP==spp),c("MINA","MAXA")]) #need this for the multinomial fit
      BigAge=seq(as.numeric(BigRange[1]),as.numeric(BigRange[2]))
      q.age <- paste("select  cruise6, stratum, tow, station, sex, length, age, indwt, maturity from svdbs.union_fscs_svbio ",
                     " where cruise6 = ", survey, " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                     " and svspp = ", spp, " and age is not null order by cruise6, stratum, tow, station", sep = '')
      age.view <- sqlQuery(oc,q.age)
      #age.data <- merge(len.data, age.view, by = c('CRUISE6','STRATUM','TOW','STATION','LENGTH'),  all.x = T, all.y=F)
      WtLenEx<- merge(len.data, age.view, by = c('CRUISE6','STRATUM','TOW','STATION','LENGTH'),  all.x = T, all.y=F)
      #Need to generate age-length keys here and then apply to the length composition
      
      #Need to make sure we have data at this stage to proceed (requires more than one age too)
      if(nrow(WtLenEx[!is.na(WtLenEx$AGE),])>0) {
        #Multinomial age length key generation
        #From Jon.... 
        #library(nnet) #required for multinom in get.mult.props function below
        # function to compute proportions of age at length using multinomial approach
        # based on code from Mike Bednarski and then stolen from ADIOS again
        get.mult.props<-function(big.len=NULL,big.age=NULL,ref.age=NULL){
          data<-WtLenEx[!is.na(WtLenEx$AGE) & WtLenEx$AGE%in%big.age,]
          #data$AGE <- relevel(as.factor(data$AGE), ref=ref.age) # relevel and make categorical
          data$AGE <- as.factor(data$AGE)
          my.levels <- as.numeric(levels(data$AGE))
          n.levels <- length(my.levels)
          mn<-nnet::multinom(AGE ~ LENGTH, data=data,verbose=F)
          Parameters <- coefficients(mn) # Parameters for multinomial key
          newdata <- data.frame(cbind(LENGTH = big.len)) # length values
          logits <- matrix(NA, nrow=length(big.len), ncol=length(my.levels))
          logits[,1] <- rep(0,nrow(newdata)) # reference age
          for (i in 1:(n.levels-1)){
            logits[,(i+1)] <- Parameters[i] + Parameters[(n.levels-1+i)]*newdata$LENGTH
          }
          # new code: to handle logit returns of large numbers
          for (i in 1:n.levels){
            logits[logits[,i] >= 500,i] <- 500
          }
          p.unscaled <- exp(logits)
          p.normalized <- p.unscaled / rowSums(p.unscaled)
          p <- matrix(0, nrow=length(big.len), ncol=length(big.age)) # note starting at min big.age
          for (i in 1:n.levels){
            #p[,(my.levels[i]+1)] <- p.normalized[,i]
            p[,which(my.levels[i]==big.age)] <- p.normalized[,i]
          }
          colnames(p)<-c(paste("pred.",big.age, sep=""))
          return(p)
        } #end get.mult.props function
        
        #Can only fit this if there is more than one age!
        if(length(unique(WtLenEx$AGE[!is.na(WtLenEx$AGE)]))>1) {
          #The multnomial should fit to all the ages in the data and then clip to the user specified range afterwards
          mult.props<- get.mult.props(big.len=(lengths),big.age=BigAge)
          #,ref.age = 3) #don't think we need this
        } else { #if there is only one age then you have 100% in one row
          p <- matrix(0, nrow=length(lengths), ncol=(max(ages)+1))
          p[min(WtLenEx$LENGTH[!is.na(WtLenEx$AGE)]):max(WtLenEx$LENGTH[!is.na(WtLenEx$AGE)])
            ,(unique(WtLenEx$AGE[!is.na(WtLenEx$AGE)])+1)]=1
          colnames(p)<-c(paste("pred.",ages, sep=""))
          mult.props=p 
        }
        
        #Now apply the age length key to the length distribution to get numbers at age  
        Naa.hat.stratum=c()
        Naa.hat.stratum=Nal.hat.stratum%*%mult.props #should give numbers at age for each stratum...
        #remove the stupid factors!
        Naa.hat.stratum=matrix(as.numeric(paste(Naa.hat.stratum)),nrow=nrow(Naa.hat.stratum))
        #Naa.hat.stratum=rbind(Naa.hat.stratum,"Total"=colSums(Naa.hat.stratum)) #add a row for the total - this is done in App.R
        Naa.hat.stratum=data.frame(strata,Naa.hat.stratum,stringsAsFactors = F) #make sure the strata names are preserved
        #Now clip out the ages that the user did not want
        #print(ages)
        #print((Naa.hat.stratum))
        Naa.hat.stratum[,2:(length(ages)+1)]=Naa.hat.stratum[,(which(BigAge%in%ages)+1)]
        Naa.hat.stratum=Naa.hat.stratum[,1:(length(ages)+1)]
        names(Naa.hat.stratum)=c("Stratum",paste0("Age",ages)) #rename cols
      } else { #No data condition
        Naa.hat.stratum=c();
        Naa.hat.stratum=data.frame(strata,matrix(NA,nrow=length(strata),ncol=(max(ages)+1)),stringsAsFactors = F)
        names(Naa.hat.stratum)=c("Stratum",paste0("Age",ages)) #rename cols
      }
      
    }
  }
  
  #data.frame of stratified indices and variances 
  dimnames(Vhat.N.W.hat.stratum)[[1]]=NULL
  dimnames(Vhat.N.W.hat.stratum)[[2]]=c("VARNUM", "VARWT")
  out <- cbind(stratum = str.size$STRATUM, M = M, m = m, mean.n.w.per.tow = N.W.hat.stratum/M
               , V.mean.n.w.per.tow = Vhat.N.W.hat.stratum/(M^2))
  out <- list(out = out)
  out$stratum.size <- str.size
  if(do.length) {
    out$Nal.hat.stratum = Nal.hat.stratum #Indices by length
    out$V.Nal.stratum = Vhat.Nal.stratum #variances by length
  }
  if(do.age) out$Naa.hat.stratum <- Naa.hat.stratum
  
  #warning(paste0("Some strata for survey have zero tows, will extrapolate data to these areas"))
  ZeroTows=c()
  if(any(out$out[,"m"] == 0)) ZeroTows=out$out[which(out$out[,"m"]==0),"stratum"] 
  out$expand = sum(M)/sum(M[which(m>0)]) #=1 if all strata are sampled
  #report warnings
  out$warnings=list(LengthRange=Lrange,UnusedStrata=out$out[,"stratum"][out$out[,"EXPCATCHNUM"]==0.0],UnsampledStrata=ZeroTows)
  
  
  #Add the station data for use later in output 
  out$catchdata=list(catch.data)
  #print(out$out)
  
  
  return(out)
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
boot.lendat.fn = function(lendat, dif=0)
{
  #print(head(lendat))
  #print(dim(lendat))
  if(is.null(lendat$id)) lendat$id = paste0(lendat$STRATUM,"_",lendat$CRUISE6, "_", lendat$STATION)
  strata = sort(unique(lendat$STRATUM))
  #print(strata)
  boot.ids = unlist(sapply(strata, function(x) {
    stratum.dat = lendat[which(lendat$STRATUM == x),]
    ids = unique(stratum.dat$id)
    #print(x)
    #print(ids)
    sample(ids, size = length(ids)-dif, replace = TRUE)
  }))
  #print(boot.ids)
  boot.index = unlist(sapply(boot.ids, function(x) which(lendat$id == x)))
  #print(boot.index)
  boot.nindex = sapply(boot.ids, function(x) sum(lendat$id == x))
  #print(boot.nindex)
  #print(length(boot.nindex))
  #print(length(boot.ids))
  boot.ids = rep(1:length(boot.ids), boot.nindex)
  #print(length(boot.ids))
  #print(length(boot.index))
  boot.lendat = lendat[boot.index,]
  boot.lendat$id = boot.ids
  return(boot.lendat)
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#modification to showNotifications to allow HTML user input
showNotification2 <- function (ui, action = NULL, duration = 5, closeButton = TRUE, 
                               id = NULL, type = c("default", "message", "warning", "error"), 
                               session = shiny:::getDefaultReactiveDomain()) {
  if (is.null(id)) 
    id <- shiny:::createUniqueId(8)
  res <- shiny:::processDeps(HTML(ui), session)
  actionRes <- shiny:::processDeps(action, session)
  #print(res$html)
  session$sendNotification("show", list(html = res$html, action = actionRes$html, 
                                        deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 
                                          1000, closeButton = closeButton, id = id, type = match.arg(type)))
  id
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
