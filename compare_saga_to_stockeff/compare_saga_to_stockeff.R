# This script compares the indices from the SAGA shiny app to
# the same indices in StockEff for GOM haddock.
# Initial comparison is without the measured-swept-area adjustment.
library(dplyr)
library(tidyr)
library(ggplot2)

directory <- "~/SAGA/compare_saga_to_stockeff/"

saga_file_spring <- paste0(directory,"HADDOCK_SPRING_2_96_.csv")
saga_file_fall   <- paste0(directory,"HADDOCK_FALL_2_96_.csv")

saga_file_spring_ws <- paste0(directory,"HADDOCK_SPRING_2_96_withswept.csv")
saga_file_fall_ws   <- paste0(directory,"HADDOCK_FALL_2_96_withswept.csv")




# Load GOM haddock StockEff stratified mean indices (numbers and biomass)
stockeff_indices <- 
  read.csv(paste0(directory,"ADIOS_SV_164744_GOM_NONE_strat_mean.csv")) %>%
  select(YEAR, SEASON, INDEX_TYPE, INDEX, LOWER_90_CI, UPPER_90_CI) %>%
  mutate(source = "StockEff") %>%
  filter(INDEX_TYPE == "Abundance (numbers/tow)")

saga_indices_SPRING <- 
  read.csv(saga_file_spring, skip = 55, nrows = 52) %>%
  select(Year, Total) %>%
  mutate(INDEX_TYPE = "Abundance (numbers/tow)") %>%
  rename(YEAR = Year, INDEX = Total) %>% #, Var = VarNum) %>%
  # rbind({read.csv(paste0(directory, "HADDOCK_SPRING_2_96_.csv"), skip = 1, nrows = 52) %>%
  #        select(Year, Wt, VarWt) %>%
  #        mutate(INDEX_TYPE = "Biomass (kg/tow)") %>%
  #        rename(YEAR = Year, INDEX = Wt, Var = VarWt)}) %>%
  mutate(`LOWER_90_CI` = NA,#INDEX - 1.645*Var,
         `UPPER_90_CI` = NA,#INDEX + 1.645*Var,
         SEASON = "SPRING") %>%
  mutate(source = "SAGA Shiny App")

saga_indices_FALL <- 
  read.csv(saga_file_fall, skip = 55, nrows = 52) %>%
  #select(Year, Num, VarNum) %>%
  select(Year, Total) %>%
  mutate(INDEX_TYPE = "Abundance (numbers/tow)") %>%
  rename(YEAR = Year, INDEX = Total) %>% #, Var = VarNum) %>%
  # rbind({read.csv(paste0(directory,"HADDOCK_FALL_2_96_.csv"), skip = 1, nrows = 52) %>%
  #     select(Year, Wt, VarWt) %>%
  #     mutate(INDEX_TYPE = "Biomass (kg/tow)") %>%
  #     rename(YEAR = Year, INDEX = Wt, Var = VarWt)}) %>%
  mutate(LOWER_90_CI = NA, #INDEX - 1.645*Var,
         UPPER_90_CI = NA, #INDEX + 1.645*Var,
         SEASON = "FALL") %>%
  mutate(source = "SAGA Shiny App")
  
         
both_indices <- rbind(stockeff_indices, saga_indices_SPRING, saga_indices_FALL)

# Plot both indices together
ggplot(both_indices, aes(x = YEAR, y = INDEX, color = source)) +
  geom_line() +
  geom_point() +
  facet_grid(SEASON ~ INDEX_TYPE) +
  theme_bw() +
  ylab("") +
  xlab("")

# Plot percent differences in indices
pc_diff_index <-
  both_indices %>%
  select(-LOWER_90_CI, -UPPER_90_CI) %>%
  spread(source, INDEX) %>%
  mutate(`Percent difference` = 100 * (`SAGA Shiny App` - StockEff)/StockEff)

ggplot(pc_diff_index, aes(x = YEAR, y = `Percent difference`)) +
  geom_line() +
  geom_point() +
  facet_grid(SEASON ~ INDEX_TYPE) +
  theme_bw()


# Load StockEff and Saga NAA
stockeff_naa <- 
  read.csv(paste0(directory,"ADIOS_SV_164744_GOM_NONE_strat_mean_age_auto.csv")) %>%
  select(YEAR, SEASON, AGE, NO_AT_AGE) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(paste0(directory,"ADIOS_SV_164744_GOM_NONE_strat_mean_age_auto.csv")) %>%
        select(YEAR, SEASON, AGE, NO_AT_AGE) %>% 
        group_by(YEAR, SEASON) %>% 
        filter(AGE >= 9) %>% 
        summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
        mutate(AGE = "9+")}) %>%
  mutate(source = "StockEff")

saga_naa_SPRING <-
  read.csv(saga_file_spring, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_spring, skip = 163, nrows = 52) %>%
             select(-nTows, -Total) %>%
             gather(AGE, NO_AT_AGE, -Year) %>%
             rename(YEAR = Year) %>%
             mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
             filter(AGE >= 9) %>% 
             group_by(YEAR) %>% 
             summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
             mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "SPRING",
         source = "SAGA Shiny App")

saga_naa_SPRING_ws <-
  read.csv(saga_file_spring_ws, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_spring_ws, skip = 163, nrows = 52) %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      filter(AGE >= 9) %>% 
      group_by(YEAR) %>% 
      summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
      mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "SPRING",
         source = "SAGA Shiny App with swept area")

saga_naa_FALL <-
  read.csv(saga_file_fall, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_fall, skip = 163, nrows = 52) %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      filter(AGE >= 9) %>% 
      group_by(YEAR) %>% 
      summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
      mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "FALL",
         source = "SAGA Shiny App")

saga_naa_FALL_ws <-
  read.csv(saga_file_fall_ws, skip = 163, nrows = 52) %>%
  select(-nTows, -Total) %>%
  gather(AGE, NO_AT_AGE, -Year) %>%
  rename(YEAR = Year) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  bind_rows({read.csv(saga_file_fall_ws, skip = 163, nrows = 52) %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      filter(AGE >= 9) %>% 
      group_by(YEAR) %>% 
      summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
      mutate(AGE = "9+")}) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "FALL",
         source = "SAGA Shiny App with swept area")
  

both_naa <- 
  rbind(stockeff_naa, saga_naa_SPRING, saga_naa_FALL,
        saga_naa_SPRING_ws, saga_naa_FALL_ws) %>%
  mutate(AGE = paste0("Age-", AGE)) %>%
  filter(YEAR >= 2009)

# Plot spring NAA comparison
ggplot(both_naa %>% filter(SEASON == "SPRING"),
       aes(x = YEAR, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Year") +
  ggtitle("Spring NAA comparison")

# Plot fall NAA comparison
ggplot(both_naa %>% filter(SEASON == "FALL"),
       aes(x = YEAR, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Year") +
  ggtitle("Fall NAA comparison")

# Facet by year rather than age
ggplot(both_naa %>% 
         filter(SEASON == "SPRING") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))),
       aes(x = AGE, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Age") +
  ggtitle("Spring NAA comparison")

# Facet by year rather than age
ggplot(both_naa %>% 
         filter(SEASON == "FALL") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))),
       aes(x = AGE, y = NO_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Stratified mean numbers-at-age") +
  xlab("Age") +
  ggtitle("Fall NAA comparison")


# Plot percent differences in indices
pc_diff_naa <-
  both_naa %>%
  spread(source, NO_AT_AGE) %>%
  mutate(`SAGA without adjustment vs StockEff` = 100 * (`SAGA Shiny App` - StockEff)/StockEff,
         `SAGA without adjustment vs SAGA with adjustment` = 
           100 * (`SAGA Shiny App` - `SAGA Shiny App with swept area`)/`SAGA Shiny App with swept area`) %>%
  select(YEAR, SEASON, AGE, `SAGA without adjustment vs StockEff`, `SAGA without adjustment vs SAGA with adjustment`) %>%
  gather(comparison, `Percent difference`, -YEAR, -SEASON, -AGE) %>%
  mutate(`Absolute percent difference` = abs(`Percent difference`))

ggplot(pc_diff_naa %>% filter(SEASON == "SPRING"), 
       aes(x = YEAR, y = `Absolute percent difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ggtitle("Spring NAA differences")

ggplot(pc_diff_naa %>% filter(SEASON == "FALL"), 
       aes(x = YEAR, y = `Absolute percent difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ggtitle("Fall NAA differences")


# Plot proportions-at-age
propaa <-
  both_naa %>%
  group_by(YEAR, SEASON, source) %>%
  mutate(PROP_AT_AGE = NO_AT_AGE/sum(NO_AT_AGE)) %>%
  select(YEAR, SEASON, source, AGE, PROP_AT_AGE) 

ggplot(propaa %>%
         filter(SEASON == "FALL") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))), 
       aes(x = AGE, y = PROP_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Proportion-at-age") +
  xlab("Age") +
  ggtitle("Fall proportion-at-age comparison")

ggplot(propaa %>%
         filter(SEASON == "SPRING") %>%
         mutate(AGE = as.numeric(substr(AGE,5,5))), 
       aes(x = AGE, y = PROP_AT_AGE, color = source)) +
  geom_line() +
  geom_point() +
  facet_wrap(~YEAR, scales = "free_y") +
  theme_bw() +
  ylab("Proportion-at-age") +
  xlab("Age") +
  ggtitle("Spring proportion-at-age comparison")


# Plot differences in prop-at-age
diff_propaa <-
  propaa %>%
  spread(source, PROP_AT_AGE) %>%
  mutate(`SAGA without adjustment vs StockEff` = `SAGA Shiny App` - StockEff,
         `SAGA without adjustment vs SAGA with adjustment` = `SAGA Shiny App` - `SAGA Shiny App with swept area`) %>%
  select(YEAR, SEASON, AGE, 
         `SAGA without adjustment vs StockEff`, `SAGA without adjustment vs SAGA with adjustment`) %>%
  gather(comparison, Difference, -YEAR, -SEASON, -AGE) %>%
  mutate(`Absolute difference` = abs(Difference))

ggplot(diff_propaa %>% filter(SEASON == "SPRING"), 
       aes(x = YEAR, y = `Absolute difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ylab("Absolute difference") +
  ggtitle("Spring proportion-at-age differences")

ggplot(diff_propaa %>% filter(SEASON == "FALL"), 
       aes(x = YEAR, y = `Absolute difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ylab("Absolute difference") +
  ggtitle("Fall proportion-at-age differences")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# DRH modification starts here
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Tim's suggestion to use Stockeff Proportion at age and SAGAr aggregate totals for testing 
#affect of swept area correction first, followed by 
#Tim's suggestion to apply the STOCKEFF ALK to SAGAr NAL below 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
set.start.Yr=2009
set.term.Yr<-2019  #Set this to a manual term year if you want one...
nages=9 #ages 1 - 9+
#species information
disp="ADIOS_SV_"
species<-164744 #Species_itis for GOM haddock
stock<-"GOM"
#This is the ADIOS! directory prefix - if it changes just make the change here..
#dir.ADIOS="file:///net/home0/pdy/pub/STOCKEFF/ADIOS/ADIOS_SV/website/webfiles/"
un="dhennen"
pw="$$dhe2###QWE"
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
require(tools)
require(RODBC)

channel1 <- odbcConnect("sole",un,pw)

select1<-paste0("select species_itis, stock_abbrev, year, season, "
                ," age, length, age_no, prop_at_age ")  
from1<-paste(" from STOCKEFF.v_sv_auto_alk_join_o ",sep="")
where1<-paste("where species_itis=",species
              ," and stock_abbrev='",stock,"'",sep="")
order1=paste0(" order by year, season, length")
qry<-paste(select1,from1,where1,order1,";",sep=" ")
dtSTKf<-RODBC::sqlQuery(channel1,qry,max=1000000)
str(dtSTKf)

maxAge=max(dtSTKf$AGE)
minAge=min(dtSTKf$AGE)
#This proportion at age is based on length (the proportion at age we want is the population 
#level proportion). 
#for any given length, the proportion at age is the number that are age y. 



#Now we just need to multiply the year specific proportion at age from this with the 
#abundance in each year from SAGAr

#Load the outputs from SAGAr [These are the ones without swept area]
load("compare_saga_to_stockeff/HADDOCK_FALL_2_96.RData")
#They look like this
str(SAGAr)
#Number is here 
str(SAGAr$Indices$Index$Num) 

#get the years
y=SAGAr$Indices$Index$Year
#make a little function to do the multiplication by year and season
GetNAA=function(y,Seas="FALL") {
  dtSTKf$PROP_AT_AGE[which(dtSTKf$YEAR==y & dtSTKf$SEASON==Seas)]*SAGAr$Indices$Index$Num[which(SAGAr$Indices$Index$Year==y)]
  }

dtSTKf$NAA=sapply(y,GetNAA)

























#________________________________________________________________________________________________________________________
# Begin Tim's second idea %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Get annual survey ALK from ADIOS - use the one with holes filled in (I hope).

select1<-paste0("select species_itis, stock_abbrev, year, season, "
                ," age, length, age_no, prop_at_age ")  
from1<-paste(" from STOCKEFF.v_sv_alk_o ",sep="")
where1<-paste("where species_itis=",species
              ," and stock_abbrev='",stock,"'",sep="")
order1=paste0(" order by year, season, length")
qry<-paste(select1,from1,where1,order1,";",sep=" ")
dtSValk<-RODBC::sqlQuery(channel1,qry,max=1000000)
str(dtSValk)

maxAge=max(dtSValk$AGE)
minAge=min(dtSValk$AGE)
#Use year specific ALK to convert this to ages 
convertALK=function(y,Seas="FALL"){
  tmpALK=dtSValk[dtSValk$YEAR==y & dtSValk$SEASON==Seas,]
  #Make plus group
  tmpALK$AGE=ifelse(tmpALK$AGE>nages,nages,tmpALK$AGE)
  #form ALK matrix
  ALK=matrix(0,nrow=length(1:max(dtSValk$LENGTH)),ncol=length(0:nages))
  # #fill matrix (this one has 0's for some reason...)
  for(j in 1:nrow(tmpALK)) ALK[tmpALK$LENGTH[j],(tmpALK$AGE[j]+1)]=ALK[tmpALK$LENGTH[j],(tmpALK$AGE[j]+1)]+1
  # #Now convert it to proportions at length
  for(j in 1:nrow(ALK)) if(sum(ALK[j,])>0) ALK[j,]=ALK[j,]/sum(ALK[j,])
  return(ALK)
}

#convertALK(y=2019,Seas="FALL")
y=(set.start.Yr:set.term.Yr)
StockEffALK=lapply(y,convertALK)



#Load the outputs from SAGAr [These are the ones without swept area]
load("compare_saga_to_stockeff/HADDOCK_FALL_2_96.RData")
#They look like this
str(SAGAr)
#Number at length is here 
str(SAGAr$Indices$NatLength) #Note this starts at 2 and goes to 96cm - I'm going to cut it off at 96 for both

#Now get the numbers at age by combining these 
ApplyALKtoNAL=function(SAGAr){
  NAAhat=c()
  nyrs=set.term.Yr-set.start.Yr+1
  for(i in 1:nyrs) NAAhat=rbind(NAAhat,as.data.frame(as.matrix(SAGAr$Indices$NatLength[i,3:ncol(SAGAr$Indices$NatLength)])%*%StockEffALK[[i]][2:97,]))
  #should give numbers at age 
  names(NAAhat)=paste0("AGE",0:9)
  NAAhat$YEAR=paste(set.start.Yr:set.term.Yr)
  return(NAAhat)
}


NAAhatFall=ApplyALKtoNAL(SAGAr)
#Get this into a form that will fit with Charles' tidy stuff
STKalkSAGArNAL_naa_FALL <-
  NAAhatFall %>%
  gather(AGE, NO_AT_AGE, -YEAR) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  mutate(AGE = as.character(AGE)) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "FALL",
         source = "StockeffALK+SAGArNAL")


#Now repeat with the swept area version
load("compare_saga_to_stockeff/HADDOCK_FALL_2_96ws.RData")
NAAhatFallws=ApplyALKtoNAL(SAGAr)
STKalkSAGArNAL_naa_FALLws <-
  NAAhatFallws %>%
  gather(AGE, NO_AT_AGE, -YEAR) %>%
  mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
  mutate(AGE = as.character(AGE)) %>%
  filter(NO_AT_AGE > 0) %>%
  mutate(SEASON = "FALL",
         source = "StockeffALK+SAGArNAL with Swept")

#check names
names(stockeff_naa)
names(STKalkSAGArNAL_naa_FALL)

#Stealing from Charles:
both_naa <- 
  rbind(stockeff_naa, STKalkSAGArNAL_naa_FALL, STKalkSAGArNAL_naa_FALLws) %>%
  mutate(AGE = paste0("Age-", AGE)) %>%
  filter(YEAR >= 2009 & SEASON=="FALL")


# Plot percent differences in indices
pc_diff_naa <-
  both_naa %>%
  spread(source, NO_AT_AGE) %>%
  mutate(`SAGA without adjustment vs StockEff` = 100 * (`StockeffALK+SAGArNAL` - StockEff)/StockEff,
         `SAGA without adjustment vs SAGA with adjustment` = 
           100 * (`StockeffALK+SAGArNAL` - `StockeffALK+SAGArNAL with Swept`)/`StockeffALK+SAGArNAL with Swept`) %>%
  select(YEAR, SEASON, AGE, `SAGA without adjustment vs StockEff`, `SAGA without adjustment vs SAGA with adjustment`) %>%
  gather(comparison, `Percent difference`, -YEAR, -SEASON, -AGE) %>%
  mutate(`Absolute percent difference` = abs(`Percent difference`))

ggplot(pc_diff_naa %>% filter(SEASON == "SPRING"), 
       aes(x = YEAR, y = `Absolute percent difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ggtitle("Spring NAA differences")

ggplot(pc_diff_naa %>% filter(SEASON == "FALL"), 
       aes(x = YEAR, y = `Absolute percent difference`, color = comparison)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ AGE) +
  theme_bw() +
  ggtitle("Fall NAA differences")

















