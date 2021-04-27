#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
source("TestHelper.R")
require(RODBC)
Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
un="dhennen"
pw="$$dhe2###QWE"
sole <- RODBC::odbcConnect(dsn="sole", uid=un, pwd=pw, believeNRows=FALSE)
#Get species specific specs from STOCKEFF
q.list <- paste0("select species_itis, stock_abbrev, season, sv_start_year, sv_end_year, sex_type, sa_year "
                 ,"from STOCKEFF.I_SV_STOCK_SURVEY_S where purpose_code='10'; ")
all.spp <- RODBC::sqlQuery(sole,q.list)
dim(all.spp)
all.spp = all.spp[all.spp$SEASON%in%c("FALL","SPRING"),] #SAGAr does not do winter survey or MADMF so far
sppTab <- read.csv('files/speciesTable.csv')
all.spp = all.spp[all.spp$SPECIES_ITIS%in%sppTab$SPECIES_ITIS,] #make sure SAGAr has this stock

#file for error reports
if(!file.exists("TestHelper")) system(paste0(" mkdir -p TestHelper;"))
error.out=paste0("TestHelper/","errorOut.txt")
cat(system.time(),file=error.out,append = F) #clear file

for(i in 1:nrow(all.spp)){
#for(i in 101:235){
  # HadTest=which(all.spp$SPECIES_ITIS==sppTab$SPECIES_ITIS[which(sppTab$COMNAME=="HADDOCK")] &
  #           all.spp$STOCK_ABBREV=="GOM") #Can be a lot of different specs!
  # all.spp[HadTest,]
  # # i=160

  # AMPLA=which(all.spp$SPECIES_ITIS==sppTab$SPECIES_ITIS[which(sppTab$COMNAME=="AMERICAN PLAICE")] &
  #          all.spp$STOCK_ABBREV=="UNIT" )
  # all.spp[AMPLA,] #check to see which SA year you want to use
  # i = 231 # 232
  
   # BUTT=which(all.spp$SPECIES_ITIS==sppTab$SPECIES_ITIS[which(sppTab$COMNAME=="BUTTERFISH")] &
   #          all.spp$STOCK_ABBREV=="UNIT" )
   # all.spp[BUTT,] #check to see which SA year you want to use
  # i = 141
  
  year = c(ifelse(all.spp$SV_START_YEAR[i]<1968,1968,all.spp$SV_START_YEAR[i]):ifelse(is.na(all.spp$SV_END_YEAR[i])
                                        ,(as.integer(substr(date(),nchar(date())-4,nchar(date())))-1)
                                          , all.spp$SV_END_YEAR[i]))
  speciesItis = all.spp$SPECIES_ITIS[i]
  stock = all.spp$STOCK_ABBREV[i]
  season = all.spp$SEASON[i]
  sex=all.spp$SEX_TYPE[i]
  #Print the name of the stock 
  print(paste(gsub(" ","_",sppTab$COMNAME[which(sppTab$SPECIES_ITIS==speciesItis)]),speciesItis,stock,sex,season,paste(range(year),collapse="_"),sep="_"))
  t1=try(TestSAGAr(year,speciesItis,stock,season,sex,un,pw))
  #write to file if errors occur
  if(!is.null(t1)) if(attr(t1,"class")=="try-error") cat(paste("SAGAr ERROR in "
            ,i,sppTab$COMNAME[which(sppTab$SPECIES_ITIS==all.spp$SPECIES_ITIS[i])]
            ,speciesItis,stock,season,"  ",t1),file=error.out,append = T)
}  
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$