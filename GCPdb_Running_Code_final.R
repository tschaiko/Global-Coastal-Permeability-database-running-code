
       ### Running Code GCPdb
       
       ###############################
       
       ##GCPM<-read.csv("PATH GCPdb.csv)
       
       ###A-Classes#######################################
       
       #A-Block 1 A_Perm_log100
       GCPM$A_Perm_log100<-GCPM$Glhymps_Ferr_Log100
       GCPM$A_Perm_log100[GCPM$A_Perm_log100=="0"]<-NA   # (corrected unexpected 0 numbers in glhymps database)
       
       #A-Block 2 A_Perm_log100_max
       GCPM$A_Perm_log100_max<-GCPM$A_Perm_log100+GCPM$Glympse_K_std_devX100
       
       #A-Block 3 A_Perm_log100_min
       GCPM$A_Perm_log100_min<-GCPM$A_Perm_log100-GCPM$Glympse_K_std_devX100
       
       #A Block 4 Compute the continental averages for a_log100
      
       GCPM$A_perm_temp_asia<-ifelse(GCPM$CONTINENT=="Asia",10^(GCPM$A_Perm_log100/100),NA)
       A_Asia_mean<-log10(median(GCPM$A_perm_temp_asia, na.rm=TRUE))*100
       A_Asia_mean<-round(A_Asia_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_asia))
       
       GCPM$A_perm_temp_europe<-ifelse(GCPM$CONTINENT=="Europe",10^(GCPM$A_Perm_log100/100),NA)
       A_Europe_mean<-log10(median(GCPM$A_perm_temp_europe, na.rm=TRUE))*100
       A_Europe_mean<-round(A_Europe_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_europe))
       
       GCPM$A_perm_temp_africa<-ifelse(GCPM$CONTINENT=="Africa",10^(GCPM$A_Perm_log100/100),NA)
       A_Africa_mean<-log10(median(GCPM$A_perm_temp_africa, na.rm=TRUE))*100
       A_Africa_mean<-round(A_Africa_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_africa))
       
       GCPM$A_perm_temp_NA<-ifelse(GCPM$CONTINENT=="North America",10^(GCPM$A_Perm_log100/100),NA)
       A_NA_mean<-log10(median(GCPM$A_perm_temp_NA, na.rm=TRUE))*100
       A_NA_mean<-round(A_NA_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_NA))
       
       GCPM$A_perm_temp_SA<-ifelse(GCPM$CONTINENT=="South America",10^(GCPM$A_Perm_log100/100),NA)
       A_SA_mean<-log10(median(GCPM$A_perm_temp_SA, na.rm=TRUE))*100
       A_SA_mean<-round(A_SA_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_SA))
       
       GCPM$A_perm_temp_australia<-ifelse(GCPM$CONTINENT=="Australia",10^(GCPM$A_Perm_log100/100),NA)
       A_Australia_mean<-log10(median(GCPM$A_perm_temp_australia, na.rm=TRUE))*100
       A_Australia_mean<-round(A_Australia_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_australia))
       
       GCPM$A_perm_temp_oceania<-ifelse(GCPM$CONTINENT=="Oceania",10^(GCPM$A_Perm_log100/100),NA)
       A_Oceania_mean<-log10(median(GCPM$A_perm_temp_oceania, na.rm=TRUE))*100
       A_Oceania_mean<-round(A_Oceania_mean,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_oceania))
       
       #A Block 5 Compute the continental averages for a_log100_max
       
       GCPM$A_perm_temp_asia_max<-ifelse(GCPM$CONTINENT=="Asia",10^(GCPM$A_Perm_log100_max/100),NA)
       A_Asia_mean_max<-log10(median(GCPM$A_perm_temp_asia_max, na.rm=TRUE))*100
       A_Asia_mean_max<-round(A_Asia_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_asia_max))
       
       GCPM$A_perm_temp_europe_max<-ifelse(GCPM$CONTINENT=="Europe",10^(GCPM$A_Perm_log100_max/100),NA)
       A_Europe_mean_max<-log10(median(GCPM$A_perm_temp_europe_max, na.rm=TRUE))*100
       A_Europe_mean_max<-round(A_Europe_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_europe_max))
       
       GCPM$A_perm_temp_africa_max<-ifelse(GCPM$CONTINENT=="Africa",10^(GCPM$A_Perm_log100_max/100),NA)
       A_Africa_mean_max<-log10(median(GCPM$A_perm_temp_africa_max, na.rm=TRUE))*100
       A_Africa_mean_max<-round(A_Africa_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_africa_max))
       
       GCPM$A_perm_temp_NA_max<-ifelse(GCPM$CONTINENT=="North America",10^(GCPM$A_Perm_log100_max/100),NA)
       A_NA_mean_max<-log10(median(GCPM$A_perm_temp_NA_max, na.rm=TRUE))*100
       A_NA_mean_max<-round(A_NA_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_NA_max))
       
       GCPM$A_perm_temp_SA_max<-ifelse(GCPM$CONTINENT=="South America",10^(GCPM$A_Perm_log100_max/100),NA)
       A_SA_mean_max<-log10(median(GCPM$A_perm_temp_SA_max, na.rm=TRUE))*100
       A_SA_mean_max<-round(A_SA_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_SA_max))
       
       GCPM$A_perm_temp_australia_max<-ifelse(GCPM$CONTINENT=="Australia",10^(GCPM$A_Perm_log100_max/100),NA)
       A_Australia_mean_max<-log10(median(GCPM$A_perm_temp_australia_max, na.rm=TRUE))*100
       A_Australia_mean_max<-round(A_Australia_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_australia_max))
       
       GCPM$A_perm_temp_oceania_max<-ifelse(GCPM$CONTINENT=="Oceania",10^(GCPM$A_Perm_log100_max/100),NA)
       A_Oceania_mean_max<-log10(median(GCPM$A_perm_temp_oceania_max, na.rm=TRUE))*100
       A_Oceania_mean_max<-round(A_Oceania_mean_max,0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_oceania_max))
       
       #A Block 6 Compute the continental averages for a_log100_min 
       
       GCPM$A_perm_temp_asia_min <- ifelse(GCPM$CONTINENT=="Asia", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_Asia_mean_min <- log10(median(GCPM$A_perm_temp_asia_min, na.rm=TRUE))*100
       A_Asia_mean_min <- round(A_Asia_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_asia_min))
       
       GCPM$A_perm_temp_europe_min <- ifelse(GCPM$CONTINENT=="Europe", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_Europe_mean_min <- log10(median(GCPM$A_perm_temp_europe_min, na.rm=TRUE))*100
       A_Europe_mean_min <- round(A_Europe_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_europe_min))
       
       GCPM$A_perm_temp_africa_min <- ifelse(GCPM$CONTINENT=="Africa", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_Africa_mean_min <- log10(median(GCPM$A_perm_temp_africa_min, na.rm=TRUE))*100
       A_Africa_mean_min <- round(A_Africa_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_africa_min))
       
       GCPM$A_perm_temp_NA_min <- ifelse(GCPM$CONTINENT=="North America", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_NA_mean_min <- log10(median(GCPM$A_perm_temp_NA_min, na.rm=TRUE))*100
       A_NA_mean_min <- round(A_NA_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_NA_min))
       
       GCPM$A_perm_temp_SA_min <- ifelse(GCPM$CONTINENT=="South America", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_SA_mean_min <- log10(median(GCPM$A_perm_temp_SA_min, na.rm=TRUE))*100
       A_SA_mean_min <- round(A_SA_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_SA_min))
       
       GCPM$A_perm_temp_australia_min <- ifelse(GCPM$CONTINENT=="Australia", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_Australia_mean_min <- log10(median(GCPM$A_perm_temp_australia_min, na.rm=TRUE))*100
       A_Australia_mean_min <- round(A_Australia_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_australia_min))
       
       GCPM$A_perm_temp_oceania_min <- ifelse(GCPM$CONTINENT=="Oceania", 10^(GCPM$A_Perm_log100_min/100), NA)
       A_Oceania_mean_min <- log10(median(GCPM$A_perm_temp_oceania_min, na.rm=TRUE))*100
       A_Oceania_mean_min <- round(A_Oceania_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(A_perm_temp_oceania_min))
       
       
       #A-Block 7 A_Perm_log100_avg missing values are filled with the averages by continents
       GCPM$A_Perm_log100_avg<-GCPM$A_Perm_log100
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="Asia",A_Asia_mean,GCPM$A_Perm_log100_avg)
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="Africa",A_Africa_mean,GCPM$A_Perm_log100_avg)
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="Australia",A_Australia_mean,GCPM$A_Perm_log100_avg)
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="Europe",A_Europe_mean,GCPM$A_Perm_log100_avg)
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="North America",A_NA_mean,GCPM$A_Perm_log100_avg)
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="South America",A_SA_mean,GCPM$A_Perm_log100_avg)
       GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg) & GCPM$CONTINENT=="Oceania",A_Oceania_mean,GCPM$A_Perm_log100_avg)
       #GCPM$A_Perm_log100_avg<-ifelse(is.na(GCPM$A_Perm_log100_avg),-1281,GCPM$A_Perm_log100_avg) #in case there is no CONTINENT label
       
       #A-Block 8 A_Perm_log100_avg_max missing values are filled with the averages by continents
       GCPM$A_Perm_log100_avg_max<-GCPM$A_Perm_log100_max
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="Asia",A_Asia_mean_max,GCPM$A_Perm_log100_avg_max)
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="Africa",A_Africa_mean_max,GCPM$A_Perm_log100_avg_max)
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="Australia",A_Australia_mean_max,GCPM$A_Perm_log100_avg_max)
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="Europe",A_Europe_mean_max,GCPM$A_Perm_log100_avg_max)
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="North America",A_NA_mean_max,GCPM$A_Perm_log100_avg_max)
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="South America",A_SA_mean_max,GCPM$A_Perm_log100_avg_max)
       GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max) & GCPM$CONTINENT=="Oceania",A_Oceania_mean_max,GCPM$A_Perm_log100_avg_max)
       #GCPM$A_Perm_log100_avg_max<-ifelse(is.na(GCPM$A_Perm_log100_avg_max),-879,GCPM$A_Perm_log100_avg_max) #in case there is no CONTINENT label
       
       #A-Block 9 A_Perm_log100_avg_min missing values are filled with the averages by continents 
       GCPM$A_Perm_log100_avg_min<-GCPM$A_Perm_log100_min
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="Asia",A_Asia_mean_min,GCPM$A_Perm_log100_avg_min)
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="Africa",A_Africa_mean_min,GCPM$A_Perm_log100_avg_min)
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="Australia",A_Australia_mean_min,GCPM$A_Perm_log100_avg_min)
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="Europe",A_Europe_mean_min,GCPM$A_Perm_log100_avg_min)
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="North America",A_NA_mean_min,GCPM$A_Perm_log100_avg_min)
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="South America",A_SA_mean_min,GCPM$A_Perm_log100_avg_min)
       GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min) & GCPM$CONTINENT=="Oceania",A_Oceania_mean_min,GCPM$A_Perm_log100_avg_min)
       #GCPM$A_Perm_log100_avg_min<-ifelse(is.na(GCPM$A_Perm_log100_avg_min),-1848,GCPM$A_Perm_log100_avg_min) #in case there is no CONTINENT label
       
       
       ###B-Classes#######################################
       
       
       #B-Block 1 Cliff_Young_sayre_Ocurrence - is set to 1 if young_cliff_percentage >= 50 % or Sayre Max_slope >= 57.3 (Means at least steeply sloping)
       GCPM$Young_Sayre_Cliff_Occurence<-ifelse(GCPM$Young_Cliff_Percentage>=50,1,NA)
       GCPM$Young_Sayre_Cliff_Occurence<-ifelse(is.na(GCPM$Young_Sayre_Cliff_Occurence),ifelse(GCPM$Sayre_Max_Slope>=57.3,1,GCPM$Young_Sayre_Cliff_Occurence),GCPM$Young_Sayre_Cliff_Occurence)
       GCPM$Young_Sayre_Cliff_Occurence<-ifelse(is.na(GCPM$Young_Sayre_Cliff_Occurence),0,GCPM$Young_Sayre_Cliff_Occurence)
       
       #B-Block 2 Building Beach Subclasses
       GCPM$B_Perm_class<-ifelse(GCPM$Luijendijk_Beach_Occ==1 & GCPM$Sayre_Waveheight<1,"B1",NA)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Luijendijk_Beach_Occ==1 & GCPM$Sayre_Waveheight>=1 & GCPM$Sayre_Waveheight<4,"B2",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class)& GCPM$Luijendijk_Beach_Occ==1 & GCPM$Sayre_Waveheight>=4,"B3",GCPM$B_Perm_class)
       
       #B-Block 3 running decision tree shoreline view Figure X Moosdorf et al. 2023
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Artificial_Occurence == 1,"P",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Spaldin_Mangrove_Occ == 1,"M2",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Mcowen_Saltmarsh_Occ == 1,"M1",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Alder_Estuary_Occ_2000m_Buffer == 1,"M1",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Lehner_river_Occ_500m_Buffer == 1,"M1",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Glim_unconsolidated_Occ == 1,"M1",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Young_Sayre_Cliff_Occurence == 1,"R",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Glim_consolidated_Occ == 1,"R",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class) & GCPM$Spalding_Coral_Occ == 1,"C",GCPM$B_Perm_class)
       GCPM$B_Perm_class<-ifelse(is.na(GCPM$B_Perm_class),"SU",GCPM$B_Perm_class)
       
       #B-Block 4 set B_Perm_log100 According to values assigned in Table X Moosdorf et al. 2023, Values for shoreline view
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="B1",-1100,NA)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="B2",-1070,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="B3",-1052,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="P",-2000,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="M1",-1230,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="M2",-1130,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="C",-992,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100<-ifelse(GCPM$B_Perm_class=="R",GCPM$Glhymps_Ferr_Log100,GCPM$B_Perm_log100)
       GCPM$B_Perm_log100[GCPM$B_Perm_log100=="0"]<-NA   # (corrected unexpected 0 numbers in glhymps database)
       
       #B-Block 5 set B_Perm_log100_max according to values assigned in Table X Moosdorf et al. 2023, Values for shoreline view. For rocky add standard variation
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="B1",-1100,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="B2",-1100,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="B3",-1100,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="P",-2000,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="M1",-1200,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="M2",-1100,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="R",GCPM$Glhymps_Ferr_Log100+GCPM$Glympse_K_std_devX100,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="C",-933,GCPM$A_Perm_log100_max)
       
       #B-Block 6 set B_Perm_log100_min according to values assigned in Table X Moosdorf et al. 2023, Values for shoreline view. for rocky subtract standard variation
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="B1",-1052,GCPM$A_Perm_log100_min)
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="B2",-1052,GCPM$A_Perm_log100_min)
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="B3",-1052,GCPM$A_Perm_log100_min)
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="P",-2000,GCPM$A_Perm_log100_min)
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="M1",-1300,GCPM$A_Perm_log100_min)
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="M2",-1200,GCPM$A_Perm_log100_min)
       GCPM$B_Perm_log100_max<-ifelse(GCPM$B_Perm_class=="R",GCPM$Glhymps_Ferr_Log100-GCPM$Glympse_K_std_devX100,GCPM$A_Perm_log100_max)
       GCPM$B_Perm_log100_min<-ifelse(GCPM$B_Perm_class=="C",-1054,GCPM$A_Perm_log100_min)
       
       #B-Block 7 Compute the continental averages for B_log100
       
       GCPM$B_Perm_temp_asia <- ifelse(GCPM$CONTINENT == "Asia", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_Asia_mean <- log10(median(GCPM$B_Perm_temp_asia, na.rm = TRUE)) * 100
       B_Asia_mean <- round(B_Asia_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_asia))
       
       GCPM$B_Perm_temp_europe <- ifelse(GCPM$CONTINENT == "Europe", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_Europe_mean <- log10(median(GCPM$B_Perm_temp_europe, na.rm = TRUE)) * 100
       B_Europe_mean <- round(B_Europe_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_europe))
       
       GCPM$B_Perm_temp_africa <- ifelse(GCPM$CONTINENT == "Africa", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_Africa_mean <- log10(median(GCPM$B_Perm_temp_africa, na.rm = TRUE)) * 100
       B_Africa_mean <- round(B_Africa_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_africa))
       
       GCPM$B_Perm_temp_NA <- ifelse(GCPM$CONTINENT == "North America", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_NA_mean <- log10(median(GCPM$B_Perm_temp_NA, na.rm = TRUE)) * 100
       B_NA_mean <- round(B_NA_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_NA))
       
       GCPM$B_Perm_temp_SA <- ifelse(GCPM$CONTINENT == "South America", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_SA_mean <- log10(median(GCPM$B_Perm_temp_SA, na.rm = TRUE)) * 100
       B_SA_mean <- round(B_SA_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_SA))
       
       GCPM$B_Perm_temp_australia <- ifelse(GCPM$CONTINENT == "Australia", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_Australia_mean <- log10(median(GCPM$B_Perm_temp_australia, na.rm = TRUE)) * 100
       B_Australia_mean <- round(B_Australia_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_australia))
       
       
       GCPM$B_Perm_temp_oceania <- ifelse(GCPM$CONTINENT == "Oceania", 10^(GCPM$B_Perm_log100 / 100), NA)
       B_Oceania_mean <- log10(median(GCPM$B_Perm_temp_oceania, na.rm = TRUE)) * 100
       B_Oceania_mean <- round(B_Oceania_mean, 0)
       GCPM <- subset(GCPM, select = -c(B_Perm_temp_oceania))
       
       #B-Block 8 Compute the continental averages for B_log100_max
       
       GCPM$B_perm_temp_asia_max <- ifelse(GCPM$CONTINENT == "Asia", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_Asia_mean_max <- log10(median(GCPM$B_perm_temp_asia_max, na.rm = TRUE))*100
       B_Asia_mean_max <- round(B_Asia_mean_max, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_asia_max))
       
       GCPM$B_perm_temp_europe_max <- ifelse(GCPM$CONTINENT == "Europe", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_Europe_mean_max <- log10(median(GCPM$B_perm_temp_europe_max, na.rm = TRUE))*100
       B_Europe_mean_max <- round(B_Europe_mean_max, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_europe_max))
       
       GCPM$B_perm_temp_africa_max <- ifelse(GCPM$CONTINENT == "Africa", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_Africa_mean_max <- log10(median(GCPM$B_perm_temp_africa_max, na.rm = TRUE))*100
       B_Africa_mean_max <- round(B_Africa_mean_max, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_africa_max))
       
       GCPM$B_perm_temp_NA_max <- ifelse(GCPM$CONTINENT == "North America", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_NA_mean_max <- log10(median(GCPM$B_perm_temp_NA_max, na.rm = TRUE))*100
       B_NA_mean_max <- round(B_NA_mean_max, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_NA_max))
       
       GCPM$B_perm_temp_SA_max <- ifelse(GCPM$CONTINENT == "South America", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_SA_mean_max <- log10(median(GCPM$B_perm_temp_SA_max, na.rm = TRUE))*100
       B_SA_mean_max <- round(B_SA_mean_max, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_SA_max))
       
       GCPM$B_perm_temp_australia_max <- ifelse(GCPM$CONTINENT == "Australia", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_Australia_mean_max <- log10(median(GCPM$B_perm_temp_australia_max, na.rm = TRUE))*100
       B_Australia_mean_max <- round(B_Australia_mean_max, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_australia_max))
       
       GCPM$B_perm_temp_oceania_max <- ifelse(GCPM$CONTINENT == "Oceania", 10^(GCPM$B_Perm_log100_max/100), NA)
       B_Oceania_mean_max <- log10(median(GCPM$B_perm_temp_oceania_max, na.rm = TRUE))*100
       B_Oceania_mean_max <- round(B_Oceania_mean_max)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_oceania_max))
       
       #B-Block 9 Compute the continental averages for B_log100_min
       
       GCPM$B_perm_temp_asia_min <- ifelse(GCPM$CONTINENT=="Asia", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_Asia_mean_min <- log10(median(GCPM$B_perm_temp_asia_min, na.rm=TRUE)) * 100
       B_Asia_mean_min <- round(B_Asia_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_asia_min))
       
       GCPM$B_perm_temp_europe_min <- ifelse(GCPM$CONTINENT=="Europe", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_Europe_mean_min <- log10(median(GCPM$B_perm_temp_europe_min, na.rm=TRUE)) * 100
       B_Europe_mean_min <- round(B_Europe_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_europe_min))
       
       GCPM$B_perm_temp_africa_min <- ifelse(GCPM$CONTINENT=="Africa", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_Africa_mean_min <- log10(median(GCPM$B_perm_temp_africa_min, na.rm=TRUE)) * 100
       B_Africa_mean_min <- round(B_Africa_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_africa_min))
       
       GCPM$B_perm_temp_NA_min <- ifelse(GCPM$CONTINENT=="North America", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_NA_mean_min <- log10(median(GCPM$B_perm_temp_NA_min, na.rm=TRUE)) * 100
       B_NA_mean_min <- round(B_NA_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_NA_min))
       
       GCPM$B_perm_temp_SA_min <- ifelse(GCPM$CONTINENT=="South America", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_SA_mean_min <- log10(median(GCPM$B_perm_temp_SA_min, na.rm=TRUE)) * 100
       B_SA_mean_min <- round(B_SA_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_SA_min))
       
       GCPM$B_perm_temp_australia_min <- ifelse(GCPM$CONTINENT=="Australia", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_Australia_mean_min <- log10(median(GCPM$B_perm_temp_australia_min, na.rm=TRUE)) * 100
       B_Australia_mean_min <- round(B_Australia_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_australia_min))
       
       GCPM$B_perm_temp_oceania_min <- ifelse(GCPM$CONTINENT=="Oceania", 10^(GCPM$B_Perm_log100_min/100), NA)
       B_Oceania_mean_min <- log10(median(GCPM$B_perm_temp_oceania_min, na.rm=TRUE)) * 100
       B_Oceania_mean_min <- round(B_Oceania_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(B_perm_temp_oceania_min))
       
       #B-Block 10 B_Perm_log100_avg missing values are filled with the averages by continents continents
       GCPM$B_Perm_log100_avg<-GCPM$B_Perm_log100
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="Asia",B_Asia_mean,GCPM$B_Perm_log100_avg)
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="Africa",B_Africa_mean,GCPM$B_Perm_log100_avg)
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="Australia",B_Australia_mean,GCPM$B_Perm_log100_avg)
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="Europe",B_Europe_mean,GCPM$B_Perm_log100_avg)
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="North America",B_NA_mean,GCPM$B_Perm_log100_avg)
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="South America",B_SA_mean,GCPM$B_Perm_log100_avg)
       GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100) & GCPM$CONTINENT=="Oceania",B_Oceania_mean,GCPM$B_Perm_log100_avg)
       #GCPM$B_Perm_log100_avg<-ifelse(is.na(GCPM$B_Perm_log100_avg),-1228,GCPM$B_Perm_log100_avg) #in case there is no CONTINENT label
       
       #B-Block 11 B_Perm_log100_avg_max missing values are filled with the averages by continents continents, segments not affiliated to a continent are assigned the general average value
       GCPM$B_Perm_log100_avg_max <- GCPM$B_Perm_log100_max
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "Asia", B_Asia_mean_max, GCPM$B_Perm_log100_avg_max)
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "Africa", B_Africa_mean_max, GCPM$B_Perm_log100_avg_max)
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "Australia", B_Australia_mean_max, GCPM$B_Perm_log100_avg_max)
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "Europe", B_Europe_mean_max, GCPM$B_Perm_log100_avg_max)
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "North America", B_NA_mean_max, GCPM$B_Perm_log100_avg_max)
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "South America", B_SA_mean_max, GCPM$B_Perm_log100_avg_max)
       GCPM$B_Perm_log100_avg_max <- ifelse(is.na(GCPM$B_Perm_log100_max) & GCPM$CONTINENT == "Oceania", B_Oceania_mean_max, GCPM$B_Perm_log100_avg_max)
       #GCPM$B_Perm_log100_avg_max<-ifelse(is.na(GCPM$B_Perm_log100_avg_max),-879,GCPM$B_Perm_log100_avg_max) #in case there is no CONTINENT label
       
       #B-Block 12 B_Perm_log100_avg_min missing values are filled with the averages by continents 
       GCPM$B_Perm_log100_avg_min <- GCPM$B_Perm_log100_min
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Asia",B_Asia_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Africa",B_Africa_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Australia",B_Australia_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Europe",B_Europe_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="North America",B_NA_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="South America",B_SA_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min <- ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Oceania",B_Oceania_mean_min,GCPM$B_Perm_log100_avg_min)
       #GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min),-1848,GCPM$B_Perm_log100_avg_min)#in case there is no CONTINENT label
       
       
       
       ###C-Classes#######################################
       
       #C-Block 1 Folk classes, first part of decision tree seeward, Figure X Moosdrof et al. 2023
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage>=0.8,"G",NA)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.8 & GCPM$dbS_Gravel_Percentage>=0.3 & GCPM$dbS_Mud_Percentage>GCPM$dbS_Sand_Percentage,"mG",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.8 & GCPM$dbS_Gravel_Percentage>=0.3 & GCPM$dbS_Sand_Percentage>=GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage*9>=GCPM$dbS_Sand_Percentage,"msG",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.8 & GCPM$dbS_Gravel_Percentage>=0.3 & GCPM$dbS_Sand_Percentage>=GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage*9<GCPM$dbS_Sand_Percentage,"sG",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.3 & GCPM$dbS_Gravel_Percentage>=0.05 & GCPM$dbS_Sand_Percentage<GCPM$dbS_Mud_Percentage,"gM",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.3 & GCPM$dbS_Gravel_Percentage>=0.05 & GCPM$dbS_Sand_Percentage>=GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage*9>=GCPM$dbS_Sand_Percentage ,"gmS",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.3 & GCPM$dbS_Gravel_Percentage>=0.05 & GCPM$dbS_Sand_Percentage>=GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage*9<GCPM$dbS_Sand_Percentage ,"gS",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.05 & GCPM$dbS_Sand_Percentage<GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage>GCPM$dbS_Sand_Percentage*9 ,"M",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.05 & GCPM$dbS_Sand_Percentage<GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage<=GCPM$dbS_Sand_Percentage*9 ,"sM",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.05 & GCPM$dbS_Sand_Percentage>GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage*9>GCPM$dbS_Sand_Percentage ,"mS",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(GCPM$dbS_Gravel_Percentage<0.05 & GCPM$dbS_Sand_Percentage>GCPM$dbS_Mud_Percentage & GCPM$dbS_Mud_Percentage>GCPM$dbS_Sand_Percentage*9 ,"S",GCPM$C_Perm_class)
       
       #C-Block 2 Running  rest of decision tree seeward, Figure X Moosdorf at al. 2023
       GCPM$C_Perm_class<-ifelse(is.na(GCPM$C_Perm_class) & GCPM$UNEP_Seagrass_Occ==1,"MS",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(is.na(GCPM$C_Perm_class) & GCPM$Spalding_Coral_Occ==1,"MC",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(is.na(GCPM$C_Perm_class) & GCPM$Lehner_river_Occ_2000m_Buffer==1,"MM",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(is.na(GCPM$C_Perm_class) & GCPM$Alder_Estuary_Occ_5000m_Buffer==1,"MM",GCPM$C_Perm_class)
       GCPM$C_Perm_class<-ifelse(is.na(GCPM$C_Perm_class),"MU",GCPM$C_Perm_class)
       
       #C-Block 3 set C_Perm_log100 according to values assigned in Table X Moosdorf et al. 2023, Values for seaward view.
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="G",-730,NA)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="mG",-1330,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="msG",-1041,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="sG",-947,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="gM",-1630,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="gmS",-1157,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="gS",-976,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="M",-1900,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="sM",-1630,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="mS",-1376,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="S",-1100,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="MS",-1376,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="MC",-992,GCPM$C_Perm_log100)
       GCPM$C_Perm_log100<-ifelse(GCPM$C_Perm_class=="MM",-1630,GCPM$C_Perm_log100)
       
       #C-Block 4 set C_Perm_log100_max according to values assigned in Table X Moosdorf et al. 2023, Values for seaward view.
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="G",-700,NA)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="mG",-1300,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="msG",-730,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="sG",-730,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="gM",-1157,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="gmS",-976,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="gS",-947,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="M",-1600,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="sM",-1400,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="mS",-1226,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="S",-900,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="MS",-1100,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="MC",-933,GCPM$C_Perm_log100_max)
       GCPM$C_Perm_log100_max<-ifelse(GCPM$C_Perm_class=="MM",-1400,GCPM$C_Perm_log100_max)
       
       #C-Block 4 set C_Perm_log100_min according to values assigned in Table X Moosdorf et al. 2023, Values for seaward view.
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="G",-1000,NA)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="mG",-1600,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="msG",-1330,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="sG",-1041,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="gM",-1900,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="gmS",-1630,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="gS",-1157,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="M",-1900,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="sM",-1900,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="mS",-1526,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="S",-1300,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="MS",-1526,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="MC",-1053,GCPM$C_Perm_log100_min)
       GCPM$C_Perm_log100_min<-ifelse(GCPM$C_Perm_class=="MM",-1900,GCPM$C_Perm_log100_min)
       
       #B-Block 5 Compute the continental averages for C_log100
       
       GCPM$C_Perm_temp_asia <- ifelse(GCPM$CONTINENT == "Asia", 10^(GCPM$C_Perm_log100 / 100), NA)
       C_Asia_mean <- log10(median(GCPM$C_Perm_temp_asia, na.rm = TRUE)) * 100
       C_Asia_mean <- round(C_Asia_mean, 0)
       GCPM <- subset(GCPM, select = -c(C_Perm_temp_asia))
       
       GCPM$C_Perm_temp_europe <- ifelse(GCPM$CONTINENT == "Europe", 10^(GCPM$C_Perm_log100 / 100), NA)
       C_Europe_mean <- log10(median(GCPM$C_Perm_temp_europe, na.rm = TRUE)) * 100
       C_Europe_mean <- round(C_Europe_mean, 0)
       GCPM <- subset(GCPM, select = -c(C_Perm_temp_europe))
       
       GCPM$C_Perm_temp_africa <- ifelse(GCPM$CONTINENT == "Africa", 10^(GCPM$C_Perm_log100 / 100), NA)
       C_Africa_mean <- log10(median(GCPM$C_Perm_temp_africa, na.rm = TRUE)) * 100
       C_Africa_mean <- round(C_Africa_mean, 0)
       GCPM <- subset(GCPM, select = -c(C_Perm_temp_africa))
       
       GCPM$C_Perm_temp_NA <- ifelse(GCPM$CONTINENT == "North America", 10^(GCPM$C_Perm_log100 / 100), NA)
       C_NA_mean <- log10(median(GCPM$C_Perm_temp_NA, na.rm = TRUE)) * 100
       C_NA_mean <- round(C_NA_mean, 0)
       GCPM <- subset(GCPM, select = -c(C_Perm_temp_NA))
       
       GCPM$C_Perm_temp_SA <- ifelse(GCPM$CONTINENT == "South America", 10^(GCPM$C_Perm_log100 / 100), NA)
       C_SA_mean <- log10(median(GCPM$C_Perm_temp_SA, na.rm = TRUE)) * 100
       C_SA_mean <- round(C_SA_mean, 0)
       GCPM <- subset(GCPM, select = -c(C_Perm_temp_SA))
       
       GCPM$C_Perm_temp_australia <- ifelse(GCPM$CONTINENT == "Australia", 10^(GCPM$C_Perm_log100 / 100), NA)
       C_Australia_mean <- log10(median(GCPM$C_Perm_temp_australia, na.rm = TRUE)) * 100
       C_Australia_mean <- round(C_Australia_mean, 0)
       GCPM <- subset(GCPM, select = -c(C_Perm_temp_australia))
       
       GCPM$C_perm_temp_oceania<-ifelse(GCPM$CONTINENT=="Oceania",10^(GCPM$C_Perm_log100/100),NA)
       C_Oceania_mean<-log10(median(GCPM$C_perm_temp_oceania, na.rm=TRUE))*100
       C_Oceania_mean<-round(C_Oceania_mean,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_oceania))
       
       
       #C-Block 6 Compute the continental averages for C_log100_max
       
       GCPM$C_perm_temp_asia_max <- ifelse(GCPM$CONTINENT=="Asia", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_Asia_mean_max <- log10(median(GCPM$C_perm_temp_asia_max, na.rm=TRUE))*100
       C_Asia_mean_max <- round(C_Asia_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_asia_max))
       
       GCPM$C_perm_temp_europe_max <- ifelse(GCPM$CONTINENT=="Europe", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_Europe_mean_max <- log10(median(GCPM$C_perm_temp_europe_max, na.rm=TRUE))*100
       C_Europe_mean_max <- round(C_Europe_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_europe_max))
       
       GCPM$C_perm_temp_africa_max <- ifelse(GCPM$CONTINENT=="Africa", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_Africa_mean_max <- log10(median(GCPM$C_perm_temp_africa_max, na.rm=TRUE))*100
       C_Africa_mean_max <- round(C_Africa_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_africa_max))
       
       GCPM$C_perm_temp_NA_max <- ifelse(GCPM$CONTINENT=="North America", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_NA_mean_max <- log10(median(GCPM$C_perm_temp_NA_max, na.rm=TRUE))*100
       C_NA_mean_max <- round(C_NA_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_NA_max))
       
       GCPM$C_perm_temp_SA_max <- ifelse(GCPM$CONTINENT=="South America", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_SA_mean_max <- log10(median(GCPM$C_perm_temp_SA_max, na.rm=TRUE))*100
       C_SA_mean_max <- round(C_SA_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_SA_max))
       
       GCPM$C_perm_temp_australia_max <- ifelse(GCPM$CONTINENT=="Australia", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_Australia_mean_max <- log10(median(GCPM$C_perm_temp_australia_max, na.rm=TRUE))*100
       C_Australia_mean_max <- round(C_Australia_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_australia_max))
       
       GCPM$C_perm_temp_oceania_max <- ifelse(GCPM$CONTINENT=="Oceania", 10^(GCPM$C_Perm_log100_max/100), NA)
       C_Oceania_mean_max <- log10(median(GCPM$C_perm_temp_oceania_max, na.rm=TRUE))*100
       C_Oceania_mean_max <- round(C_Oceania_mean_max,0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_oceania_max))
       
       #C-Block 7 Compute the continental averages for C_log100_min
       
       GCPM$C_perm_temp_asia_min <- ifelse(GCPM$CONTINENT=="Asia", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_Asia_mean_min <- log10(median(GCPM$C_perm_temp_asia_min, na.rm=TRUE)) * 100
       C_Asia_mean_min <- round(C_Asia_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_asia_min))
       
       GCPM$C_perm_temp_europe_min <- ifelse(GCPM$CONTINENT=="Europe", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_Europe_mean_min <- log10(median(GCPM$C_perm_temp_europe_min, na.rm=TRUE)) * 100
       C_Europe_mean_min <- round(C_Europe_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_europe_min))
       
       GCPM$C_perm_temp_africa_min <- ifelse(GCPM$CONTINENT=="Africa", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_Africa_mean_min <- log10(median(GCPM$C_perm_temp_africa_min, na.rm=TRUE)) * 100
       C_Africa_mean_min <- round(C_Africa_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_africa_min))
       
       GCPM$C_perm_temp_NA_min <- ifelse(GCPM$CONTINENT=="North America", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_NA_mean_min <- log10(median(GCPM$C_perm_temp_NA_min, na.rm=TRUE)) * 100
       C_NA_mean_min <- round(C_NA_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_NA_min))
       
       GCPM$C_perm_temp_SA_min <- ifelse(GCPM$CONTINENT=="South America", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_SA_mean_min <- log10(median(GCPM$C_perm_temp_SA_min, na.rm=TRUE)) * 100
       C_SA_mean_min <- round(C_SA_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_SA_min))
       
       GCPM$C_perm_temp_australia_min <- ifelse(GCPM$CONTINENT=="Australia", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_Australia_mean_min <- log10(median(GCPM$C_perm_temp_australia_min, na.rm=TRUE)) * 100
       C_Australia_mean_min <- round(C_Australia_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_australia_min))
       
       GCPM$C_perm_temp_oceania_min <- ifelse(GCPM$CONTINENT=="Oceania", 10^(GCPM$C_Perm_log100_min/100), NA)
       C_Oceania_mean_min <- log10(median(GCPM$C_perm_temp_oceania_min, na.rm=TRUE)) * 100
       C_Oceania_mean_min <- round(C_Oceania_mean_min, 0)
       GCPM <- subset(GCPM, select = -c(C_perm_temp_oceania_min))
       
       #C-Block 8 C_Perm_log100_avg missing values are filled with the averages by continents 
       GCPM$C_Perm_log100_avg <- GCPM$C_Perm_log100
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="Asia", C_Asia_mean, GCPM$C_Perm_log100_avg)
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="Africa", C_Africa_mean, GCPM$C_Perm_log100_avg)
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="Australia", C_Australia_mean, GCPM$C_Perm_log100_avg)
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="Europe", C_Europe_mean, GCPM$C_Perm_log100_avg)
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="North America", C_NA_mean, GCPM$C_Perm_log100_avg)
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="South America", C_SA_mean, GCPM$C_Perm_log100_avg)
       GCPM$C_Perm_log100_avg <- ifelse(is.na(GCPM$C_Perm_log100) & GCPM$CONTINENT=="Oceania", C_Oceania_mean, GCPM$C_Perm_log100_avg)
       #GCPM$C_Perm_log100_avg<-ifelse(is.na(GCPM$C_Perm_log100_avg),-1447,GCPM$C_Perm_log100_avg) #in case there is no CONTINENT label
       
       #C-Block 10 C_Perm_log100_avg_max missing values are filled with the averages by continents 
       GCPM$C_Perm_log100_avg_max <- GCPM$C_Perm_log100_max
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="Asia", C_Asia_mean_max, GCPM$C_Perm_log100_avg_max)
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="Africa", C_Africa_mean_max, GCPM$C_Perm_log100_avg_max)
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="Australia", C_Australia_mean_max, GCPM$C_Perm_log100_avg_max)
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="Europe", C_Europe_mean_max, GCPM$C_Perm_log100_avg_max)
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="North America", C_NA_mean_max, GCPM$C_Perm_log100_avg_max)
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="South America", C_SA_mean_max, GCPM$C_Perm_log100_avg_max)
       GCPM$C_Perm_log100_avg_max <- ifelse(is.na(GCPM$C_Perm_log100_max) & GCPM$CONTINENT=="Oceania", C_Oceania_mean_max, GCPM$C_Perm_log100_avg_max)
       
       #C-Block 10 C_Perm_log100_avg_min missing values are filled with the averages by continents 
       GCPM$B_Perm_log100_avg_min<-GCPM$B_Perm_log100_min
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Asia",C_Asia_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Africa",C_Africa_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Australia",C_Australia_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Europe",C_Europe_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="North America",C_NA_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="South America",C_SA_mean_min,GCPM$B_Perm_log100_avg_min)
       GCPM$B_Perm_log100_avg_min<-ifelse(is.na(GCPM$B_Perm_log100_avg_min) & GCPM$CONTINENT=="Oceania",C_Oceania_mean_min,GCPM$B_Perm_log100_avg_min)
       
       #TADAA
       
       