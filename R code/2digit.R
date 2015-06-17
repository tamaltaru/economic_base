####################################################################################
##                  Economic Base analysis, U.S &Kings county, CA     c        ####
##                  NAME- DEBMALYA SINHA   UW STUDENT NO- 1325990              ####
####################################################################################

# 2-digit code

NICS_2 <-read.csv("2_digit.csv",header=T,strip.white=T)

colnames(NICS_2)

# DATA CLEANUP

clean_1<-NICS_2[-which(NICS_2$OPTAX.id=="10" |
                         NICS_2$OPTAX.id=="20" | 
                         NICS_2$OPTAX.id=="T" | 
                         NICS_2$OPTAX.id=="Y"),]

df.us<-subset(clean_1,GEO.display.label=="United States",select=c(NAICS.id,EMP))
df.king<-subset(clean_1,GEO.display.label=="Kings County, California",select=c(NAICS.id,EMP))

colnames(df.us)[2]<-"US_EMP"
colnames(df.king)[2]<-"Kings_EMP"

combined<-merge(df.us,df.king,by="NAICS.id", all.x=T)

attach(combined)

summary(combined)
combined<-as.data.frame(sapply(combined,gsub,pattern="c",replacement="175"))

combined_numeric<-transform(combined,
                  NAICS.id=as.character(NAICS.id),
                  US_EMP=as.numeric(as.character(US_EMP)),
                  Kings_EMP=as.numeric(as.character(Kings_EMP)))

combined_numeric[9,2]<-6607511

summary(combined_numeric)


#AGRICULTURE DATA FROM COUNTY BUSINESS PARTNER

agri<-c(11,172105,678)

# COMBINING THE CLEAN DATASETS

clean_data<-rbind(agri,combined_numeric)

summary(clean_data)

# CALCULATION

attach(clean_data)

us_total<-sum(US_EMP, na.rm=T)
Kings_total<-sum(Kings_EMP, na.rm=T)

clean_data$Kings_e<-clean_data$Kings_EMP/Kings_total
clean_data$Kings_E<-clean_data$US_EMP/us_total
clean_data$Kings_LQ<-clean_data$Kings_e/clean_data$Kings_E
clean_data$Kings_bi<-((clean_data$Kings_LQ-1)/clean_data$Kings_LQ)*clean_data$Kings_EMP

attach(clean_data)
Kings_bi_sum<-sum(Kings_bi[Kings_bi>0], na.rm=T)
Kings_BM<-Kings_total/Kings_bi_sum
clean_data[2,8] <-"BASE MULTIPLIER" 
clean_data[3,8] <- Kings_BM

# CREATING CSV FILE FROM THE DATAFRAME

write.csv(clean_data, file="2digit_EconomicBase.csv")