####################################################################################
##                  EConomic base Analysis- U.S vs Kings County                 ####
##                  NAME- DEBMALYA SINHA UW STUDENT NO- 1325990                 ####
####################################################################################

# 4-digit code

NICS_4 <-read.csv("4_digit.csv",header=T,strip.white=T)

colnames(NICS_4)

# DATA CLEANUPS, AFTER CHECKING THE FILES

clean_1<-NICS_4[-which(NICS_4$OPTAX.id=="10" |
                         NICS_4$OPTAX.id=="20" | 
                         NICS_4$OPTAX.id=="T" | 
                         NICS_4$OPTAX.id=="Y"),]

df.us<-subset(clean_1,GEO.display.label=="United States",select=c(NAICS.id,EMP))
df.kings<-subset(clean_1,GEO.display.label=="Kings County, California",select=c(NAICS.id,EMP))

colnames(df.us)[2]<-"US_EMP"
colnames(df.kings)[2]<-"kings_EMP"

combined<-merge(df.us,df.kings,by="NAICS.id", all.x=T)

# AGRICLUTURAL DATA FROM COUNTRY BUSINESS PATTERN

agri_us<-read.csv("agri_us.csv",header=T,strip.white=T)
agri_kings<-read.csv("agri_kings.csv",header=T,strip.white=T)

agri_us$NAICS.code<-as.character(agri_us$NAICS.code)
agri_kings$NAICS.code<-as.character(agri_kings$NAICS.code)

agri_us<-subset(agri_us,nchar(NAICS.code)==4,select=c(1,3))
agri_kings<-subset(agri_kings,nchar(NAICS.code)==4,select=c(1,3))
agri_kings <- agri_kings[1:2,]
colnames(agri_us)[2]<-"US_EMP"
colnames(agri_kings)[2]<-"kings_EMP"

agri<-merge(agri_us,agri_kings,by="NAICS.code",all.x=TRUE)

colnames(agri)[1]<-"NAICS.id"

# COMBINING THE DATASETS

combined<-rbind(agri,combined)

summary(combined)

#INPLYING NUMBERS TO CHARACTERS AS DEFINED ( INSERTING THE AVERAGE)

attach(combined)

which(kings_EMP=="i")

combined<-as.data.frame(sapply(combined,gsub,pattern="i",
                               replacement=as.character((5000+9999)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="e",
                               replacement=as.character((250+499)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="j",
                               replacement=as.character((10000+24999)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="h",
                               replacement=as.character((2500+4999)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="g",
                               replacement=as.character((1000+2499)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="f",
                               replacement=as.character((500+999)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="c",
                               replacement=as.character((100+249)/2)))
combined<-as.data.frame(sapply(combined,gsub,pattern="b",
                               replacement=as.character((20+99)/2)))                    
combined<-as.data.frame(sapply(combined,gsub,pattern="m",
                               replacement=as.character(100000))) 

summary(combined)

clean_data<-transform(combined,
                  NAICS.id=as.character(NAICS.id),
                  US_EMP=as.numeric(as.character(US_EMP)),
                  kings_EMP=as.numeric(as.character(kings_EMP)))
clean_data[188,2]<-19062
summary(clean_data)


attach(clean_data)

# CALCULATION

us_total<-sum(US_EMP, na.rm=T)
kings_total<-sum(kings_EMP, na.rm=T)

clean_data$kings_e<-clean_data$kings_EMP/kings_total
clean_data$kings_E<-clean_data$US_EMP/us_total
clean_data$kings_LQ<-clean_data$kings_e/clean_data$kings_E
clean_data$kings_bi<-((clean_data$kings_LQ-1)/clean_data$kings_LQ)*clean_data$kings_EMP

attach(clean_data)
kings_bi_sum<-sum(kings_bi[kings_bi>0], na.rm=T)
kings_BM<-kings_total/kings_bi_sum
clean_data[2,8] <-"BASE MULTIPLIER" 
clean_data[3,8] <- kings_BM

# CREATING CSV FILE FROM THE DATAFRAME

write.csv(clean_data, file="4digit_EconomicBase.csv")