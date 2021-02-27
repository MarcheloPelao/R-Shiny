#CARGA DATOS
df<-read.csv("data/Chicago_crime_limpia.csv",header = TRUE,sep = ",")
df<-df[,2:14]
#TRANSFORMAR A FECHA
df$DATE..OF.OCCURRENCE<-as.Date(df$DATE..OF.OCCURRENCE)

