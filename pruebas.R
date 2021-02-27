df2<-df
head(df)
df2<-df[,3:14]
View(head(df2))
dim(df)
df2['CANTIDAD']=1
head(df2)
dim(df2)

str(df)
df$DATE..OF.OCCURRENCE<-timestamp(df$DATE..OF.OCCURRENCE,"%Y-%m-%d%H%M%S")
str(df)
head(df$DATE..OF.OCCURRENCE)
unique(df$GRAVITY_OF_CRIME)

dplyr::count(df$PRIMARY.DESCRIPTION)

casos<-length(df$GRAVITY_OF_CRIME)
str(medium)
medium<-df %>% dplyr::filter(GRAVITY_OF_CRIME=="MEDIUM") %>% count()
str(medium)
as.numeric(medium)
str(medium)
df %>% dplyr::filter(GRAVITY_OF_CRIME=="HIGH") %>% count()
df %>% dplyr::filter(GRAVITY_OF_CRIME=="LOW") %>% count()


TotalCrimes<-function(){
  
  casos<-length(df$GRAVITY_OF_CRIME)
  medium<-df%>% dplyr::filter(GRAVITY_OF_CRIME=="MEDIUM") %>% count()
  serious<-df %>% dplyr::filter(GRAVITY_OF_CRIME=="HIGH") %>% count()
  low<-df %>% dplyr::filter(GRAVITY_OF_CRIME=="LOW") %>% count()
  
  list(
    casos = casos,
    medium = medium,
    serious = serious,
    low=low
  )
  
}


datos_totales<-df
datos_totales['CANTIDAD']=1
datos_High<-datos_totales %>% dplyr::group_by(DATE..OF.OCCURRENCE,GRAVITY_OF_CRIME=="HIGH") %>%sum(CANTIDAD)
datos_Medium<-datos_totales%>% dplyr::group_by(DATE..OF.OCCURRENCE,GRAVITY_OF_CRIME)%>%summarise(total_casos=sum(CANTIDAD))
datos_Low<-datos_totales %>% dplyr::filter(GRAVITY_OF_CRIME=="LOW")

datos_Medium %>% dplyr::filter(datos_Medium$GRAVITY_OF_CRIME=='HIGH') %>% sum(total_casos)

suma=datos_Medium %>%dplyr::group_by(DATE..OF.OCCURRENCE,GRAVITY_OF_CRIME) %>%filter(GRAVITY_OF_CRIME=='HIGH') %>%
  summarise(total_casos)

SUMA=datos_Medium %>%filter(GRAVITY_OF_CRIME=='HIGH') %>%
  summarise(total_casos)

TotalCrimes()

library(ggplot2)
str(datos_Medium)

ggplot(datos_Medium)+
  geom_line(aes(x=DATE..OF.OCCURRENCE,y=sum(CANTIDAD)))+
  labs(title="Chicago Gravity of Crime by Date", 
       x="Gravity", y = "Count")

datos_TOTAL<-datos_totales%>% dplyr::group_by(GRAVITY_OF_CRIME)%>%summarise(total_casos=sum(CANTIDAD))


datos_bar<-df
datos_bar['CANTIDAD']=1
datos_bar<-datos_bar%>%
  dplyr::group_by(GRAVITY_OF_CRIME)%>%
  summarise(total_casos=sum(CANTIDAD))

high=datos_bar %>% dplyr::filter(GRAVITY_OF_CRIME=='HIGH')

ggplot(high, aes(x = GRAVITY_OF_CRIME, weight =total_casos,fill = GRAVITY_OF_CRIME)) + 
  geom_bar()+guides(fill=FALSE)+theme_classic()+scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac"))

datos_totales<-df
datos_totales['CANTIDAD']=1
datos_totales<-datos_totales%>%
  dplyr::group_by(DATE..OF.OCCURRENCE,GRAVITY_OF_CRIME)%>%
  summarise(total_casos=sum(CANTIDAD))

alto=datos_totales %>% dplyr::filter(GRAVITY_OF_CRIME=='HIGH')

ggplot(alto, aes(x = DATE..OF.OCCURRENCE, y =total_casos,fill = GRAVITY_OF_CRIME)) + 
  geom_line()+labs(title=paste0("Chicago Gravity of Crime "),
                                   x="Gravity", y = "Count")+guide('none')

library(leaflet)
map <- leaflet(width = 400, height = 200) %>% 
  addTiles() %>% 
  addMarkers(lng = -123.251,
             lat = 49.263, 
             popup = "You are here.")
map



datamap<-df
datamap['CANTIDAD']=1
datamap_High<-datamap %>% dplyr::group_by(LATITUDE,LONGITUDE,PRIMARY.DESCRIPTION,GRAVITY_OF_CRIME) %>% 
  summarise(total_casos=sum(CANTIDAD)) %>% filter(GRAVITY_OF_CRIME=="HIGH"&total_casos>=10)

datamap_High


map<- leaflet() %>%
  addProviderTiles("Stamen.Watercolor", group="background 1") %>% 
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>% 
  addCircleMarkers(data=datamap_High, lng=~LONGITUDE , lat=~LATITUDE, radius=8 ,
                   color="red", 
                   fillColor="red",
                   stroke = FALSE, 
                   fillOpacity = 0.2, 
                   group="Red",
                   label=~total_casos,
                   popup=~total_casos,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto'))

map


datamap_Medium<-datamap %>% dplyr::filter(GRAVITY_OF_CRIME=="MEDIUM")
datamnap_Low<-datamap %>% dplyr::filter(GRAVITY_OF_CRIME=="LOW")

install.packages("shinycssloaders")

