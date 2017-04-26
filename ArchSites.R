library(XML)
library(dplyr)
library(leaflet)
library(htmltools)
kml.text <- readLines("http://www.archaeological.org/Archaeology%20of%20America%20and%20Canada.kml")  
list<-xmlToList(kml.text)
raw<-unlist(list)
raw.df<-data.frame(var=as.character(attr(raw,'')),data=as.character(raw),stringsAsFactors = F)
names<-subset(raw.df,var=='Document.Folder.Folder.Placemark.name')
#clean coords
coords<-subset(raw.df,var=='Document.Folder.Folder.Placemark.Point.coordinates')
coords$long<-as.numeric(lapply(strsplit(coords$data,','),"[",1))
coords$lat<-as.numeric(lapply(strsplit(coords$data,','),"[",2))
data<-data.frame(place=names$data,long=coords$long,lat=coords$lat,stringsAsFactors = F)
desc<-subset(raw.df,var=='Document.Folder.Folder.Placemark.ExtendedData.Data.value')

#clean desc
not<-grep('/|_',desc$data,value=T)
final<-desc[!desc$data %in% not,]
final$len<-sapply(gregexpr("\\W+", final$data), length) + 1
final<-subset(final,len >7)
#image
imgs<-subset(these,var=='Document.Folder.Folder.Placemark.ExtendedData.Data.value')
noti<-grep('wikipedia',imgs$data,value=T)
imgs<-desc[imgs$data %in% noti,]

agg<-NULL
for(i in 1:nrow(data)){
  id<-data$place[i]
  temp.desc<-grep(id,final$data,value=T)
  urls<-strsplit(tolower(imgs$data),'/')
  img<-grep(id,imgs$data,value = T,fixed = T)
  len<-length(temp.desc)>0
  leni<-length(img)>0
  temp.desc<-ifelse(len==T,temp.desc,'NA')
  img<-ifelse(leni,img,'NA')
  found<-data.frame(place=id,desc=temp.desc,img=img,stringsAsFactors = F)
  agg<-rbind(found,agg)
}
all<-merge(data,agg,by=c('place'))
all<-all[!duplicated(all$place),]
all$color<-ifelse(all$desc=='NA','#444444','#84bc5b')
all$info<-ifelse(all$desc=='NA','Info Unavailable','Available Info')


mound<-subset(all,place=='Moundville')
leaflet(all) %>% addProviderTiles("CartoDB.Positron")%>%
  setView(lng = -99.29, lat = 41.1987, zoom = 4)%>%
  addCircleMarkers(~long, ~lat,radius=ifelse(all$desc=='NA',2,5),color=~color,popup = paste(sep="<br/>",ifelse(all$img!='NA',paste0("<b><a href=\"",all$img,'\ target=\"_blank\" ">',all$place,'</a>',"</b>"),paste("<b>",all$place,"</b>")),paste('(',all$long,',',all$lat,')'),all$desc),group=~info)%>%
  addPopups(mound$long, mound$lat, popup = paste(sep="<br/>",ifelse(mound$img!='NA',paste0("<b><a href=\"",mound$img,'\" target=\"_blank\" >',mound$place,'</a>',"</b>"),paste("<b>",mound$place,"</b>")),paste('(',mound$long,',',mound$lat,')'),mound$desc))%>%
  addLayersControl(
    overlayGroups = ~info,
    options = layersControlOptions(collapsed = FALSE)
  )
