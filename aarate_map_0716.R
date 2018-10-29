install.packages("rgeos")
install.packages("maptools")
install.packages("gpclib")

library(rgeos)
library(maptools)
library(gpclib)  # may be needed, may not be

# MAP
setwd("E:/job/interm/melanoma/map/PA_Counties_clip.shp")
PA <- readShapeSpatial("PA_Counties_clip.shp")
# VERIFY IT LOADED PROPERLY
plot(PA)
PA$COUNTYFP
library(ggplot2)
PA_county<-fortify(PA,region="COUNTYFP")
PA_county$County<-recode(PA_county$id, '001'= "Adams",	'069'="Lackawanna",'003'= "Allegheny",'071'="Lancaster",'005'="Armstrong",'073'="Lawrence",
                                                 '007'="Beaver",'075'="Lebanon",'009'="Bedford",'077'="Lehigh",'011'="Berks",'079'="Luzerne",'013'="Blair",'081'="Lycoming",'015'="Bradford",	
                                                 '083'="McKean",'017'="Bucks",'085'="Mercer",'019'="Butler",'087'="Mifflin",'021'="Cambria",	'089'="Monroe",'023'="Cameron",	'091'="Montgomery",
                                                 '025'="Carbon",		'093'="Montour",'027'="Centre",'095'="Northampton",'029'="Chester",	'097'="Northumberland",'031'="Clarion",	'099'="Perry",
                                                 '033'="Clearfield",'101'="Philadelphia",'035'="Clinton",'103'="Pike",'037'="Columbia",'105'="Potter",'039'="Crawford", '107'="Schuylkill",
                                                 '041'="Cumberland", '109'="Snyder",'043'="Dauphin",'111'="Somerset",'045'="Delaware", '113'="Sullivan",'047'="Elk", '115'="Susquehanna",
                                                 '049'="Erie", '117'="Tioga",'051'="Fayette", '119'="Union",'053'="Forest", '121'="Venango",'055'="Franklin",'123'="Warren", '057'="Fulton",'125'="Washington",
                                                 '059'="Greene", '127'="Wayne", '061'="Huntingdon", '129'="Westmoreland",'063'="Indiana", '131'="Wyoming",'065'="Jefferson",'133'="York",'067'="Juniata")


setwd("E:/job/interm/melanoma/")
data<-read.csv("test.csv",header=TRUE)
#test tranfer county number 1 to 001 (same as COUNTYFP)
data$test<-ifelse(data$County %in% c(1,3,5,7,9),paste("00",data$County,sep=""),ifelse(data$County %in% c(11:99),paste("0",data$County,sep=""),as.character(data$County))) 
library(scales)
#for county label
distcenters <- ddply(PA_county, .(County), summarize, clat = mean(lat), clong = mean(long))
ggplot() + geom_map(data = data, aes(map_id = test, fill = adjusted.rate), 
                    map = PA_county) + expand_limits(x = PA_county$long , y = PA_county$lat)+ 
                 scale_fill_gradient2(low = muted("blue"), mid = "white", midpoint = 20, high = muted("red"), limits = c(0,40))+
          geom_text(data = distcenters, aes(x = clong, y = clat, label = County))

