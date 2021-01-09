
# ---------------------------------------
#library a bunch of packages we may (or may not) use - install them first if not installed already. 
# ---------------------------------------

library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(knitr)
library(pls)
library(spatialreg)
library(AlphaPart)
library(PerformanceAnalytics)

# ---------------------------------------
#data standardize
# ---------------------------------------

#read originaldata
Original_data <- read_csv("E:/UCL/0005/final/data/Finaldata.csv", 
                              na = c("", "NA", "n/a"),
                              col_names = TRUE, 
                              locale = locale(encoding = 'Latin1'))

head(Original_data)

#standardize data
Standard_data<-scale(Original_data[6:20],center=F,scale=T)
head(Standard_data)

#change into dataframe
regression_data <- data.frame(Standard_data)

#check normalization
ggplot(regression_data, aes(x=Sample_Count_Pointsdensity)) + 
  geom_histogram()

ggplot(regression_data, aes(Sample_Count_Pointsdensity)) + 
  geom_point()

symbox(~Sample_Count_Pointsdensity, 
       Standard_data, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

ggplot(regression_data, aes(x=Sample_Count_Pointsdensity^-1.5)) + 
  geom_histogram()

# ---------------------------------------
#multiple regression
# ---------------------------------------

#check variable
chart.Correlation(Standard_data, histogram=TRUE, pch=19)
cor(Standard_data)
pairs(Standard_data, panel = panel.smooth)

#build model
model1 <- lm(Sample_Count_Pointsdensity~Busstops_Count_Pointsdensity+ 
                Company_Count_Pointsdensity+ 
                Daily_Count_Pointsdensity+   
                Green_Count_Densitypoints+    
                Hospital_Count_Pointsdensity+  
                Restuarant_Count_Pointsdensity+
                Shops_Count_Pointsdensity+    
                Subway_Count_Pointsdensity+    
                Merged_pm25mean+             
                Merged_pm10mean+             
                Merged_no2mean+               
                Merged_so2mean+                
                Education_Count_Pointsdensity+
                Carroads_Roadensity,data=regression_data)

# print estimated coefficients in a tidy data frame
summary(model1)
Summary <- summary(model1)
BETA <- Summary$coefficients
kable(BETA, results = "asis",caption = "Results of Coefficient Estimation")

#delect variable and build model2
model2 <- lm(Sample_Count_Pointsdensity~Busstops_Count_Pointsdensity+ 
               Company_Count_Pointsdensity+ 
               Daily_Count_Pointsdensity+   
               Green_Count_Densitypoints+    
               Hospital_Count_Pointsdensity+  
               Restuarant_Count_Pointsdensity+
               Subway_Count_Pointsdensity+    
               Merged_pm25mean+             
               Merged_pm10mean+             
               Merged_no2mean+               
               Merged_so2mean+                
               Education_Count_Pointsdensity+
               Carroads_Roadensity,data=regression_data)
summary(model2)
vif(model2)

#delect variable and build model3
model3<- lm(Sample_Count_Pointsdensity~ 
              Company_Count_Pointsdensity+ 
              Daily_Count_Pointsdensity+   
              Green_Count_Densitypoints+    
              Hospital_Count_Pointsdensity+  
              Restuarant_Count_Pointsdensity+
              Subway_Count_Pointsdensity+    
              Merged_pm25mean+             
              Merged_pm10mean+             
              Merged_no2mean+               
              Merged_so2mean+                
              Education_Count_Pointsdensity+
              Carroads_Roadensity,data=regression_data)
summary(model3)
vif(model3)

#delect variable and build model4
model4<- lm(Sample_Count_Pointsdensity~ 
              Company_Count_Pointsdensity+ 
              Green_Count_Densitypoints+    
              Hospital_Count_Pointsdensity+  
              Restuarant_Count_Pointsdensity+
              Subway_Count_Pointsdensity+    
              Merged_pm25mean+             
              Merged_pm10mean+             
              Merged_no2mean+               
              Merged_so2mean+                
              Education_Count_Pointsdensity+
              Carroads_Roadensity,data=regression_data)
summary(model4)
vif(model4)

#delect variable and build model5
model5<- lm(Sample_Count_Pointsdensity~ 
              Company_Count_Pointsdensity+ 
              Green_Count_Densitypoints+    
              Hospital_Count_Pointsdensity+  
              Restuarant_Count_Pointsdensity+
              Subway_Count_Pointsdensity+    
              Merged_pm25mean+             
              Merged_pm10mean+             
              Merged_no2mean+               
              Merged_so2mean+                
              Carroads_Roadensity,data=regression_data)
summary(model5)
vif(model5)
plot(model5)
par(mfrow = c(2,2))
step(model5, direction = "both")

#delete variables that have no effect on dependent variables and build model5_1
model5_1<- lm(Sample_Count_Pointsdensity~ 
              Company_Count_Pointsdensity+ 
              Green_Count_Densitypoints+    
              Hospital_Count_Pointsdensity+  
              Restuarant_Count_Pointsdensity+
              Subway_Count_Pointsdensity+    
              Carroads_Roadensity,data=regression_data)
summary(model5_1)
vif(model5_1)
plot(model5)
par(mfrow = c(2,2))
step(model5_1, direction = "both")

#change data
Wuhanzones <-transform(Wuhanzones, Sample_Cou = as.numeric(Sample_Cou),
                       Company_Co = as.numeric(Company_Co),
                       Green_Coun = as.numeric(Green_Coun),
                       Hospital_C = as.numeric(Hospital_C),
                       Restuarant = as.numeric(Restuarant),
                       Subway_Cou = as.numeric(Subway_Cou),
                       Carroads_R = as.numeric(Carroads_R),
                       Busstops_C = as.numeric(Busstops_C),
                       Daily_Coun = as.numeric(Daily_Coun),
                       Shops_Coun = as.numeric(Shops_Coun),
                       Merged_pm2 = as.numeric(Merged_pm2),
                       Merged_pm1 = as.numeric(Merged_pm1))

#the same data with spatial information is used to get the model6
model6<- lm(Sample_Cou~ 
              Company_Co+ 
              Green_Coun+    
              Hospital_C+  
              Restuarant+
              Subway_Cou+    
              Carroads_R,
            data=Wuhanzones)
summary(model6)
glance(model6)
par(mfrow = c(2,2))
step(model6, direction = "both")
plot(model6)
tidy(model6)

#the model7 was optimized by AIC
model7<- lm(Sample_Cou~ 
              Company_Co+ 
              Hospital_C+  
              Restuarant,
            data=Wuhanzones)
summary(model7)
glance(model7)
par(mfrow = c(2,2))
step(model7, direction = "both")
vif(model7)
plot(model7)

#model7 residuals
model_data7 <- model7 %>%
  augment(., Wuhanzones)

model_data7 <- model7 %>%
  augment(., Wuhanzones2)

#also add them to the shapelayer
Wuhanzones <- Wuhanzones %>%
  mutate(model7resids = resid(model7))

Wuhanzones2 <- Wuhanzones2 %>%
  mutate(model7resids = resid(model7))

# ---------------------------------------
#try spatial model
# ---------------------------------------

#run durbin-watson test
DW <- durbinWatsonTest(model7)
tidy(DW)

#load the map
Wuhanzones <- 
  st_read(here::here("Analysis", "Finalwuhan2.shp"))
qtm(Wuhanzones)

Wuhanzones2 <- 
  st_read(here::here("Analysis", "Finalwuhan2.shp"))
qtm(Wuhanzones2)

#calculate the centroids of all Wards in London
coordsW <- Wuhanzones%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

#to generate a spatial weights matrix
Wuhanmap_nb <-Wuhanzones%>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

plot(Wuhanmap_nb, st_geometry(coordsW), col="red")
plot(LWard_knn, st_geometry(coordsW), col="blue")

#create a spatial weights matrix object from these weights
write.table(regression_data,file="c.txt",quote=F,col.name=T,row.names=F)

Lward.queens_weight <- Wuhanmap_nb %>%
  nb2listw(., style="C")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

#check residuals
Queen <- Wuhanzones %>%
  #st_drop_geometry()%>%
  dplyr::select(model7resids)%>%
  pull()%>%
  moran.test(., Lward.queens_weight)%>%
  tidy()

Nearest_neighbour <- Wuhanzones %>%
  #st_drop_geometry()%>%
  dplyr::select(model7resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Queen
Nearest_neighbour

qtm(Wuhanzones2, fill = "model7resids")

#try spatial model,though there is no need
model7_space <- lagsarlm(Sample_Cou~ 
                           Company_Co+ 
                           Hospital_C+  
                           Restuarant,
                           data=Wuhanzones, 
                                 nb2listw(Wuhanmap_nb, style="C"), 
                                 method = "eigen")

#what do the outputs show?
tidy(model7_space)
glance(model7_space)

qtm(Wuhanzones2, fill = "code")
plot(Wuhanzones2)