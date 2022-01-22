# Project 1  
# Real estate project

setwd("C:/Users/Anupam/Desktop/Data")
getwd()
hd_train = read.csv("housing_train.csv", stringsAsFactors = F)
hd_train
hd_test=read.csv("housing_test.csv", stringsAsFactors = F)
hd_test

View(hd_train)
View(hd_test)
library(dplyr)
glimpse(hd_train)
dim(hd_train)
dim(hd_test)
hd_test$Price = NA
View(hd_test)
dim(hd_test)
hd_train$data ="train"
hd_test$data ="test"
dim(hd_train)
dim(hd_test)
glimpse(hd_train)
glimpse(hd_test)
hd_all = rbind(hd_train, hd_test)
View(hd_all)
glimpse(hd_all)


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

sapply(hd_all, function(x) is.character (x))

names(hd_all)[sapply(hd_all,function(x) is.character(x))]

hd_all$Address = NULL
glimpse(hd_all)

cat_cols = c("Suburb", "Type", "Method", "SellerG", "CouncilArea")

for(cat in cat_cols){
  hd_all = CreateDummies(hd_all,cat,100)
  
}
glimpse(hd_all)

#Missing values

sum(is.na(hd_all))
(is.na(hd_all))

for(col in names(hd_all)){
  
  if(sum(is.na (hd_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    hd_all[is.na(hd_all[,col]),col]=mean(hd_all[,col],na.rm=T)
  }
  
}
  
  sum(is.na(hd_all))
  

hd_train = hd_all[hd_all$data == "train", ]
table(hd_all$data)

hd_test = hd_all[hd_all$data == "test", ]
hd_train$data = NULL
hd_test$data = NULL

OR 

hd_train=hd_all %>% filter(data=="train") %>% select(-data)
dim(hd_train)
View(hd_train)

hd_test=hd_all %>% filter(data=="test") %>% select(-data,-Price)
View(hd_test)
dim(hd_test)

# Building model:

set.seed(2)
s=sample(1:nrow(hd_train),0.7*nrow(hd_train))
hd_train1=hd_train[s,]
hd_train2=hd_train[-s,]
glimpse(hd_train1)

library(car)

fit =lm(Price~.,data=hd_train1)
summary(fit)

library(car)
vif(fit)
summary(fit)
sort(vif(fit), decreasing = TRUE)[1:3]
vif(fit)

fit = step(fit)

summary(fit)
formula(fit)
fit = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
           Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
           Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
           Suburb_Balwyn + Suburb_Camberwell + Suburb_PortMelbourne + 
           Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
           Suburb_Brighton + Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + 
           Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + Type_u + 
           Type_h + Method_SP + Method_S + SellerG_Kay + SellerG_Miles + 
           SellerG_Greg + SellerG_RT + SellerG_Fletchers + SellerG_Biggin + 
           SellerG_Marshall + SellerG_hockingstuart + SellerG_Jellis + 
           CouncilArea_Whitehorse + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
           CouncilArea_Melbourne + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
           CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_Darebin + 
           CouncilArea_MooneeValley + CouncilArea_Boroondara + CouncilArea_
         , data = hd_train1)


fit = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         BuildingArea + YearBuilt + Suburb_Doncaster + 
           Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
           Suburb_Balwyn + Suburb_Camberwell + Suburb_PortMelbourne + 
           Suburb_BrightonEast + Suburb_Hawthorn + Suburb_BalwynNorth + 
           Suburb_Brighton + Suburb_Essendon + Suburb_SouthYarra + Suburb_StKilda + 
           Suburb_Preston + Suburb_Richmond + Suburb_Reservoir + Type_u + 
           Type_h + Method_SP + Method_S + SellerG_Kay + SellerG_Miles + 
           SellerG_Greg + SellerG_RT  + SellerG_Biggin + 
           SellerG_Marshall + SellerG_Jellis + 
           CouncilArea_Whitehorse  + CouncilArea_HobsonsBay + 
           CouncilArea_Melbourne + CouncilArea_Yarra  + 
           CouncilArea_Stonnington + CouncilArea_GlenEira + CouncilArea_Darebin + 
           CouncilArea_MooneeValley + CouncilArea_Boroondara + CouncilArea_
         , data = hd_train1)
summary(fit)


prediction = predict(fit, newdata = hd_train2)

rmse = sqrt(mean((hd_train2$Price - prediction)**2))

Score = 212467/rmse
Score = 0.5365

# Assumptions

plot(fit, which = 1)

plot(fit, which = 2)

plot(fit, which = 3)

plot(fit, which = 4)

# Final model

fit.final=lm(Price~. ,data=hd_train)
summary(fit.final)

vif(fit.final)
sort(vif(fit.final), decreasing = TRUE)[1:3]

fit.final=lm(Price~.-CouncilArea_ ,data=hd_train)
summary(fit.final)
sort(vif(fit.final), decreasing = TRUE)[1:3]


fit.final=step(fit.final)
formula(fit.final)
fit.final = lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
                 Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
                 Suburb_MalvernEast + Suburb_Camberwell + Suburb_PortMelbourne + 
                  Suburb_BrightonEast + Suburb_Hawthorn + 
                 Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
                 Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
                 Suburb_Reservoir + Type_u + Type_h  + Method_PI + 
                 Method_S + SellerG_Kay + SellerG_Miles  + 
                 SellerG_RT + SellerG_Biggin  + SellerG_Marshall + 
                 SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
                 CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
                 CouncilArea_PortPhillip   + 
                 CouncilArea_Stonnington + CouncilArea_Darebin + CouncilArea_Moreland + 
                 CouncilArea_Boroondara
               , data = hd_train)


summary(fit.final)

test.predictions = predict(fit.final, newdata = hd_test)

# For project submission

write.csv(test.predictions, "Anupam_Dua_P1_part2",row.names = FALSE)







