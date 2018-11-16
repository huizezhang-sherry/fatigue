library(cobs)
# fit a constrained B-spline model with specified increasing slope by match_num
fit_cbs_match=c()
for (i in levels(Federer$match_num)){
  cbs <-  cobs(y = Federer[(Federer$ServeNumber == 2)&(Federer$match_num==i),]$Speed_KMH, 
               x=Federer[(Federer$ServeNumber == 2)&(Federer$match_num==i),]$Count,
               constraint= "increase")
  for (j in Federer[(Federer$ServeNumber == 2)&(Federer$match_num==i),]$Count){
    fit_cbs_match[j]<-predict(cbs,z =j)[,2]
  } 
}

# fit a constrained B-spline model with specified increasing slope by SetNo
fit_cbs_set = c()
for (i in levels(as.factor(Federer$SetNo))){
  cbs <-  cobs(y = Federer[(Federer$ServeNumber == 2)&(Federer$SetNo==i),]$Speed_KMH, 
               x=Federer[(Federer$ServeNumber == 2)&(Federer$SetNo==i),]$Count,
               constraint= "increase")
  for (j in Federer[(Federer$ServeNumber == 2)&(Federer$SetNo==i),]$Count){
    fit_cbs_set[j]<-predict(cbs,z =j)[,2]
  } 
}

