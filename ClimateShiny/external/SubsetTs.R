SubsetTs<-function(Ts,YearSubset,RCPSubset,ProjSubset,MonthSubset){
if(missing(YearSubset)) YearSubset<-unique(Ts@Year)
if(missing(MonthSubset)) MonthSubset<-1:12

if(!missing(ProjSubset)){
 include<-Ts@Proj%in%ProjSubset
 Ts@Ts<-as.matrix(Ts@Ts[,include])
 Ts@Rcp<-Ts@Rcp[include]
 Ts@Proj<-Ts@Proj[include]
} 

ind<-(Ts@Year>=YearSubset[1] & Ts@Year<=YearSubset[2] & Ts@Month%in%as.vector(MonthSubset))
 Ts@Ts<-as.matrix(Ts@Ts[ind,])
 Ts@Month<-Ts@Month[ind]
 Ts@Time<-Ts@Time[ind]
 Ts@Year<-Ts@Year[ind]

if(!missing(RCPSubset)) browser()

return(Ts)
}