library("FITSio")
a<-readFrameFromFITS("D://cB58_Lyman_break.fit")
target<-a$FLUX

name = list.files("data") #names of .fit
loc = paste("./data/",name,sep='')
data.list= list()
setwd("D:/")

shift=c(NULL)
distance=c(NULL)

for (i in 1:100){

  temp=readFrameFromFITS(file=loc[i])
  for(j in 2:length(temp[,1])){
    if (temp[j,4]!=0) temp[j,1]=temp[j-1,1]
     } 
  std=scale(temp[,1],center=T,scale=T)  #only keep the standardized result for 1st col, i.e. flux
  st=std[,1]
  mark=0
  maxscore=-1

  for(k in 1:(length(st)-2180)){
    tempk<-st[k:(k+2180)]
    tempcor<-cor(target,tempk,method = c("pearson"))
    if (is.na(tempcor)) {
      mark=1
      maxscore=0
    }
    else if (tempcor>=maxscore){
      mark=k
      maxscore=tempcor
    }
  }
  shift=c(shift,mark)
  distance=c(distance,maxscore)
}
outcome<-data.frame(distance,name,shift)
write.csv(outcome,file = "hw2.csv")
outcome[order(outcome$distance),]
table=read.csv("D://hw2.csv")