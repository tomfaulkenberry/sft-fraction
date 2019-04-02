library(tidyverse)

# import raw data
setwd("~/github/sft-fraction/exp1/")
fileList = dir("data", full.name=TRUE)
rawdata = data.frame()
for (file in fileList){
  d = read_csv(file)
  d$sub = 100+as.numeric(substr(file,14,14))
  rawdata = rbind(rawdata,d)
}

# clean
data = rawdata %>%
  select(correct, numerator, denominator, response_time, sub) %>%
  filter(correct==1) %>%
  mutate(rt = response_time) %>%
  filter(rt > 200 & rt < 1200) 

targets = data %>%
  filter(numerator != 7 & numerator != 3) %>%
  filter(denominator != 7 & denominator != 3)

numSalience=numeric(length(targets$rt))
denomSalience=numeric(length(targets$rt))
for (i in 1:length(targets$rt)){
  if(abs(targets$numerator[i] - 5) == 1){
    numSalience[i] = "L"
    }
  else {
    numSalience[i] = "H"
  }
}

for (i in 1:length(targets$rt)){
  if(abs(targets$denominator[i] - 5) == 1){
    denomSalience[i] = "L"
  }
  else {
    denomSalience[i] = "H"
  }
}

targets$numSalience = numSalience
targets$denomSalience = denomSalience


###################
# plot MIC using functions from Thiele, Haaf, & Rouder's sysfac paper

mySE=function(ao,I)
{
  SS=ao[[2]][[1]][[2]]+ao[[3]][[1]][[2]]+ao[[4]][[1]][[2]]
  df=ao[[2]][[1]][[1]]+ao[[3]][[1]][[1]]+ao[[4]][[1]][[1]]
  se=sqrt((SS/df)/I)
  return(se)
}

dat = targets %>%
  group_by(numSalience, denomSalience, sub) %>%
  summarize(meanRT = mean(rt))

ao=summary(aov(meanRT~numSalience*denomSalience+Error(sub/(numSalience*denomSalience)),data=dat))
I=length(unique(dat$sub))
eb=mySE(ao,I)*qt(.975,I-1)
  

mrt=tapply(dat$meanRT,list(dat$denomSalience,dat$numSalience),mean)
matplot(1:2,t(mrt),
        axes=F,
        ylab="Response Time (sec)",
        lty=1:2,
        lwd=2,
        col=c('black','darkblue'),
        typ='l',
        xlim=c(.9,2.1),
        ylim=c(400,700),
        xlab="Change in Angle")

arrows(1:2,t(mrt)[,1]-eb,1:2,t(mrt)[,1]+eb,code=3,angle=90,length=.1)
arrows(1:2,t(mrt)[,2]-eb,1:2,t(mrt)[,2]+eb,code=3,angle=90,length=.1)
matpoints(1:2,t(mrt),pch=21,bg=c('white','blue'),col=1,cex=1.2)
axis(2)
  axis(1,at=1:2,labels=c("Low","High"))
  box()	
  m1=((mrt[1,1]-mrt[1,2])+(mrt[2,1]-mrt[2,2]))/2
  m2=((mrt[1,1]-mrt[2,1])+(mrt[1,2]-mrt[2,2]))/2

