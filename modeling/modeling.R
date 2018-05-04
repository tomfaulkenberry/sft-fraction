library(tidyverse)

# define subject number
exp=1
subj=5

dataStringA = paste0("https://raw.githubusercontent.com/tomfaulkenberry/sft-fraction/master/exp", 
                      exp,
                      "/data/subject-", subj ,"a.csv")
dataStringB = paste0("https://raw.githubusercontent.com/tomfaulkenberry/sft-fraction/master/exp", 
                      exp,
                      "/data/subject-", subj ,"b.csv")

rawdata = rbind(read_csv(dataStringA), read_csv(dataStringB))
#rawdata = read_csv(dataStringA)

# clean data file
data = rawdata %>%
  select(correct, denomSalience, numSalience, numerator, denominator, response_time, subject_nr) %>%
  filter(correct==1) %>%
  mutate(rt = response_time) %>%
  filter(rt > 200 & rt < 1200)


data %>%
  ggplot(aes(x=rt))+
  geom_density()
  

# construct survivor functions

hh = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="high" & denomSalience == "high") %>%
  pull(rt)

hl = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="high" & denomSalience == "low") %>%
  pull(rt)
  
lh = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="low" & denomSalience == "high") %>%
  pull(rt)

ll = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="low" & denomSalience == "low") %>%
  pull(rt)
  
rts = sort(unique(c(hh, hl, lh, ll)))
hh.ecdf = ecdf(hh)
hl.ecdf = ecdf(hl)
lh.ecdf = ecdf(lh)
ll.ecdf = ecdf(ll)


# plot survivor functions
par(cex.main = 1.3, cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
plot(rts, rep(1,length(rts))-hh.ecdf(rts), type="l", lwd=2, lty=1,
     ylab="", xlab="", axes=F,
     main = "")
lines(rts, rep(1,length(rts))-hl.ecdf(rts), lwd=2, lty=3 )
lines(rts, rep(1,length(rts))-lh.ecdf(rts), lwd=1, lty=1 )
lines(rts, rep(1,length(rts))-ll.ecdf(rts), lwd=1, lty=3)
axis(1)
mtext("RT (msec)", side = 1, line = 3, cex = 1.3, font = 2)
axis(2)
par(las = 0)
mtext("Probability", side = 2, line = 2.8, cex = 1.3, font = 2)
legend(x=600, y=0.8, legend=c("HH","HL","LH","LL"),lty=c(1,3,1,3), lwd=c(2,2,1,1))


# test selective influence
# first, just get a quick look
data %>%
  filter(numerator > 5 & denominator > 5) %>%
  group_by(denomSalience, numSalience) %>%
  summarize(mRT = mean(rt), sdRT = sd(rt))

# code below from Joe Houpt's R functions
# note: alternative = "less" because ks.test works with CDF, not survivor function

siDominance = function(HH, HL, LH, LL){
  ll.hh = suppressWarnings(ks.test(LL,HH,alternative="less",exact=FALSE))
  ll.hl = suppressWarnings(ks.test(LL,HL,alternative="less",exact=FALSE))
  ll.lh = suppressWarnings(ks.test(LL,LH,alternative="less",exact=FALSE))
  lh.hh = suppressWarnings(ks.test(LH,HH,alternative="less",exact=FALSE))
  hl.hh = suppressWarnings(ks.test(HL,HH,alternative="less",exact=FALSE))
  

  dominance = as.data.frame(
    list(Test=c("S.ll > S.hh",
                "S.ll > S.hl",
                "S.ll > S.lh",
                "S.lh > S.hh",
                "S.hl > S.hh"),
                
          statistic=c(ll.hh$statistic, 
                      ll.hl$statistic,
                      ll.lh$statistic,
                      lh.hh$statistic,
                      hl.hh$statistic),
         
          p.value=c(ll.hh$p.value, 
                    ll.hl$p.value,
                    ll.lh$p.value,
                    lh.hh$p.value,
                    hl.hh$p.value)
         ) 
    )
  return(dominance)
}

siDominance(hh,hl,lh,ll)

# plot SIC curve
sic = lh.ecdf(rts) + hl.ecdf(rts) - hh.ecdf(rts) - ll.ecdf(rts)

## compute Houpt-Townsend KS statistic
m = 1/(1/length(hh) + 1/length(hl) + 1/length(lh) + 1/length(ll))
Dplus = max(0,sic)
p.Dplus = exp(-2*m*Dplus^2)
Dminus = abs(min(0,sic))
p.Dminus = exp(-2*m*Dminus^2)

par(cex.main = 1.3, cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
plot(rts, sic, type="l", lwd=1,
     ylim=c(-0.4,0.4),
     xlab="", ylab="", axes=F)
abline(h=0, lty=2)
axis(1)
mtext("RT (msec)", side = 1, line = 3, cex = 1.3, font = 2)
axis(2)
par(las = 0)
mtext("SIC", side = 2, line = 2.8, cex = 1.3, font = 2)
text(600,0.4,sprintf("D+ = %.3f, p = %.3f", Dplus, p.Dplus), pos=4)
text(600,0.3,sprintf("D- = %.3f, p = %.3f", Dminus, p.Dminus), pos=4)



###################
# capacity 

# construct hazard functions

AB = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  pull(rt)

A = data %>%
  filter(numerator > 5 & denominator < 5) %>%
  pull(rt)

B = data %>%
  filter(numerator < 5 & denominator > 5) %>%
  pull(rt)

rts = sort(unique(c(AB, A, B)))
AB.ecdf = ecdf(AB)
A.ecdf = ecdf(A)
B.ecdf = ecdf(B)

capOr = log((1-A.ecdf(rts))*(1-B.ecdf(rts))/(1-AB.ecdf(rts)))

par(cex.main = 1.3, cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.2, bty = "n", las = 1)
plot(rts, capOr, type="l", lwd=1,
     ylim=c(-10,10),
     xlab="", ylab="", axes=F)
abline(h=0, lty=2)
axis(1)
mtext("RT (msec)", side = 1, line = 3, cex = 1.3, font = 2)
axis(2)
par(las = 0)
