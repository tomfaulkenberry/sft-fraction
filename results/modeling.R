library(tidyverse)
# subject 1

rawdata = read_csv("~/github/sft-fraction/results/data/subject-1a.csv")

# clean data file
data = rawdata %>%
  select(correct, denomSalience, numSalience, numerator, denominator, response_time, subject_nr) %>%
  filter(correct==1) %>%
  mutate(rt = response_time) %>%
  filter(rt > 200 & rt < mean(rt)+3*sd(rt))

# look at selective influence
data %>%
  filter(numerator > 5 & denominator > 5) %>%
  group_by(denomSalience, numSalience) %>%
  summarize(mRT = mean(rt), sdRT = sd(rt))


data %>%
  ggplot(aes(x=rt))+
  geom_density()
  

# plot survivor functions

hh = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="high" & denomSalience == "high") %>%
  select(rt)

hl = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="high" & denomSalience == "low") %>%
  select(rt)
  
lh = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="low" & denomSalience == "high") %>%
  select(rt)

ll = data %>%
  filter(numerator > 5 & denominator > 5) %>%
  filter(numSalience=="low" & denomSalience == "low") %>%
  select(rt)
  
rts = sort(unique(c(hh$rt, hl$rt, lh$rt, ll$rt)))
hh.ecdf = ecdf(hh$rt)
hl.ecdf = ecdf(hl$rt)
lh.ecdf = ecdf(lh$rt)
ll.ecdf = ecdf(ll$rt)

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
legend(x=800, y=0.8, legend=c("HH","HL","LH","LL"),lty=c(1,3,1,3), lwd=c(2,2,1,1))


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
  