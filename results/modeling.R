library(tidyverse)
# subject 1

rawdata = read_csv("~/github/sft-fraction/results/data/subject-1a.csv")

# clean data file
data = rawdata %>%
  select(correct, denomSalience, numSalience, numerator, denominator, response_time, subject_nr) %>%
  filter(correct==1) %>%
  mutate(rt = response_time) %>%
  filter(rt > 200 & rt < median(rt)+6*mad(rt))

# look at selective influence
data %>%
  filter(numerator > 5 & denominator > 5) %>%
  group_by(denomSalience, numSalience) %>%
  summarize(medRT = median(rt))


data %>%
  ggplot(aes(x=rt))+
  geom_density()
  
  