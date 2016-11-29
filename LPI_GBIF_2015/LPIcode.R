# Living Planet Index Code
# Critical Thinking - 31 January 2016
# Written by Isla Myers-Smith (isla.myers-smith@ed.ac.uk)

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Anne's function to output useful things from lm()s
output.lm<-function(x) c(intercept=summary(x)$coefficients[1,1], slope=summary(x)$coefficients[2,1], intercept_SE=summary(x)$coefficients[1,2], slope_SE=summary(x)$coefficients[2,2], intercept_p=summary(x)$coefficients[1,4], slope_p=summary(x)$coefficients[2,4])

# Load data
LPI <- read.csv(file.choose())
LPIGBIFkey <- read.csv(file.choose())

# Reshape data into long form
LPI2 <- gather(LPI, "year", "pop", 26:70)
LPI2$year <- extract_numeric(LPI2$year)

# Extract UK data
LPIUK <- filter(LPI2, Country.list == "United Kingdom")
head(LPIUK)

# Number of unique species
length(unique(LPIUK$Common.Name))

# Pipe to extract linear model fits to the population data for all species and populations with greater than 5 years of data
LPIUKsummary <- LPIUK %>% distinct(.) %>% filter(., is.finite(pop)) %>% mutate(., species = paste(Genus, Species, sep = ' ')) %>% group_by(.,Common.Name,species,id) %>% summarise(., maxyear = max(year), minyear = min(year)) %>% mutate(., lengthyear = maxyear-minyear)

LPIUKsummary <- merge(LPIUK, LPIUKsummary)

# Pipe to extract mean population size
LPIUKpops <- LPIUKsummary %>% distinct(.) %>% filter(., is.finite(pop)) %>% group_by(.,Common.Name,species,id,Units) %>% summarise(., meanpop = mean(pop)) %>% group_by(.,Common.Name,species,id) %>% summarise(., meanpop = mean(meanpop))

# Write object to a csv file
# write.table(LPIUKpops, file = "LPIUKpops.csv", sep = ",", col.names = NA)

LPIUKslopes <- LPIUKsummary %>% distinct(.) %>% filter(., is.finite(pop)) %>% filter(., lengthyear > 10) %>% group_by(.,Common.Name,species,id,Units) %>% mutate(scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>% filter(., is.finite(scalepop))%>% do(mod = lm(scalepop~year, data = .)) %>% mutate(n = df.residual(mod), intercept=summary(mod)$coeff[1], slope=summary(mod)$coeff[2], intercept_SE=summary(mod)$coeff[3], slope_SE=summary(mod)$coeff[4], intercept_p=summary(mod)$coeff[7], slope_p=summary(mod)$coeff[8]) %>% select(-mod) %>% filter(., n>5) %>% arrange(., slope)

# Write object to a csv file
# write.table(LPIUKslopes, file = "LPIUKslopes.csv", sep = ",", col.names = NA)

LPIUKslopeskey <- merge(LPIUKslopes, LPIGBIFkey)
LPIUKpopskey <- merge(LPIUKpops, LPIGBIFkey)
LPIUKall <- merge(LPIUKslopeskey, LPIUKpopskey)

# Load categorical data
GBIFcat <- read.csv(file.choose())

# merge LPIUK and GBIFcat
LPIGBIF <- merge(LPIUKall, GBIFcat)
LPIGBIF <- distinct(LPIGBIF)

LPIGBIF$geographic.range <- factor(LPIGBIF$geographic.range, levels = c("UK", "partEurope", "Europe", "partWorld", "World"))

# write.table(LPIGBIF, file = "LPIGBIF.csv", sep = ",", col.names = NA)

# Load GBIF data
GBIF <- read.csv(file.choose())

GBIFUK <- merge(GBIF, LPIGBIFkey, by.x = "species.LPI", by.y = "species")
LPIGBIFUK <- merge(LPIGBIF, GBIFUK)

# Write object to a csv file
# write.table(LPIGBIFUK, file = "LPIGBIFUK.csv", sep = ",", col.names = NA)

# Histogram of population sizes of the LPI data
ggplot(LPIGBIFUK, aes(x = meanpop)) + geom_histogram(bin=1000) +theme_bw() +ylab("Count\n")+xlab("Population Size (number of individuals")+theme(plot.title = element_text(size = 24),legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="plain"),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.5),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())

# Histogram of population change of the LPI data
ggplot(LPIGBIFUK, aes(x = slope)) + geom_histogram() +theme_bw() +ylab("Count\n")+xlab("Population Change (slopes)")+theme(plot.title = element_text(size = 24),legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="plain"),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.5),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())

# Comparison of range size estimations
ggplot(LPIGBIFUK, aes(x=geographic.range, y=range)) +
  geom_point() +geom_pointrange(aes(ymin = slope-slope_SE,ymax = slope+slope_SE), ) +geom_hline(yintercept=0, linetype="dashed") +theme_bw() +ylab("Geographic range (occurrences)\n")+xlab("Geographic range (occurrences)")+theme(plot.title = element_text(size = 24),legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="plain"),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.5),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())

# Range size versus population change
ggplot(LPIGBIFUK, aes(x=range, y=slope)) +
  geom_point() +geom_pointrange(aes(ymin = slope-slope_SE,ymax = slope+slope_SE), ) +geom_hline(yintercept=0, linetype="dashed") +theme_bw() +ylab("Population Change (slopes)\n")+xlab("Geographic range (occurrences)")+theme(plot.title = element_text(size = 24),legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="plain"),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.5),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())

# Local population size (logged) versus population change
ggplot(LPIGBIFUK, aes(x=log(meanpop), y=slope)) +
  geom_point() +geom_pointrange(aes(ymin = slope-slope_SE,ymax = slope+slope_SE), ) +geom_hline(yintercept=0, linetype="dashed") +theme_bw() +ylab("Population Change (slopes)\n")+xlab("Log population size (number of individuals)")+theme(plot.title = element_text(size = 24),legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="plain"),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.5),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())
