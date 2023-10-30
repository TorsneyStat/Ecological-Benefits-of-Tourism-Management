#####################################################################  ###################
################################START OF MY ACTUAL CODE#################################
########################################################################################
##Directory entered 
setwd("C:/Users/AndrewT/OneDrive - CAAS (Environmental Services) Ltd/Desktop/AndyPhD/PhD_Data_R")

##had to download the 'vegan' package for indices and others
##install.packages("vegan")
##install.packages("dplyr")
##install.packages("pwr")
##install.packages("permute")
##install.packages("lattice")
##install.packages("dplyr")
##install.packages("plyr")
#install.packages("naniar")
#install.packages("viridis")
#install.packages("ggeasy")
#install.packages("lmerTest")
#install.packages("lmtest")


#install.packages("cowplot")


##if a pachage is installed (check packages bottom right) then add library as follows
## Adding the package
library(plyr)
library(dplyr)
library(permute)
library(lattice)
library(vegan)
library(naniar)
##library(diveRsity)
library(ggplot2)
library(viridis)
library(ggeasy)
library(cowplot)
library(Matrix)
library(lme4)
library(lmerTest)
library(lmtest)

########################################################################################
########################################################################################
#################### Power Test Code ###################################################
########################################################################################
########################################################################################

#power test
#effect is difference over root of the sum of the SD^2 for each treatment
#treatment SD at this point are assumed to be equal
#effectbot <- sqrt((sd(divers)^2)+(sd(divers)^2))
#pwr.t.test(d=(0-0.07)/effectbot, power=0.9,sig.level=.05,type="two.sample",alternative="two.sided")

########################################################################################
########################################################################################


##Read data in
Derrynane_OG <- read.csv("Derrynane2019_2022_Master_Feb2023.csv", header=TRUE)


mydata <- data.frame(Derrynane_OG)
mydata %>% replace_with_na(condition = ~.x == N/A)



mydata<-mydata[!(mydata$Year=="2018"),]

##mydata$Absorption <- mydata$L1 - mydata$L2

## tell r the number of species
no_species <- 42

##lists what you want
##names column names
dput(names(mydata))


##create list of species names

speciesnames <- c("Fescue", "Onion.Couch", "Cocksfoot", "Blue.Sedge", "Luzula.campestrus", 
                  "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                  "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                  "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                  "Kidney.Vetch", "Ragwort", "Buttercup", "Holcus.molinia", "Horsetail", 
                  "Mayweed", "Sea.carrot", "Sand.Sedge", "Chickweed", "Black.Napweed", 
                  "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Couch.grass.", 
                  "Perrennial.Ryegrass", "Self.heal", "Sea.Sandwort", "Poa.sp.", 
                  "Fairy.flax", "Crested.Hair.Grass", "Dandilion", "Hogweed", "Meadowsweet", 
                  "Sand.Couch.X.Sea.Couch")



mydata$richness <- apply(select(mydata, speciesnames)[,-1]>0,1,sum)
mydata$SimpsonsIndex <- diversity(mydata[speciesnames], index = "simpson", MARGIN = 1, base = exp(1))



variables_forave <- c("L1", "L2", "Absorption", "Height", "BioMass.Alive", "Biomass.Dead", "Bare", 
                      "Moss", "Fescue", "Onion.Couch", "Cocksfoot", "Blue.Sedge", "Luzula.campestrus", 
                      "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                      "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                      "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                      "Kidney.Vetch", "Ragwort", "Buttercup", "Holcus.molinia", "Horsetail", 
                      "Mayweed", "Sea.carrot", "Sand.Sedge", "Chickweed", "Black.Napweed", 
                      "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Couch.grass.", 
                      "Perrennial.Ryegrass", "Self.heal", "Sea.Sandwort", "Poa.sp.", 
                      "Fairy.flax", "Crested.Hair.Grass", "Dandilion", "Hogweed", "Meadowsweet", 
                      "Sand.Couch.X.Sea.Couch", "richness")

##making species occurrence data numeric fields using the list created above
##mydata[speciesnames] <- as.numeric(unlist((mydata[speciesnames])))
mydata[variables_forave] <- as.numeric(unlist((mydata[variables_forave])))

##Species richness field


dput(names(mydata))
##get average plot level data using the quadrats as agregates
plotdata <- mydata %>%
  group_by(Year, Plot.Number) %>% 
  summarise_at(vars(variables_forave), mean) 

##round richness up
plotdata$richness <- round(plotdata$richness, digits = 0)


##check %cover is 100 for each species as a separate dataset
percentagecover <- data.frame(plotdata$Plot.Number)
percentagecover$Percentage.Cover <- rowSums(plotdata[, speciesnames])


##richness from the working data
##need to set each variable as a dataframe
mydata_rich <- data.frame(mydata)
mydata_rich$richness <- apply(select(mydata_rich,speciesnames)[,-1]>0,1,sum)
mydata_rich <- mydata_rich %>%
  group_by(Year, Plot.Number) %>% 
  summarise_at(vars(richness), mean)
mydata_rich <- mydata_rich %>% mutate_at(vars(richness), funs(round(., 0)))


## Diversity Calculation 
##Diversity using the package 'diveRsity'using vegan, which needs lattice etc.
variables_forDIV <- c("SimpsonsIndex", "L1", "L2", "BioMass.Alive", "Biomass.Dead", "Absorption", "Height", "Bare", "Moss", 
                      "Fescue", "Onion.Couch", "Cocksfoot", "Blue.Sedge", "Luzula.campestrus", 
                      "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                      "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                      "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                      "Kidney.Vetch", "Ragwort", "Buttercup", "Holcus.molinia", "Horsetail", 
                      "Mayweed", "Sea.carrot", "Sand.Sedge", "Chickweed", "Black.Napweed", 
                      "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Couch.grass.", 
                      "Perrennial.Ryegrass", "Self.heal", "Sea.Sandwort", "Poa.sp.", 
                      "Fairy.flax", "Crested.Hair.Grass", "Dandilion", "Hogweed", "Meadowsweet", 
                      "Sand.Couch.X.Sea.Couch")

##removed cause they are incomplete "BioMass.Alive", "Biomass.Dead",


divers <- data.frame(plotdata)
divers$SimpsonsIndex <- diversity(divers[speciesnames], index = "simpson", MARGIN = 1, base = exp(1))

Plot_divers <- divers %>%
  group_by(Year, Plot.Number) %>% 
  summarise_at(vars(variables_forDIV), mean)
Plot_divers$Treatment <- Plot_divers$Plot.Number
Plot_divers$Treatment <- ifelse(grepl("C", Plot_divers$Treatment),'Grazed = Control','Ungrazed = Treatment')
Plot_divers$Treatment <- as.factor(Plot_divers$Treatment)


Plot_BARE <- Plot_divers %>%
  group_by(Year, Treatment) %>% 
  summarise_at(vars(Bare), mean)

Plot_Moss <- Plot_divers %>%
  group_by(Year, Treatment) %>% 
  count(Plot_divers$Moss < 1)




Quad_Moss <- Derrynane_OG %>%
  group_by(Year, Treatment) %>% 
  count(Derrynane_OG$Moss == 0)

##write.csv(divers, "C:\\Users\\AndrewT\\OneDrive - CAAS (Environmental Services) Ltd\\Desktop\\AndyPhD\\Perception Study\\PhotoData.csv")
Heights <- Plot_divers %>%
  group_by(Year, Treatment) %>% 
  summarise_at(vars(Height), mean)

####


boxplot(Plot_divers$SimpsonsIndex~Plot_divers$Treatment, col=  c("cyan4", "coral"))

t.test(Plot_divers$SimpsonsIndex~Plot_divers$Treatment)

##diversityModel <- glm(SimpsonsIndex ~ Treatment + Year + Absorption + BioMass.Alive + Biomass.Dead, data=Plot_divers)


diversityModel <- glm(SimpsonsIndex ~ Treatment + Year + Absorption + Biomass.Dead, data=Plot_divers)

summary(diversityModel)


#(diversityModel)


Plot_divers$Treatment <- factor(Plot_divers$Treatment)
graph <- ggplot(Plot_divers, aes(x = SimpsonsIndex, y = Treatment/Absorption)) +
  geom_line(aes(colour = Treatment), linewidth = 1) +
  labs(x = 'diabetes pedigree function', y = 'test of diabetes')
graph




########Machair checking

ForMachaire <- c("Fescue", "Onion.Couch", "Cocksfoot", "Blue.Sedge", "Luzula.campestrus", 
                 "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                 "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                 "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                  "Kidney.Vetch", "Ragwort", "Buttercup", "Holcus.molinia", "Horsetail", 
                  "Mayweed", "Sea.carrot", "Sand.Sedge", "Chickweed", "Black.Napweed", 
                  "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Couch.grass.", 
                  "Perrennial.Ryegrass", "Self.heal", "Sea.Sandwort", "Poa.sp.", 
                  "Fairy.flax", "Crested.Hair.Grass", "Dandilion", "Hogweed", "Meadowsweet", 
                  "Sand.Couch.X.Sea.Couch")

machairID <- mydata[ForMachaire]
speciesnames 

machairID[machairID > 0] <- 1

AddColumns <- c("Year", "Date", "Plot.Number", "Treatment", "Quadrat")

machairID$Plot.Number <- mydata$Plot.Number
machairID$Treatment <- mydata$Treatment
machairID$Quadrat <- mydata$Quadrat
machairID$Year <- mydata$Year

machairID <- machairID[,c("Year", "Plot.Number", "Treatment", "Quadrat", 
                          "Fescue", "Onion.Couch", "Cocksfoot", "Blue.Sedge", "Luzula.campestrus", 
                          "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                          "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                          "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                          "Kidney.Vetch", "Ragwort", "Buttercup", "Holcus.molinia", "Horsetail", 
                          "Mayweed", "Sea.carrot", "Sand.Sedge", "Chickweed", "Black.Napweed", 
                          "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Couch.grass.", 
                          "Perrennial.Ryegrass", "Self.heal", "Sea.Sandwort", "Poa.sp.", 
                          "Fairy.flax", "Crested.Hair.Grass", "Dandilion", "Hogweed", "Meadowsweet", 
                          "Sand.Couch.X.Sea.Couch")]

MIndicators <- c("Fescue", "Blue.Sedge", "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover","Eyebright", "Yellow.Rattle", "Ribwort.Plantain",
                 "Marsh.Orchid","Sand.Sedge", "Chickweed", "Wild.thyme", "Pyramidal.Orchid","Self.heal", "Fairy.flax")


IndicatorData <- machairID[MIndicators]
IndicatorData$Plot.Number <- machairID$Plot.Number
IndicatorData$Treatment <- machairID$Treatment
IndicatorData$Quadrat <- machairID$Quadrat
IndicatorData$Year <- machairID$Year
IndicatorData <- IndicatorData[,c("Year", "Plot.Number", "Treatment", "Quadrat", "Fescue", "Blue.Sedge", "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover","Eyebright", "Yellow.Rattle", "Ribwort.Plantain",
                                  "Marsh.Orchid","Sand.Sedge", "Chickweed", "Wild.thyme", "Pyramidal.Orchid","Self.heal", "Fairy.flax")]
IndicatorData$TOTAL_Pos <- apply(select(IndicatorData,MIndicators)[,-1]>0,1,sum)

  
IndicatorCounts <- data.frame(IndicatorData %>% group_by(Treatment, Year) %>% count(IndicatorData$TOTAL_Pos > 6))
Indicator2 <- IndicatorData %>% group_by(Treatment, Year) %>% count(IndicatorData$TOTAL_Pos > 3)


dput(names(IndicatorCounts))

IndicatorCountsPerc <- IndicatorCounts %>%
  group_by(Treatment, Year) %>% 
  mutate(percent = ((n/sum(n))*100))

df2<-IndicatorCountsPerc[!(IndicatorCountsPerc$IndicatorData.TOTAL_Pos...6 =="FALSE"),]


Heights$PosIndPerc <- df2$percent
Heights$PosIndCont <- df2$n


MachairFailTable <- Heights

frobs <- c("Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
           "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
           "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
           "Kidney.Vetch", "Ragwort", "Buttercup", "Horsetail", 
           "Mayweed", "Sea.carrot", "Chickweed", "Black.Napweed", 
           "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica",
           "Self.heal", "Sea.Sandwort", 
           "Fairy.flax", "Dandilion", "Hogweed", "Meadowsweet")

gramanoid <- c("Fescue", "Onion.Couch", "Cocksfoot", "Blue.Sedge", "Luzula.campestrus", 
               "Holcus.molinia", "Sand.Sedge", "Couch.grass.", "Perrennial.Ryegrass", 
               "Poa.sp.", "Crested.Hair.Grass", "Sand.Couch.X.Sea.Couch")



Plot_divers$frobs = rowSums(Plot_divers[,c("Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                                           "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                                           "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                                           "Kidney.Vetch", "Ragwort", "Buttercup", "Horsetail", 
                                           "Mayweed", "Sea.carrot", "Chickweed", "Black.Napweed", 
                                           "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Self.heal", "Sea.Sandwort", 
                                           "Fairy.flax", "Dandilion", "Hogweed", "Meadowsweet")])
Flowering <-data.frame(Plot_divers %>% group_by(Treatment, Year) %>% count(Plot_divers$frobs > 40))

Derrynane_OG$frobs = rowSums(Derrynane_OG[,c("Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", "Red.Clover", 
                                           "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", "Autumnalis", 
                                           "Catsear", "Sheeps.sorrel", "Marsh.Orchid", "Common.Milkwort", 
                                           "Kidney.Vetch", "Ragwort", "Buttercup", "Horsetail", 
                                           "Mayweed", "Sea.carrot", "Chickweed", "Black.Napweed", 
                                           "Wild.thyme", "Pyramidal.Orchid", "Wild.angellica", "Self.heal", "Sea.Sandwort", 
                                           "Fairy.flax", "Dandilion", "Hogweed", "Meadowsweet")])

Flowering2 <-data.frame(Derrynane_OG %>% group_by(Treatment, Year) %>% count(Derrynane_OG$frobs > 40))


##number of relevees that have flowering indicator species

Derrynane_OG$floweringMachair = rowSums(Derrynane_OG[,c("Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover","Eyebright", "Yellow.Rattle", "Ribwort.Plantain",
                                             "Marsh.Orchid", "Chickweed", "Wild.thyme", "Pyramidal.Orchid","Self.heal", "Fairy.flax")])

Flowering3 <-data.frame(Derrynane_OG %>% group_by(Treatment, Year) %>% count(Derrynane_OG$floweringMachair > 40))





DiversityPlot_peryear <-ggplot(data=Plot_divers, aes(x=Year, y=SimpsonsIndex, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("cyan4",
                             "coral"))
DiversityPlot_peryear

boxplot(Plot_divers$SimpsonsIndex~Plot_divers$Treatment, col=  c("cyan4", "coral"), xlab ="Treatment", ylab="Diverity - Simpsons")
 


###################################
###Graphing

##################################
#graph of diversity over time
PerYearDivers <- ggplot(Plot_divers, aes(x=Treatment, y=SimpsonsIndex)) +
  geom_boxplot(aes(fill=Treatment), position=position_dodge(.9), outlier.shape = NA) +
  facet_wrap(~Year, nrow = 1) +
  geom_jitter(color="black", size=2, alpha=0.9, shape=1) +
  scale_fill_manual(values=c("cyan4", "coral")) +
  scale_x_discrete(labels=c("Grazed", "Ungrazed")) +
  labs(y = "Simpsons Index") +  # Rename the Y-axis label
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  theme(strip.text = element_text(size = 20,face="bold")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=20))
FullGraph <- PerYearDivers
FullGraph


##Graph Alive Biomass
#Bounded by zero so log transformed.
Plot_divers$BioMass.AliveLog <- log(Plot_divers$BioMass.Alive)

AliveYears <- ggplot(Plot_divers, aes(x=Treatment, y=BioMass.AliveLog)) +
  geom_boxplot(aes(fill=Treatment), position=position_dodge(.9), outlier.shape = NA) +
  facet_wrap(~Year, nrow = 1) +
  geom_jitter(color="black", size=2, alpha=0.9, shape=1) +
  scale_fill_manual(values=c("cyan4", "coral")) +
  scale_x_discrete(labels=c("Grazed", "Ungrazed")) +
  labs(y = "Alive Biomass Yield\nln(grams)") +  # Rename the Y-axis label
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  theme(strip.text = element_text(size = 20,face="bold")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=20))
FullGraphAliveYears <- AliveYears
FullGraphAliveYears



##Graph Dead Biomass
#Bounded by zero so log transformed.
Plot_divers$Biomass.DeadLog <- log(Plot_divers$Biomass.Dead)

DeadYears <- ggplot(Plot_divers, aes(x=Treatment, y=Biomass.DeadLog)) +
  geom_boxplot(aes(fill=Treatment), position=position_dodge(.9), outlier.shape = NA) +
  facet_wrap(~Year, nrow = 1) +
  geom_jitter(color="black", size=2, alpha=0.9, shape=1) +
  scale_fill_manual(values=c("cyan4", "coral")) +
  scale_x_discrete(labels=c("Grazed", "Ungrazed")) +
  labs(y = "Dead Biomass Yield\nln(grams)") +  # Rename the Y-axis label
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  theme(strip.text = element_text(size = 20,face="bold")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=20))
FullGraphDeadYears <- DeadYears
FullGraphDeadYears


##Graph Absorption
Plot_divers$Absorptionsqrt <- sqrt(Plot_divers$Absorption)
AbsorbEffect <- ggplot(Plot_divers, aes(x=Treatment, y=Absorptionsqrt)) +
  geom_boxplot(aes(fill=Treatment), position=position_dodge(.9), outlier.shape = NA) +
  facet_wrap(~Year, nrow = 1) +
  geom_jitter(color="black", size=2, alpha=0.9, shape=1) +
  scale_fill_manual(values=c("cyan4", "coral")) +
  scale_x_discrete(labels=c("Grazed", "Ungrazed")) +
  labs(y = "Light absorption\nsqrt(PAR Photosynthetic Photon Flux Density)\nsqrt(nµmol photons m-2 s-1)") +  # Rename the Y-axis label
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  theme(strip.text = element_text(size = 20,face="bold")) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size=20))
FullGraphAbsorbEffect <- AbsorbEffect
FullGraphAbsorbEffect


#########################
############################




#################Modelling######################

########Diversity Over Time##################
Plot_divers$Year <- scale(Plot_divers$Year)
Plot_divers$PlotID <- gsub("C", "", Plot_divers$Plot.Number)
Plot_divers$PlotID <- as.factor(Plot_divers$PlotID)

#DiverOverTime <- glm(SimpsonsIndex ~ Treatment + Year, data=Plot_divers)
DiverOverTime <- lmerTest::lmer(SimpsonsIndex ~ Treatment*Year + (1|PlotID), data = Plot_divers)
# Obtain the summary of the model
summary_DiverOverTime <- summary(DiverOverTime)
summary_DiverOverTime

######################
##Test the assumptions
plot(resid(DiverOverTime) ~ fitted(DiverOverTime))
# Check homoscedasticity
plot(fitted(DiverOverTime), resid(DiverOverTime))
# Check independence of residuals (no specific patterns)
plot(resid(DiverOverTime) ~ seq_along(resid(DiverOverTime)))
# Check normality of residuals with a Q-Q plot
qqnorm(resid(DiverOverTime))
qqline(resid(DiverOverTime))
# Formal normality test (e.g., Shapiro-Wilk)
shapiro.test(resid(DiverOverTime))





####Alive Biomass Model
AliveOverTime <- lmerTest::lmer(BioMass.AliveLog ~ Treatment*Year + (1|PlotID), data = Plot_divers)
# Obtain the summary of the model
summary_AliveOverTime <- summary(AliveOverTime)
summary_AliveOverTime

######################
##Test the assumptions
plot(resid(AliveOverTime) ~ fitted(AliveOverTime))
# Check homoscedasticity
plot(fitted(AliveOverTime), resid(AliveOverTime))
# Check independence of residuals (no specific patterns)
plot(resid(AliveOverTime) ~ seq_along(resid(AliveOverTime)))
# Check normality of residuals with a Q-Q plot
qqnorm(resid(AliveOverTime))
qqline(resid(AliveOverTime))
# Formal normality test (e.g., Shapiro-Wilk)
shapiro.test(resid(AliveOverTime))


####Dead Biomass Model
DeadOverTime <- lmerTest::lmer(Biomass.DeadLog ~ Treatment*Year + (1|PlotID), data = Plot_divers)
# Obtain the summary of the model
summary_DeadOverTime <- summary(DeadOverTime)
summary_DeadOverTime

######################
##Test the assumptions
plot(resid(DeadOverTime) ~ fitted(DeadOverTime))
# Check homoscedasticity
plot(fitted(DeadOverTime), resid(DeadOverTime))
# Check independence of residuals (no specific patterns)
plot(resid(DeadOverTime) ~ seq_along(resid(DeadOverTime)))
# Check normality of residuals with a Q-Q plot
qqnorm(resid(DeadOverTime))
qqline(resid(DeadOverTime))
# Formal normality test (e.g., Shapiro-Wilk)
shapiro.test(resid(DeadOverTime))



####Absorption Model
AbsorptionOverTime <- lmerTest::lmer(Absorptionsqrt ~ Treatment*Year + (1|PlotID), data = Plot_divers)
# Obtain the summary of the model
summary_AbsorptionOverTime <- summary(AbsorptionOverTime)
summary_AbsorptionOverTime

######################
##Test the assumptions
plot(resid(AbsorptionOverTime) ~ fitted(AbsorptionOverTime))
# Check homoscedasticity
plot(fitted(AbsorptionOverTime), resid(AbsorptionOverTime))
# Check independence of residuals (no specific patterns)
plot(resid(AbsorptionOverTime) ~ seq_along(resid(AbsorptionOverTime)))
# Check normality of residuals with a Q-Q plot
qqnorm(resid(AbsorptionOverTime))
qqline(resid(AbsorptionOverTime))
# Formal normality test (e.g., Shapiro-Wilk)
shapiro.test(resid(AbsorptionOverTime))





summary_DiverOverTime
summary_AliveOverTime
summary_DeadOverTime
summary_AbsorptionOverTime


# ##testvariables
# 
# TestBioMass.Alive <- glm(BioMass.Alive ~ Treatment + Year, data=Plot_divers)
# summary(TestBioMass.Alive)
# TestBioMass.Alive2 <- glm(BioMass.Alive ~ Treatment + Year + Treatment:Year, data=Plot_divers)
# summary(TestBioMass.Alive2)
# TestBioMass.Alive3 <- glm(BioMass.Alive ~ Treatment + Treatment:Year, data=Plot_divers)
# summary(TestBioMass.Alive3)
# TestBioMass.Alive4 <- glm(BioMass.Alive ~ Treatment, data=Plot_divers)
# summary(TestBioMass.Alive4)
# 
# AIC(TestBioMass.Alive)
# AIC(TestBioMass.Alive2)
# AIC(TestBioMass.Alive3)
# AIC(TestBioMass.Alive4)
# 
# TestBiomass.Dead <- glm(Biomass.Dead ~ Treatment + Year, data=Plot_divers)
# summary(TestBiomass.Dead)
# TestBiomass.Dead2 <- glm(Biomass.Dead ~ Treatment + Year + Treatment:Year, data=Plot_divers)
# summary(TestBiomass.Dead2)
# TestBiomass.Dead3 <- glm(Biomass.Dead ~ Treatment + Treatment:Year, data=Plot_divers)
# summary(TestBiomass.Dead3)
# TestBiomass.Dead4 <- glm(Biomass.Dead ~ Treatment, data=Plot_divers)
# summary(TestBiomass.Dead4)
# 
# AIC(TestBiomass.Dead)
# AIC(TestBiomass.Dead2)
# AIC(TestBiomass.Dead3)
# AIC(TestBiomass.Dead4)
# 
# 
# TestAbsorption <- glm(Absorption ~ Treatment + Year, data=Plot_divers)
# summary(TestAbsorption)
# TestAbsorption2 <- glm(Absorption ~ Treatment + Year + Treatment:Year, data=Plot_divers)
# summary(TestAbsorption2)
# TestAbsorption3 <- glm(Absorption ~ Treatment + Treatment:Year, data=Plot_divers)
# summary(TestAbsorption3)
# TestAbsorption4 <- glm(Absorption ~ Treatment, data=Plot_divers)
# summary(TestAbsorption4)
# 
# AIC(TestAbsorption)
# AIC(TestAbsorption2)
# AIC(TestAbsorption3)
# AIC(TestAbsorption4)



################################
########FixedDune checking###################
#####################################
ForFixedDune <- c("Fescue", "Blue.Sedge", "Luzula.campestrus", 
                  "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", 
                  "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", 
                  "Catsear", "Marsh.Orchid", "Kidney.Vetch", "Holcus.molinia", "Horsetail", 
                  "Sea.carrot", "Sand.Sedge", "Chickweed", "Wild.thyme", 
                  "Poa.sp.", "Fairy.flax")

FixedDuneID <- mydata[ForFixedDune]
speciesnames 

FixedDuneID[FixedDuneID > 0] <- 1

AddColumns <- c("Year", "Date", "Plot.Number", "Treatment", "Quadrat")

FixedDuneID$Plot.Number <- mydata$Plot.Number
FixedDuneID$Treatment <- mydata$Treatment
FixedDuneID$Quadrat <- mydata$Quadrat
FixedDuneID$Year <- mydata$Year
FixedDuneID <- as.data.frame(FixedDuneID)
FixedDuneID <- FixedDuneID[,c("Year", "Plot.Number", "Treatment", "Quadrat", 
                              "Fescue", "Blue.Sedge", "Luzula.campestrus", 
                              "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", 
                              "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", 
                              "Catsear", "Marsh.Orchid", "Kidney.Vetch", "Holcus.molinia", "Horsetail", 
                              "Sea.carrot", "Sand.Sedge", "Chickweed", "Wild.thyme", 
                              "Poa.sp.", "Fairy.flax")]

FIndicators <- c("Fescue", "Blue.Sedge", "Luzula.campestrus", 
                "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", 
                "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", 
                "Catsear", "Marsh.Orchid", "Kidney.Vetch", "Holcus.molinia", "Horsetail", 
                "Sea.carrot", "Sand.Sedge", "Chickweed", "Wild.thyme", 
                "Poa.sp.", "Fairy.flax")


FIndicatorData <- FixedDuneID[FIndicators]
FIndicatorData$Plot.Number <- FixedDuneID$Plot.Number
FIndicatorData$Treatment <- FixedDuneID$Treatment
FIndicatorData$Quadrat <- FixedDuneID$Quadrat
FIndicatorData$Year <- FixedDuneID$Year
FIndicatorData <- FIndicatorData[,c("Year", "Plot.Number", "Treatment", "Quadrat", "Fescue", "Blue.Sedge", "Luzula.campestrus", 
                                    "Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", 
                                    "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", 
                                    "Catsear", "Marsh.Orchid", "Kidney.Vetch", "Holcus.molinia", "Horsetail", 
                                    "Sea.carrot", "Sand.Sedge", "Chickweed", "Wild.thyme", 
                                    "Poa.sp.", "Fairy.flax")]
FIndicatorData$TOTAL_Pos <- apply(select(FIndicatorData,FIndicators)[,-1]>0,1,sum)


FIndicatorCounts <- data.frame(FIndicatorData %>% group_by(Treatment, Year) %>% count(FIndicatorData$TOTAL_Pos > 8))
FIndicator2 <- FIndicatorData %>% group_by(Treatment, Year) %>% count(FIndicatorData$TOTAL_Pos > 4)


dput(names(FIndicatorCounts))

FIndicatorCountsPerc <- FIndicatorCounts %>%
  group_by(Treatment, Year) %>% 
  mutate(percent = ((n/sum(n))*100))

Fdf2<-FIndicatorCountsPerc[!(FIndicatorCountsPerc$FIndicatorData.TOTAL_Pos...8 =="FALSE"),]


HeightsFixed <- Plot_divers %>%
  group_by(Year, Treatment) %>% 
  summarise_at(vars(Height), mean)
HeightsFixed$FPosIndPerc <- Fdf2$percent
HeightsFixed$FPosIndCont <- Fdf2$n



FixedDuneFailTable <- HeightsFixed






##number of relevees that have flowering indicator species

Derrynane_OG$floweringFixed = rowSums(Derrynane_OG[,c("Lady.s.Bedstraw", "Birdsfoot.Trefoil", "White.Clover", 
                                                        "Yarrow", "Eyebright", "Yellow.Rattle", "Ribwort.Plantain", 
                                                        "Catsear", "Marsh.Orchid", "Kidney.Vetch", "Sea.carrot", "Chickweed", "Wild.thyme", 
                                                        "Fairy.flax")])

FloweringFixed <-data.frame(Derrynane_OG %>% group_by(Treatment, Year) %>% count(Derrynane_OG$floweringFixed > 40))
FloweringFixedHeight <-data.frame(Derrynane_OG %>% group_by(Treatment, Year) %>% count(Derrynane_OG$Height < 10, Derrynane_OG$Height > 2))
FloweringFixedHeight$lower <- Derrynane_OG %>% group_by(Treatment, Year) %>% count(Derrynane_OG$Height > 2)
FloweringFixedHeight$higher <- Derrynane_OG %>% group_by(Treatment, Year) %>% count(Derrynane_OG$Height > 10)
Plot_divers_Mean <- as.data.frame(Plot_divers %>%
  group_by(Year, Treatment) %>% 
  summarise_at(vars(SimpsonsIndex), mean))
