##############################################
### SECTION 1 - Data Prep and Exploration
##############################################

# Import Data - Note comment added to each line to include Measure Name

airpoll <- read.csv(file="a1data/airpoll.csv", skip=4,header=TRUE) # PM2.5 air pollution, mean annual exposure
lifeexp <- read.csv(file="a1data/lifeexp.csv", skip=4,header=TRUE) # Life expectancy at birth, total (years)
disease <- read.csv(file="a1data/disease.csv", skip=4,header=TRUE) # Cause of death, by communicable diseases and maternal, prenatal and nutrition conditions (% of total)
tb <- read.csv(file="a1data/tb.csv", skip=4,header=TRUE) # Incidence of tuberculosis (per 100,000 people)
injury <- read.csv(file="a1data/injury.csv", skip=4,header=TRUE) # Cause of death, by injury (% of total)
neonat <- read.csv(file="a1data/neonat.csv", skip=4,header=TRUE) # Mortality rate, neonatal (per 1,000 live births)
death <- read.csv(file="a1data/death.csv", skip=4,header=TRUE) # Death rate, crude (per 1,000 people)
lowbw <- read.csv(file="a1data/lowbb.csv", skip=4,header=TRUE) # Low-birthweight babies (% of births)
childmort <- read.csv(file="a1data/childmort.csv", skip=4,header=TRUE) # Mortality rate, under-5 (per 1,000 live births)
anemia <- read.csv(file="a1data/anemia.csv", skip=4,header=TRUE) # Prevalence of anemia among children (% of children under 5)
co2 <- read.csv(file="a1data/co2.csv", skip=4,header=TRUE) # CO2 emissions (metric tons per capita)
eduexp <- read.csv(file="a1data/eduexp.csv", skip=4,header=TRUE) # Expenditure on education as % of total government expenditure (%)
elec <- read.csv(file="a1data/elec.csv", skip=4,header=TRUE) # Access to electricity (% of population)
gdp <- read.csv(file="a1data/gdp.csv", skip=4,header=TRUE) # GDP per capita (current US$)
immdpt <- read.csv(file="a1data/immdpt.csv", skip=4,header=TRUE) # Immunization, DPT (% of children ages 12-23 months)
popgr <- read.csv(file="a1data/popgr.csv", skip=4,header=TRUE) #Population growth (annual %)
pov190 <- read.csv(file="a1data/pov190.csv", skip=4,header=TRUE) # Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)
primcomp <- read.csv(file="a1data/primcomp.csv", skip=4,header=TRUE) # Primary completion rate, total (% of relevant age group)
sanit <- read.csv(file="a1data/sanit.csv", skip=4,header=TRUE) # Improved sanitation facilities (% of population with access)
slums <- read.csv(file="a1data/slums.csv", skip=4,header=TRUE) # Population living in slums, (% of urban population)
undernour <- read.csv(file="a1data/undernour.csv", skip=4,header=TRUE) # Prevalence of undernourishment (% of population)
unempl <- read.csv(file="a1data/unemp.csv", skip=4,header=TRUE) # Unemployment, total (% of total labor force)
water <- read.csv(file="a1data/water.csv", skip=4,header=TRUE) # Improved water source (% of population with access)
alit <- read.csv(file="a1data/alit.csv", skip=4,header=TRUE) # Adult literacy rate, population 15+ years, both sexes (%)
tbrate <- read.csv(file="a1data/tbrate.csv", skip=4,header=TRUE) # Tuberculosis case detection rate (%, all forms)
diab <- read.csv(file="a1data/diab.csv", skip=4,header=TRUE) # Diabetes prevalence (% of population ages 20 to 79)
surv <- read.csv(file="a1data/surv.csv", skip=4,header=TRUE) # Survival to age 65, male (% of cohort)
hecap <- read.csv(file="a1data/hecap.csv", skip=4,header=TRUE) # Health expenditure per capita, PPP (constant 2011 international $)
hegov <- read.csv(file="a1data/hegov.csv", skip=4,header=TRUE) # Health expenditure, public (% of government expenditure)
hegdp <- read.csv(file="a1data/hegdp.csv", skip=4,header=TRUE) # Health expenditure, total (% of GDP)
hedocs <- read.csv(file="a1data/hedocs.csv", skip=4,header=TRUE) # Physicians (per 1,000 people)
geoloc <- read.csv(file="a1data/geoloc.csv", header=TRUE)

# Define Values for variables

country <- lifeexp$Country.Name
country_code <- lifeexp$Country.Code
airpoll <- airpoll$X2013
lifeexp <- lifeexp$X2014
disease <- disease$X2012
tb <- tb$X2014
injury <-injury$X2012
neonat <- neonat$X2015
death <- rowMeans(death[,50:59], na.rm = TRUE)
childmort <- childmort$X2015
anemia <- anemia$X2011
lowbw <- rowMeans(lowbw[,50:59], na.rm = TRUE)
tbrate <- tbrate$X2014
co2 <- co2$X2011
eduexp <- rowMeans(eduexp[,50:59], na.rm = TRUE)
elec <- elec$X2012
gdp <- rowMeans(gdp[,50:59], na.rm = TRUE)
immdpt <- immdpt$X2014
popgr <- popgr$X2015
pov190 <- rowMeans(pov190[,50:59], na.rm = TRUE)
primcomp <- rowMeans(primcomp[,50:59], na.rm = TRUE)
sanit <- sanit$X2015
slums <- rowMeans(slums[,50:59], na.rm = TRUE)
undernour <- undernour$X2015
unempl <- unempl$X2014
water <- water$X2015
alit <- rowMeans(alit[,50:59], na.rm = TRUE)
lat <- geoloc$Lat
long <- geoloc$Long
diab <- diab$X2015
surv <- surv$X2014
hegov <- hegov$X2014
hecap <- hecap$X2014
hegdp <- hegdp$X2014
hedocs <- hedocs <- rowMeans(hedocs[,50:59], na.rm = TRUE)


#Create Master Data Frame

master <- data.frame(country, country_code,lat,long,airpoll,lifeexp,disease,tb,injury,tbrate,lowbw,neonat,surv, death,childmort,anemia,diab,co2,eduexp,elec,gdp,immdpt,popgr,pov190,primcomp,sanit,slums,undernour,unempl,water,alit, hegov, hegdp, hecap, hedocs)
  
# Remove Non-Countries (with no Lat/Long Listings)

master <- master[complete.cases(master[,3:4]),]
rownames(master) <- NULL 

# Re-define Targets and Predictors to include countries only - 211 counts
country <- master$country
country_code <- master$country_code
lat <- master$lat
long <- master$long
lifeexp <- master$lifeexp
disease <- master$disease
childmort <- master$childmort
tb <- master$tb
injury <- master$injury
lowbw <- master$lowbw
death <- master$death
tbrate <- master$tbrate
neonat <- master$neonat
anemia <- master$anemia
airpoll <- master$airpoll
co2 <- master$co2
eduexp <- master$eduexp
elec <- master$elec
gdp <- master$gdp
water <- master$water
popgr <- master$popgr
pov190 <- master$pov190
primcomp <- master$primcomp
sanit <- master$sanit
slums <- master$slums
undernour <- master$undernour
unempl <- master$unempl
alit <- master$alit
immdpt <- master$immdpt
diab <- master$diab
surv <- master$surv
hegov <- master$hegov
hecap <- master$hecap
hegdp <- master$hegdp
hedocs <- master$hedocs


# Plot Targets

# Boxplots

par(mfrow=c(2,2))
boxplot(lifeexp)
boxplot(disease)
boxplot(childmort)
boxplot(tb)
boxplot(death)
boxplot(neonat)
boxplot(anemia)
boxplot(tbrate)
boxplot(diab)
boxplot(lowbw)
boxplot(tbrate)

# Density Plots - NA Values Removed

par(mfrow=c(3,2))
plot (density(lifeexp, na.rm=TRUE))
plot (density(disease, na.rm=TRUE))
plot (density(childmort, na.rm=TRUE))
plot (density(tb, na.rm=TRUE))
plot (density(death, na.rm=TRUE))
plot (density(anemia, na.rm=TRUE))
plot (density(neonat, na.rm=TRUE))
plot (density(tbrate, na.rm=TRUE))
plot (density(lowbw, na.rm=TRUE))
plot (density(diab, na.rm=TRUE))

#Correlations

library (corrplot)
library(psych)
targets <- data.frame (lifeexp, death, childmort, disease, tb, lowbw, injury,tbrate,diab,surv)
targets.cor <- cor (targets, use="pairwise.complete.obs", method="pearson")
par(mfrow=c(1,1))
corrplot (targets.cor, method = c("number"),type="lower", diag = TRUE)
corrplot (targets.cor, method = c("square"),type="lower", diag = TRUE)
pairs.panels (targets, density=TRUE, scale=TRUE, col="red")

# Plot Predictors

# Boxplots

boxplot(airpoll)
boxplot(co2)
boxplot(eduexp)
boxplot(elec)
boxplot(gdp)
boxplot(water)
boxplot(popgr)
boxplot(pov190)
boxplot(primcomp)
boxplot(sanit)
boxplot(slums)
boxplot(undernour)
boxplot(unempl)
boxplot(alit)
boxplot (immdpt)

#Density Plots

par(mfrow=c(3,2))
plot (density(airpoll, na.rm=TRUE))
plot (density(co2, na.rm=TRUE))
plot (density(eduexp, na.rm=TRUE))
plot (density(elec, na.rm=TRUE))
plot (density(gdp, na.rm=TRUE))
plot (density(immdpt, na.rm=TRUE))
plot (density(water, na.rm=TRUE))
plot (density(popgr, na.rm=TRUE))
plot (density(pov190, na.rm=TRUE))
plot (density(primcomp, na.rm=TRUE))
plot (density(sanit, na.rm=TRUE))
plot (density(slums, na.rm=TRUE))
plot (density(undernour, na.rm=TRUE))
plot (density(unempl, na.rm=TRUE))
plot (density(alit, na.rm=TRUE))

# Check Correlations

predictors <- data.frame (airpoll, co2, immdpt, elec, eduexp, gdp, water, popgr, pov190, primcomp, sanit, slums, undernour, unempl, alit)
predictors.cor <- cor (predictors, use="pairwise.complete.obs", method="pearson")
round (predictors.cor, digits=3)

par(mfrow=c(1,1))
corrplot (predictors.cor, method = c("square"), type="lower", diag = TRUE)

pairs.panels(predictors[1:8], density= TRUE,  col="red")
pairs.panels(predictors[9:15], density= TRUE,  col="red")

# Check correlations between targets and variables

all <- data.frame (targets, predictors)
all.cor <- cor (all, use="pairwise.complete.obs", method="pearson")
round (all.cor, digits=2)
corrplot (all.cor, method = c("number"), number.digits=1, type="lower", diag = TRUE)
corrplot (all.cor, method = c("square"), number.digits=1, type="lower", diag = TRUE)


# Remove Unsuitable Predictors - Slums, Co2, Unemployment

master <- data.frame(country, country_code,lat,long,airpoll,lifeexp,disease,tb,diab,death,childmort,anemia,tbrate,eduexp,elec,gdp,immdpt,popgr,pov190,primcomp,sanit,undernour,water,alit)
predictors <- data.frame (airpoll, immdpt, elec, eduexp, gdp, water, popgr, pov190, primcomp, sanit, undernour, alit)

# Use MICE to impute missing values for remaining predictors

# Impute Missing Values for Predictors with MICE
library (mice)
library (lattice)

# Create Data Frame for Predictors + Lat/Long

predforimp <- data.frame(predictors, master$lat, master$long)

# View Missing Data Details

md.pattern (predforimp)

# run MICE to Impute Values

imp.predictors <- mice(predforimp, m=5, seed="2000")

# View Details and Check that distributions for imputed values are similar to original data

summary (imp.predictors)
densityplot (imp.predictors)

# Combine imputed values with original data

predictors.comp <- complete (imp.predictors,1)

# Check no NA values present

summary(predictors.comp)

# Combine with Targets

master.comp <- data.frame(targets,predictors.comp)

# Redefine variables to include new imputed values

airpoll <- master.comp$airpoll
eduexp <- master.comp$eduexp
elec <- master.comp$elec
gdp <- master.comp$gdp
water <- master.comp$water
popgr <- master.comp$popgr
pov190 <- master.comp$pov190
primcomp <- master.comp$primcomp
sanit <- master.comp$sanit
undernour <- master.comp$undernour
alit <- master.comp$alit
immdpt <- master.comp$immdpt

################################################
### SECTION 2 - K-NN and Google Maps
################################################

# Scale All Variables

lifeexp.scaled <- scale (lifeexp)
disease.scaled <- scale (disease)
childmort.scaled <- scale (childmort)
tb.scaled <- scale (tb)
anemia.scaled <- scale (anemia)
death.scaled <- scale (death)
neonat.scaled <- scale (neonat)
immdpt.scaled <- scale (immdpt)
airpoll.scaled <- scale (airpoll)
eduexp.scaled <- scale (eduexp)
elec.scaled <- scale (elec)
gdp.scaled <- scale (gdp)
water.scaled <- scale (water)
popgr.scaled <- scale (popgr)
pov190.scaled <- scale (pov190)
primcomp.scaled <- scale (primcomp)
sanit.scaled <- scale (sanit)
undernour.scaled <- scale (undernour)
alit.scaled <- scale (alit)
lat.scaled <- scale (lat)
long.scaled <- scale (long)

master.scaled <- data.frame(master$country,master$country_code,lat.scaled,long.scaled,lifeexp.scaled,disease.scaled,childmort.scaled,tb.scaled,death.scaled,neonat.scaled,anemia.scaled,immdpt.scaled,airpoll.scaled,eduexp.scaled,elec.scaled,gdp.scaled,water.scaled,popgr.scaled,pov190.scaled,primcomp.scaled,sanit.scaled,undernour.scaled,alit.scaled)

# Create "Health" Target with Scaled Targets - Life Expectancy, Disease, Child Mortality

health <- (lifeexp.scaled-disease.scaled-childmort.scaled)

summary (health)

master.scaled <- data.frame(master.scaled,health)

hist (health)

# Classify Health based on natural breaks in histogram

health.class <- 
  ifelse(health < -2, "Poor",
         ifelse(health < 2, "Average",
                "Good"))

summary (health.class)
master.scaled["health.class"] <- factor(health.class)


# Create KNN Model for Health Target using Pov190, GDP, PopGR plus Lat/Long as Predictors

health.class.pred <- data.frame (master.scaled$health.class, pov190.scaled, gdp.scaled, popgr.scaled, lat.scaled, long.scaled, lat, long)

# Remove NA Values

health.classpred.clean <- subset(health.class.pred, 
                            !(is.na(health.class) | 
                                is.na(lat.scaled)))


#Create Training and Test Sets - 171 Rows Remaining.  65% Training, 35% Testing

set.seed (2017)
he.kn.training.size <- 0.65
he.kn.training <- sample.int(length(health.classpred.clean$master.scaled.health.class), round(length(health.classpred.clean$master.scaled.health.class) * he.kn.training.size))
he.kn.trainfull <- health.classpred.clean[he.kn.training,]
he.kn.validfull <- health.classpred.clean[-he.kn.training,]


he.kn.train <- he.kn.trainfull[,2:6]
he.kn.test <- he.kn.validfull[,2:6]
he.kn.leclass <- he.kn.trainfull[,1]
he.kn.testclass <- he.kn.validfull[,1]

library (class)

# Run KNN - K = 5

pred.model5 <- knn (he.kn.train,he.kn.test,he.kn.leclass,k=5,use.all=TRUE)

# Run KNN - K = 3

pred.model3 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=3,use.all=TRUE)

# Run KNN - K = 10

pred.model10 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=10,use.all=TRUE)

# Run KNN - K = 7

pred.model7 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=7,use.all=TRUE)

# Run KNN - K = 13 

pred.model13 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=13,use.all=TRUE)

# Test KNN with Cross Tables

library (gmodels)
CrossTable (he.kn.testclass, pred.model5)   
CrossTable (he.kn.testclass, pred.model3)  
CrossTable (he.kn.testclass, pred.model10)  
CrossTable (he.kn.testclass, pred.model7) 
CrossTable (he.kn.testclass, pred.model13) 

install.packages("caret", dependencies = TRUE)
library(caret)
confusionMatrix(table(he.kn.testclass, pred.model5))
confusionMatrix(table(he.kn.testclass, pred.model7))

###########################
# Mapping with Google Maps
###########################

map.health <- qmap("africa", zoom=2, maptype="terrain")
map.health+ 
  ggtitle("Health Classification") +
  geom_point(data=health.class.pred, 
             aes(x=long, y=lat, color=master.scaled.health.class), 
             alpha=0.5, size=4) +
  scale_color_manual(name = "Health Classification",
                     labels = c("Average", "Good", "Poor"),
                     values = c("orange", "blue", "red"))

# Map Health Classification - Predicted vs Test Cases
pred.test <- data.frame (he.kn.validfull[1:4], he.kn.validfull[7:8],pred.model7)

map.pred <- qmap("africa", zoom=2, maptype="terrain")
map.pred+ 
  ggtitle("KNN Classifier - Predictions vs Actual Values") +
  geom_point(data=pred.test, 
             aes(x=long, y=lat, color=master.scaled.health.class, shape=pred.model7), 
             alpha=0.5, size=5) +
  scale_color_manual(name = "Actual Values",
                     labels = c("Average", "Good", "Poor"),
                     values = c("orange", "blue", "red")) +
  scale_shape_manual(name = "Predicted Values",
                     labels = c("Average", "Good", "Poor"),
                     values = c(15, 18, 17))

# Map Health against Water

# Classify Water

water.class <- 
  ifelse(water < 50, "<50%",
                ifelse(water < 75, "<75%",
                       ifelse(water < 96, "<96%",
                              ">96%")))
summary (water.class)
master["water.class"] <- factor(water.class)

health.water <- data.frame (master$water.class, master.scaled$health.class)

map.water <- qmap("africa", zoom=2, maptype="terrain")
map.water+ 
  ggtitle("Health Classification vs Access to Improved Water") +
  geom_point(data=health.water, 
             aes(x=long, y=lat, color=master.scaled.health.class, shape=master.water.class), 
             alpha=0.5, size=5) +
  scale_color_manual(name = "Health Classification",
                     labels = c("Below Average", "Good", "Poor"),
                     values = c("orange", "blue", "red")) +
  scale_shape_manual(name = "Access to Improved Water",
                     labels = c("<50%", "<75%", "<96%", ">96%"),
                     values = c(15, 16, 18, 17))

# Plot Life Expectancy, Child Mortality and Death from Communicable Diseases

# Classify disease by Quartiles

disease.box <- summary (disease)
disease.box

low.disease <- disease.box[2] # 1st Qu.
blwavg.disease <- disease.box[3] # Median / 2nd Qu.
abvavg.disease <- disease.box[5] # Median / 2nd Qu.
high.disease <- disease.box[6] # Max
disease.class <- 
  ifelse(is.na(disease), "no value",
         ifelse(disease < low.disease, "low",
                ifelse(disease < blwavg.disease, "below average",
                       ifelse(disease < abvavg.disease, "above average",
                              "high"))))
master["dis.class"] <- factor(disease.class)

# Classify ChildMort by Quartiles

childmort.box <- summary (childmort)
childmort.box
hist (childmort)

low.childmort <- childmort.box[2] # 1st Qu.
blwavg.childmort <- childmort.box[3] # Median / 2nd Qu.
abvavg.childmort <- childmort.box[5] # Median / 2nd Qu.
high.childmort <- childmort.box[6] # Max
childmort.class <- 
  ifelse(is.na(childmort), "no value",
         ifelse(childmort < low.childmort, "low",
                ifelse(childmort < blwavg.childmort, "below average",
                       ifelse(childmort < abvavg.childmort, "above average",
                              "high"))))
master["cmort.class"] <- factor(childmort.class)

# Map the 3 Targets for Africa/Europe/Asia

library (ggmap)

map.le.di.cm <- qmap("africa", zoom=2, maptype="terrain")
map.le.di.cm + 
  ggtitle("Africa - Life Expectancy vs Disease vs Child Mortality") +
  geom_point(data=master, 
             aes(x=long, y=lat, size=lifeexp, color=cmort.class, shape=dis.class), 
             alpha=0.5) +
  scale_color_manual(name = "Child Mortality",
                     labels = c("above average", "below average", "high", "low", "no value"),
                     values = c("orange", "yellow", "red", "blue", "grey")) +   
  scale_shape_manual(name = "Disease",
                     labels = c("above average", "below average", "high", "low", "no value"),
                     values = c(15, 18, 17, 16, 4)) +
  scale_size_continuous (name = "Life Expectancy",
                         range = c(1, 10), 
                         breaks=c(50, 55, 60, 65, 70, 75, 80))

# Map Life Expectancy, Electricty Access and GDP

# Classify LifeExp by Quartiles

lifeexp.box <- summary (lifeexp)

low.lifeexp <- lifeexp.box[2] # 1st Qu.
blwavg.lifeexp <- lifeexp.box[3] # Median / 2nd Qu.
abvavg.lifeexp <- lifeexp.box[5] # Median / 2nd Qu.
long.lifeexp <- lifeexp.box[6] # Max
lifeexp.class <- 
  ifelse(is.na(lifeexp), "no value",
         ifelse(lifeexp < low.lifeexp, "low",
                ifelse(lifeexp < blwavg.lifeexp, "below average",
                       ifelse(lifeexp < abvavg.lifeexp, "above average",
                              "long"))))
master["le.class"] <- factor(lifeexp.class)

# Classify PopGrowth by Quartiles

popgr.box <- summary (popgr)

low.popgr <- popgr.box[2] # 1st Qu.
blwavg.popgr <- popgr.box[3] # Median / 2nd Qu.
abvavg.popgr <- popgr.box[5] # Median / 2nd Qu.
long.popgr <- popgr.box[6] # Max
popgr.class <- 
  ifelse(is.na(popgr), "no value",
         ifelse(popgr < low.popgr, "low",
                ifelse(popgr < blwavg.popgr, "below average",
                       ifelse(popgr < abvavg.popgr, "above average",
                              "long"))))
master["popgr.class"] <- factor(popgr.class)

# Classify Elec  by Quartiles

elec.box <- summary (elec)
elec.box

low.elec <- elec.box[2] # 1st Qu.
blwavg.elec <- elec.box[3] # Median / 2nd Qu.
abvavg.elec <- elec.box[5] # Median / 2nd Qu.
high.elec <- elec.box[6] # Max
elec.class <- 
  ifelse(is.na(elec), "no value",
         ifelse(elec < low.elec, "low",
                ifelse(elec < blwavg.elec, "below average",
                       ifelse(elec < abvavg.elec, "above average",
                              "high"))))
master["elec.class"] <- factor(elec.class)


# Map the 3 Variables for Africa/Europe/Asia

library (ggmap)

map.le.el.gdp <- qmap("africa", zoom=2, maptype="terrain")
map.le.el.gdp+ 
  ggtitle("Africa - Population Growth vs Electricty Access vs GDP") +
  geom_point(data=master, 
             aes(x=long, y=lat, size=gdp, color=popgr.class, shape=elec.class), 
             alpha=0.5) +
  scale_color_manual(name = "Population Growth",
                     labels = c("above average", "below average", "high", "low", "no value"),
                     values = c("orange", "yellow", "red", "blue", "grey")) +   
  scale_shape_manual(name = "Electricity Access",
                     labels = c("above average", "below average", "high", "low", "no value"),
                     values = c(15, 18, 17, 16, 4)) +
  scale_size_continuous (name = "GDP",
                         range = c(1, 10), 
                         breaks=c(500, 1500, 4500, 10000, 18500, 30000))

####################  
# Additional K-NN Tests - Results Not Included in Report
####################

# Try with Health Target and 9 predictors

health.class.pred <- data.frame (master.scaled$health.class, master$lat, master$long, pov190.scaled, elec.scaled,sanit.scaled)

# Remove NA Values

health.classpred.clean <- subset(health.class.pred, 
                                 !(is.na(health.class) | 
                                     is.na(water.scaled)))

#Create Training and Test Sets - 70% Training, 30% Test

set.seed (2017)
he.kn.training.size <- 0.65
he.kn.training <- sample.int(length(health.classpred.clean$master.scaled.health.class), round(length(health.classpred.clean$master.scaled.health.class) * he.kn.training.size))
he.kn.trainfull <- health.classpred.clean[he.kn.training,]
he.kn.validfull <- health.classpred.clean[-he.kn.training,]


he.kn.train <- he.kn.trainfull[,2:5]
he.kn.test <- he.kn.validfull[,2:5]
he.kn.leclass <- he.kn.trainfull[,1]
he.kn.testclass <- he.kn.validfull[,1]

# Run KNN - K = 5
library (class)
pred.model5 <- knn (he.kn.train,he.kn.test,he.kn.leclass,k=5,use.all=TRUE)

# Run KNN - K = 3

pred.model3 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=3,use.all=TRUE)

# Run KNN - K = 10

pred.model10 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=10,use.all=TRUE)

# Run KNN - K = 7

pred.model7 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=7,use.all=TRUE)

# Run KNN - K = 13 (close to Sq Rt of 154)

pred.model13 <- knn (he.kn.train, he.kn.test,he.kn.leclass,k=13,use.all=TRUE)

# Test KNN

library (gmodels)
CrossTable (he.kn.testclass, pred.model5)  # 32/51 
CrossTable (he.kn.testclass, pred.model3)  # 29   
CrossTable (he.kn.testclass, pred.model10) #32  
CrossTable (he.kn.testclass, pred.model7) #34
CrossTable (he.kn.testclass, pred.model13) #32


# KNN with Life Expectancy

# Classify Life Expectancy 

lifeexp.box <- summary (lifeexp)
lifeexp.box

low.lifeexp <- lifeexp.box[2] # < 1st Q
blwavg.lifeexp <- lifeexp.box[3] # < Median
abvavg.lifeexp <- lifeexp.box[5] # < 3rd Q
long.lifeexp <- lifeexp.box[6] # > 3rd Q
lifeexp.class <- 
  ifelse(lifeexp < low.lifeexp, "low",
         ifelse(lifeexp < blwavg.lifeexp, "below average",
                ifelse(lifeexp < abvavg.lifeexp, "above average",
                       "long")))

summary (lifeexp.class)
master.scaled["le.class"] <- factor(lifeexp.class)

# Create data frame with le.class, lat/long and 5 predictors

leclass.pred <- data.frame (master.scaled$le.class, master$lat, master$long, gdp.scaled, water.scaled, elec.scaled, sanit.scaled, primcomp.scaled)

# Remove NA Values

leclasspred.clean <- subset(leclass.pred, 
                            !(is.na(master.scaled$le.class) | 
                                is.na(gdp.scaled) |
                                is.na(elec.scaled) |
                                is.na(sanit.scaled) |
                                is.na(primcomp.scaled) |
                                is.na(water.scaled)))
rownames(leclasspred.clean) <- NULL                       
summary (leclasspred.clean)   # 194 Rows Remaining

#Create Training and Test Sets - 70% Training, 30% Test

set.seed (2017)
le.kn.training.size <- 0.7
le.kn.training <- sample.int(length(leclasspred.clean$master.scaled.le.class), round(length(leclasspred.clean$master.scaled.le.class) * le.kn.training.size))
le.kn.trainfull <- leclasspred.clean[le.kn.training,]
le.kn.validfull <- leclasspred.clean[-le.kn.training,]


le.kn.train <- le.kn.trainfull[,2:8]
le.kn.test <- le.kn.validfull[,2:8]
le.kn.leclass <- le.kn.trainfull[,1]
le.kn.testclass <- le.kn.validfull[,1]

# Run KNN - K = 5
library (class)
pred.model5 <- knn (le.kn.train,le.kn.test,le.kn.leclass,k=5,use.all=TRUE)

# Run KNN - K = 3

pred.model3 <- knn (le.kn.train, le.kn.test,le.kn.leclass,k=3,use.all=TRUE)

# Run KNN - K = 10

pred.model10 <- knn (le.kn.train, le.kn.test,le.kn.leclass,k=10,use.all=TRUE)

# Run KNN - K = 7

pred.model7 <- knn (le.kn.train, le.kn.test,le.kn.leclass,k=7,use.all=TRUE)

# Run KNN - K = 13 (close to Sq Rt of 154)

pred.model13 <- knn (le.kn.train, le.kn.test,le.kn.leclass,k=13,use.all=TRUE)

# Test KNN

library (gmodels)
CrossTable (le.kn.testclass, pred.model5)  # 33/68
CrossTable (le.kn.testclass, pred.model3)  # 37
CrossTable (le.kn.testclass, pred.model10) #35
CrossTable (le.kn.testclass, pred.model7) #33
CrossTable (le.kn.testclass, pred.model13) #35

# Try Model with Lat/Long, ImmDPT, PrimComp, GDP, water, popgr, pov190 (regression results)

leclass.pred2 <- data.frame (master.scaled$le.class, master$lat, master$long, gdp.scaled, water.scaled, immdpt.scaled, pov190.scaled, popgr.scaled, primcomp.scaled)

# Remove NA Values

leclasspred.clean2 <- subset(leclass.pred2, 
                            !(is.na(master.scaled$le.class)))
rownames(leclasspred.clean2) <- NULL                       
summary (leclasspred.clean2)   # 194 Rows Remaining



#Create Training and Test Sets 
set.seed (2017)
le.kn.training.size <- 0.7
le.kn.training <- sample.int(length(leclasspred.clean2$master.scaled.le.class), round(length(leclasspred.clean2$master.scaled.le.class) * le.kn.training.size))
le.kn.trainfull <- leclasspred.clean2[le.kn.training,]
le.kn.validfull <- leclasspred.clean2[-le.kn.training,]


le.kn.train2 <- le.kn.trainfull[,2:8]
le.kn.test2 <- le.kn.validfull[,2:8]
le.kn.leclass2 <- le.kn.trainfull[,1]
le.kn.testclass2 <- le.kn.validfull[,1]


# Run KNN - K = 5
library (class)
pred.model52 <- knn (le.kn.train2, le.kn.test2,le.kn.leclass2,k=5,use.all=TRUE)

# Run KNN - K = 3

pred.model32 <- knn (le.kn.train2, le.kn.test2,le.kn.leclass2,k=3,use.all=TRUE)

# Run KNN - K = 10

pred.model102 <- knn (le.kn.train2, le.kn.test2,le.kn.leclass2,k=10,use.all=TRUE)

# Run KNN - K = 7

pred.model72 <- knn (le.kn.train2, le.kn.test2,le.kn.leclass2,k=7,use.all=TRUE)

# Run KNN - K = 13

pred.model172 <- knn (le.kn.train2, le.kn.test2,le.kn.leclass2,k=13,use.all=TRUE)

# Test KNN

library (gmodels)
CrossTable (le.kn.testclass2, pred.model52) # 28/58
CrossTable (le.kn.testclass2, pred.model32) #31
CrossTable (le.kn.testclass2, pred.model102) #29
CrossTable (le.kn.testclass2, pred.model72) #28/50
CrossTable (le.kn.testclass2, pred.model172) #26/50

# New model with ONLY LAT LONG

leclass.pred4 <- data.frame (master.scaled$le.class, master$lat, master$long)
summary (leclass.pred4)

leclasspred4.clean <- subset(leclass.pred4, 
       !(is.na(master.scaled$le.class) | 
           is.na(master$lat) |
           is.na(master$long)))
rownames(leclasspred4.clean) <- NULL                       
summary (leclasspred4.clean)  # 194 Rows Remaining

#  le.kn.vars4 <- leclasspred4.clean [c(2:3) ]

#Create Training and Test Sets - 134 training, 60 test 

set.seed (2017)
le.kn.training.size <- 0.7
le.kn.training <- sample.int(length(leclasspred4.clean$master.scaled.le.class), round(length(leclasspred4.clean$master.scaled.le.class) * le.kn.training.size))
le.kn.trainfull <- leclasspred4.clean[le.kn.training,]
le.kn.validfull <- leclasspred4.clean[-le.kn.training,]


le.kn.train4 <- le.kn.trainfull[,2:3]
le.kn.test4 <- le.kn.validfull[,2:3]
le.kn.leclass4 <- le.kn.trainfull[,1]
le.kn.testclass4 <- le.kn.validfull[,1]


# Run KNN - K = 5
library (class)
pred.model54 <- knn (le.kn.train4, le.kn.test4,le.kn.leclass4,k=5,use.all=TRUE)

# Run KNN - K = 3

pred.model34 <- knn (le.kn.train4, le.kn.test4,le.kn.leclass4,k=3,use.all=TRUE)

# Run KNN - K = 10

pred.model104 <- knn (le.kn.train4, le.kn.test4,le.kn.leclass4,k=10,use.all=TRUE)

# Run KNN - K = 7

pred.model74 <- knn (le.kn.train4, le.kn.test4,le.kn.leclass4,k=7,use.all=TRUE)

# Run KNN - K = 2

pred.model24 <- knn (le.kn.train4, le.kn.test4,le.kn.leclass4,k=2,use.all=TRUE)

# Run KNN - K = 1

pred.model14 <- knn (le.kn.train4, le.kn.test4,le.kn.leclass4,k=1,use.all=TRUE)

# Test KNN

library (gmodels)
CrossTable (le.kn.testclass4, pred.model54) # 29/58
CrossTable (le.kn.testclass4, pred.model34) #32
CrossTable (le.kn.testclass4, pred.model104) #28
CrossTable (le.kn.testclass4, pred.model74) #29
CrossTable (le.kn.testclass4, pred.model24) #25
CrossTable (le.kn.testclass4, pred.model14) #27

# Try Model with Lat/Long and predictors with highest correlation - PrimComp, Water, Sanit, Elec (Alit and Pov190 ignored due to high NAs)

leclass.pred3 <- data.frame (master.scaled$le.class, master$lat, master$long, primcomp.scaled, water.scaled, sanit.scaled, elec.scaled)
summary (leclass.pred3)
# Remove NA Values

leclasspred3.clean <- subset(leclass.pred3, 
                                !(is.na(master.scaled$le.class) | 
                                    is.na(master$lat) |
                                    is.na(master$long)))
rownames(leclasspred3.clean) <- NULL                       
summary (leclasspred3.clean) # 194 Rows Remaining


#Create Training and Test Sets - 105 training, 50 test

set.seed (2017)
le.kn.training.size <- 0.7
le.kn.training <- sample.int(length(leclasspred3.clean$master.scaled.le.class), round(length(leclasspred3.clean$master.scaled.le.class) * le.kn.training.size))
le.kn.trainfull <- leclasspred3.clean[le.kn.training,]
le.kn.validfull <- leclasspred3.clean[-le.kn.training,]


le.kn.train3 <- le.kn.trainfull[,2:3]
le.kn.test3 <- le.kn.validfull[,2:3]
le.kn.leclass3 <- le.kn.trainfull[,1]
le.kn.testclass3 <- le.kn.validfull[,1]


# Run KNN - K = 5
library (class)
pred.model53 <- knn (le.kn.train3, le.kn.test3,le.kn.leclass3,k=5,use.all=TRUE)

# Run KNN - K = 3

pred.model33 <- knn (le.kn.train3, le.kn.test3,le.kn.leclass3,k=3,use.all=TRUE)

# Run KNN - K = 10

pred.model103 <- knn (le.kn.train3, le.kn.test3,le.kn.leclass3,k=10,use.all=TRUE)

# Run KNN - K = 7

pred.model73 <- knn (le.kn.train3, le.kn.test3,le.kn.leclass3,k=7,use.all=TRUE)

# Run KNN - K = 13

pred.model173 <- knn (le.kn.train3, le.kn.test3,le.kn.leclass3,k=13,use.all=TRUE)

# Run KNN - K = 2

pred.model23 <- knn (le.kn.train3, le.kn.test3,le.kn.leclass3,k=2,use.all=TRUE)

# Test KNN

library (gmodels)
CrossTable (le.kn.testclass3, pred.model53) #29/58
CrossTable (le.kn.testclass3, pred.model33) # 32
CrossTable (le.kn.testclass3, pred.model103) # 28
CrossTable (le.kn.testclass3, pred.model73) # 29
CrossTable (le.kn.testclass3, pred.model173) # 27
CrossTable (le.kn.testclass3, pred.model23) # 34


###########################################################
### Section 3 - Regression Analysis
###########################################################

# Begin Regression Analysis

library(Hmisc)
library(psych)
library(car)

####################
# Use Life Expectancy as Target
####################

# Remove Rows with Missing Target Values

le.regdata <- subset(master.comp, 
                     !(is.na(lifeexp)))
rownames(le.regdata) <- NULL

# Remove All Other Targets

le.regdata <- data.frame(le.regdata[1],le.regdata[11:22])
summary (le.regdata)

# Check Predictors for Normal Distributions

pairs.panels(le.regdata [1:13], col="red")

# Transform highly skewed distributions using log10

#le.regdata$airpoll <- log10(le.regdata$airpoll)
#le.regdata$immdpt <- log10(le.regdata$immdpt)
#le.regdata$elec <- log10(le.regdata$elec)
#le.regdata$gdp <- log10(le.regdata$gdp)
#le.regdata$water <- log10(le.regdata$water)
#le.regdata$pov190 <- log10(le.regdata$pov190)
#le.regdata$primcomp <- log10(le.regdata$primcomp)
#le.regdata$sanit <- log10(le.regdata$sanit)
#le.regdata$undernour <- log10(le.regdata$undernour)
#le.regdata$alit <- log10(le.regdata$alit)

#summary(le.regdata$airpoll)
#pairs.panels(le.regdata [2:13], col="red")

# Split into Training and Validation Data Sets
set.seed (2017)
le.training.size <- 0.8 
le.training <- sample.int(length(le.regdata$lifeexp), round(length(le.regdata$lifeexp) * le.training.size))
le.train.master <- le.regdata[le.training,]
le.valid <- le.regdata[-le.training,]
le.train <-le.train.master

# Fit Model using all variables

le.fit <- lm(lifeexp ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=le.train)
summary(le.fit) #R2 = 0.8069, F=54.64

# Check Linearity

crPlots(le.fit)


# Check for Extereme Values

le.cutoff <- 4/((nrow(le.train)-length(le.fit$coefficients)-2))
plot(le.fit, which=4, cook.levels=le.cutoff)  
plot(le.fit, which=5, cook.levels=le.cutoff)  

le.train <- le.train[-which(rownames(le.train) %in% c("104", "3", "164")),] 

# Try Model Again

le.fit <- lm(lifeexp ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=le.train)
summary(le.fit) #R2 = 0.8347, F = 64.54

# Check for Extreme Values

le.cutoff <- 4/((nrow(le.train)-length(le.fit$coefficients)-2))
plot(le.fit, which=4, cook.levels=le.cutoff) 
plot(le.fit, which=5, cook.levels=le.cutoff) 

le.train <- le.train[-which(rownames(le.train) %in% c("111", "52", "191")),] 

#Try model Again

le.fit <- lm(lifeexp ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=le.train)
summary(le.fit) #R2 = .8555, F = 74.01

# Check for Extereme Values

le.cutoff <- 4/((nrow(le.train)-length(le.fit$coefficients)-2))
plot(le.fit, which=4, cook.levels=le.cutoff) 
plot(le.fit, which=5, cook.levels=le.cutoff)

le.train <- le.train[-which(rownames(le.train) %in% c("127", "166", "35")),] 


#Try model Again

le.fit <- lm(lifeexp ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=le.train)
summary(le.fit) #R2 = .8653, F = 78.6

# Check for Extereme Values

le.cutoff <- 4/((nrow(le.train)-length(le.fit$coefficients)-2))
plot(le.fit, which=4, cook.levels=le.cutoff)
plot(le.fit, which=5, cook.levels=le.cutoff)

# Check for Multi-Collinearity

vif(le.fit)

# Try Model Again - Remove elec due to high VIF values

le.fit <- lm(lifeexp ~ airpoll+immdpt+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=le.train)
summary(le.fit) #R2 = .8663, F = 86.39

vif(le.fit)

# Try Model Again - Remove sanit due to high VIF values

le.fit <- lm(lifeexp ~ airpoll+immdpt+eduexp+gdp+water+popgr+pov190+primcomp+alit+undernour, data=le.train)
summary(le.fit) #R2 = .8425, F = 78.55

vif(le.fit)

# Try Model Again - Remove undernour due to p value


le.fit <- lm(lifeexp ~ airpoll+immdpt+eduexp+gdp+water+popgr+pov190+primcomp+alit, data=le.train)
summary(le.fit) #R2 = .8436, F = 87.93

vif(le.fit)

# Try Model Again - Remove eduexp due to p value

le.fit <- lm(lifeexp ~ airpoll+immdpt+gdp+water+popgr+pov190+primcomp+alit, data=le.train)
summary(le.fit) #R2 = .8448, F = 99.64

# Try Model Again - Remove alit due to p value

le.fit <- lm(lifeexp ~ airpoll+immdpt+gdp+water+popgr+pov190+primcomp, data=le.train)
summary(le.fit) #R2 = .8443, F = 113.3

# Try Model Again - Remove airpoll due to p value

le.fit <- lm(lifeexp ~ immdpt+gdp+water+popgr+pov190+primcomp, data=le.train)
summary(le.fit) #R2 = .8424, F = 130.2


# Try Model Again - Remove immdpt due to vif value

le.fit <- lm(lifeexp ~ gdp+water+popgr+pov190+primcomp, data=le.train)
summary(le.fit) #R2 = .8414, F = 154.9


# Try Model Again - Remove water due to p value

le.fit <- lm(lifeexp ~ gdp+popgr+pov190+primcomp, data=le.train)
summary(le.fit) #R2 = .8317, F = 180.2

# Model Evaluation

# Check Distribution of Residuals

plot(le.fit)

#Remove Extreme Values

le.train <- le.train[-which(rownames(le.train) %in% c("145", "155")),]

# Try Model Again

le.fit <- lm(lifeexp ~ gdp+popgr+pov190+primcomp, data=le.train)
summary(le.fit) #R2 = .8526, F = 207.7


plot(le.fit)

plot (le.fit$residuals)
hist (le.fit$residuals)



# Create Prediction Sets

le.train$pred.le <- predict(le.fit, newdata = subset(le.train, select=c(lifeexp, primcomp, gdp, popgr,pov190)))
le.valid$pred.le <- predict(le.fit, newdata = subset(le.valid, select=c(lifeexp, primcomp, gdp, popgr,pov190)))

# Check on Training Set

le.train.corr <- round(cor(le.train$pred.le, le.train$lifeexp), 2)
le.train.RMSE <- round(sqrt(mean((le.train$pred.le - le.train$lifeexp)^2)), 2)
le.train.MAE <- round(mean(abs(le.train$pred.le - le.train$lifeexp)), 2)
c(le.train.corr^2, le.train.RMSE, le.train.MAE)  

# Check on Validation Set

le.valid.corr <- round(cor(le.valid$pred.le, le.valid$lifeexp), 2)
le.valid.RMSE <- round(sqrt(mean((le.valid$pred.le - le.valid$lifeexp)^2)), 2)
le.valid.MAE <- round(mean(abs(le.valid$pred.le - le.valid$lifeexp)), 2)
c(le.valid.corr^2, le.valid.RMSE, le.valid.MAE)  


##########################
# Use Death Rate as Target
##########################

# Remove Rows with Missing Target Values

de.regdata <- subset(master.comp, 
                     !(is.na(death)))
rownames(de.regdata) <- NULL

# Remove All Other Targets

de.regdata <- data.frame(de.regdata[2],de.regdata[11:22])
summary (de.regdata)

# Check Predictors for Normal Distributions

pairs.panels(de.regdata [1:13], col="red")

# Transform highly skewed distributions using log10

# de.regdata$airpoll <- log10(de.regdata$airpoll)
# de.regdata$immdpt <- log10(de.regdata$immdpt)
# de.regdata$elec <- log10(de.regdata$elec)
# de.regdata$gdp <- log10(de.regdata$gdp)
# de.regdata$water <- log10(de.regdata$water)
# de.regdata$pov190 <- log10(de.regdata$pov190)
# de.regdata$primcomp <- log10(de.regdata$primcomp)
# de.regdata$sanit <- log10(de.regdata$sanit)
# de.regdata$undernour <- log10(de.regdata$undernour)
# de.regdata$alit <- log10(de.regdata$alit)


# Split into Training and Validation Data Sets
set.seed (1122)
de.training.size <- 0.8 
de.training <- sample.int(length(de.regdata$death), round(length(de.regdata$death) * de.training.size))
de.train <- de.regdata[de.training,]
de.valid <- de.regdata[-de.training,]

# Fit Model using all variables

de.fit <- lm(death ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=de.train)
summary(de.fit) #R2 = 0.4341, F=9.52

# Check Linearity

crPlots(de.fit)

# Check for Extereme Values

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff)   
plot(de.fit, which=5, cook.levels=de.cutoff) 

de.train <- de.train[-which(rownames(de.train) %in% c("2", "56", "110")),] 

# Try Model Again

de.fit <- lm(death ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=de.train)
summary(de.fit) #R2 = 0.4932, F = 11.84 - improvement

# Check for Extereme Values

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff) 

de.train <- de.train[-which(rownames(de.train) %in% c("175")),] 

# Check for Multi-Collinearity

vif(de.fit)

# Try Model Again - Remove elec due to high VIF value

de.fit <- lm(death ~ airpoll+immdpt+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=de.train)
summary(de.fit) #R2 = .4898, F = 12.74

vif(de.fit)

# Try Model Again - Remove sanit due to high VIF value

de.fit <- lm(death ~ airpoll+immdpt+eduexp+gdp+water+popgr+pov190+primcomp+undernour+alit, data=de.train)
summary(de.fit) #R2 = .4441, F = 11.74

vif(de.fit)

# Check for Extereme Values

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)

# Try Model Again - Remove water due to p value


de.fit <- lm(death ~ airpoll+immdpt+eduexp+gdp+popgr+pov190+primcomp+undernour+alit, data=de.train)
summary(de.fit) #R2 = .4438, F = 13.12

# Try Model Again - Remove alit due to p value


de.fit <- lm(death ~ airpoll+immdpt+eduexp+gdp+popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .4432, F = 14.82

# Try Model Again - Remove gdp due to p value


de.fit <- lm(death ~ airpoll+immdpt+eduexp+popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .4409, F = 16.9

# Check for extremes

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)

de.train <- de.train[-which(rownames(de.train) %in% c("200", "135", "18")),]

de.fit <- lm(death ~ airpoll+immdpt+eduexp+popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .4903, F = 20.2

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)

de.train <- de.train[-which(rownames(de.train) %in% c("141", "203", "153")),]

de.fit <- lm(death ~ airpoll+immdpt+eduexp+popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .5193, F = 22.23

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)


vif (de.fit)

# Try Model Again - Remove immdpt due to p value

de.fit <- lm(death ~ airpoll+eduexp+popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .5193, F = 26.11

# Try Model Again - Remove airpoll due to p value

de.fit <- lm(death ~ eduexp+popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .51512, F = 31.03

# Try Model Again - Remove eduexp due to p value

de.fit <- lm(death ~ popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .5031, F = 37.2

# Check for Extremes

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)

de.train <- de.train[-which(rownames(de.train) %in% c("202", "64", "149")),]

# Try Model Again

de.fit <- lm(death ~ popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .5336, F = 41.19

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)

de.train <- de.train[-which(rownames(de.train) %in% c("176", "97", "117")),]

de.fit <- lm(death ~ popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .5422, F = 41.74

de.cutoff <- 4/((nrow(de.train)-length(de.fit$coefficients)-2))
plot(de.fit, which=4, cook.levels=de.cutoff) 
plot(de.fit, which=5, cook.levels=de.cutoff)

de.train <- de.train[-which(rownames(de.train) %in% c("37", "19", "72")),]

de.fit <- lm(death ~ popgr+pov190+primcomp+undernour, data=de.train)
summary(de.fit) #R2 = .5356, F = 39.79

# Try Model Again - Remove undernour due to p value

de.fit <- lm(death ~ popgr+pov190+primcomp, data=de.train)
summary(de.fit) #R2 = .5665, F = 59.25

vif (de.fit)

# Model Evaluation

# Check Distribution of Residuals

plot(de.fit)

# Remove Extremes

de.train <- de.train[-which(rownames(de.train) %in% c("23", "156", "120")),]

de.fit <- lm(death ~ popgr+pov190+primcomp, data=de.train)
summary(de.fit) #R2 = .5665, F = 59.25

plot(de.fit)

plot (de.fit$residuals)
hist (de.fit$residuals)

# Create Prediction Sets

de.train$pred.le <- predict(de.fit, newdata = subset(de.train, select=c(death,pov190,popgr,primcomp)))
de.valid$pred.le <- predict(de.fit, newdata = subset(de.valid, select=c(death,pov190,popgr,primcomp)))

# Check on Training Set

de.train.corr <- round(cor(de.train$pred.le, de.train$death), 2)
de.train.RMSE <- round(sqrt(mean((de.train$pred.le - de.train$death)^2)), 2)
de.train.MAE <- round(mean(abs(de.train$pred.le - de.train$death)), 2)
c(de.train.corr^2, de.train.RMSE, de.train.MAE) # 0.5625 1.7500 1.4400

# Check on Validation Set

de.valid.corr <- round(cor(de.valid$pred.le, de.valid$death), 2)
de.valid.RMSE <- round(sqrt(mean((de.valid$pred.le - de.valid$death)^2)), 2)
de.valid.MAE <- round(mean(abs(de.valid$pred.le - de.valid$death)), 2)
c(de.valid.corr^2, de.valid.RMSE, de.valid.MAE)  # 0.3364 2.7800 2.0800

##########################
# Use Diab as Target
##########################

# Remove Rows with Missing Target Values

diab.regdata <- subset(master.comp, 
                     !(is.na(diab)))
rownames(diab.regdata) <- NULL

# Remove All Other Targets

diab.regdata <- data.frame(diab.regdata[9],diab.regdata[11:22])
summary (diab.regdata)

# Check Predictors for Normal Distributions

pairs.panels(diab.regdata [1:13], col="red")

# Transform highly skewed distributions using log10

#diab.regdata$airpoll <- log10(diab.regdata$airpoll)
#diab.regdata$immdpt <- log10(diab.regdata$immdpt)
#diab.regdata$elec <- log10(diab.regdata$elec)
#diab.regdata$gdp <- log10(diab.regdata$gdp)
#diab.regdata$water <- log10(diab.regdata$water)
#diab.regdata$pov190 <- log10(diab.regdata$pov190)
#diab.regdata$primcomp <- log10(diab.regdata$primcomp)
#diab.regdata$sanit <- log10(diab.regdata$sanit)
#diab.regdata$undernour <- log10(diab.regdata$undernour)
#diab.regdata$alit <- log10(diab.regdata$alit)

#summary(diab.regdata$airpoll)
#pairs.panels(diab.regdata [2:13], col="red")

# Split into Training and Validation Data Sets
set.seed (5432)
diab.training.size <- 0.8 
diab.training <- sample.int(length(diab.regdata$diab), round(length(diab.regdata$diab) * diab.training.size))
diab.train <- diab.regdata[diab.training,]
diab.valid <- diab.regdata[-diab.training,]

# Fit Model using all variables

diab.fit <- lm(diab ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=diab.train)
summary(diab.fit) #R2 = 0.2818, F=4.9

# Check Linearity

crPlots(diab.fit)


# Check for Extereme Values

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff) 

diab.train <- diab.train[-which(rownames(diab.train) %in% c("146", "138", "96")),] 

# Try Model Again

diab.fit <- lm(diab ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+sanit+undernour+alit, data=diab.train)
summary(diab.fit) #R2 = .3359, F = 6.19

# Check for Extereme Values

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff) #110,141,120
plot(diab.fit, which=5, cook.levels=diab.cutoff) 


# Check for Multi-Collinearity

vif(diab.fit)

# Try Model Again - Remove sanit due to high VIF values

diab.fit <- lm(diab ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+pov190+primcomp+undernour+alit, data=diab.train)
summary(diab.fit) #R2 = .2861, F = 5.392

vif(diab.fit)

# Try Model Again - Remove pov190 due to high VIF values

diab.fit <- lm(diab ~ airpoll+immdpt+elec+eduexp+gdp+water+popgr+primcomp+undernour+alit, data=diab.train)
summary(diab.fit) #R2 = .263, F = 5.317

vif(diab.fit)

# Try Model Again - Remove elec due to p value

diab.fit <- lm(diab ~ airpoll+immdpt+eduexp+gdp+water+popgr+primcomp+undernour+alit, data=diab.train)
summary(diab.fit) #R2 = .2616, F = 5.906

# Try Model Again - Remove unernour due to p value


diab.fit <- lm(diab ~ airpoll+immdpt+eduexp+gdp+water+popgr+primcomp+alit, data=diab.train)
summary(diab.fit) #R2 = .2602, F = 6.63

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff) 

diab.train <- diab.train[-which(rownames(diab.train) %in% c("153", "157", "99")),]

# Try Model Again 

diab.fit <- lm(diab ~ airpoll+immdpt+eduexp+gdp+water+popgr+primcomp+alit, data=diab.train)
summary(diab.fit) #R2 = .2408, F = 5.869

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff) 

diab.train <- diab.train[-which(rownames(diab.train) %in% c("6", "19", "24")),]


# Try Model Again 

diab.fit <- lm(diab ~ airpoll+immdpt+eduexp+gdp+water+popgr+primcomp+alit, data=diab.train)
summary(diab.fit) #R2 = .2921, F = 7.47

# Try Model Again - Remove eduexp due to p value

diab.fit <- lm(diab ~ airpoll+immdpt+gdp+water+popgr+primcomp+alit, data=diab.train)
summary(diab.fit) #R2 = .2906, F = 8.54

# Try Model Again - Remove primcomp due to p value

diab.fit <- lm(diab ~ airpoll+immdpt+gdp+water+popgr+alit, data=diab.train)
summary(diab.fit) #R2 = .2833, F = 9.68

vif (diab.fit)

# Check for extreme

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff) 

diab.train <- diab.train[-which(rownames(diab.train) %in% c("194", "140", "167")),]

diab.fit <- lm(diab ~ airpoll+immdpt+gdp+water+popgr+alit, data=diab.train)
summary(diab.fit) #R2 = .3169, F = 11.13

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff) 

diab.train <- diab.train[-which(rownames(diab.train) %in% c("3", "35", "105")),]

diab.fit <- lm(diab ~ airpoll+immdpt+gdp+water+popgr+alit, data=diab.train)
summary(diab.fit) #R2 = .3534, F = 12.84

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff) 

diab.train <- diab.train[-which(rownames(diab.train) %in% c("127", "17", "198")),]
diab.fit <- lm(diab ~ airpoll+immdpt+gdp+water+popgr+alit, data=diab.train)
summary(diab.fit) #R2 = .3874, F = 14.55

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff)

diab.train <- diab.train[-which(rownames(diab.train) %in% c("70", "82", "44")),]
diab.fit <- lm(diab ~ airpoll+immdpt+gdp+water+popgr+alit, data=diab.train)
summary(diab.fit) #R2 = .4176, F = 16.13

diab.cutoff <- 4/((nrow(diab.train)-length(diab.fit$coefficients)-2))
plot(diab.fit, which=4, cook.levels=diab.cutoff)  
plot(diab.fit, which=5, cook.levels=diab.cutoff)

# Try model again - remove immdpt due to P value

diab.fit <- lm(diab ~ airpoll+gdp+water+popgr+alit, data=diab.train)
summary(diab.fit) #R2 = .4105, F = 18.94

# Try model again - remove alit due to P value

diab.fit <- lm(diab ~ airpoll+gdp+water+popgr, data=diab.train)
summary(diab.fit) #R2 = .4033, F = 23.15

# Try model again - remove popgr due to P value

diab.fit <- lm(diab ~ airpoll+gdp+water, data=diab.train)
summary(diab.fit) #R2 = .3854, F = 28.85

# Model Evaluation

# Check Distribution of Residuals

plot(diab.fit)

diab.train <- diab.train[-which(rownames(diab.train) %in% c("145", "74", "129")),]
diab.fit <- lm(diab ~ airpoll+gdp+water, data=diab.train)
summary(diab.fit) #R2 = .405, F = 30.63

plot(diab.fit)

diab.train <- diab.train[-which(rownames(diab.train) %in% c("145", "74", "129")),]
diab.fit <- lm(diab ~ airpoll+gdp+water, data=diab.train)
summary(diab.fit) #R2 = .405, F = 30.63

plot (diab.fit$residuals)
hist (diab.fit$residuals)

# Create Prediction Sets

diab.train$pred.le <- predict(diab.fit, newdata = subset(diab.train, select=c(diab,airpoll,gdp,water)))
diab.valid$pred.le <- predict(diab.fit, newdata = subset(diab.valid, select=c(diab,airpoll,gdp,water)))

# Check on Training Set

diab.train.corr <- round(cor(diab.train$pred.le, diab.train$diab), 2)
diab.train.RMSE <- round(sqrt(mean((diab.train$pred.le - diab.train$diab)^2)),2)
diab.train.MAE <- round(mean(abs(diab.train$pred.le - diab.train$diab)),2)
c(diab.train.corr^2, diab.train.RMSE, diab.train.MAE) # 0.4096 2.6600 2.1000

# Check on Validation Set

diab.valid.corr <- round(cor(diab.valid$pred.le, diab.valid$diab), 2)
diab.valid.RMSE <- round(sqrt(mean((diab.valid$pred.le - diab.valid$diab)^2)),2)
diab.valid.MAE <- round(mean(abs(diab.valid$pred.le - diab.valid$diab)),2)
c(diab.valid.corr^2, diab.valid.RMSE, diab.valid.MAE) # 0.2401 4.8800 4.0200

summary (diab)
sd (diab, na.rm=TRUE)



######################################################
###  Section 4 - Additional Insights
######################################################

# Explore Health Expenditure vs Targets

hecap <- read.csv(file="a1data/hecap.csv", skip=4,header=TRUE)
hegov <- read.csv(file="a1data/hegov.csv", skip=4,header=TRUE)
hegdp <- read.csv(file="a1data/hegdp.csv", skip=4,header=TRUE)
hedocs <- read.csv(file="a1data/hedocs.csv", skip=4,header=TRUE)
henurses <- read.csv(file="a1data/henurses.csv", skip=4,header=TRUE)
hebeds <- read.csv(file="a1data/hebeds.csv", skip=4,header=TRUE)
helifeexp <- read.csv(file="a1data/lifeexp.csv", skip=4,header=TRUE)
hedisease <- read.csv(file="a1data/disease.csv", skip=4,header=TRUE)
hetb <- read.csv(file="a1data/tb.csv", skip=4,header=TRUE)
hedeath <- read.csv(file="a1data/death.csv", skip=4,header=TRUE)
helowbw <- read.csv(file="a1data/lowbb.csv", skip=4,header=TRUE)
hechildmort <- read.csv(file="a1data/childmort.csv", skip=4,header=TRUE)
hegeoloc <- read.csv(file="a1data/geoloc.csv", header=TRUE)

hecountry <- helifeexp$Country.Name
hegov <- hegov$X2014
hecap <- hecap$X2014
hegdp <- hegdp$X2014
hedocs <- rowMeans(hedocs[,50:59], na.rm = TRUE)
henurses <- rowMeans(henurses[,50:59], na.rm = TRUE)
hebeds <- rowMeans(hebeds[,50:59], na.rm = TRUE)
helifeexp <- helifeexp$X2014
hedisease <- hedisease$X2012
hetb <- hetb$X2014
hedeath <- rowMeans(hedeath[,50:59], na.rm = TRUE)
hechildmort <- hechildmort$X2015
helat <- hegeoloc$Lat
helong <- hegeoloc$Long

hemaster <- data.frame(hecountry, helat,helong, hegov, hegdp, hecap, hebeds, hedocs, henurses, helifeexp,hedisease,hetb,hedeath,hechildmort)

# Remove Non-Countries (with no Lat/Long Listings)

hemaster <- hemaster[complete.cases(hemaster[,2:3]),]
rownames(hemaster) <- NULL 

# Rename Variables

hecountry <- hemaster$hecountry
hegov <- hemaster$hegov
hecap <- hemaster$hecap
hegdp <- hemaster$hegdp
hedocs <- hemaster$hedocs
henurses <- hemaster$henurses
hebeds <- hemaster$hebeds
helifeexp <- hemaster$helifeexp
hedisease <- hemaster$hedisease
hetb <- hemaster$hetb
hedeath <- hemaster$hedeath
hechildmort <- hemaster$hechildmort
helat <- hemaster$helat
helong <- hemaster$helong

hemaster.cor <- cor (hemaster[4:13], use="pairwise.complete.obs", method="pearson")
round (hemaster.cor, digits=3)
corrplot (hemaster.cor, method = c("number"), number.digits=2, type="lower", diag = TRUE)

# Explore HECap vs Predictors

hecap.pred <- data.frame (hecap, hedocs, pov190, popgr, gdp, primcomp, immdpt )
hecap.cor <- cor (hecap.pred, use="pairwise.complete.obs", method="pearson")
round (hecap.cor, digits=3)
corrplot (hecap.cor, method = c("number"), number.digits=2, type="lower", diag = TRUE)
