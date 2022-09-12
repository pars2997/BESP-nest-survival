# Load Packages
library(tidyverse)
library(lubridate)
library(jagsUI)
library(ggplot2)


# Load data
setwd("C:/Users/A02323599/Dropbox/IWS/Nest Survival/BESP/DoItAgain")

precip <- read.csv("./Data/precip.csv")
substrate <- read.csv("./Data/BESPSubstrate.csv")
breedseas <- read.csv("./Data/BreedingSeason.csv")
dat <- read.csv("./Data/BESPsurv_proofed_SAM_padded.csv")
nestinit <- read.csv("./Data/NestInitiation.csv")

#Make common column and combine nest initiation dates
#Note - if this is failing (i.e. leaving NAs) check the main data file
#To work properly, the nest ID must be padded to three digits in the excel file
dat$nest.year <- paste0(dat$Year,"_",dat$Nest)
nestinit$nest.year <- paste0(nestinit$Year,"_",nestinit$Nest.ID)

dat <- dat %>% 
  as_tibble() %>% 
  left_join(nestinit, "nest.year") %>% 
  filter(Camera..Status == 1) %>% 
  select(Date, Year.x, Nest, Fate, Nest.Status,Nest.Stage,Cause,Cause2,
         nest.year,Egg.Laying.Date,Incubation.Date) %>% 
  mutate(Date = as.Date(Date,format = ("%m/%d/%Y"))) %>% 
  mutate(Egg.Laying.Date = as.Date(Egg.Laying.Date,format = ("%m/%d/%Y"))) %>% 
  mutate(Incubation.Date = as.Date(Incubation.Date,format = ("%m/%d/%Y"))) %>% 
  rename(Year = Year.x)

#Create a list of unique nests
uni.nests <- unique(dat$nest.year)
interval <- list()

#Calculate the interval between checks for each check
#Vast majority are 1, but if camera malfunctioned may be longer
for(i in 1:length(uni.nests)){
  temp <- dat %>% 
    filter(nest.year == uni.nests[i])
  temp2 <- vector()
  if(nrow(temp)>1){
    for(j in 2:nrow(temp)){
      temp2[j] <- as.numeric(temp$Date[j] - temp$Date[j-1]) 
    }
    interval[[i]] <- temp2
  }else{interval[[i]] <- NA}
}

#NAs are just the first day of each camera, replace with 1
dat$interval <- unlist(interval)
dat <- dat %>% 
  mutate(interval = case_when(
    is.na(interval) ~ 1,
    TRUE ~ interval
  ))

#Add substrate and breeding season length data to dataframe
substrate <- substrate %>% 
  mutate(nest.year = paste0(substrate$Year,"_",substrate$Nest))
breedseas <- breedseas %>% 
  mutate(Start = as.Date(Start, format = ("%m/%d/%Y")),
         End = as.Date(End, format = ("%m/%d/%Y")),
         Mid = as.Date(Mid, format = ("%m/%d/%Y"))) %>% 
  as_tibble()

dat <- dat %>% 
  left_join(precip, by = "Year") %>% 
  left_join(substrate, by = "nest.year") %>% 
  rename(Year = Year.x) %>% 
  left_join(breedseas,by = "Year")

#Create new columns for the day of the season, age of the nest, and initiation
#day of season for each monitored dat
dat$DOS <- as.numeric(dat$Date - dat$Start)
dat$age <- as.numeric(dat$Date - dat$Egg.Laying.Date) 
dat$initDOS <- as.numeric(dat$Egg.Laying.Date - dat$Start)

#Add a column for fate that is 0 for success and cause of mortality for failures
dat <- dat %>% 
  select(Date,Year,nest.year,Nest.Status,Nest.Stage,Cause,
         Cause2,interval,Precip,Substrate2,DOS,age,initDOS) %>% 
  mutate(Fate = case_when(
    Nest.Status == 0 ~ "0",
    Nest.Status == 1 ~ dat$Cause2
  ))

#Recode column to be numeric: 1 = survive, 2 = native predator
#3 = invasive predator, 4 = other
dat <- dat %>% 
  mutate(Fate = case_when(
    Fate == 0 ~ "1",
    Fate == "Native" ~ "2",
    Fate == "Invasive" ~ "3",
    Fate == "Other" ~ "4"
  )) %>% 
  #Add 1/0 column for each year
  mutate(Y12 = as.numeric(Year == 2012),
         Y13 = as.numeric(Year == 2013),
         Y14 = as.numeric(Year == 2014),
         Y15 = as.numeric(Year == 2015),
         Y16 = as.numeric(Year == 2016),
         Y17 = as.numeric(Year == 2017),
         Y18 = as.numeric(Year == 2018),
         Y19 = as.numeric(Year == 2019)) %>% 
  #Add 1/0 columns for substrate
  mutate(Boxthorn = as.numeric(Substrate2 == "Boxthorn"),
         Sagebrush = as.numeric(Substrate2 == "Artemesia"),
         Other = as.numeric(Substrate2 == "Other")) %>% 
  #Trim to needed columns
  select(nest.year,Date,Fate,DOS,age,initDOS,Nest.Stage,interval,Precip,
         Y12,Y13,Y14,Y15,Y16,Y17,Y18,Y19,Boxthorn,Sagebrush,Other,Year) %>% 
  #Make year 1-8 for easier coding
  mutate(Year = Year-2011) %>% 
  #Scale variables for modelconvergence
  mutate(DOSscale = as.numeric(scale(initDOS))) %>% 
  mutate(DOS2 = DOSscale^2) %>% 
  mutate(Precipscale = as.numeric(scale(Precip)))

# equals <- rep(NA, nrow(dat))
# for (i in 2:length(equals)){
#   equals[i] <- dat$nest.year[i] == dat$nest.year[i-1]
# }
# equals[1] = FALSE
# dat$equals <- equals
# # nestdata <- subset(dat,equals == TRUE)
nestdata <- dat

n <- length(nestdata$interval)
Surv <- rep(0,n)
Native <- rep(0,n)
Invasive <- rep(0,n)
Other <- rep(0,n)

for (i in 1:n){
  Surv[i][nestdata$Fate[i] == 1]  <- 1
  Native[i][nestdata$Fate[i] == 2] <- 1
  Invasive[i][nestdata$Fate[i] == 3] <- 1
  Other[i][nestdata$Fate[i] == 4] <- 1
}
Fate <- cbind(Surv,Native,Invasive,Other)

#Create survival column for each check, 1 = alive, 0 = failed
nestdata <- nestdata %>% 
  mutate(Surv = case_when(
    Fate == "1" ~ 1,
    Fate == "2" ~ 0,
    Fate == "3" ~ 0,
    Fate == "4" ~ 0
  ))
#Create drought column
nestdata <- nestdata %>% 
  mutate(drought = case_when(
    Year%in% c(2,3,4,7) ~ 1,
    Year %in% c(1,5,6,8) ~ 0
  ))



#### Null Model ####

sink("LogExp_model.txt")
cat(
  "
model{
  #priors
  alpha.a ~ dnorm(0,0.01)

  
  #random effect for years, with effect of precipitation
  for(i in 1:nYear){
  eta.y[i] ~ dnorm(0, tau.y)
  beta.year[i] <- eta.y[i]
  }

  #hyperparameters for random effect
  tau.y <- 1/(pow(sigma.y,2))
  sigma.y ~ dunif(0,50)
  
  #survival model
  for(i in 1:n){
  #daily survival estimate is on logit scale
  logit(S[i]) <- alpha.a + 
                    beta.year[year[i]]
  #success/failure on a given day is bernoulli trial of survival estimate
  surv[i] ~ dbern(S[i]^interval[i])
  }
    # x is estimated null daily survival
    x<-exp(alpha.a)/(exp(alpha.a)+1)
    # csurv is estimated nest success over 23 days
    csurv<-pow(x,23)

}
    ",fill = T)
sink()

#package data for analysis in JAGS
win.data.sc <- list(n = nrow(nestdata),    
                    interval = as.numeric(nestdata$interval),
                    surv = nestdata$Surv,
                    year = nestdata$Year,
                    nYear = nrow(precip))

#define function to draw initial values for MCMC chains
inits <- function() {list(alpha.n = rnorm(1, 0, 1),
                          sigma.y = rlnorm(1))}

#list parameters to monitor
params <- c("sigma.y","alpha","beta.year",
            "alpha.a","x","csurv")

ni <- 50000
nb <- 20000
nt <- 3
nc <- 3

nullmodel.fit <- jags(win.data.sc, inits, params, "LogExp_model.txt", 
                  n.iter=ni, n.thin=nt, n.burnin=nb, n.chains=nc, parallel=TRUE)

nullmodel.fit

# saveRDS(nullmodel.fit,file = "./Code/NullLogExp")

nullmodel.fit <- readRDS("./Code/NullLogExp")

#### Base Nest Survival Model ####
sink("LogExp_model.txt")
cat(
  "
model{
  #priors
  alpha.a ~ dnorm(0,0.01)
  beta.pre ~ dnorm(0,0.01)
  beta.dos ~ dnorm(0,0.01)
  beta.dos2 ~ dnorm(0,0.01)
  beta.sage ~ dnorm(0,0.01)
  beta.other ~ dnorm(0,0.01)

  
  #random effect for years, with effect of precipitation
  for(i in 1:nYear){
  eta.y[i] ~ dnorm(0, tau.y)
  beta.year[i] <- beta.pre*pre[i] + eta.y[i]
  }

  #hyperparameters for random effect
  tau.y <- 1/(pow(sigma.y,2))
  sigma.y ~ dunif(0,50)
  
  #survival model
  for(i in 1:n){
  #daily survival estimate is on logit scale
  logit(S[i]) <- alpha.a + 
                    beta.dos*dos[i] +
                    beta.dos2*dos2[i] +
                    beta.sage*sage[i] +
                    beta.other*other[i] +
                    beta.year[year[i]]
  #success/failure on a given day is bernoulli trial of survival estimate
  surv[i] ~ dbern(S[i]^interval[i])
  }
    # x is estimated null daily survival
    x<-exp(alpha.a)/(exp(alpha.a)+1)
    xsage<-exp(alpha.a + beta.sage)/(exp(alpha.a + beta.sage)+1)
    xother<-exp(alpha.a + beta.other)/(exp(alpha.a + beta.other)+1)
    # csurv is estimated nest success over 23 days
    csurv<-pow(x,23)

}
    ",fill = T)
sink()

#package data for analysis in JAGS
win.data.sc <- list(pre = as.numeric(scale(precip$Precip)),
                    dos = nestdata$DOSscale,
                    dos2 = nestdata$DOS2,
                    n = nrow(nestdata),    
                    interval = as.numeric(nestdata$interval),
                    surv = nestdata$Surv,
                    year = nestdata$Year,
                    nYear = nrow(precip),
                    sage = nestdata$Sagebrush,
                    other = nestdata$Other)

#define function to draw initial values for MCMC chains
inits <- function() {list(alpha.n = rnorm(1, 0, 1),
                          sigma.y = rlnorm(1),
                          beta.pre = rnorm(1,0,1),
                          beta.dos = rnorm(1,0,1),
                          beta.dos2 = rnorm(1,0,1),
                          beta.sage = rnorm(1,0,1),
                          beta.other = rnorm(1,0,1))}

#list parameters to monitor
params <- c("sigma.y","alpha","beta.pre","beta.dos",
            "beta.dos2","beta.year","beta.sage","beta.other",
            "alpha.a","x","xsage","xother","csurv")

ni <- 50000
nb <- 20000
nt <- 3
nc <- 3

model.fit <- jags(win.data.sc, inits, params, "LogExp_model.txt", 
                    n.iter=ni, n.thin=nt, n.burnin=nb, n.chains=nc, parallel=TRUE)

model.fit
saveRDS(model.fit,file = "./Code/LogExpResults")

model.fit <- readRDS("./Code/LogExpResults")

params <- c('sigma','pre','dos','dos2','2012','2013','2014','2015',
            '2016','2017','2018','2019','sage','other','alpha','box dsr','sagedsr','otherdsr','csr','dev')
probs <- c(0.025,0.05,0.1,0.5,0.9,0.95,0.975)
CIs <- matrix(NA,nrow = length(params), ncol = length(probs))
for(i in 1:length(params)){
  CIs[i,] <- round(as.numeric(quantile(c(model.fit$samples[[1]][,i],model.fit$samples[[2]][,i],
             model.fit$samples[[3]][,i]),probs = probs)),4)
}

CIs <- data.frame(CIs)
CIs$param <- params
colnames(CIs) <- c(as.character(probs),"parameter")
CIs <- CIs[,c(8,1:7)]
CIs$overlap0_90 <- CIs$`0.05` * CIs$`0.95` < 0 
CIs
CIs <- CIs[,c(1,5,3,7,9)]

colnames(CIs) <- c("Parameter", "Median", "90% CI Lower", "90% CI Upper","Overlap_0")
write.csv(CIs, file = "LogExp_CIs.csv")


pdf("basemodelplots.pdf")
plot(model.fit)
dev.off()


#### Competing Causes Model Specification ####

sink("BUGS_model_subset_select.txt ") 
cat("
model {
  #HyperPriors
   for (i in 1:nYear){
    eta.s[i] ~ dnorm(0, tau)   #year effect

    
    beta.year.n[i] <- beta.pre.n*pre[i] + eta.s[i]
    beta.year.i[i] <- beta.pre.i*pre[i] + eta.s[i]
    beta.year.o[i] <- beta.pre.o*pre[i] + eta.s[i]
    }
  #Precision hyperparameters
    tau <- 1/(pow(sigma.n,2))
  
    beta.pre.n ~ dnorm(0,0.01)
    beta.pre.i ~ dnorm(0,0.01)
    beta.pre.o ~ dnorm(0,0.01)

  #Standard Deviation hyperparameters
    sigma.n ~ dunif(0,15)

 # Priors for native sources of mortality
    alpha.n ~ dnorm(0, 0.01) #intercept
    beta.n.dos ~ dnorm(0, 0.01) #dos slope
    beta.n.dos2 ~ dnorm(0, 0.01) #dos2 slope
    beta.n.sage ~ dnorm(0,0.01)
    beta.n.other ~ dnorm(0,0.01)

 #priors for invasive
    alpha.i ~ dnorm(0, 0.01)  #intercept
    beta.i.dos ~ dnorm(0, 0.01) #dos slope
    beta.i.dos2 ~ dnorm(0, 0.01) #dos2 slope
    beta.i.sage ~ dnorm(0,0.01)
    beta.i.other ~ dnorm(0,0.01)

   #priors for other
    alpha.o ~ dnorm(0,0.01)  #intercept
    beta.o.dos ~ dnorm(0, 0.01) #dos slope
    beta.o.dos2 ~ dnorm(0, 0.01) #dos2 slope
    beta.o.sage ~ dnorm(0,0.01)
    beta.o.other ~ dnorm(0,0.01)
   
 # Likelihood
  for (i in 1:n) {
   #linear predictors (Equation 2)
   #other: 
    cto[i] <- exp(alpha.o + beta.year.o[year[i]]
                    + beta.o.dos*dos[i]
                    + beta.o.dos2*dos2[i]
                    + beta.o.sage*sage[i]
                    + beta.o.other*other[i])

   #native:
    ctn[i] <- exp(alpha.n + beta.year.n[year[i]] 
                    + beta.n.dos*dos[i]
                    + beta.n.dos2*dos2[i]
	                	+ beta.n.sage*sage[i]
                    + beta.n.other*other[i])

   #invasive:
    cti[i] <- exp(alpha.i + beta.year.i[year[i]]
                    + beta.i.dos*dos[i]
                    + beta.i.dos2*dos2[i]
	                	+ beta.i.sage*sage[i]
                    + beta.i.other*other[i])
    cts[i] <- 1

   #Equation 5
    den[i] <- cto[i] + ctn[i] + cti[i] + cts[i]  
    survp[i] <- cts[i]/den[i]
#interval nest loss probabilities (Equation 4)
#other: 
p[i,4] <- ((cto[i]/(den[i]))/(1 - survp[i]))*(1 - pow(survp[i], interval[i])) 

#invasive
p[i,3] <- ((cti[i]/(den[i]))/(1 - survp[i]))*(1 - pow(survp[i], interval[i])) 
 
#native
p[i,2] <- ((ctn[i]/(den[i]))/(1 - survp[i]))*(1 - pow(survp[i], interval[i])) 

#interval survival probability 
p[i,1] <- pow(survp[i],interval[i])

#Equation 1    
Fate[i,1:4] ~ dmulti(p[i,] , 1 )
    
}
  ctn.a <- exp(alpha.n)
  cti.a <- exp(alpha.i)
  cto.a <- exp(alpha.o)
  den.a <- (ctn.a + cti.a + cto.a + 1)
  
  Surv <- 1/den.a
  Native <- ctn.a/den.a
  Invasive <- cti.a/den.a
  Other <- cto.a/den.a
  csurv<-pow(Surv,23)
  cnat<-pow((1-Native),23)
  cinv<-pow((1-Invasive),23)
  coth<-pow((1-Other),23)
    }
    
", fill=T)
sink()



#package data for analysis in JAGS
win.data.sc <- list(pre = as.numeric(scale(precip$Precip)), n = nrow(nestdata),    
                    year = as.numeric(nestdata$Year),
                    nYear = length(unique(nestdata$Year)),
                    interval = as.numeric(nestdata$interval),
                    Fate = Fate, 
                    dos = nestdata$DOSscale,
                    dos2 = nestdata$DOS2,
                    sage = nestdata$Sagebrush,
                    other = nestdata$Other)

#define function to draw initial values for MCMC chains
inits <- function() {list(alpha.n = rnorm(1, 0, 1),
                          alpha.i =   rnorm(1, 0, 1),
                          alpha.o = rnorm(1,0,1),
                          sigma.n = rlnorm(1),
                          beta.pre.n = rnorm(1,0,1),
                          beta.n.dos = rnorm(1,0,1),
                          beta.n.dos2 = rnorm(1,0,1),
                          beta.n.sage = rnorm(1,0,1),
                          beta.n.other = rnorm(1,0,1),
                          beta.pre.i = rnorm(1,0,1),
                          beta.i.dos = rnorm(1,0,1),
                          beta.i.dos2 = rnorm(1,0,1),
                          beta.i.sage = rnorm(1,0,1),
                          beta.i.other = rnorm(1,0,1),
                          beta.pre.o = rnorm(1,0,1),
                          beta.o.dos = rnorm(1,0,1),
                          beta.o.dos2 = rnorm(1,0,1),
                          beta.o.sage = rnorm(1,0,1),
                          beta.o.other = rnorm(1,0,1))}

#list parameters to monitor
params <- c("alpha.n", "alpha.i", "alpha.o",
            "beta.pre.n","beta.n.dos","beta.n.dos2",
            "beta.pre.i","beta.i.dos","beta.i.dos2",
            "beta.pre.o","beta.o.dos","beta.o.dos2",
            "beta.n.sage","beta.i.sage","beta.o.sage",
            "beta.n.other","beta.i.other","beta.o.other",
            "Surv","Native","Invasive","Other","csurv",
            "cnat","cinv","coth")

ni <- 50000
nb <- 20000
nt <- 3
nc <- 3

csm.output <- jags(win.data.sc, inits, params, "BUGS_model_subset_select.txt", 
                    n.iter=ni, n.thin=nt, n.burnin=nb, n.chains=nc, parallel=TRUE)

csm.output

pdf("competingcausesplots-year.pdf")
plot(csm.output)
dev.off()

saveRDS(csm.output,file = "./Code/CSM_Model")

FullCSM <- readRDS("./Code/CSM_Model")

params <- c('alpha.n','alpha.i','alpha.o','pre.n','dos.n','dos2.n',
            'pre.i','dos.i','dos2.i','pre.o','dos.o','dos2.o','sage.n',
            'sage.i','sage.o','other.n','other.i','other.o','Surv','Native','Invasive',
            'Other','csurv','cnat','cinv','coth','dev')

probs <- c(0.5,0.05,0.95)
CIs <- matrix(NA,nrow = length(params), ncol = length(probs))
for(i in 1:length(params)){
  CIs[i,] <- round(as.numeric(quantile(c(FullCSM$samples[[1]][,i],FullCSM$samples[[2]][,i],
                                   FullCSM$samples[[3]][,i]),probs = probs)),4)
}



CIs <- data.frame(CIs)
CIs$param <- params
colnames(CIs) <- c(as.character(probs),"parameter")
CIs <- CIs[,c(4,1:3)]
CIs$overlap0_90 <- CIs$`0.05` * CIs$`0.95` < 0 
CIs
colnames(CIs) <- c("Parameter", "Median", "90% CI Lower", "90% CI Upper","Overlap_0")
# write.csv(CIs, file = "FullCSM_CIs.csv")



native <- 0.023
invasive <- 0.008
other <- 0.013
day <- 1:23

native_cum <- vector()
for(i in 1:23){
  native_cum[i] <- (1-native)^i
}

invasive_cum <- vector()
for(i in 1:23){
  invasive_cum[i] <- (1-invasive)^i
}

other_cum <- vector()
for(i in 1:23){
  other_cum[i] <- (1-other)^i
}

seasonsurvse <- function(dsr,se,slength){
  seasonse <- slength * se * sqrt(dsr ^ (2*slength-2))
  return(seasonse)
}


plotdat <- data.frame(cbind(day,native_cum,invasive_cum,other_cum))
plotdat$native_l <- NA
plotdat$native_u <- NA
plotdat$invasive_l <- NA
plotdat$invasive_u <- NA
plotdat$other_l <- NA
plotdat$other_u <- NA


for(i in 1:23){
  tempN <- (seasonsurvse((1-FullCSM$mean$Native),FullCSM$sd$Native,i))
  tempI <- (seasonsurvse((1-FullCSM$mean$Invasive),FullCSM$sd$Invasive,i))
  tempO <- (seasonsurvse((1-FullCSM$mean$Other),FullCSM$sd$Other,i))
  
  plotdat$native_l[i] <- plotdat$native_cum[i] - tempN
  plotdat$native_u[i] <- min(plotdat$native_cum[i] + tempN,1)
  plotdat$invasive_l[i] <- plotdat$invasive_cum[i] - tempI
  plotdat$invasive_u[i] <- min(plotdat$invasive_cum[i] + tempI,1)
  plotdat$other_l[i] <- plotdat$other_cum[i] - tempO
  plotdat$other_u[i] <- min(plotdat$other_cum[i] + tempO,1)
  
}

plotdatN <- plotdat[,c(1,2,5,6)]
plotdatN$cause <- "Native"
colnames(plotdatN) <- c("Day","Mean","Lower","Upper","Cause")
plotdatI <- plotdat[,c(1,3,7,8)]
plotdatI$cause <- "Non-native"
colnames(plotdatI) <- c("Day","Mean","Lower","Upper","Cause")
plotdatO <- plotdat[,c(1,4,9,10)]
plotdatO$cause <- "Other"
colnames(plotdatO) <- c("Day","Mean","Lower","Upper","Cause")

plotdat2 <- rbind(plotdatN,plotdatI,plotdatO)
plotdat2$Cause <- as.factor(plotdat2$Cause)
plotdat2$Cause <- ordered(plotdat2$Cause, levels = c("Native","Non-native","Other"))



cause.plot <- ggplot(data = plotdat2,mapping = aes(x = Day, y = Mean, col = Cause))+
  geom_line(size = 1.5)+
  geom_ribbon(linetype = "dashed", aes(ymin = Lower, ymax = Upper, fill = Cause),alpha = 0.1)+
  scale_fill_manual(values = c("gray10","gray40","gray70"))+
  scale_color_manual(values = c("gray10","gray40","gray70"))+
  theme_classic()+
  xlab("Day")+
  ylab("Nest Survival probability")+
  theme(axis.text = element_text(size=10))+
  theme(axis.title = element_text(size=12))+
  theme(legend.title = element_text(size = 12))+
  theme(legend.text = element_text(size = 10))+
  ylim(0,1)+
  facet_grid(cols = vars(Cause))+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_blank())

png(file = "./Figures/causeplot.png",width = 6, height = 3,res = 300, units = "in")
cause.plot
dev.off()


plotdat3 <- plotdat2[plotdat2$Day == 23,]

par(mar = c(5,6,2,2))
plot(plotdat3$Mean ~ c(1,2,3), ylim = c(0,1),xaxt = "n",pch = 19, 
     cex.lab = 1.75, cex.axis = 1.25, xlab = "Cause of mortality",
     ylab = "Nest survival rate",xlim = c(0.5,3.5),cex = 2,bty = "l")
axis(side = 1, at = c(1,2,3),labels = c("Native", "Invasive", "Other"),cex.axis = 1.25)
arrows(c(1,2,3),y0 = plotdat3$Upper, x1 = c(1,2,3), y1 = plotdat3$Lower, angle = 90,
       code = 3,lwd = 2)

day <- 1:23
box_cum <- NA
sage_cum <- NA
other_cum <- NA
box_l <- NA
box_u <- NA
sage_l <- NA
sage_u <- NA
other_l <- NA
other_u <- NA




plotdat.substrate <- data.frame(cbind(day,box_cum,box_l,box_u,
                                      sage_cum,sage_l,sage_u,
                                      other_cum,other_l,other_u))
for(i in 1:23){
  tempB <- (seasonsurvse((model.fit$mean$x),model.fit$sd$x,i))
  tempS <- (seasonsurvse((model.fit$mean$xsage),model.fit$sd$xsage,i))
  tempO <- (seasonsurvse((model.fit$mean$xother),model.fit$sd$xother,i))
  
  plotdat.substrate$box_cum[i] <- model.fit$mean$x^i
  plotdat.substrate$box_l[i] <- model.fit$mean$x^i - tempB
  plotdat.substrate$box_u[i] <- min(model.fit$mean$x^i + tempB,1)
  
  plotdat.substrate$sage_cum[i] <- model.fit$mean$xsage^i
  plotdat.substrate$sage_l[i] <- model.fit$mean$xsage^i - tempS
  plotdat.substrate$sage_u[i] <- min(model.fit$mean$xsage^i + tempS,1)
  
  plotdat.substrate$other_cum[i] <- model.fit$mean$xother^i
  plotdat.substrate$other_l[i] <- model.fit$mean$xother^i - tempO
  plotdat.substrate$other_u[i] <- min(model.fit$mean$xother^i + tempO,1)
}

plotdat.substrate

plotdatB <- plotdat.substrate[,c(1,2,3,4)]
plotdatB$substrate <- "Boxthorn"
colnames(plotdatB) <- c("Day","Mean","Lower","Upper","Substrate")
plotdatS <- plotdat.substrate[,c(1,5,6,7)]
plotdatS$substrate <- "Sagebrush"
colnames(plotdatS) <- c("Day","Mean","Lower","Upper","Substrate")
plotdatO <- plotdat.substrate[,c(1,8,9,10)]
plotdatO$substrate <- "Other"
colnames(plotdatO) <- c("Day","Mean","Lower","Upper","Substrate")

plotdat.substrate2 <- rbind(plotdatB,plotdatS,plotdatO)
plotdat.substrate2$Substrate <- as.factor(plotdat.substrate2$Substrate)
plotdat.substrate2$Substrate <- ordered(plotdat.substrate2$Substrate, levels = c("Boxthorn","Sagebrush","Other"))



sub.plot <- ggplot(data = plotdat.substrate2,mapping = aes(x = Day, y = Mean, col = Substrate))+
  geom_line(size = 1.5)+
  geom_ribbon(linetype = "dashed",aes(ymin = Lower, ymax = Upper, fill = Substrate),alpha = 0.2)+
  scale_fill_manual(values = c("gray10","gray40","gray70"))+
  scale_color_manual(values = c("gray10","gray40","gray70"))+
  theme_classic()+
  xlab("Day")+
  ylab(" Nest Survival probability")+
  theme(axis.text = element_text(size=10))+
  theme(axis.title = element_text(size=12))+
  theme(legend.title = element_text(size = 12))+
  theme(legend.text = element_text(size = 10))+
  ylim(0,1)+
  facet_grid(cols = vars(Substrate))+
  theme(legend.position = "bottom")+
  theme(strip.text.x = element_blank())

png(file = "./Figures/substrateplot.png",width = 6, height = 3,units = "in",res = 300)
sub.plot
dev.off()

plotdat.substrate3 <- plotdat.substrate2[plotdat2$Day == 23,]

par(mar = c(5,6,2,2))
plot(plotdat.substrate3$Mean ~ c(1,2,3), ylim = c(0,1),xaxt = "n",pch = 19, 
     cex.lab = 1.75, cex.axis = 1.25, xlab = "Nest Substrate",
     ylab = "Nest survival rate",xlim = c(0.5,3.5),cex = 2,bty = "l")
axis(side = 1, at = c(1,2,3),labels = c("Boxthorn", "Sagebrush", "Other"),cex.axis = 1.25)
arrows(c(1,2,3),y0 = plotdat.substrate3$Upper, x1 = c(1,2,3), y1 = plotdat.substrate3$Lower, angle = 90,
       code = 3,lwd = 2)
