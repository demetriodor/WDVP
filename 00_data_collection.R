### This file replicates the data collection and plotting for the
### World Data Visualization Prize, 2019. Done by Dimiter Toshkov.
### Last updated. 13 January 2019.
setwd('F:/Dataviz competition Project')
# Libraries ---------------------------------------------------------------
library(wbstats) #World Bank data
library(car) #for recoding
library(countrycode) #to switch between country names and codes
library(extrafont) #to use custom fonts
library(sysfonts) #to load custom fonts
library(jsonlite) #needed by sysfonts
library(RCurl)
library(showtext) #to use the custom fonts
font_add_google('Cairo') #get the fonts 
font_add_google("Montserrat")
font_families() #check that the fonts are installed and available
# Data collection ---------------------------------------------------------
# Get the GDP, GDPpc, unemployment, and population data from the WB ---------------------------------
d.econ17<-wb(indicator = c("NY.GDP.MKTP.PP.KD","SP.POP.TOTL","NY.GDP.PCAP.PP.KD","SL.UEM.TOTL.ZS"), country='all', startdate = 2017, enddate = 2017,  return_wide = TRUE)
d.econ12<-wb(indicator = c("NY.GDP.MKTP.PP.KD","SP.POP.TOTL","NY.GDP.PCAP.PP.KD","SL.UEM.TOTL.ZS"), country='all', startdate = 2012, enddate = 2012,  return_wide = TRUE)
d.econ07<-wb(indicator = c("NY.GDP.MKTP.PP.KD","SP.POP.TOTL","NY.GDP.PCAP.PP.KD","SL.UEM.TOTL.ZS"), country='all', startdate = 2007, enddate = 2007,  return_wide = TRUE)
colnames(d.econ17)[5:8]<-c('gdp','gdp.pc','unempl','popul')

d.econ17$gdp.c.12<-(d.econ17[,5]-d.econ12[,5])/d.econ12[,5]*100
d.econ17$gdp.c.07<-(d.econ17[,5]-d.econ07[,5])/d.econ07[,5]*100
d.econ17$gdp.pc.c.12<-(d.econ17[,6]-d.econ12[,6])/d.econ12[,6]*100
d.econ17$gdp.pc.c.07<-(d.econ17[,6]-d.econ07[,6])/d.econ07[,6]*100
d.econ17$unempl.c.12<-(d.econ17[,7]-d.econ12[,7])
d.econ17$unempl.c.07<-(d.econ17[,7]-d.econ07[,7])
d.econ17$popul.c.12<-(d.econ17[,8]-d.econ12[,8])/d.econ12[,8]*100
d.econ17$popul.c.07<-(d.econ17[,8]-d.econ07[,8])/d.econ07[,8]*100

# Get the WB Governance Indicators ---------------------------------
d.gov17<-wb(indicator = c("CC.EST","GE.EST","PV.EST","RL.EST","RQ.EST","VA.EST"), country='all', startdate = 2017, enddate = 2017,  return_wide = TRUE)
d.gov12<-wb(indicator = c("CC.EST","GE.EST","PV.EST","RL.EST","RQ.EST","VA.EST"), country='all', startdate = 2012, enddate = 2012,  return_wide = TRUE)
d.gov07<-wb(indicator = c("CC.EST","GE.EST","PV.EST","RL.EST","RQ.EST","VA.EST"), country='all', startdate = 2007, enddate = 2007,  return_wide = TRUE)
colnames(d.gov17)[5:10]<-c('corupt','govef','polviol','ruleoflaw', 'regqual', 'voice')
colnames(d.gov12)[5:10]<-c('corupt','govef','polviol','ruleoflaw', 'regqual', 'voice')
colnames(d.gov07)[5:10]<-c('corupt','govef','polviol','ruleoflaw', 'regqual', 'voice')

d.gov17<-subset(d.gov17, d.gov17$country!='Greenland' & d.gov17$country!='South Sudan'& d.gov17$country!='Jersey, Channel Islands')
u<-unique(d.gov17$iso2c)
for (i in 1:length(u)){
  d.gov17$corupt.c.12[d.gov17$iso2c==u[i]]<-d.gov17$corupt[d.gov17$iso2c==u[i]]-d.gov12$corupt[d.gov12$iso2c==u[i]]
  d.gov17$voice.c.12[d.gov17$iso2c==u[i]]<-d.gov17$voice[d.gov17$iso2c==u[i]]-d.gov12$voice[d.gov12$iso2c==u[i]]
  d.gov17$regqual.c.12[d.gov17$iso2c==u[i]]<-d.gov17$regqual[d.gov17$iso2c==u[i]]-d.gov12$regqual[d.gov12$iso2c==u[i]]
  d.gov17$ruleoflaw.c.12[d.gov17$iso2c==u[i]]<-d.gov17$ruleoflaw[d.gov17$iso2c==u[i]]-d.gov12$ruleoflaw[d.gov12$iso2c==u[i]]
  d.gov17$polviol.c.12[d.gov17$iso2c==u[i]]<-d.gov17$polviol[d.gov17$iso2c==u[i]]-d.gov12$polviol[d.gov12$iso2c==u[i]]
  d.gov17$govef.c.12[d.gov17$iso2c==u[i]]<-d.gov17$govef[d.gov17$iso2c==u[i]]-d.gov12$govef[d.gov12$iso2c==u[i]]
  
  d.gov17$corupt.12[d.gov17$iso2c==u[i]]<-d.gov12$corupt[d.gov12$iso2c==u[i]]
  d.gov17$voice.12[d.gov17$iso2c==u[i]]<-d.gov12$voice[d.gov12$iso2c==u[i]]
  d.gov17$regqual.12[d.gov17$iso2c==u[i]]<-d.gov12$regqual[d.gov12$iso2c==u[i]]
  d.gov17$ruleoflaw.12[d.gov17$iso2c==u[i]]<-d.gov12$ruleoflaw[d.gov12$iso2c==u[i]]
  d.gov17$polviol.12[d.gov17$iso2c==u[i]]<-d.gov12$polviol[d.gov12$iso2c==u[i]]
  d.gov17$govef.12[d.gov17$iso2c==u[i]]<-d.gov12$govef[d.gov12$iso2c==u[i]]
  
  d.gov17$corupt.07[d.gov17$iso2c==u[i]]<-d.gov07$corupt[d.gov07$iso2c==u[i]]
  d.gov17$voice.07[d.gov17$iso2c==u[i]]<-d.gov07$voice[d.gov07$iso2c==u[i]]
  d.gov17$regqual.07[d.gov17$iso2c==u[i]]<-d.gov07$regqual[d.gov07$iso2c==u[i]]
  d.gov17$ruleoflaw.07[d.gov17$iso2c==u[i]]<-d.gov07$ruleoflaw[d.gov07$iso2c==u[i]]
  d.gov17$polviol.07[d.gov17$iso2c==u[i]]<-d.gov07$polviol[d.gov07$iso2c==u[i]]
  d.gov17$govef.07[d.gov17$iso2c==u[i]]<-d.gov07$govef[d.gov07$iso2c==u[i]]
}

d.econgov<-merge(d.econ17, d.gov17, by='iso3c')

d.econ17$country[(d.econ17$country%in%d.econgov$country.x)==F]
d.gov17$country[(d.gov17$country%in%d.econgov$country.x)==F]

# Get the HDI data ---------------------------------
d.hdi<-read.table('hdi_data.txt', header=T, sep='\t', dec='.')
d.hdi$iso3c<-countrycode(d.hdi$country, 'country.name', 'iso3c')

# Get the Happyness data ---------------------------------
d.happy<-read.table('happiness_data.txt', header=T, sep='\t', dec='.')
d.happy2017<-subset(d.happy, d.happy$year==2017)
d.happy2012<-subset(d.happy, d.happy$year==2012)

u<-unique(d.happy$country)
d.happy2<-data.frame(matrix(NA, nrow=length(u), ncol=ncol(d.happy)))
colnames(d.happy2)<-colnames(d.happy)
for (i in 1:length(u)){
  d.happy.s<-subset(d.happy, d.happy$country==u[i])
  d.happy2[i,1]<-as.character(u[i])
  d.happy2[i,-1]<-d.happy.s[dim(d.happy.s)[1],-1]
}

d.happy2$iso3c<-countrycode(d.happy2$country, 'country.name', 'iso3c')
# Get the data provided by WDVP ---------------------------------
w<-read.table("wdvp.txt",sep='\t', dec='.',header=T)
colnames(w)[2]<-'iso3c'
w$size<-ifelse(w$pop/1000000<5, 'small', ifelse(w$pop/1000000<35, 'medium', 'big'))
w$size2<-ifelse(w$pop/1000000<1.5, 'extrasmall', ifelse(w$pop/1000000<5, 'small', ifelse(w$pop/1000000<35, 'medium', 'big')))

# Get continents and regions ---------------------------------
cc<-read.table('country_classes.txt', sep='\t', header=T)

# Merge the datasets ---------------------------------
d.h<-merge(d.happy2, d.hdi, by='iso3c')
d.hdi$country[(d.hdi$iso3c%in%d.happy2$iso3c)==F]
d.happy2$country[(d.happy2$iso3c%in%d.hdi$iso3c)==F]
d.gh<-merge(d.h, d.econgov, by='iso3c')
d<-merge(w, d.gh, by='iso3c')
d<-merge(d, cc, by.x='iso3c', by.y='iso3')

d$country<-d$country.x
d$continent<-d$un.geo_region
d$pop.r<-rescale(log(d$popul), to = c(1, 9))
d$pop.r2<-rescale(log(d$popul), to = c(1, 7))
d$country<-recode(d$country, "'United Arab Emirates'='UAE';'Central African Republic'='CAR';'Czech Republic'='Czechia';'United States'='USA'")

# Figure 1 ----------------------------------------------------------------
### see file '01_figure1_all_states.R'
# Figure 2 ----------------------------------------------------------------
w1<-merge(w, d.gov17, by='iso3c')
w1<-merge(w1, d.hdi, by='iso3c', all.x=T)
w1$hdi.c.0017<-w1$hdi.2017-w1$hdi.00
w1$hdi.c.1017<-w1$hdi.2017-w1$hdi.10
w1$govef.c.07<-w1$govef.y-w1$govef.07
w1$size3<-factor(w1$size,levels(as.factor(w1$size))[c(3:1)] )
levels(w1$size3)<-c('small states','midsized states','big states')
w1$freedom<-(w1$polrights+w1$civil)/2

###see file '02_figure2_small_states.R'

###END
