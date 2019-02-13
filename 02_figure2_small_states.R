### Figure 2. Small states.
# Data modifications and settings ----------------------------------------------------------------
w1$pop.r<-rescale(log(w1$pop), to = c(2, 5))
w1$pop.r2<-rescale(log(w1$pop), to = c(1, 7))

#classify countries in four groups for plotting of lables
w1$country.x<-recode(w1$country.x, "'Gambia, The'='The Gambia';'United Arab Emirates'='UAE';'Central African Republic'='CAR';'Czech Republic'='Czechia';'United States'='USA'")
w1$country.x<-recode(w1$country.x, "'CAR'='Central African Republic'")

w1$right<-recode(w1$country.x, "'Central African Republic'=1;'The Gambia'=1;'Bhutan'=1;'Andorra'=1;'Estonia'=1;'Samoa'=1;'Georgia'=1;'Croatia'=1;else=0")
w1$right2<-recode(w1$country.x, "'Namibia'=1; 'Mauritius'=1;else=0")
w1$down<-recode(w1$country.x, "'Moldova'=1;'Guyana'=1; 'Latvia'=1; 'Brunei'=1; 'Dominica'=1; else=0")
w1$up<-recode(w1$country.x, "'Guinea-Bissau'=1;'Marshall Islands'=1;'Iceland'=1;else=0")
w1$left<-recode(w1$country.x, "'Gabon'=1;'Equatorial Guinea'=1; 'Comoros'=1; 'Bahrain'=1; 'Djibouti'=1;else=0")

col1<-rgb(225, 25, 27, alpha=255, max=255)
col2<-rgb(30, 93, 160, alpha=255, max=255)
col3<-rgb(255, 220, 69, alpha=255, max=255)
col1a<-rgb(225, 25, 27, alpha=195, max=255)
col2a<-rgb(30, 93, 160, alpha=195, max=255)
col3a<-rgb(255, 220, 69, alpha=195, max=255)
col3b<-rgb(248, 202, 8, alpha=255, max=255)
bg.col<-rgb(255, 251, 245, alpha=255,max=255)
dark.color=rgb(35, 35, 58, max=255) # dark color

showtext_auto() #this is to turn on the custom fonts availability
showtext_opts(dpi = 96) #set the resolution: 96 is default
# Set par settngs and initialize the figure -----------------------------------
png('wdvp_small_big_redux.png', width=3000, height=1680, res=96) 

layout(matrix(c(1, 1, 1, 1, 2,
                1, 1, 1, 1, 3,
                1, 1, 1, 1, 4,
                1, 1, 1, 1, 5,
                6, 7, 8, 9, 10), nrow=5, byrow=TRUE))

par(mar=c(8,3,8,3),
    oma=c(6,8,4,2),
    bg=background.color,
    bty='n',
    family='Montserrat')
# Plot 1 ------------------------------------------------------------------
plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=c(-2.2,2.2), ylim=c(0.34,1)) #start with an empty plot
abline(v=seq(-2,2,1), col='grey80', lwd=2) #add the vertical grid
abline(h=seq(0.4,1,by=0.2), col='grey80', lwd=2) #add the horizontal grid

#title
mtext('Small States Can Be Big Players in Development and Good Governance', line=8, font=2, at=-2.52, col=dark.color, cex=3.8, adj=0, padj=1)

rect(xleft=-2.196, xright=0.1, ybottom=0.88, ytop=0.98, col=background.color, border=NA)
mtext(expression('There are more than 77 ' * phantom(' small states ') * ' (with population below 5 million) that account for'), line=-6, font=1, at=-2.195, col=dark.color, cex=1.8, adj=0, padj=1)
mtext(expression(phantom('There are more than 77 ') * ' small states ' * phantom(' (with population below 5 million) that account for')), line=-6, font=1, at=-2.195, col=col1, cex=1.8, adj=0, padj=1)

mtext(expression('more than ' * phantom(' 1.5%') * ' of the world population and for close to ' * phantom ('  3% ') * ' of the global wealth.'), line=-8.5, font=1, at=-2.195, col=dark.color, cex=1.8, adj=0, padj=1)
mtext(expression(phantom('more than ') * ' 1.5%' * phantom(' of the world population and for close to ') * '  3% ' * phantom(' of the global wealth.')), line=-8.5, font=1, at=-2.195, col=col1, cex=1.8, adj=0, padj=1)

mtext(expression('Some ' *  phantom(' small states ') * ' are amongst the most developed nations'), line=-11, font=1, at=-2.195, col=dark.color, cex=1.8, adj=0, padj=1)
mtext(expression(phantom('Some ') * ' small states ' *  phantom(' are amongst the most developed nations')), line=-11, font=1, at=-2.195, col=col1, cex=1.8, adj=0, padj=1)

mtext('with highest levels of government effectiveness.', line=-13.5, font=1, at=-2.195, col=dark.color, cex=1.8, adj=0, padj=1)

rect(xleft=1.15, xright=2.298, ybottom=0.41, ytop=0.55, col=background.color, border=NA)
mtext('In all these charts, states are grouped', line=-57, font=1, at=2.295, col=dark.color, cex=1.8, adj=1, padj=1)
mtext('and colored by population size:', line=-59.5, font=1, at=2.295, col=dark.color, cex=1.8, adj=1, padj=1)
mtext(expression('big states' * phantom(' (more than 35 million)')), line=-62, font=1, at=2.295, col=col2, cex=1.8, adj=1, padj=1)
mtext(expression(phantom('big states') * ' (more than 35 million)'), line=-62, font=1, at=2.295, col=dark.color, cex=1.8, adj=1, padj=1)
mtext(expression('midsized ' * phantom(' (between 5 and 35 million)')), line=-64.5, font=2, at=2.295, col=col3b, cex=1.8, adj=1, padj=1)
mtext(expression(phantom('midsized ') * ' (between 5 and 35 million)'), line=-64.5, font=1, at=2.295, col=dark.color, cex=1.8, adj=1, padj=1)
mtext(expression('small states ' * phantom(' (less than 5 million)')), line=-67, font=2, at=2.295, col=col1, cex=1.8, adj=1, padj=1)
mtext(expression(phantom('small states ' ) * ' (less than 5 million)'), line=-67, font=2, at=2.295, col=dark.color, cex=1.8, adj=1, padj=1)

#More notes
mtext('These charts show the distribution', side=1, line=19.5, font=1, at=-2.52, col=dark.color, cex=1.8, adj=0, padj=1)
mtext('densities of different variables', side=1, line=22, font=1, at=-2.52, col=dark.color, cex=1.8, adj=0, padj=1)
mtext('by three groups based on state size:', side=1, line=24.5, font=1, at=-2.52, col=dark.color, cex=1.8, adj=0, padj=1)

#Notes
mtext(text=expression(phantom("Data:") * " Human Development Index from Human Development Reports. Government Effectiveness, Control of Corruption, and Political Stability from Worldwide Governance Indicators. Freedom Rating from Freedom House."),
      side=1, line=35, outer=F, font=1, col=dark.color, cex=1.3, at=-2.52, adj=0, padj=1)
mtext(text=expression("Data: " * phantom(" Human Development Index from Human Development Reports. Government Effectiveness, Control of Corruption, and Political Stability from Worldwide Governance Indicators. Freedom Rating from Freedom House")),
      side=1, line=35, outer=F, font=1, col=col1, cex=1.3, at=-2.52, adj=0, padj=1)

#horizontal axis
axis (1, font=1, tck=-0.00, col=background.color, col.axis=dark.color, at=seq(-2,2,1),line=1, labels=c(-2,-1,'',1,2),cex.axis=3) 
mtext(side=1,text=expression("Government Effectiveness (2017)"),line=3, font=1, col=dark.color, cex=2.5)
#vertical axis
axis (2, font=1, tck=-0.00, lwd=0, col=background.color, col.axis=dark.color, at=seq(0.4,1,by=0.2),line=1, labels=c('0.4','','','1.0'), las=1, cex.axis=3) 
mtext(side=2,text=expression("Human Development Index (2017)"),line=1, font=1, col=dark.color, cex=2.5)

points(x=w1$govef.y, y=w1$hdi.2017, pch=21, cex=10, bg=ifelse(w1$size=='small', col1a, ifelse(w1$size=='big', col2a, col3a)),  
       col=ifelse(w1$size=='small', col1, ifelse(w1$size=='big', col2, col3)))

#the country labels
text(w1$country.x[w1$up==1], x=w1$govef.y[w1$up==1], y=w1$hdi.2017[w1$up==1]+0.028, cex=2.1, col='black', font=3)
text(w1$country.x[w1$down==1], x=w1$govef.y[w1$down==1], y=w1$hdi.2017[w1$down==1]-0.021, cex=1.7, col='black', font=3)
text(w1$country.x[w1$left==1], x=w1$govef.y[w1$left==1]-0.065, y=w1$hdi.2017[w1$left==1], adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$right==1], x=w1$govef.y[w1$right==1]+0.065, y=w1$hdi.2017[w1$right==1], adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$right2==1], x=w1$govef.y[w1$right2==1]+0.06, y=w1$hdi.2017[w1$right2==1], adj=c(0,0.5), cex=2.1, col='black', font=3)

text(w1$country.x[w1$country.x=='Liberia'], x=w1$govef.y[w1$country.x=='Liberia']+0.06, y=w1$hdi.2017[w1$country.x=='Liberia']+0.01, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Solomon Islands'], x=w1$govef.y[w1$country.x=='Solomon Islands']-0.07, y=w1$hdi.2017[w1$country.x=='Solomon Islands']+0.013, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Lesotho'], x=w1$govef.y[w1$country.x=='Lesotho']-0.076, y=w1$hdi.2017[w1$country.x=='Lesotho']-0.02, cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Timor-Leste'], x=w1$govef.y[w1$country.x=='Timor-Leste']-0.07, y=w1$hdi.2017[w1$country.x=='Timor-Leste']+0.005, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Congo (Rep.)'], x=w1$govef.y[w1$country.x=='Congo (Rep.)']-0.07, y=w1$hdi.2017[w1$country.x=='Congo (Rep.)']+0.01, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Eswatini'], x=w1$govef.y[w1$country.x=='Eswatini']+0.055, y=w1$hdi.2017[w1$country.x=='Eswatini']-0.0175, adj=c(0,0.5), cex=2, col='black', font=3)
text(w1$country.x[w1$country.x=='Suriname'], x=w1$govef.y[w1$country.x=='Suriname']-0.07, y=w1$hdi.2017[w1$country.x=='Suriname']+0.02, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Belize'], x=w1$govef.y[w1$country.x=='Belize']-0.07, y=w1$hdi.2017[w1$country.x=='Belize']+0.02, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Bosnia and Herzegovina'], x=w1$govef.y[w1$country.x=='Bosnia and Herzegovina']-0.07, y=w1$hdi.2017[w1$country.x=='Bosnia and Herzegovina']+0.02, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Palau'], x=w1$govef.y[w1$country.x=='Palau']-0.07, y=w1$hdi.2017[w1$country.x=='Palau']-0.01, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Kiribati'], x=w1$govef.y[w1$country.x=='Kiribati']+0.07, y=w1$hdi.2017[w1$country.x=='Kiribati']+0.013, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Cabo Verde'], x=w1$govef.y[w1$country.x=='Cabo Verde']+0.06, y=w1$hdi.2017[w1$country.x=='Cabo Verde']+0.018, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Jamaica'], x=w1$govef.y[w1$country.x=='Jamaica']+0.06, y=w1$hdi.2017[w1$country.x=='Jamaica']+0.015, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Luxembourg'], x=w1$govef.y[w1$country.x=='Luxembourg']+0.03, y=w1$hdi.2017[w1$country.x=='Luxembourg']-0.023, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='New Zealand'], x=w1$govef.y[w1$country.x=='New Zealand']+0.03, y=w1$hdi.2017[w1$country.x=='New Zealand']-0.023, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Botswana'], x=w1$govef.y[w1$country.x=='Botswana']+0.025, y=w1$hdi.2017[w1$country.x=='Botswana']-0.022, cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Barbados'], x=w1$govef.y[w1$country.x=='Barbados']+0.037, y=w1$hdi.2017[w1$country.x=='Barbados']+0.013, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Ireland'], x=w1$govef.y[w1$country.x=='Ireland']-0.07, y=w1$hdi.2017[w1$country.x=='Ireland']+0.005, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Malta'], x=w1$govef.y[w1$country.x=='Malta']-0.07, y=w1$hdi.2017[w1$country.x=='Malta']+0.018, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Cyprus'], x=w1$govef.y[w1$country.x=='Cyprus']-0.07, y=w1$hdi.2017[w1$country.x=='Cyprus']+0.015, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Oman'], x=w1$govef.y[w1$country.x=='Oman']-0.089, y=w1$hdi.2017[w1$country.x=='Oman']+0.012, adj=c(1,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Micronesia'], x=w1$govef.y[w1$country.x=='Micronesia']+0.065, y=w1$hdi.2017[w1$country.x=='Micronesia']-0.005, adj=c(0,0.5), cex=2.1, col='black', font=3)
text(w1$country.x[w1$country.x=='Lebanon'], x=w1$govef.y[w1$country.x=='Lebanon']-0.065, y=w1$hdi.2017[w1$country.x=='Lebanon']+0.0198, adj=c(1,0.5), cex=2., col='black', font=3)
text(w1$country.x[w1$country.x=='Uruguay'], x=w1$govef.y[w1$country.x=='Uruguay']-0.007, y=w1$hdi.2017[w1$country.x=='Uruguay']+0.022, cex=1.8, col='black', font=3)
text(w1$country.x[w1$country.x=='Vanuatu'], x=w1$govef.y[w1$country.x=='Vanuatu']+0.04, y=w1$hdi.2017[w1$country.x=='Vanuatu']+0.022, cex=1.8, col='black', font=3)


# Close the plot ----------------------------------------------------------
dev.off()


# Plot the map ------------------------------------------------------------
library(sp)
library(rworldmap) # this pkg has waaaay better world shapefiles
library(rgdal)
ws<-w[,c('country','iso3c','size')]
sPDF <- joinCountryData2Map( ws, joinCode = "ISO3", nameJoinColumn = "iso3c", mapResolution='low', verbose=T)
#sPDF <-sPDF[which(sPDF$ADMIN!='Antarctica'),]
sPDF <- spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))

#pdf('F:/wmap.pdf', width=12, height=7) 
#png('F:/wmap.png', width=1680, height=1050, res=96) 
png('wmap_big.png', width=3000, height=1550, res=96) 
par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
mapCountryData( sPDF, nameColumnToPlot="size", addLegend=F,colourPalette=c(col2, col3, col1),borderCol='grey82',
                mapTitle='', oceanCol=bg.col, missingCountryCol='white',lwd=0.1)
dev.off()



# Ridge plots with ggplot2 ------------------------------------------------
library(ggplot2)
library(ggridges)
library(forcats)

# Ridge 1. Political stability --------------------------------------------
w1$var<-w1$polviol

png("polviol__size_ridges_redux.png",width=800, height=400, res=96)
par(family='Montserrat')

ggplot(w1, aes(x=w1$var, y=w1$size3, fill=w1$size3, color=w1$size3)) +
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col3, col2)) +
  scale_x_continuous(limits=c(min(w1$var,na.rm=T),max(w1$var,na.rm=T)), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + labs(title = 'Small states tend to be more politicaly stable,', x='Political stability (2017)')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.title = element_text(color=dark.color, size=24, face="bold", hjust=0.99),
                                                    plot.subtitle = element_text(color=dark.color, size=14, face="italic"),
                                                    plot.background = element_rect(fill=background.color, colour=dark.color),
                                                    axis.text.y=element_text(size=20, face='italic'), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color=dark.color,hjust=0.5))+
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
dev.off()

# Ridge 2. Political stability --------------------------------------------
w1$var<-w1$corupt

png("corupt__size_ridges.png",width=600, height=400, res=96)
par(family='Montserrat')

ggplot(w1, aes(x=w1$var, y=w1$size3, fill=w1$size3, color=w1$size3)) +
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col3, col2)) +
  scale_x_continuous(limits=c(min(w1$var,na.rm=T),max(w1$var,na.rm=T)), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + labs(title = 'and have lower levels of corruption,', x='Control of Corruption (2017)')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.title = element_text(color=dark.color, size=24, face="bold", hjust=0),
                                                    plot.subtitle = element_text(color=dark.color, size=14, face="italic"),
                                                    plot.background = element_rect(fill=background.color, colour=dark.color),
                                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color=dark.color,hjust=0.5))+
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
dev.off()

# Ridge 3. Political freedoms --------------------------------------------
w1$var<-w1$freedom
png("polrights__size_ridges.png",width=600, height=400, res=96)
par(family='Montserrat')

ggplot(w1, aes(x=w1$var, y=w1$size3, fill=w1$size3, color=w1$size3)) +
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col3, col2)) +
  scale_x_continuous(limits=c(min(w1$var,na.rm=T),max(w1$var,na.rm=T)), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + labs(title = 'and enjoy more political freedoms.', x='Freedom Rating [1="Free"] (2017)')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.title = element_text(color=dark.color, size=24, face="bold", hjust=0),
                                                    plot.subtitle = element_text(color=dark.color, size=14, face="italic"),
                                                    plot.background = element_rect(fill=background.color, colour=dark.color),
                                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color=dark.color,hjust=0.5))+
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
dev.off()

# Ridge 4. Change in HDI --------------------------------------------
w1$var<-w1$hdi.c.0017
png("hdi_change_ridges.png",width=600, height=400, res=96)
par(family='Montserrat')

ggplot(w1, aes(x=w1$var, y=w1$size3, fill=w1$size3, color=w1$size3)) +
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col2, col3)) +
  scale_x_continuous(limits=c(min(w1$var,na.rm=T),max(w1$var,na.rm=T)), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + labs(title = 'But small states have improved less', x='Change in HDI, 2017-2000')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.title = element_text(color=dark.color, size=24, face="bold", hjust=0),
                                                    plot.subtitle = element_text(color=dark.color, size=14, face="italic"),
                                                    plot.background = element_rect(fill=background.color, colour=dark.color),
                                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color=dark.color,hjust=0.5))+
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
dev.off()

# Ridge 5. Change in Government Effectiveness --------------------------------------------
w1$var<-w1$govef.c.12
png("govef_change_ridges.png",width=600, height=400, res=96)
par(family='Montserrat')

ggplot(w1, aes(x=w1$var, y=w1$size3, fill=w1$size3, color=w1$size3)) +
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c(col1a,col3a, col2a)) +
  scale_color_cyclical(values = c(col1,col3, col2)) +
  scale_x_continuous(limits=c(min(w1$var,na.rm=T),max(w1$var,na.rm=T)), expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) + labs(title = 'and have even lost some ground.', x='Change in Government Effectiveness, 2017-2012')+
  theme_ridges(font_size = 20, grid = TRUE) + theme(plot.margin = unit(c(1,1,1,1), "lines"), 
                                                    plot.title = element_text(color=dark.color, size=24, face="bold", hjust=0),
                                                    plot.subtitle = element_text(color=dark.color, size=14, face="italic"),
                                                    plot.background = element_rect(fill=background.color, colour=dark.color),
                                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                                    axis.title.y = element_blank(), axis.title.x = element_text(color=dark.color,size=20, hjust=0.5))+
  coord_cartesian(ylim = c(1, 7), clip = 'off') 
dev.off()

