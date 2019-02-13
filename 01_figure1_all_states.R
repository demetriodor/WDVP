### Figure 1. All states.
# Data modifications and settings ----------------------------------------------------------------
d$pop.r<-rescale(log(d$popul), to = c(2, 16)) #rescale the population variables
d$pop.r2<-rescale(log(d$popul), to = c(2, 12))#rescale the population variables

#classify countries in four groups for plotting of lables
d$down<-recode(d$country, "'Laos'=1;'Rwanda'=1;'China'=1;'Burundi'=1;'USA'=1;else=0")
d$up<-recode(d$country, "'Singapore'=1;'UAE'=1;'Myanmar'=1;'Malawi'= 1;'Jordan'=1;else=0")
d$right<-recode(d$country, "'Tanzania'=1;'Norway'=1;'Haiti'=1;'Iraq'=1;'Iran'=1;
                'Croatia'=1;'Venezuela'=1;'Tunisia'=1;'Sri Lanka'=1;'Japan'=1;'Portugal'=1;
                'Saudi Arabia'=1;'Kuwait'=1; 'Qatar'=1; 'Egypt'=1;'Greece'=1;
                'Uganda'=1;'Ethiopia'=1;'Kenya'=1;'Togo'=1;'Switzerland'=1;'Afghanistan'=1;
                'Syria'= 1;'Botswana'=1;'South Sudan'=1;'Angola'=1;'Japan'=1;'Russia'=1;'Zimbabwe'=1; 'Sweden'=1; else=0")
d$left<-recode(d$country, "'Gabon'=1 ;'Kyrgyzstan'=1 ;'Tajikistan'=1 ; 'Ghana'=1 ;'India'=1;
               'Niger'=1; 'Liberia'=1; 'Denmark'=1; 'United Kingdom'=1; 'New Zealand'=1; 'Czechia'=1; 
               'CAR'=1;'Cameroon'=1;'Costa Rica'=1;'Nicaragua'=1;'Honduras'=1;'Panama'=1;
               'Finland'=1;'France'=1; 'Chad'=1; 'Benin'=1; 'Belgium'=1; 'Colombia'=1; 'Brazil'=1; 'Yemen'=1; 
               'Nigeria'=1; 'Vietnam'=1;'Netherlands'=1; 'Canada'=1; else=0")


main.color=rgb(238, 80, 121, alpha=175, max=255) #main color
main.color2<-rgb(64,193,230, alpha=175, max=255)
main.color3<-rgb(50,100,10, alpha=175, max=255)
main.color4<-rgb(80,10,100, alpha=175, max=255)
main.color5<-rgb(155,138,4, alpha=175, max=255)

main.colora=rgb(238, 80, 121, alpha=255, max=255) #main color
main.color2a<-rgb(64,193,230, alpha=255, max=255)
main.color3a<-rgb(50,100,10, alpha=255, max=255)
main.color4a<-rgb(80,10,100, alpha=255, max=255)
main.color5a<-rgb(155,138,4, alpha=255, max=255)

colors<-c(main.color, main.color3, main.color4, main.color2, main.color5 )
colors2<-c(main.colora, main.color3a, main.color4a, main.color2a, main.color5a)

background.color=rgb(255, 251, 245, max=255) #color for the background
dark.color=rgb(35, 35, 58, max=255) # dark color

showtext_auto() #this is to turn on the custom fonts availability
showtext_opts(dpi = 96) #set the resolution: 96 is default


# Set par settngs and initialize the figure -----------------------------------
pdf('wdvp_allstates.pdf', width=32, height=18) 
png('wdvp_allstates_big.png', width=3000, height=1680, res=96) 

layout(matrix(c(1, 1, 2, 3,
                1, 1, 4, 5), nrow=2, byrow=TRUE))
par(mar=c(14,4,9,5),
    oma=c(4,6,2,4),
    bg=background.color,
    bty='n',
    family='Montserrat')

# Plot 1 ------------------------------------------------------------------
plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=c(6,12), ylim=c(2.5,8.1)) #start with an empty plot
abline(v=c(log(1000), log(10000), log(100000)), col='grey80') #add the vertical grid
abline(h=seq(2,8,by=1), col='grey80') #add the horizontal grid

#title
mtext('When money does not buy happiness ...', line=8, font=2, at=5.5, col=dark.color, cex=3.8, adj=0, padj=1)
mtext('There is a lot of work for governments to do:', line=8, font=2, at=12.6, col=dark.color, cex=3.8, adj=0, padj=1)
mtext('These plots show the relationships of four variables with the residual variance', line=-54, font=3, at=16, col=main.colora, cex=2, adj=0.5, padj=1)
mtext('in happiness that is not accounted for by the level of prosperity of a country.', line=-56.5, font=3, at=16, col=main.colora, cex=2, adj=0.5, padj=1)

test<-expression("Dots represent 158 countries (some are labeled).")
test1<-expression("Dot color corresponds to United Nations regions:")
test2a<-expression("Africa, " * phantom(" America, Asia, Europe, and Oceania."))
test2b<-expression(phantom("Africa, ") * " America, " * phantom(" Asia, Europe, and Oceania."))
test2c<-expression(phantom("Africa, America, ") * " Asia, " * phantom(" Europe, and Oceania."))
test2d<-expression(phantom("Africa, America, Asia, ") * " Europe, " * phantom(" and Oceania."))
test2e<-expression(phantom("Africa, America, Asia, Europe, ") * " and Oceania.")
test3<-expression("Dot size is proportional to population (scaled).")

test10<-expression("Prosperity accounts for " * phantom(" 62% ") * " of the variance in happiness across countries.")
test10a<-expression(phantom("Prosperity accounts for ") * " 62% " * phantom(" of the variance in happiness across countries."))
test7<-expression("As GNI per capita doubles, happiness increases")
test8<-expression("on average by " * phantom (" half a point ") * " on the 0-to-10 scale.")
test8a<-expression(phantom("on average by ") * " half a point " * phantom(" on the 0-to-10 scale."))

rect(xleft=5.81, xright=9.32, ybottom=7.74, ytop=8.7, col=background.color, border=NA)
text(test10, x=5.85, y=8.15, col=dark.color, cex=2.5, adj=c(0,0.5), font=1) 
text(test10a, x=5.85, y=8.15, col=main.colora, cex=2.5, adj=c(0,0.5), font=1) 
text(test7, x=5.85, y=8.15-1*0.15, col=dark.color, cex=2.5, adj=c(0,0.5), font=1) 
text(test8, x=5.85, y=8.15-2*0.15, col=dark.color, cex=2.5, adj=c(0,0.5), font=1) 
text(test8a, x=5.85, y=8.15-2*0.15, col=main.colora, cex=2.5, adj=c(0,0.5), font=1) 

rect(xleft=8.8, xright=12.25, ybottom=2.41, ytop=3.2, col=background.color, border=NA)
text(test, x=12.2, y=3.1, col=dark.color, cex=2.5, adj=c(1,0.5), font=1) 
text(test3, x=12.2, y=3.1-1*0.15, col=dark.color, cex=2.5, adj=c(1,0.5), font=1) 
text(test1, x=12.2, y=3.1-2*0.15, col=dark.color, cex=2.5, adj=c(1,0.5), font=1) 
text(test2a, x=12.2, y=3.1-3*0.15, col=colors2[1], cex=2.5, adj=c(1,0.5), font=1) 
text(test2b, x=12.2, y=3.1-3*0.15, col=colors2[2], cex=2.5, adj=c(1,0.5), font=1) 
text(test2c, x=12.2, y=3.1-3*0.15, col=colors2[3], cex=2.5, adj=c(1,0.5), font=1) 
text(test2d, x=12.2, y=3.1-3*0.15, col=colors2[4], cex=2.5, adj=c(1,0.5), font=1) 
text(test2e, x=12.2, y=3.1-3*0.15, col=colors2[5], cex=2.5, adj=c(1,0.5), font=1) 

#Notes
mtext(text=expression(phantom("Data:") * " Average Happiness score (2017) from World Happiness Report."),
      side=1, line=13, outer=F, font=1, col=dark.color, cex=1.4, at=5.5, adj=0, padj=1)
mtext(text=expression("Data: " * phantom(" Average Happiness score (2017) from World Happiness Report.")),
      side=1, line=13, outer=F, font=1, col=main.colora, cex=1.4, at=5.5, adj=0, padj=1)
mtext(text=expression("GNI per capita (2017) in constant 2011 international dollars from World Bank."),
      side=1, line=15, outer=F, font=1, col=dark.color, cex=1.4, at=5.5, adj=0, padj=1)

mtext(text=expression(phantom("Data: ") * " Regulatory Quality (2017) and Political Stability (2017) from Worldwide Governance Indicators."),
      side=1, line=13, outer=F, font=1, col=dark.color, cex=1.4, at=19.6, adj=1, padj=1)
mtext(text=expression("Data: " * phantom(" Regulatory Quality (2017) and Political Stability (2017) from Worldwide Governance Indicators.")),
      side=1, line=13, outer=F, font=1, col=main.colora, cex=1.4, at=19.6, adj=1, padj=1)
mtext(text=expression("Unemployment levels (2017) from ILO. Gender Development Index (2017) from Human Development Reports."),
      side=1, line=15, outer=F, font=1, col=dark.color, cex=1.4, at=19.6, adj=1, padj=1)

#horizontal axis
axis (1, font=1, tck=-0.01, col=background.color, col.axis=dark.color, at=c(log(1000), log(10000), log(100000)),line=0, labels=c('1,000$','10,000$','100,000$'),cex.axis=2.6) 
mtext(side=1,text=expression("Logarithm of Gross National Income (GNI) per capita"),line=6, font=1, col=dark.color, cex=2.4)

#vertical axis
axis (2, font=1, tck=-0.00, lwd=0, col=background.color, col.axis=dark.color, at=seq(3,8,by=1),line=1, labels=seq(3,8,by=1), las=1, cex.axis=2.6) 
mtext(side=2,text=expression("Average Happiness score"),line=5, font=1, col=dark.color, cex=2.6)
mtext(side=4,text=expression("Residuals (actual happiness minus predicted from prosperity)"),line=7, font=1, col=dark.color, cex=2.4)

#linear regression lines
m1<-lm(d$happy.y~log(d$gni.pc.2017))
xt<-seq(min(log(d$gni.pc.2017)-0.5, na.rm=T), max(log(d$gdp.pc)+0.2, na.rm=T), 0.1)
yt1<-m1$coef[1]+m1$coef[2]*xt 

lines(xt, yt1, col=dark.color, lty=1, lwd=5)

points(x=log(d$gni.pc.2017), y=d$happy.y, pch=21, cex=d$pop.r, col='grey80', bg=colors[d$continent])

#the country labels
d$rr<-rescale(d$pop.r, to = c(0.07, 0.20))
text(d$country[d$down==1], x=log(d$gni.pc.2017[d$down==1]), y=d$happy.y[d$down==1]-d$rr[d$down==1], cex=1.9, col=dark.color, font=3)
text(d$country[d$up==1], x=log(d$gni.pc.2017[d$up==1]), y=d$happy.y[d$up==1]+d$rr[d$up==1], cex=1.9, col=dark.color, font=3)
text(d$country[d$right==1], x=log(d$gni.pc.2017[d$right==1])+d$rr[d$right==1], y=d$happy.y[d$right==1], adj=c(0,0.5), cex=1.9, col=dark.color, font=3)
text(d$country[d$left==1], x=log(d$gni.pc.2017[d$left==1])-d$rr[d$left==1], y=d$happy.y[d$left==1], adj=c(1,0.5), cex=1.9, col=dark.color, font=3)

# Plot 2. Regulatory Quality ------------------------------------------------------------------
r<-resid(m1)
plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=c(-2.3,2.2), ylim=c(-2.3,2.3)) #start with an empty plot
abline(v=seq(-2,2, by=1), col='grey80') #add the vertical grid
abline(h=seq(-2,2,by=1), col='grey80') #add the horizontal grid
#abline(h=0, lwd=2,  col=dark.color)#add the horizontal grid

#horizontal axis
axis (1, font=1, tck=-0.01, col=background.color, col.axis=dark.color, at=seq(-2,2, by=1),line=0, labels=seq(-2,2, by=1), cex.axis=2.6) 
mtext(side=1,text=expression(paste("Regulatory Quality")),line=6, font=1, col=dark.color, cex=2.2)

#vertical axis
axis (4, font=1, tck=-0.00, lwd=0, col=background.color, col.axis=dark.color, at=seq(-2,2,by=1),line=3, labels=seq(-2,2,by=1), las=1, cex.axis=2.6) 

#linear regression lines
m2<-lm(r~d$regqual.y)
xt<-seq(-2.2,2.2, by=0.1)
yt1<-m2$coef[1]+m2$coef[2]*xt 
#the data!
points(x=d$regqual.y, y=r, pch=21, cex=d$pop.r2, col='grey80', bg=colors[d$continent])
lines(xt, yt1, col=main.colora, lty=1, lwd=4)
rect(xleft=-2.3, xright=2.3, ybottom=2.09, ytop=2.5, col=background.color, border=NA)
mtext('Attain good governance', line=1, font=3, at=0, col=dark.color, cex=2.8, adj=0.5, padj=1)

# Plot 3. Unemployment ------------------------------------------------------------------
plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=c(0,28), ylim=c(-2.3,2.3)) #start with an empty plot
abline(v=seq(5,25,by=10), col='grey80') #add the vertical grid
abline(h=seq(-2,2,by=1), col='grey80') #add the horizontal grid
#abline(h=0, lwd=2,  col=dark.color)#add the horizontal grid

#horizontal axis
axis (1, font=1, tck=-0.01, col=background.color, col.axis=dark.color, at=seq(5,25,by=10),line=0, labels=c('5%','15%','25%'), cex.axis=2.6) 
mtext(side=1,text=expression(paste("Unemployment level")),line=6, font=1, col=dark.color, cex=2.2)

#linear regression lines
m2<-lm(r~d$unempl.y)
xt<-seq(0,28, by=0.1)
yt1<-m2$coef[1]+m2$coef[2]*xt 
#the data!
points(x=d$unempl.y, y=r, pch=21, cex=d$pop.r2, col='grey80', bg=colors[d$continent])
lines(xt, yt1, col=main.colora, lty=1, lwd=4)
rect(xleft=-1, xright=29, ybottom=2.09, ytop=2.5, col=background.color, border=NA)
mtext('Reduce unemployment', line=1, font=3, at=14, col=dark.color, cex=2.8, adj=0.5, padj=1)

# Plot 4. Stability ------------------------------------------------------------------
plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=c(-3,1.6), ylim=c(-2.3,2.3)) #start with an empty plot
abline(v=seq(-2,2, by=1), col='grey80') #add the vertical grid
abline(h=seq(-2,2,by=1), col='grey80') #add the horizontal grid
#abline(h=0, lwd=2,  col=dark.color)#add the horizontal grid

#horizontal axis
axis (1, font=1, tck=-0.01, col=background.color, col.axis=dark.color, at=seq(-2,2, by=1),line=0, labels=seq(-2,2, by=1), cex.axis=2.6) 
mtext(side=1,text=expression(paste("Political Stability")),line=6, font=1, col=dark.color, cex=2.2)

#vertical axis
axis (4, font=1, tck=-0.00, lwd=0, col=background.color, col.axis=dark.color, at=seq(-2,2,by=1),line=3, labels=seq(-2,2,by=1), las=1, cex.axis=2.6) 

#linear regression lines
m2<-lm(r~d$polviol)
xt<-seq(-3,1.6, by=0.1)
yt1<-m2$coef[1]+m2$coef[2]*xt 
#the data!
points(x=d$polviol, y=r, pch=21, cex=d$pop.r2, col='grey80', bg=colors[d$continent])
lines(xt, yt1, col=main.colora, lty=1, lwd=4)
rect(xleft=-3.1, xright=1.7, ybottom=2.09, ytop=2.5, col=background.color, border=NA)
mtext('Ensure peace & stability', line=1, font=3, at=-0.35, col=dark.color, cex=2.8, adj=0.5, padj=1)

# Plot 5. Equality ------------------------------------------------------------------
plot(NULL,type='n',axes=FALSE,ann=FALSE, xlim=c(0.4,1.1), ylim=c(-2.3,2.3)) #start with an empty plot
abline(v=seq(0.5,1,by=0.25), col='grey80') #add the vertical grid
abline(h=seq(-2,2,by=1), col='grey80') #add the horizontal grid

#horizontal axis
axis (1, font=1, tck=-0.01, col=background.color, col.axis=dark.color, at=seq(0.5,1,by=0.25),line=0, labels=seq(0.5,1,by=0.25), cex.axis=2.6) 
mtext(side=1,text=expression(paste("Gender Development Index")),line=6, font=1, col=dark.color, cex=2.2)

#linear regression lines
m2<-lm(r~d$gdi.2017)
xt<-seq(0.4,1.04, by=0.01)
yt1<-m2$coef[1]+m2$coef[2]*xt 
#the data!
points(x=d$gdi.2017, y=r, pch=21, cex=d$pop.r2, col='grey80', bg=colors[d$continent])
lines(xt, yt1, col=main.colora, lty=1, lwd=4)
rect(xleft=0.3, xright=1.2, ybottom=2.09, ytop=2.5, col=background.color, border=NA)
mtext('Support equality', line=1, font=3, at=0.75, col=dark.color, cex=2.8, adj=0.5, padj=1)


# End figure --------------------------------------------------------------
dev.off()
