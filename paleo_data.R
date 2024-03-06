#These are some plots I made while putting some data together for Week 11 lecture on paleoclimates and human evolution in Africa.
#My goals here were to compare some insolation models with CO2 concentrations and marine isotope data and make some visuals for my class to help think about what's going on in the Pliocene-Pleistocene, especially as it relates to human evolution.
#Maybe someday I'll put some fossil age ranges in here and get fancy.

#Datsets and sources:

#Everything is sourced from the NOAA paleo data archive at: https://www.ncdc.noaa.gov/paleo-search/?dataTypeId=14

#C02 - Zeebe, Richard E., and Lucas J. Lourens. 2019. “Solar System Chaos and the Paleocene–Eocene Boundary Age Constrained by Geology and Astronomy.” Science 365 (6456): 926–29. https://doi.org/10.1126/scie
#Orbital model - Zeebe, Richard E., and Lucas J. Lourens. 2019. “Solar System Chaos and the Paleocene–Eocene Boundary Age Constrained by Geology and Astronomy.” Science 365 (6456): 926–29. https://doi.org/10.1126/science.aax0612.
#dO16 - Huybers, P.  2006. Pleistocene Depth-derived Age Model and Composite d18O Record. IGBP PAGES/World Data Center for Paleoclimatology Data Contribution Series # 2006-075.NOAA/NCDC Paleoclimatology Program, Boulder CO, USA.
#ODP Marine core O isotope record - Lüthi, Dieter, Martine Le Floch, Bernhard Bereiter, Thomas Blunier, Jean-Marc Barnola, Urs Siegenthaler, Dominique Raynaud, et al. 2008. “High-Resolution Carbon Dioxide Concentration Record 650,000–800,000 Years before Present.” Nature 453 (7193): 379–82. https://doi.org/10.1038/nature06949.

#We're going to need to use the z-score function to fit these different measurements on the same scale. 

zscore<-function(y1) {
  n1 <- mean(y1)
  n2 <- sd(y1)
  zscore <- (y1-n1)/n2
  zscore
}

#This is a stack of read.table read.csv commands to pull these different datasets. If you open the original datasets, you might notice that some are comma delimited and other tab delimited.
#Commands reflect format of file..

CO2=read.csv("data/CO2.csv",header=TRUE)
orbital=read.table("data/orbital.txt",header=TRUE,sep="")
dO16=read.table("data/dO16.txt",header=TRUE,sep="")
odp1143=read.table("data/odp1143-tab.txt",header=TRUE,sep="")
#odp1143 record includes -999 values for NA, just exclude them since trying to plot NA causes errors.

odp1143=odp1143[odp1143$d18Oc.wuell>-200,]
odp1143=odp1143[odp1143$d13Cc.wuell>-200,]

#For the first plot, I thought I would zoom in on the Pleistocene. 

#At the moment I'm short on time and don't want to find another record, so the CO2 data is limited to the last 800kyr. 
#So let's make these roughly the same length by trimming the orbital data.

kyr800=orbital[orbital$age_kyr<800,]
kyr800O16=dO16[dO16$Age<800,]
myr25=orbital[orbital$age_kyr<2500,]

#Need to set these on a common scale for comparison using zscore.

z800kyr=as.data.frame(apply(kyr800,2,zscore))
zCO2=as.data.frame(apply(CO2,2,zscore))
zdO16_800kyr=as.data.frame(apply(kyr800O16,2,zscore))

#Plotting is easier if we grab the ages separately and reckon both as kyr. This sets up the 800kyr data. 

CO2age=CO2$Gasage..yr.BP./1000
orbage800kyr=kyr800$age_kyr
dO16age800kyr=kyr800O16$Age

#Flat stacked plot of CO2, orbital eccentricty, and the angle of Earth's axis over the last 800kyr. This is ugly. Perhaps someone else cuold make it more attractive by changing the plotting options.

plot(orbage800kyr,z800kyr$ecc,type="l",lty=2,col="red",ylim=c(-3,3))
lines(orbage800kyr,z800kyr$inc_deg,lty=1,col="black",lwd=1.5)
points(CO2age,zCO2$CO2..ppmv.,pch=19,col="blue",cex=0.2)
lines(CO2age,zCO2$CO2..ppmv.,lty=1,col="blue",lwd=0.5)
points(dO16age800kyr,zdO16_800kyr$all_Recs,pch=19,col="dark green",cex=0.2)

# Time to move on to stacked plots of Marine oxygen isotopes and orbital eccentricity.

#We need to grab new ages and zscores for this. 
#Ages

orbage2mya=myr25$age_kyr
dO162mya=dO16$Age

#Zscores

zorb2mya=as.data.frame(apply(myr25,2,zscore))
zdO162mya=as.data.frame(apply(dO16,2,zscore))

#Plots

plot(orbage2mya,zorb2mya$inc_deg,type="l",lty=1,col="orange",ylim=c(-3,3),lwd=2)
lines(dO162mya,zdO162mya$all_Recs,lty=1,col="black",lwd=2)
lines(CO2age,zCO2$CO2..ppmv.,lty=2,col="blue",lwd=2)

#Had a minor breakthrough. Can smooth the lines with loess() by running it against time, then using predict() to create smoothed curve.
#Let's just use the odp1134 data and orbital data, since that's simplest.
#Use zscores to set the datasets on the same scale, then smooth them.

orbital=read.table("data/orbital.txt",header=TRUE,sep="")
odp1143=read.table("data/odp1143-tab.txt",header=TRUE,sep="")

#odp1143 record includes -999 values for NA, just exclude them since trying to plot NA fucks shit up.

odp1143=odp1143[odp1143$d18Oc.wuell>-200,]
odp1143=odp1143[odp1143$d13Cc.wuell>-200,]

#orbital data is 10myr at kyr, while odp is 5.6-ish myr at yr BP so let's set the time axis to kyr and cut orbital down to the max date from the odp record.

odpKyr=odp1143$yrBP/1000
odp1143=cbind(odp1143,odpKyr)
orb5myr=orbital[orbital$age_kyr<(max(odp1143$odpKyr)),]

#Make orbital age object to go with odpKyr, for plotting.

orbKyr=orb5myr$age_kyr

#zscores

zorb5mya=as.data.frame(apply(orb5myr,2,zscore))
zodp=as.data.frame(apply(odp1143,2,zscore))

#Loess smoothing and prediction object for isotopes.

lodpz=loess(zodp$d18Oc.wuell~odpKyr,span=0.02) #Loess smoothing
prodpz=predict(lodpz)

#To make plotting easier, grab rounded down/up min and max values.

minODP=floor(min(zodp$d18Oc.wuell))
maxODP=ceiling(max(zodp$d18Oc.wuell))

minORB=floor(min(zorb5mya$inc_deg))
maxORB=ceiling(max(zorb5mya$inc_deg))

#Plot w/offset between different vectors.

plot(odpKyr,
     zodp$d18Oc.wuell,
     col="orange",
     axes=FALSE,
     ann=FALSE,
     ylim=c(minODP,(1+maxORB+abs(minODP)+maxODP)),
     pch=19,
     cex=0.2)

lines(odpKyr,prodpz,lwd=1)

lines(orbKyr, zorb5mya$inc_deg+(1+maxODP+(-1*minODP)),col="blue")

#Adding axes manually.

axis(1,0:50*100,labels=seq(0,5,0.1),cex=0.8)
axis(2,minODP:maxODP,labels=c(minODP:maxODP),cex=0.8)
axis(4,(minORB:maxORB)+(1+maxODP-minODP),labels =c(minORB:maxORB),cex=0.8)
lines(c(0,5000),c(0,0),lty=3)
lines(c(0,5000),c(7,7),lty=3)
title(main="Oxygen Isotopes and Solar Insolation: 5 mya - Present")
title(ylab="Z-Score corrected value",cex=0.8)
title(xlab="Millions of Years Before Present",cex=0.5)

#Here's the final plot, nicely showing turnover in marine oxygen isotope levels as major parts of Pleistocene glacial-interglacial oscillations set in. 


