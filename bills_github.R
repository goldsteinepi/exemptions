#################
# Trends in vaccination bills
# Citation: Goldstein ND, Suder JS, Purtle J. Trends and characteristics of proposed and enacted state legislation on childhood vaccination exemption, 2011 – 2017. Manuscript in preparation.
# 12/21/17 -- Neal Goldstein
#################


### FUNCTIONS ###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(lme4) #mixed effects models
library(blme) #bayesian nonlinear mixed effects (bglmer)
library(ggplot2) #mapping
library(mapproj) #coordinates to projections


### READ DATA ###

bills = read.csv("Analysis_data.csv", as.is=T, stringsAsFactors=F, na.strings="")
states = map_data("state")


### SUBSET and RECODE ###

bills = bills[bills$Year!=2014, ]

bills$Region = ifelse(bills$State=="CT" | bills$State=="ME" | bills$State=="MA" | bills$State=="NH" | bills$State=="RI" | bills$State=="VT" | bills$State=="NJ" | bills$State=="NY" | bills$State=="PA", "Northeast", NA)
bills$Region = ifelse(bills$State=="IL" | bills$State=="IN" | bills$State=="MI" | bills$State=="OH" | bills$State=="WI" | bills$State=="IA" | bills$State=="KS" | bills$State=="MN" | bills$State=="MO" | bills$State=="NE" | bills$State=="ND" | bills$State=="SD", "Midwest", bills$Region)
bills$Region = ifelse(bills$State=="DE" | bills$State=="FL" | bills$State=="GA" | bills$State=="MD" | bills$State=="NC" | bills$State=="SC" | bills$State=="VA" | bills$State=="DC" | bills$State=="WV" | bills$State=="AL" | bills$State=="KY" | bills$State=="MS" | bills$State=="TN" | bills$State=="AR" | bills$State=="LA" | bills$State=="OK" | bills$State=="TX", "South", bills$Region)
bills$Region = ifelse(bills$State=="AZ" | bills$State=="CO" | bills$State=="ID" | bills$State=="MT" | bills$State=="NV" | bills$State=="NM" | bills$State=="UT" | bills$State=="WY" | bills$State=="AK" | bills$State=="CA" | bills$State=="HI" | bills$State=="OR" | bills$State=="WA", "West", bills$Region)
bills$Region = as.factor(bills$Region)

#outcomes
bills$Assessment_pro = ifelse(bills$Assessment=="Pro", 1, 0)
bills$Assessment_anti = ifelse(bills$Assessment=="Anti", 1, 0)
bills$Assessment_neutral = ifelse(bills$Assessment=="Neutral", 1, 0)
bills$Law = ifelse(bills$Law=="Yes", 1, 0)

#trim DC to obtain lower 48 states
states = states[states$region!="district of columbia",]


### DESCRIPTIVES ###

CrossTable(bills$State)
state.abb[!state.abb %in% unique(bills$State)]
CrossTable(bills$Law)
CrossTable(bills$Assessment)
table(bills$Thematic_analysis)
prop.table(table(bills$Thematic_analysis))

CrossTable(bills$Assessment)
CrossTable(bills$Law)
CrossTable(bills$Legislature)
CrossTable(bills$Legislative_majority)
CrossTable(bills$Governor_party)
CrossTable(bills$Governor_gender)
describe(bills$Population_2017); IQR(bills$Population_2017)
CrossTable(bills$Region)

CrossTable(bills$Law, bills$Assessment, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(bills$Legislature, bills$Assessment, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(bills$Legislative_majority, bills$Assessment, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(bills$Governor_party, bills$Assessment, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(bills$Governor_gender, bills$Assessment, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(bills$Population_2017, bills$Assessment)
IQR(bills$Population_2017[bills$Assessment=="Anti"])
IQR(bills$Population_2017[bills$Assessment=="Pro"])
IQR(bills$Population_2017[bills$Assessment=="Neutral"])
summary(aov(bills$Population_2017 ~ bills$Assessment))
CrossTable(bills$Region, bills$Assessment, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


### LONGITUDINAL PLOT ###

#tiff("Figure 1.tif",height=6,width=10,units='in',res=1200) 
plot(table(bills$Year), type="l", lwd=4, xlab="Year", ylab="No. of Proposed Bills")
lines(x=c(2011:2013,2015:2017), y=table(bills$Assessment, bills$Year)["Pro",], lty=1, lwd=1)
lines(x=c(2011:2013,2015:2017), y=table(bills$Assessment, bills$Year)["Anti",], lty=2, lwd=2)
lines(x=c(2011:2013,2015:2017), y=table(bills$Assessment, bills$Year)["Neutral",], lty=3, lwd=2)
legend("topleft", lty=c(1,1,2,3), lwd=c(4,1,2,2), c("Total","Pro","Anti","Neutral"), cex=0.7)
#dev.off() 

### MODELS ###

#trends
yrs = c(2011:2013,2015:2017)
bls = table(bills$Year)
summary(lm(bls ~ yrs))

bls = table(bills$Year[bills$Assessment=="Anti"])
summary(lm(bls ~ yrs))

bls = table(bills$Year[bills$Assessment=="Pro"])
bls = c(0, bls)
summary(lm(bls ~ yrs))

bls = c(0,table(bills$Year[bills$Assessment=="Neutral"]))
summary(lm(bls ~ yrs))

#predictors
model = bglmer(Assessment_anti ~ (1 | State) + scale(Year) + as.factor(Law) + as.factor(Legislature) + as.factor(Legislative_majority) + as.factor(Governor_party) + as.factor(Governor_gender) + scale(Population_2017) + relevel(Region, ref="South"), family=binomial(), data=bills, control=glmerControl(optimizer="bobyqa"), fixef.prior=normal) 
model = bglmer(Assessment_pro ~ (1 | State) + scale(Year) + as.factor(Law) + as.factor(Legislature) + as.factor(Legislative_majority) + as.factor(Governor_party) + as.factor(Governor_gender) + scale(Population_2017) + relevel(Region, ref="South"), family=binomial(), data=bills, control=glmerControl(optimizer="bobyqa"), fixef.prior=normal) 
model = bglmer(Assessment_neutral ~ (1 | State) + scale(Year) + as.factor(Law) + as.factor(Legislature) + as.factor(Legislative_majority) + as.factor(Governor_party) + as.factor(Governor_gender) + scale(Population_2017) + relevel(Region, ref="South"), family=binomial(), data=bills, control=glmerControl(optimizer="bobyqa"), fixef.prior=normal) 

summary(model)
round(exp(fixef(model)),2)
round(exp(confint.merMod(model, method="Wald")),2)


### SPATIAL MAPS ###
#see https://www.r-bloggers.com/us-state-maps-using-map_data/

#connect bill data to mapping data
bills$State_name = tolower(state.name[match(bills$State,state.abb)])
states$total = NA
states$total_pro = NA
states$total_anti = NA

state_index = unique(states$region)
for (i in 1:length(state_index))
{
  states$total[states$region==state_index[i]] = sum(bills$State_name==state_index[i])
  states$total_pro[states$region==state_index[i]] = sum(bills$Assessment_pro[bills$State_name==state_index[i]])
  states$total_anti[states$region==state_index[i]] = sum(bills$Assessment_anti[bills$State_name==state_index[i]])
}

#total bills choropleth map
#tiff("Figure 2a.tif",height=6,width=10,units='in',res=1200) 
ggplot() + geom_polygon(data=states, aes(x=long, y=lat, group=group, fill=states$total),colour="white") + scale_fill_continuous(low = "gray95", high = "gray10", guide="colorbar") + theme_bw() + labs(fill="No. bills" ,title = "", x="", y="") + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border=element_blank(), legend.position=c(0.9,0.25)) + coord_map() + theme(legend.text=element_text(colour="black", size=10, face="bold")) + theme(plot.margin=unit(c(0,0,0,0), "cm"))
#dev.off()

#anti-vaccination choropleth map
#tiff("Figure 2b.tif",height=6,width=10,units='in',res=1200) 
ggplot() + geom_polygon(data=states, aes(x=long, y=lat, group=group, fill=states$total_anti),colour="white") + scale_fill_continuous(low = "gray95", high = "gray10", guide="colorbar") + theme_bw() + labs(fill="No. bills" ,title = "", x="", y="") + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border=element_blank(), legend.position=c(0.9,0.25)) + coord_map() + theme(legend.text=element_text(colour="black", size=10, face="bold")) + theme(plot.margin=unit(c(0,0,0,0), "cm"))
#dev.off()

#pro-vaccination choropleth map
#tiff("Figure 2c.tif",height=6,width=10,units='in',res=1200) 
ggplot() + geom_polygon(data=states, aes(x=long, y=lat, group=group, fill=states$total_pro),colour="white") + scale_fill_continuous(low = "gray95", high = "gray10", guide="colorbar") + theme_bw() + labs(fill="No. bills" ,title = "", x="", y="") + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border=element_blank(), legend.position=c(0.9,0.25)) + coord_map() + theme(legend.text=element_text(colour="black", size=10, face="bold")) + theme(plot.margin=unit(c(0,0,0,0), "cm"))
#dev.off()
