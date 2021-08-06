####packages
library(data.table)
library(foreign)
library(dplyr)
library(ggplot2)
library(reshape)

sample<-read.csv2(file ="D://task1.5//Core_newdata//fullsample.csv", sep=";", header=TRUE)
sample<-subset(sample, earliestyear<=2016)
sample<-subset(sample, (core!=0 | enab_score!=0))
#stargazer(sample2)
#check<-subset(sample, is.na(sample$s_analytics))


setwd("D://task1.5//Core_newdata")
#enabling weights
digital<-read.dta(file="enablingweights.dta") #wrong file with the core info, twice the non enabling

#kicks out the smart application
#def enabling and core score 
digital$enab_score<-digital$s_analytics + digital$s_security + digital$s_ai + digital$s_position + digital$s_power + digital$s_threedim + digital$s_interface
#core weights to add to digital
core<-read.dta(file="coretech_oldversion.dta")
core$appln_id<-as.numeric(core$appln_id)
core$core<-core$s_connectivity+core$s_hardware+core$s_software
digital<-merge(digital, core, by.x="appln_id", by.y = "appln_id", all.x = TRUE)

##family info
faminfo<-as.data.frame(fread("family_id.tsv"))
table(faminfo$earliestyear) #covers nicely up to 2018
names(faminfo)[1]<-"docdb"
names(faminfo)[2]<-"appln_id"
names(faminfo)[3]<-"earliestyear"
length(unique(faminfo$docdb)) #3334444 families
familyearly<-faminfo %>% group_by(docdb) %>% arrange(earliestyear) %>% slice(1L) #arranges to the exact number of docdb

agebreak<-merge(digital, faminfo, by.x = "appln_id", by.y = "appln_id", all.x=TRUE)

#slices with one patent appli per family
familydigi<- agebreak %>% group_by(docdb) %>% arrange(earliestyear) %>% slice(1L) #goes back to the number of families initially extracted

###keeps only familes with positive score for digital patents
fullsample<-subset(familydigi, s_digi>0 & earliestyear<=2016)
#######################################################################################
#######STANDARDIZES THE OVERLAP OF WEIGHTS
########################################################################################
#recomputes the relative score of each tech to control for overlapping tech fields
########################################################################################
fullsample$s_analytics2<-fullsample$s_analytics/fullsample$enab_score
fullsample$s_security2<-fullsample$s_security/fullsample$enab_score
fullsample$s_ai2<-fullsample$s_ai/fullsample$enab_score
fullsample$s_position2<-fullsample$s_position/fullsample$enab_score
fullsample$s_power2<-fullsample$s_power/fullsample$enab_score
fullsample$s_threedim2<-fullsample$s_threedim/fullsample$enab_score
fullsample$s_interface2<-fullsample$s_interface/fullsample$enab_score
fullsample$enab_score2<-fullsample$s_analytics2 + fullsample$s_security2 + fullsample$s_ai2 + fullsample$s_position2 + fullsample$s_power2 + fullsample$s_threedim2 + fullsample$s_interface2
summary(fullsample$enab_score2)
summary(fullsample$enab_score)
#replaces when the conditions of the weird weight is met across all categories
fullsample$s_security[fullsample$enab_score>1]<-fullsample$s_security2 
fullsample$s_ai[fullsample$enab_score>1]<-fullsample$s_ai2
fullsample$s_position[fullsample$enab_score>1]<-fullsample$s_position2
fullsample$s_power[fullsample$enab_score>1]<-fullsample$s_power2
fullsample$s_threedim[fullsample$enab_score>1]<-fullsample$s_threedim2
fullsample$s_interface[fullsample$enab_score>1]<-fullsample$s_interface2
fullsample$s_analytics[fullsample$enab_score>1]<-fullsample$s_analytics2

summary(fullsample$s_analytics)


#replaces the overall score: worked even with errors
fullsample$enab_score[fullsample$enab_score>1]<-fullsample$enab_score2
fullsample$enab_score[is.na(fullsample$enab_score)]<-1
summary(fullsample$enab_score) ###corrected score


####corrects the core above 1 using now the NEW variables for the descriptives
fullsample$s_hardware2<-fullsample$s_hardware/fullsample$core
fullsample$s_software2<-fullsample$s_software/fullsample$core
fullsample$s_connectivity2<-fullsample$s_connectivity/fullsample$core
fullsample$core2<-fullsample$s_hardware2+fullsample$s_software2+fullsample$s_connectivity2
fullsample$core2[is.na(fullsample$core2)]<-0
fullsample$core2[fullsample$core2==2]<-1
fullsample$core[fullsample$core>1]<-fullsample$core2
summary(fullsample$core)

#noisy sample with core and twin
table(fullsample$core>0)
#noisy sample with enabling and twin
table(fullsample$enab_score>0)

##########################################################################################################################
######delineates the subsamples based on the corrected weights
#enabling
enabling<-subset(fullsample, enab_score>0 & core==0)
#core
core_tech<-subset(fullsample, core>0 & enab_score==0)
#twin
twin<-subset(fullsample, core>0 & enab_score>0)
#main sample with techno splits
sample<-subset(fullsample, (core!=0 | enab_score!=0)) #correct sample to have only core and or enabling
#sample$technology[sample$core==0 & sample$enab_score==0]<-"smart"
sample$technology[sample$core!=0 & sample$enab_score!=0]<-"twin"
sample$technology[sample$core==0 & sample$enab_score!=0]<-"enabling"
sample$technology[sample$core!=0 & sample$enab_score==0]<-"core"
#write.csv2(sample, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//samplewosmart.csv", row.names=FALSE, col.names=TRUE, sep=";")
#write.csv2(core_tech, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//samplecoreonly.csv", row.names=FALSE, col.names=TRUE, sep=";")
#write.csv2(twin, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//sampletwin.csv", row.names=FALSE, col.names=TRUE, sep=";")
#write.csv2(enabling, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//sampleenabonly.csv", row.names=FALSE, col.names=TRUE, sep=";")


#smart appli
smart<-subset(fullsample, core==0 & enab_score==0)
smart<-subset(smart, earliestyear<=2016) #check the right amount
smartcount<-aggregate(appln_id ~ earliestyear, data=smart, FUN=length)
names(smartcount)[2]<-"smartappli"
allpatinfo<-merge(allpatinfo, smartcount, by.x = "earliestyear", by.y = "earliestyear", all.x=TRUE)


########################################################################################################################
####### descriptives analysis part 1
#########################################################################################################################
#pure counting exercise
basicount<-aggregate(appln_id ~ earliestyear, data=sample, FUN=length)

ggplot(data=basicount, aes(x=earliestyear, y=appln_id))+geom_line()+  theme_classic()+labs(x="Year",y="Patent families") +
  theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5)))
summary(basicount$appln_id)
colSums(basicount) #417807
#write.csv2(basicount, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//patentcountv2.csv", row.names=FALSE, col.names=TRUE, sep="\t", dec=",")
write.table(basicount, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//patcountall_fig3.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

#all patent families
allpatinfo<-as.data.frame(fread(file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//fam_time.tsv"))
allpatinfo<-subset(allpatinfo, EARLIEST_FILING_YEAR >=1980 & EARLIEST_FILING_YEAR<=2016)
names(allpatinfo)[2]<-"earliestyear"

#adds the core enabling and twin
allpatinfo<-merge(allpatinfo, basicount, by.x = "earliestyear", by.y = "earliestyear", all.x = TRUE)
names(allpatinfo)[4]<-"digitech"

fig1 <- melt(allpatinfo, id.vars="earliestyear")
#write.csv2(d, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//ICT_frac_count_alltech.csv", row.names=FALSE, col.names=TRUE, sep=";")

# Everything on the same plot
fig1plot<-ggplot(fig1, aes(earliestyear,value, col=variable), size=2) + geom_line(size=2) + scale_colour_manual(values=c("black", "#7CAE00","#f8766D"))
fig1plot  + scale_fill_discrete(name = "Technonologies") + theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),legend.text = element_text(size = rel(1.5)))

#adds the core enabling and twin
familycount<-merge(familycount, basicount, by.x = "earliestyear", by.y = "earliestyear", all.x = TRUE)
names(familycount)[4]<-"digitech"

#plots to compare the patenting trends in 4.0 areas and non 4.0
bettinafig<-subset(familycount, select=c("earliestyear", "smartappli", "digitech", "digi", "non4.0"))
names(bettinafig)[3]<-"Core Enab Twin"
names(bettinafig)[4]<-"4.0"
write.table(bettinafig, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig1_updatedversion.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)
fig1 <- melt(bettinafig, id.vars="earliestyear")
#write.csv2(d, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//ICT_frac_count_alltech.csv", row.names=FALSE, col.names=TRUE, sep=";")

# Everything on the same plot
fig1plot<-ggplot(fig1, aes(earliestyear,value, col=variable), size=2) + geom_line(size=2) + scale_colour_manual(values=c("coral4", "chocolate2","cadetblue3", "black"))
fig1plot  + scale_fill_discrete(name = "Technonologies") + theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),legend.text = element_text(size = rel(1.5)))
#counts up to 2016
familycount<-aggregate(docdb~earliestyear, data=familyearly, FUN=length)
familycount<-subset(familycount, earliestyear<=2016)
familycount$digi<-familycount$smartappli + familycount$digitech
familycount$non4.0<-familycount$docdb-familycount$digi
#smart appli
smart<-subset(fullsample, core==0 & enab_score==0)
smart<-subset(smart, earliestyear<=2016) #check the right amount
smartcount<-aggregate(appln_id ~ earliestyear, data=smart, FUN=length)
names(smartcount)[2]<-"smartappli"
allpatinfo<-merge(allpatinfo, smartcount, by.x = "earliestyear", by.y = "earliestyear", all.x=TRUE)

#check
table(sample$technology)

#sum stsats for tab 4
tab4<-sample %>% group_by(technology) %>% summarise(#mCPC=mean(s_digi),
                                                   # mnondi=mean(s_nondigi),
                                                   # mcore=mean(core),
                                                    menab=mean(enab_score),
                                                    manal=mean(s_analytics),
                                                    msec=mean(s_security),
                                                    mai=mean(s_ai),
                                                    mGPS=mean(s_position),
                                                    mpow=mean(s_power),
                                                    m3D=mean(s_threedim),
                                                    mint=mean(s_interface), na.rm = TRUE)
                                                   # mhard=mean(s_hardware),
                                                   # msoft=mean(s_software),
                                                   # mcon=mean(s_connectivity)
                                                   
tab4

#########fractional count
####share of IPC classes going to digitalization over time across categories
##############################################################################
digiori<-aggregate(s_digi ~ earliestyear + technology, data=sample, FUN=sum)
digiN<-aggregate(appln_id ~ earliestyear + technology, data=sample, FUN=length)
write.table(digiN, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//patcount_techsplit__fig3.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)
#quick overview of missing
colSums(digiori$s_digi)
#write.csv2(digiori, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fracpatcount_decimal.csv", row.names=FALSE, col.names=TRUE, sep=";",  dec=".")
write.table(digiori, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fracount_fig3.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

#weighted patent count: digital orientation
#############################################
ggplot(data=digiori, aes(x=earliestyear, y=s_digi, group=technology))+ geom_line(size=2, aes(color=technology))+labs(x="Year",y="",fill = "Techno")+ theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
                                                                                                                                                                                   legend.text = element_text(size = rel(1.5)),
                                                                                                                                                                                   legend.title = element_text(size = rel(1.5))) + theme_classic()
#mean of the shareof IPC classes in digital
#################################################
sharedigi<-aggregate(s_digi ~ earliestyear + technology, data=sample, FUN=mean)
#write.csv2(sharedigi, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//meandigiCPC_time_techareas.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(sharedigi, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//digi_score_fig4.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

ggplot(data=sharedigi, aes(x=earliestyear, y=s_digi, group=technology))+ geom_line(size=2, aes(color=technology))+labs(x="Year",y="Averaged share of digital CPC classes",fill = "Techno") +
  theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)))


######################################################################################################################
######################################################################################################################
###Splits of tech areas in twin, core, enabling: FRACTIONAL COUNT
#first: tries to plot the ten different trends in twin
secu<-aggregate(s_security ~ earliestyear, data=twin, FUN=sum)
interf<-aggregate(s_interface ~ earliestyear, data=twin, FUN=sum)
anal<-aggregate(s_analytics ~ earliestyear, data=twin, FUN=sum)
ai<-aggregate(s_ai ~ earliestyear, data=twin, FUN=sum)
gps<-aggregate(s_position ~ earliestyear, data=twin, FUN=sum)
threeD<-aggregate(s_threedim ~ earliestyear, data=twin, FUN=sum)
pow<-aggregate(s_power ~ earliestyear, data=twin, FUN=sum)
allenab<-cbind(secu, interf, anal, ai, gps, threeD, pow)
#allenab<-subset(allenab, technology=="enabling")
#subselects the right columns
allenab<-allenab[,c(1,2,4,6,8,10,12,14)]

####all enabling tech together over time 
#test_data_long <- melt(allenab, id="earliestyear")  # convert to long format

###same with core technologies
hard<-aggregate(s_hardware ~ earliestyear, data=twin, FUN=sum)
soft<-aggregate(s_software ~ earliestyear, data=twin, FUN=sum)
connec<-aggregate(s_connectivity ~ earliestyear, data=twin, FUN=sum)

allcore<-cbind(hard, soft, connec)
allcore<-allcore[,c(1,2,4,6)]
alltech<-cbind(allenab, allcore)
#write.csv2(alltech, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//twin_frac_count.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(alltech, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig6twin_fracpat.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

test_data_core <- melt(alltech, id="earliestyear")  # convert to long format

split<-ggplot(data=test_data_core,
              aes(x=earliestyear, y=value, colour=variable)) +scale_color_brewer(palette="Paired") +
  geom_line(size=2) + labs(x="Year", y="Fractional count", fill="technologies") +  theme_classic()
split+ theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
              legend.text = element_text(size = rel(1.5)),
              legend.title = element_text(size = rel(1.5)))


##################################################################################################################
###enabling technologies
##################################################################################################################
secu<-aggregate(s_security ~ earliestyear, data=enabling, FUN=sum)
interf<-aggregate(s_interface ~ earliestyear, data=enabling, FUN=sum)
anal<-aggregate(s_analytics ~ earliestyear, data=enabling, FUN=sum)
ai<-aggregate(s_ai ~ earliestyear, data=enabling, FUN=sum)
gps<-aggregate(s_position ~ earliestyear, data=enabling, FUN=sum)
threeD<-aggregate(s_threedim ~ earliestyear, data=enabling, FUN=sum)
pow<-aggregate(s_power ~ earliestyear, data=enabling, FUN=sum)
allenab<-cbind(secu, interf, anal, ai, gps, threeD, pow)
#allenab<-subset(allenab, technology=="enabling")
#subselects the right columns
allenab<-allenab[,c(1,2,4,6,8,10,12,14)]
####all enabling tech together over time 
#write.csv2(allenab, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//enab_frac_count.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(allenab, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig6enalbing_fracpat.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

test_data_long <- melt(allenab, id="earliestyear")
enab<-ggplot(data=test_data_long,
             aes(x=earliestyear, y=value, colour=variable)) +scale_color_brewer(palette="Paired") +
  geom_line(size=2) + labs(x="Year", y="Fractional count", fill="technologies") +  theme_classic()
enab + theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title = element_text(size = rel(1.5)))

#######################################################################################################################
####core tech trends
########################################################################################################################
hard<-aggregate(s_hardware ~ earliestyear, data=core_tech, FUN=sum)
soft<-aggregate(s_software ~ earliestyear, data=core_tech, FUN=sum)
connec<-aggregate(s_connectivity ~ earliestyear, data=core_tech, FUN=sum)

allcore<-cbind(hard, soft, connec)
allcore<-allcore[,c(1,2,4,6)]
#write.csv2(allcore, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//core_frac_count.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(allcore, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig6core_fracpat.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

test_data_core <- melt(allcore, id="earliestyear")  # convert to long format

split<-ggplot(data=test_data_core,
              aes(x=earliestyear, y=value, colour=variable)) +scale_color_manual(values=c("orange", "mediumpurple1", "darkmagenta")) +
  geom_line(size=2) + labs(x="Year", y="Fractional count", fill="technologies") +  theme_classic()
split+ theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
               legend.text = element_text(size = rel(1.5)),
               legend.title = element_text(size = rel(1.5)))

###########################################################################################################################
#####splits of digital intensity across technologies over time
###########################################################################################################################
#first: tries to plot the ten different trends in twin
######digital intensity over time across technologies
secu<-aggregate(s_security ~ earliestyear, data=twin, FUN=mean)
interf<-aggregate(s_interface ~ earliestyear, data=twin, FUN=mean)
anal<-aggregate(s_analytics ~ earliestyear, data=twin, FUN=mean)
ai<-aggregate(s_ai ~ earliestyear, data=twin, FUN=mean)
gps<-aggregate(s_position ~ earliestyear, data=twin, FUN=mean)
threeD<-aggregate(s_threedim ~ earliestyear, data=twin, FUN=mean)
pow<-aggregate(s_power ~ earliestyear, data=twin, FUN=mean)
allenab<-cbind(secu, interf, anal, ai, gps, threeD, pow)
#allenab<-subset(allenab, technology=="enabling")
#subselects the right columns
allenab<-allenab[,c(1,2,4,6,8,10,12,14)]

####all enabling tech together over time 
#test_data_long <- melt(allenab, id="earliestyear")  # convert to long format

###same with core technologies
hard<-aggregate(s_hardware ~ earliestyear, data=twin, FUN=mean)
soft<-aggregate(s_software ~ earliestyear, data=twin, FUN=mean)
connec<-aggregate(s_connectivity ~ earliestyear, data=twin, FUN=mean)

allcore<-cbind(hard, soft, connec)
allcore<-allcore[,c(1,2,4,6)]
alltech<-cbind(allenab, allcore)
#write.csv2(alltech, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//twin_mean_time.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(alltech, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig7twin_intensity.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

test_data_core <- melt(alltech, id="earliestyear")  # convert to long format

split<-ggplot(data=test_data_core,
              aes(x=earliestyear, y=value, colour=variable)) +scale_color_brewer(palette="Paired") +
  geom_line(size=2) + labs(x="Year", y="Digital intensity", fill="technologies") +  theme_classic()
split+ theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
             legend.text = element_text(size = rel(1.5)),
             legend.title = element_text(size = rel(1.5)))


##################################################################################################################
###enabling technologies
##################################################################################################################
secu<-aggregate(s_security ~ earliestyear, data=enabling, FUN=mean)
interf<-aggregate(s_interface ~ earliestyear, data=enabling, FUN=mean)
anal<-aggregate(s_analytics ~ earliestyear, data=enabling, FUN=mean)
ai<-aggregate(s_ai ~ earliestyear, data=enabling, FUN=mean)
gps<-aggregate(s_position ~ earliestyear, data=enabling, FUN=mean)
threeD<-aggregate(s_threedim ~ earliestyear, data=enabling, FUN=mean)
pow<-aggregate(s_power ~ earliestyear, data=enabling, FUN=mean)
allenab<-cbind(secu, interf, anal, ai, gps, threeD, pow)
#allenab<-subset(allenab, technology=="enabling")
#subselects the right columns
allenab<-allenab[,c(1,2,4,6,8,10,12,14)]
####all enabling tech together over time 
#write.csv2(allenab, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//enab_mean_digi_time.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(allenab, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig7enabling_intensity.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

test_data_long <- melt(allenab, id="earliestyear")
enab<-ggplot(data=test_data_long,
             aes(x=earliestyear, y=value, colour=variable)) +scale_color_brewer(palette="Paired") +
  geom_line(size=2) + labs(x="Year", y="digital intemsity", fill="technologies") +  theme_classic()
enab + theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
             legend.text = element_text(size = rel(1.5)),
             legend.title = element_text(size = rel(1.5)))

#######################################################################################################################
####core tech trends
########################################################################################################################
hard<-aggregate(s_hardware ~ earliestyear, data=core_tech, FUN=mean)
soft<-aggregate(s_software ~ earliestyear, data=core_tech, FUN=mean)
connec<-aggregate(s_connectivity ~ earliestyear, data=core_tech, FUN=mean)

allcore<-cbind(hard, soft, connec)
allcore<-allcore[,c(1,2,4,6)]
write.table(allcore, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig7core_intensity.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

test_data_core <- melt(allcore, id="earliestyear")  # convert to long format

split<-ggplot(data=test_data_core,
              aes(x=earliestyear, y=value, colour=variable)) +scale_color_manual(values=c("orange", "mediumpurple1", "darkmagenta")) +
  geom_line(size=2) + labs(x="Year", y="Digital intensity", fill="technologies") +  theme_classic()
split+ theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
             legend.text = element_text(size = rel(1.5)),
             legend.title = element_text(size = rel(1.5)))

##################################################################
####NACE
###############################################################
naceinfo<-fread(file="gwp_16_NACE.txt", header=TRUE, dec=",")
table(naceinfo$NACE2_CODE) #already aggregated by Vanessa

#merges with the sample of digi tech considered
samplenace<-merge(sample, naceinfo, by.x = "appln_id", by.y = "APPLN_ID", all.x = TRUE)
samplenace$NACE2<-substr(samplenace$NACE2_CODE, 1, 2)
samplenace<-unique(samplenace) #multiple entries due to the different nace weights for different categories
#all digital tech
digitalnace<-subset(samplenace, s_digi>0)
#all enabling
enablingnace<-subset(samplenace, enab_score>0 & core==0)
length(unique(enablingnace$appln_id))
#all core
corenace<-subset(samplenace, core>0 & enab_score==0)
length(unique(corenace$appln_id))
#all twins
twinnace<-subset(samplenace, core>0 & enab_score>0)
length(unique(twinnace$appln_id))

##aggregates the dynamic across sectors and periods
####################################################
sectwin<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=twinnace, FUN=sum)
secenab<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=enablingnace, FUN=sum)
seccore<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=corenace, FUN=sum)
###different lengths between both vectors showing that enabling are more concentrated in a few sectors at some periods
secenab<-merge(sectwin, secenab, by.x = c("factor(earliestyear)", "NACE2"), by.y = c("factor(earliestyear)", "NACE2"), all.y=TRUE)
secenab$WEIGHT.y[is.na(secenab$WEIGHT.y)]<-0


secenab<-merge(secenab, seccore, by.x = c("factor(earliestyear)", "NACE2"), by.y = c("factor(earliestyear)", "NACE2"), all.x=TRUE)
secenab$WEIGHT.x[is.na(secenab$WEIGHT.x)]<-0
secenab$WEIGHT[is.na(secenab$WEIGHT)]<-0

###respective performance in terms of use across sectors producing 4.0 tech
names(secenab)[1]<-"year"
names(secenab)[3]<-"WeightPatEnab"
names(secenab)[4]<-"WeightPatTwin"
names(secenab)[5]<-"WeightPatCore"


#creates a dummy for manufacturing types
secenab$NACE2 <- as.numeric(as.character(secenab$NACE2)
                            #  table(secenab$NACE2)
                            #HT
                            secenab$manuf<-0
                            secenab$manuf[secenab$NACE2==26 | secenab$NACE2==21]<-"HT"
                            #  table(secenab$HTmanuf)
                            
                            #med high manuf
                            secenab$manuf[secenab$NACE2==20 | (secenab$NACE2>=27 &secenab$NACE2<=30)]<-"medHT"
                            #table(secenab$medhmanuf)
                            
                            #med low tech manu
                            secenab$manuf[secenab$NACE2==19 | (secenab$NACE2>=22 &secenab$NACE2<=25)]<-"medlow"
                            #table(secenab$medlowmanuf)
                            
                            #low tech
                            secenab$manuf[secenab$NACE2==31 | secenab$NACE2==32 | (secenab$NACE2>=10 &secenab$NACE2<=18)]<-"LT"
                            
                            #sum of patenting between across manufacturing types in both trends
                            enabmanuf<-aggregate(WeightPatEnab~manuf+year, data = secenab, FUN=sum)
                            twinmanuf<-aggregate(WeightPatTwin~manuf+year, data = secenab, FUN=sum)
                            coremanuf<-aggregate(WeightPatCore~manuf+year, data = secenab, FUN=sum)
                            
                            totalenab<-aggregate(WeightPatEnab~year, data = secenab, FUN=sum)    
                            totaltwin<-aggregate(WeightPatTwin~year, data = secenab, FUN=sum)
                            totalcore<-aggregate(WeightPatCore~year, data = secenab, FUN=sum)
                            
                            #makes the share for all cases
                            totalenab<-merge(totalenab, enabmanuf, by.x = "year", by.y = "year", all.x = TRUE)
                            totalcore<-merge(totalcore, coremanuf, by.x = "year", by.y = "year", all.x = TRUE)
                            totaltwin<-merge(totaltwin, twinmanuf, by.x = "year", by.y = "year", all.x = TRUE)
                            
                            totalenab$share<-totalenab$WeightPatEnab.y/totalenab$WeightPatEnab.x
                            totalcore$share<-totalcore$WeightPatCore.y/totalcore$WeightPatCore.x
                            totaltwin$share<-totaltwin$WeightPatTwin.y/totaltwin$WeightPatTwin.x
                            
                            totalenab$year <- as.numeric(as.character(totalenab$year))
                            totalcore$year <- as.numeric(as.character(totalcore$year))
                            totaltwin$year <- as.numeric(as.character(totaltwin$year))
                            
                            
                            #plots with different lines types the type of tech and colors the sector type
                            ggplot(data=totalenab, aes(x=year))+geom_line(aes(y = share, color = factor(manuf)))+ labs(x="Year",y="Weighted share of enabling patents in manufacturing")
                            ggplot(data=totalcore, aes(x=year))+geom_line(aes(y = share, color = factor(manuf)))+ labs(x="Year",y="Weighted share of core patents in manufacturing")
                            ggplot(data=totaltwin, aes(x=year))+geom_line(aes(y = share, color = factor(manuf)))+ labs(x="Year",y="Weighted share of twin patents in manufacturing")
                            
                            
                            
                            
                            
               
  
  #takes the families up to 2016
  familydigi<-subset(familydigi, earliestyear<=2016)
  familydigi<-merge(familydigi, naceinfo, by.x = "appln_id", by.y = "appln_id", all.x = TRUE)

                            #aggreagtes the relative weight nace2 at the nace and earliest year level to get the importance of a sector in a given year
                            secdigi<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=digitalnace, FUN=sum)
                            secenab<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=enablingnace, FUN=sum)
                            seccore<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=corenace, FUN=sum)
                            sectwin<-aggregate(WEIGHT ~ NACE2 + factor(earliestyear), data=twinnace, FUN=sum)
 ###############################################################################
  ########distinguishes the number of sectors from the sum of the NACE weights
  ####################################################
  Ndigi<-aggregate(NACE2 ~ year, data=secdigi, FUN=length)
 Nenab<-aggregate(NACE2~ year, data=secenab, FUN=length)
Ndigi<-merge(Ndigi, Nenab, by.x = "year", by.y = "year", all.x = TRUE)
 Nnonen<-aggregate(NACE2~ year, data=seccore, FUN=length)
  Ndigi<-merge(Ndigi, Nnonen, by.x = "year", by.y = "year", all.x = TRUE)
  names(Ndigi)[2]<-"Digital"
  names(Ndigi)[3]<-"Enabling"
   names(Ndigi)[4]<-"Core"
 ##adds the twins
 Ntwin<-aggregate(NACE2~ year, data=sectwin, FUN=length)
Ndigi<-merge(Ndigi, Ntwin, by.x = "year", by.y = "year", all.x = TRUE)
names(Ndigi)[5]<-"Twin"
  Ndigi$year <- as.numeric(as.character(Ndigi$year))
  write.table(Ndigi, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig9_sectorsall.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)
  
 test_data_long2 <- melt(Ndigi, id="year")  # convert to long format
 #exports the serie
 write.csv2(test_data_long2, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//sectors_time_alldigitech.csv", row.names=FALSE, col.names=TRUE, sep=";")
 
 
 
#plots with a linear approximation to show the trend underpinning the diffusion to other sectors                       
Nsec<-ggplot(data=test_data_long2, aes(x=year, y=value, colour=variable)) + geom_line(show.legend = T) + geom_smooth(aes(y =value, color = variable, size=0.6), method = lm, linetype = 3, se = F, show.legend = T) +  ggtitle("Number of patenting sectors over time") + labs(x="Year", y="Number of sectors") + scale_colour_manual(values=c("black", "#7CAE00","#f8766D", "#00BFC4")) + theme_classic()
Nsec +  theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
         legend.text = element_text(size = rel(1.5)),
         legend.title = element_text(size = rel(1.5)))

#########################################################################################                          
####summarises the weights across tech areas and year
########################################################################################
###different lengths between both vectors showing that enabling are more concentrated in a few sectors at some periods
length(unique(secenab$NACE2)) #same amount of sectors in both
length(unique(secdigi$NACE2))
secdigi<-merge(secdigi, secenab, by.x = c("factor(earliestyear)", "NACE2"), by.y = c("factor(earliestyear)", "NACE2"), all.x=TRUE)
secdigi$WEIGHT.y[is.na(secdigi$WEIGHT.y)]<-0
length(unique(secdigi$WEIGHT.x))
secdigi<-merge(secdigi, seccore, by.x = c("factor(earliestyear)", "NACE2"), by.y = c("factor(earliestyear)", "NACE2"), all.x=TRUE)

###respective performance in terms of use across sectors producing 4.0 tech
names(secdigi)[1]<-"year"
names(secdigi)[3]<-"WeightDigi"
names(secdigi)[4]<-"WeightEnab"
names(secdigi)[5]<-"WeightNonEnab"
secdigi$WeightNonEnab[is.na(secdigi$WeightNonEnab)]<-0

#####merges with twin
secdigi<-merge(secdigi, sectwin, by.x = c("year", "NACE2"), by.y = c("factor(earliestyear)", "NACE2"), all.x=TRUE)
names(secdigi)[6]<-"WeightTwin"
secdigi$WeightTwin[is.na(secdigi$WeightTwin)]<-0
#harmonizes years
names(secenab)[2]<-"year"
names(seccore)[2]<-"year"
names(sectwin)[2]<-"year"
secdigi<-secdigi[,c(1:6)]
######################################################################                            
 #creates a dummy for manufacturing types
#######################################################################
 secdigi$NACE2 <- as.numeric(as.character(secdigi$NACE2)
table(secdigi$NACE2)
#creates a blackbox in manuf
secdigi$manuf<-0

#HT
secdigi$manuf[secdigi$NACE2==26 | secdigi$NACE2==21]<-"HT"
#  table(secdigi$HTmanuf)

#med high manuf
secdigi$manuf[secdigi$NACE2==20 | (secdigi$NACE2>=27 &secdigi$NACE2<=30)]<-"medHT"
#table(secdigi$medhmanuf)

#med low tech manu
secdigi$manuf[secdigi$NACE2==19 | (secdigi$NACE2>=22 &secdigi$NACE2<=25)]<-"medlow"
#table(secdigi$medlowmanuf)

#low tech
secdigi$manuf[secdigi$NACE2==31 | secdigi$NACE2==32 | (secdigi$NACE2>=10 &secdigi$NACE2<=18)]<-"LT"

#checks the potential number of sectors related to ICT
table(secdigi$manuf, secdigi$NACE2)

#sum of patenting between across manufacturing types in both trends //
####digital share
manufdigi<-aggregate(WeightDigi~manuf+year, data = secdigi, FUN=sum)

##here 0 refers to construction and ICT - removed to plot only manufacturing
manufdigi<-subset(manufdigi, manuf %in% c("HT", "medHT", "medlow", "LT"))

#plots with different lines types the type of tech and colors the sector type
manufdigi$year <- as.numeric(as.character(manufdigi$year))
#exports the file
#(manufdigi, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//manufacturing_frac_count_digital.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(manufdigi, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig11.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)


#digital fractional count
ggplot(data=manufdigi, aes(x=year))+geom_line(aes(y = WeightDigi, color = factor(manuf)), size=2)  + ggtitle("") + labs(x="Year",y="")+ scale_fill_discrete(name = "Techno", labels = c("High Tech", "Low Tech", "Med/High Tech", "Med/Low Tech")) + theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
                                                                                                                                                                                                                                                                            test_data_long2                                                                                                                                                                                                                                                                    legend.text = element_text(size = rel(1.5)),
                                                                                                                                                                                                                                                                    legend.title = element_text(size = rel(1.5))) 
##################################################
#enabling fractional count
##################################################
manufenab<-aggregate(WeightEnab~manuf+year, data = secdigi, FUN=sum)
manufenab<-subset(manufenab, manuf %in% c("HT", "medHT", "medlow", "LT"))
manufenab$year <- as.numeric(as.character(manufenab$year))  
#exports and plots
#write.csv2(manufenab, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//manufacturing_frac_count_enabling.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(manufenab, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig12.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

ggplot(data=manufenab, aes(x=year))+geom_line(aes(y = WeightEnab, color = factor(manuf)), size=2)  + ggtitle("") + labs(x="Year",y="")+ scale_fill_discrete(name = "Techno", labels = c("High Tech", "Low Tech", "Med/High Tech", "Med/Low Tech")) + theme_classic() +  theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
                                                                                                                                                                                                                                                                                               legend.text = element_text(size = rel(1.5)),
                                                                                                                                                                                                                                                                                               legend.title = element_text(size = rel(1.5))) 
                                                                                                                                                                                                                                                                            
#core fractional count
manufcore<-aggregate(WeightNonEnab~manuf+year, data = secdigi, FUN=sum)
manufcore<-subset(manufcore, manuf %in% c("HT", "medHT", "medlow", "LT"))
manufcore$year <- as.numeric(as.character(manufcore$year))
#write.csv2(manufcore, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//manufacturing_frac_count_core.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(manufcore, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig13.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

ggplot(data=manufcore, aes(x=year))+geom_line(aes(y = WeightNonEnab, color = factor(manuf)), size=2)  + ggtitle("") + labs(x="Year",y="")+ scale_fill_discrete(name = "Techno", labels = c("High Tech", "Low Tech", "Med/High Tech", "Med/Low Tech")) + theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
                                                                                                                                                                                                                                                                            legend.title = element_text(size = rel(1.5))) 
#twin fractional count
manuftwin<-aggregate(WeightTwin~manuf+year, data = secdigi, FUN=sum)
manuftwin<-subset(manuftwin, manuf %in% c("HT", "medHT", "medlow", "LT"))
manuftwin$year <- as.numeric(as.character(manuftwin$year))
#write.csv2(manuftwin, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//manufacturing_frac_count_twin.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(manuftwin, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig14.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

ggplot(data=manuftwin, aes(x=year))+geom_line(aes(y = WeightTwin, color = factor(manuf)), size=2)  + ggtitle("") + labs(x="Year",y="")+ scale_fill_discrete(name = "Techno", labels = c("High Tech", "Low Tech", "Med/High Tech", "Med/Low Tech")) + theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
                                                                                                                                                                                                                                                                               legend.text = element_text(size = rel(1.5))
###############################################################################
####ICT aside as nace 62
#############################
ICT<-subset(secdigi, NACE2 %in% c("62"))
ICT<-ICT[,c(1,3,4,5,6)]  
ICT$year <- as.numeric(as.character(ICT$year))

d <- melt(ICT, id.vars="year")
#write.csv2(d, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input_graphs//ICT_frac_count_alltech.csv", row.names=FALSE, col.names=TRUE, sep=";")
write.table(ICT, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig10.txt", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

# Everything on the same plot
ictplot<-ggplot(d, aes(year,value, col=variable), size=2) + geom_line(size=2) + scale_colour_manual(values=c("black", "#7CAE00","#f8766D", "#00BFC4"))
ictplot  + scale_fill_discrete(name = "Technonologies", labels = c("Digital", "Enabling only", "Core only", "Twin")) + theme_classic() +theme(axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),legend.text = element_text(size = rel(1.5)))
                                                                                                                                                                                                    


########################################################
#correlation   between the different enabling tech different with core --- proxy techn complementarity
#########################################################
###Applied to Twin tech only
complem<-subset(twin, select=c("s_security", "s_ai", "s_position", "s_power", "s_threedim", "s_interface", "s_analytics",
                               "s_hardware", "s_software", "s_connectivity", "earliestyear"))
names(complem)[1]<-"Security"
names(complem)[2]<-"AI"
names(complem)[3]<-"GPS"
names(complem)[4]<-"Power"
names(complem)[5]<-"3D"
names(complem)[6]<-"Interface"
names(complem)[7]<-"Analytics"
names(complem)[8]<-"Hardware"
names(complem)[9]<-"Software"
names(complem)[10]<-"Connectivity"

#missing among some tech areas?
colSums(complem)
complem<-complem[complete.cases(complem), ]
table(complem$earliestyear)

###Dynamic: comparative analysis across 4 main periods of developments
###early phase: 1980-1995
compl1<-subset(complem, earliestyear>=1980 & earliestyear<=1989)
####mid phase: 1996-2004
compl2<-subset(complem, earliestyear>=1990 & earliestyear<=1999)

###advanced phase: 2005-2012
compl3<-subset(complem, earliestyear>=2000 & earliestyear<=2009)

###recent phase: 2013-2016
compl4<-subset(complem, earliestyear>=2010 & earliestyear<=2016)

##removes years from the db
compl1<-compl1[,-c(11)]
compl2<-compl2[,-c(11)]
compl3<-compl3[,-c(11)]
compl4<-compl4[,-c(11)]

#overall
compl0<-complem[,-c(11)]
m0<-round(cor(compl0), 1)
ggcorrplot(m0)

#corr1
m1<-round(cor(compl1), 1)
ggcorrplot(m1)

#cor(complem)

#corr2
m2<-round(cor(compl2), 1)
ggcorrplot(m2)

#cor(complem)

#corr3
m3<-round(cor(compl3), 1)
ggcorrplot(m3)

#cor(complem)

#corr4
m4<-round(cor(compl4), 1)
ggcorrplot(m4)


#################################################################################################################################################
#DASHBOARDS --- overview key results on Tableau
################################################################################################################################################
#######quick aggregated trends for the interactive dashboards
##################################################################
digital<-read.table(file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig11.txt", header=TRUE, sep="\t")
digisum<-digital %>% group_by(year) %>% summarise (sumyear=sum(WeightDigi))

digital<-merge(digital, digisum, by.x = "year", by.y = "year", all.x=TRUE)
digital$share<-digital$WeightDigi/digital$sumyear
write.table(digital, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//share_dashboard.txt", row.names=FALSE, col.names=TRUE, sep=";", dec=",")

##cross sectorial trends
ict<-read.csv2(file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//fig10.txt", sep="\t", header=TRUE, dec=".")
ict<-ict[,c(1,2)]
dash<-read.csv2(file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//dashboardGWP.csv", sep=";", header=TRUE)
dash<-dash[,c(1:3)]
names(dash)[2]<-"sector"
ict$sector<-"ICT"


ftp<-melt(ict, id.vars=c("year", "sector"))
ftp<-ftp[,c(1,2,4)]
names(ftp)[3]<-"WeightDigi"
alljoin<-rbind(dash, ftp)
alljoin<-arrange(alljoin, -desc(year))

sum<-aggregate(WeightDigi ~ year, data=alljoin, FUN=sum)
addsfraccount<-alljoin %>% group_by(year) %>% summarise(sum=sum(WeightDigi))
alljoin<-merge(alljoin, addsfraccount, by.x=c("year"), by.y = c("year"), all.x = TRUE)
alljoin$share<-alljoin$WeightDigi/alljoin$sum
alljoin$codesect[alljoin$sector=="HT"]<-4
alljoin$codesect[alljoin$sector=="medHT"]<-3
alljoin$codesect[alljoin$sector=="medlow"]<-2
alljoin$codesect[alljoin$sector=="LT"]<-1
alljoin$codesect[alljoin$sector=="ICT"]<-5
table(alljoin$codesect)

write.csv2(alljoin, file="I://!Projekte//EU_H2020_GrowInPro_131327//WP1//T1_4_Industry4//tables_input//sectorial_frac_count.csv", sep=";", col.names = TRUE, row.names=TRUE)

