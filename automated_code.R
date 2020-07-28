#automated



rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if("truncdist" %in% rownames(installed.packages())==FALSE){install.packages("truncdist"); require(truncdist)}else{require(truncdist)}
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}

#----------------------simulation code---------------------------------

suppressMessages(suppressWarnings(source("UA_ecolab_2020_20200115.R")))





#-----------------------validation code--------------------------------

suppressMessages(suppressWarnings(source("comparison_to_allegra.R")))
suppressMessages(suppressWarnings(source("schoen_ashbolt_water.R")))


#comparison to simulated shower from Allegra et al.
schoen.ashbolt(10000,C.air=2.9e3,IR=7.5e-3)
model.simshower.ashbolt<-model
hamilton(10000,C.air=2.9e3,IR=7.5e-3)
model.simshower.hamilton<-model

Dose<-c(model.simshower.ashbolt$DD,model.simshower.hamilton$DD)
model<-c(rep("Schoen & Ashbolt",10000),rep("Hamilton et al",10000))
data<-data.frame(Dose=Dose,model=model)

windows()
ggplot(data)+geom_histogram(aes(x=Dose,y=..density..,group=model,fill=model),color="black",bins = 50,alpha=0.3)+
  geom_density(aes(x=Dose,group=model,fill=model),alpha=0.4)+
  scale_fill_discrete(name="")+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Deposited Dose")+theme_pubr()

windows()
ggplot(data)+geom_histogram(aes(x=22-Dose,y=..density..,group=model,fill=model),color="black",bins = 50,alpha=0.3)+
  geom_density(aes(x=22-Dose,group=model,fill=model),alpha=0.4)+
  scale_fill_discrete(name="")+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Deposited Dose")+theme_pubr()



#comparison to vibrating-mesh nebulizer
#schoen.ashbolt(10000,C.air=8.9e3,IR=7.5e-3)
#model.vibmesh<-model

#ggplot(data=model.vibmesh)+geom_histogram(aes(x=DD),color="black")+
#  geom_vline(xintercept=22,linetype="dashed",colour="red",size=1)+
#  scale_x_continuous(name="Deposited Dose")+theme_pubr()

#using water conc as start as opposed to air
#schoen.ashbolt.water(10000,C.water=3.3e4,IR=7.5e-3)
#model.shower<-model2
