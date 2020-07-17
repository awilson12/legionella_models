#automated



rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if("truncdist" %in% rownames(installed.packages())==FALSE){install.packages("truncdist"); require(truncdist)}else{require(truncdist)}
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}

suppressMessages(suppressWarnings(source("comparison_to_allegra.R")))
suppressMessages(suppressWarnings(source("schoen_ashbolt_water.R")))


#---------- schoen & ashbolt-----------------------------------
#comparison to simulated shower from Allegra et al.
schoen.ashbolt(10000,C.air=2.9e3,IR=7.5e-3)
model.simshower<-model

ggplot(data=model.simshower)+geom_histogram(aes(x=DD),color="black")+
  geom_vline(xintercept=22,linetype="dashed",colour="red",size=1)+
  scale_x_continuous(name="Deposited Dose")+theme_pubr()

#comparison to vibrating-mesh nebulizer
schoen.ashbolt(10000,C.air=8.9e3,IR=7.5e-3)
model.vibmesh<-model

ggplot(data=model.vibmesh)+geom_histogram(aes(x=DD),color="black")+
  geom_vline(xintercept=22,linetype="dashed",colour="red",size=1)+
  scale_x_continuous(name="Deposited Dose")+theme_pubr()

#using water conc as start as opposed to air
schoen.ashbolt.water(10000,C.water=3.3e4,IR=7.5e-3)
model.shower<-model2

#-------------hamilton-----------------------------------------

#hamilton with water conc from simulated shower
hamilton(10000,IR=7.5e-3)


#------------comparison-------------------------------------------

#setting up vectors for frame
DD.all<-c(model.shower$DD,model.conv$Dose.fixture.conv,model.eff$Dose.fixture.eff)
DD.all<-c(DD.all,DD.all)
models<-c(rep("Schoen & Ashbolt",10000),rep("Hamilton",10000*2),rep("Combined",10000*3))
showertype<-c(rep("conventional or unspecified",10000*2),rep("water efficient",10000),rep("combined",10000*3))

#creating frame
frameall<<-data.frame(Dose=DD.all,model=models,showertype=showertype)

#comparison plot
windows()
ggplot(data=frameall)+geom_violin(aes(x=model,y=Dose,colour=showertype),draw_quantiles = c(0.25,0.5,0.75))+scale_y_continuous(name="Dose",trans="log10")+
  scale_x_discrete(name="Model Source")+scale_colour_discrete(name="Shower Type")+theme_pubr()
