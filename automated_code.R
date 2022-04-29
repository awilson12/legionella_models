#automated



rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if("truncdist" %in% rownames(installed.packages())==FALSE){install.packages("truncdist"); require(truncdist)}else{require(truncdist)}
if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2"); require(ggplot2)}else{require(ggplot2)}
if("ggpubr" %in% rownames(installed.packages())==FALSE){install.packages("ggpubr"); require(ggpubr)}else{require(ggpubr)}
if("triangle" %in% rownames(installed.packages())==FALSE){install.packages("triangle"); require(triangle)}else{require(triangle)}

#----------------------simulation code---------------------------------

suppressMessages(suppressWarnings(source("UA_ecolab_2020_20200115.R")))


comparison(10000,7.8,100)
suppressMessages(suppressWarnings(source("probability_of_illness estimation.R")))

#-----------------------validation code--------------------------------

suppressMessages(suppressWarnings(source("comparison_to_allegra.R")))

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
  scale_fill_manual(name="",values=c("#0066CC","#3300CC"))+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Deposited Dose",trans="log10")+theme_pubr()+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18))

mean(data$Dose[data$model=="Schoen & Ashbolt"])
mean(data$Dose[data$model=="Hamilton et al"])

test.output<-wilcox.test(data$Dose[data$model=="Schoen & Ashbolt"],data$Dose[data$model=="Hamilton et al"])
t.test(data$Dose[data$model=="Schoen & Ashbolt"],data$Dose[data$model=="Hamilton et al"])
