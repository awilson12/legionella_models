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
model<-c(rep("Model 1",10000),rep("Model 2",10000))
data<-data.frame(Dose=Dose,model=model)

tiff("figure3.tiff", units="in",width=6,height=7,res=600)
ggplot(data)+geom_histogram(aes(x=Dose,y=..density..,group=model,fill=model),color="black",bins = 50,alpha=0.3)+
  geom_density(aes(x=Dose,group=model,fill=model),alpha=0.4)+
  scale_fill_manual(name="",values=c("#0066CC","#3300CC"))+
  scale_y_continuous(name="Density")+
  scale_x_continuous(name="Deposited Dose",trans="log10")+theme_pubr()+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18),legend.text=element_text(size=14),
        legend.position = "none",strip.text = element_text(size=18))+
  facet_wrap(~model,ncol=1)
dev.off()

mean(data$Dose[data$model=="Model 1"])
mean(data$Dose[data$model=="Model 2"])

test.output<-wilcox.test(data$Dose[data$model=="Model 1"],data$Dose[data$model=="Model 2"])
t.test(data$Dose[data$model=="Model 1"],data$Dose[data$model=="Model 2"])
