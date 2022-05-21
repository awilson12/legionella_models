

require(truncdist)
require(triangle)
require(ggplot2)
require(ggpubr)



#---------PART 1 - STOCHASTIC APPROACH TO SCHOEN & ASHBOLT (2011) MODEL--------------------

set.seed(37)

# This model is based on one discussed by Schoen & Ashbolt (2011) An in-premise model for
# Legionella exposure during showering events

schoen.ashbolt<-function(iterations,showerduration,C.water){
  
  #model function inputs
  #1-iterations
  #2-shower duration (minutes)
  #3-C.water (concentration of L. pneumophila in water - CFU/L)
  
  #-----------model input parameters--------------------------
  
  #flow rate (L/hr)
  FR<-rtriangle(iterations,a=350,b=370,c=360)
  #min = best estimate - 10 L/hr
  #max = best estimate + 10 L/hr
  #ARBITRARY BOUNDS RIGHT NOW
  
  #convert to L/min since inhalation rates are in m^3/min
  FR<-FR/60 # L/hr x 1 hr /60 min = L/min
  
  #Breathing rate (m^3/min) (21 years to greater than 81)
  #used table 6-2 from exposure factors handbook
  #mean of 1.2e-2 and used 1.7e-2 95th percentile to estimate
  #SD where SD ~ (95th percentile-mean)/2
  sd.breathing<-((1.7E-2)-(1.2E-2))/2
  IR<-rtrunc(iterations,"norm",a=0,mean=1.2E-2,sd=sd.breathing)
  
  #min = best estimate value
  #max = high value
  
  #partitioning coefficient
  PC<-rtriangle(iterations,a=1*10^-6,c=1*10^-5,b=1.9*10^-5)
  #min = low value
  #max = (Best estimate value - low value) + Best estimate value
  
  #-------aerosolizing parameters--------------------------------
  
  #----------aerosol size 1 - 5 um--------------
  
  #fraction of total aerosolized organisms in aeorosol size 1-5
  #fraction of total aerosolized organisms in aeorosol size 1-5
  F1.15<-rtriangle(iterations,a=0.5,c=0.75,b=1)
  #min = best estimate value
  #max = high value
  
  #fraction of total aerosols of size range 1-5um deposited at the alveoli
  F2.15<-rtriangle(iterations,a=0,c=0.2,b=0.54)
  
  #min = best estimate value
  #max = high value
  
  #---------aerosol size 5 - 6 um---------------
  
  #fraction of total aerosolized organisms in aeorosol size 5-6
  #F1.56<-(1-F1.15)*0.36
  #12% of aerosols other than 1-5
  
  #fraction of total aerosols of size range 5-6 deposited at the alveoli
  #F2.56<-rtriangle(iterations,a=0.1,c=0.65,b=1)
  
  #--------aerosol size 6 - 10 um----------------
  
  #fraction of total aerosolized organisms in aerosol size 6-10
  #F1.610<-(1-F1.15)*0.56
  
  #fraction of total aerosols of size range 6-10 deposited at the alveoli
  #F2.610<-rtriangle(iterations,a=0.01,c=0.1,b=1)
  
  ###########################################################
  
  #concentration of legionella in water (CFU/L)
  #c.water<-W.water/V.water #I think this equation is only needed if accounting for biofilm
  
  #exponential dose response
  k<-rlnorm(iterations,-2.93,0.49)
  #k<-5.99e-02
  #initializing vectors
  DD<-rep(0,iterations)
  infection.risk<-rep(0,iterations)
  
  C.air<-rep(0,iterations)
  V.air<-rep(0,iterations)
  exposure<-rep(0,iterations)
  exposure.char<-rep(0,iterations)
  
  #in minutes
  exposure<-showerduration
  
  #volume of water used in shower  
  V.water<-FR*exposure
  
  V.air<-IR*exposure
  
  #concentration of legionella in air (CFU/m^3)
  C.air<-C.water*PC
  
  #calculating deposited dose
  DD<-C.air*V.air*(F1.15*F2.15)
  
  #calculating infection risk
  infection.risk<-1-exp(-k*(DD))
  
  
  #saving inputs and outputs
  
  model<<-data.frame(FR=FR,IR=IR,PC=PC,F1.15=F1.15,F2.15=F2.15,k=k,DD=DD,infection.risk=infection.risk)
  require(ggplot2)
  require(ggpubr)
  require(corrplot)
  require(gridExtra)
  require(grid)
  require(gridGraphics)
  
  mydata.cor=cor(model,method=c("spearman"))
  spearman.cor<-mydata.cor[,8]
  inputvar<-c("FR","IR","PC","F1.15","F2.15",
              "k","DD","Infection Risk")
  outputvar<-"Infection Risk"
  
  frame.schoen<-data.frame(spearman.cor=spearman.cor,inputvar=inputvar,outputvar=outputvar)
  frame.schoen<-frame.schoen[frame.schoen$inputvar!="Infection Risk",]
  
  
  frame.schoen$inputvar<-factor(frame.schoen$inputvar,levels=inputvar)
  
  plotA<<-ggplot(frame.schoen)+geom_tile(aes(x=outputvar,y=inputvar,fill=spearman.cor))+
    geom_text(aes(label=signif(spearman.cor,2),x=outputvar,y=inputvar),size=7)+
    scale_fill_gradient2(low="99CCCC",mid="white",high="#6666FF",name="",midpoint=0)+
    theme_bw()+
    theme(axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.key.width = unit(1, "cm"),
          legend.position="none",
          plot.title = element_text(hjust = 0.5,size=16))+
    scale_x_discrete(name="")+
    scale_y_discrete(name="")+
    ggtitle("Schoen & Ashbolt")
  
}


schoen.ashbolt(10000,8,100)


#--------PART 2 - HAMILTON ET AL. (2019) MODEL----------------------------------------------

# This model is based on one discussed by Hamilton et al. (2019) Risk-based critical levels
# of Legionella pneumophila for indoor water uses. Environmental Science & Technology.

# "volume of aerosols of various size diameters that are large enough to hold
# L. pneumophila bacteria but small enough to deposit at the alveoli (1 um < diameter < 10um)
# were considered" - equation 2 from paper


hamilton<-function(iterations,showerduration,C.water){
  
  
  #model function inputs
  #1-iterations
  #2-shower duration (minutes)
  #3-C.water (concentration of L. pneumophila in water - CFU/mL)
  
  
  #truncnorm dist
  require(truncnorm)
  
  #vectors for loop
  
  #Dose for conventional fixture
  Dose.fixture.conv<-rep(0,iterations)
  
  #Dose for water efficient fixture
  Dose.fixture.eff<-rep(0,iterations)
  
  #Breathing rate
  B<-rep(0,iterations)
  
  #Exposure time
  t.shower<-rep(0,iterations)
  
  #Concentration of legionella in water
  C.leg<-rep(0,iterations)
  
  #Probability of infection - conventional fixture
  P.infection.conv<-rep(0,iterations)
  
  #Probability of infection water efficient fixture
  P.infection.eff<-rep(0,iterations)
  
  #Breathing rate (m^3/min) (21 years to greater than 81)
  #used table 6-2 from exposure factors handbook
  #mean of 1.2e-2 and used 1.7e-2 95th percentile to estimate
  #SD where SD ~ (95th percentile-mean)/2
  sd.breathing<-((1.7E-2)-(1.2E-2))/2
  B<-rtrunc(iterations,"norm",a=0,mean=1.2E-2,sd=sd.breathing)
  
  #Exposure duration (min)
  t.shower<-showerduration
  
  #Legionella concentration
  C.leg<-C.water*1000 #convert CFU/L to CFU/m^3
  
  #CONVENTIONAL
  #concentration of aerosols at diameter 1-2
  C.aer.1.conv<-rlnorm(iterations,17.5,0.30)
  
  #concentration of aerosols at diameter 2-3
  C.aer.2.conv<-rlnorm(iterations,17.5,0.17)
  
  #concentration of aerosols at diameter 3-6
  C.aer.3.conv<-rlnorm(iterations,19.5,0.35)
  C.aer.4.conv<-rlnorm(iterations,19.5,0.35)
  C.aer.5.conv<-rlnorm(iterations,19.5,0.35)
  
  #concentration of aerosols at diameter 6-10
  #C.aer.6.conv<-rlnorm(iterations,20,0.31)
  #C.aer.7.conv<-rlnorm(iterations,20,0.31)
  #C.aer.8.conv<-rlnorm(iterations,20,0.31)
  #C.aer.9.conv<-rlnorm(iterations,20,0.31)
  #C.aer.10.conv<-rlnorm(iterations,20,0.31)
  
  #WATER EFFICIENT
  #concentration of aerosols at diameter 1-2
  C.aer.1.eff<-rlnorm(iterations,18.1,0.57)
  #concentration of aerosols at diameter 2-3
  C.aer.2.eff<-rlnorm(iterations,17.9,0.64)
  #concentration of aerosols at diameter 3-6
  C.aer.3.eff<-rlnorm(iterations,18.7,0.52)
  C.aer.4.eff<-rlnorm(iterations,18.7,0.52)
  C.aer.5.eff<-rlnorm(iterations,18.7,0.52)
  #concentration of aerosols at diameter 6-10
  #C.aer.6.eff<-rlnorm(iterations,18.3,0.14)
  #C.aer.7.eff<-rlnorm(iterations,18.3,0.14)
  #C.aer.8.eff<-rlnorm(iterations,18.3,0.14)
  #C.aer.8.eff<-rlnorm(iterations,18.3,0.14)
  #C.aer.8.eff<-rlnorm(iterations,18.3,0.14)
  #C.aer.9.eff<-rlnorm(iterations,18.3,0.14)
  #C.aer.10.eff<-rlnorm(iterations,18.3,0.14)
  
  #deposition efficiencies
  DE.1<-runif(iterations,0.23,0.25)
  DE.2<-runif(iterations,0.40,0.53)
  DE.3<-runif(iterations,0.36,0.62)
  DE.4<-runif(iterations,0.29,0.61)
  DE.5<-runif(iterations,0.19,0.52)
  #DE.6<-runif(iterations,0.10,0.4)
  #DE.7<-runif(iterations,0.06,0.29)
  #DE.8<-runif(iterations,0.03,0.19)
  #DE.9<-runif(iterations,0.01,0.12)
  #DE.10<-runif(iterations,0.01,0.06)
  
  #fraction aerosolized
  F.1<-.1750
  F.2<-.1639
  F.3<-.1556
  F.4<-.0667
  F.5<-.0389
  #F.6<-.0250
  #F.7<-.0278
  #F.8<-.0500
  #F.9<-.0528
  #F.10<-.0389
  
  #volume of aerosol for size bin i
  V.1<-(4/3)*pi*(1*10^(-6)/2)^3
  V.2<-(4/3)*pi*(2*10^(-6)/2)^3
  V.3<-(4/3)*pi*(3*10^(-6)/2)^3
  V.4<-(4/3)*pi*(4*10^(-6)/2)^3
  V.5<-(4/3)*pi*(5*10^(-6)/2)^3
  #V.6<-(4/3)*pi*(6*10^(-6)/2)^3
  #V.7<-(4/3)*pi*(7*10^(-6)/2)^3
  #V.8<-(4/3)*pi*(8*10^(-6)/2)^3
  #V.9<-(4/3)*pi*(9*10^(-6)/2)^3
  #V.10<-(4/3)*pi*(10*10^(-6)/2)^3
  
  #conventional
  #part1.conv<-(C.aer.1.conv*V.1)+(C.aer.2.conv*V.2)+(C.aer.3.conv*V.3)+
  #  (C.aer.4.conv*V.4)+(C.aer.5.conv*V.5)+(C.aer.6.conv*V.6)+
  #  (C.aer.7.conv*V.7)+(C.aer.8.conv*V.8)+(C.aer.9.conv*V.9)+
  #  (C.aer.10.conv*V.10)
  
  part1.conv<-(C.aer.1.conv*V.1)+(C.aer.2.conv*V.2)+(C.aer.3.conv*V.3)+
    (C.aer.4.conv*V.4)+(C.aer.5.conv*V.5)
  
  #efficient
  #part1.eff<-(C.aer.1.eff*V.1)+(C.aer.2.eff*V.2)+(C.aer.3.eff*V.3)+
  #  (C.aer.4.eff*V.4)+(C.aer.5.eff*V.5)+(C.aer.6.eff*V.6)+
  #  (C.aer.7.eff*V.7)+(C.aer.8.eff*V.8)+(C.aer.9.eff*V.9)+
  #  (C.aer.10.eff*V.10)
  
  part1.eff<-(C.aer.1.eff*V.1)+(C.aer.2.eff*V.2)+(C.aer.3.eff*V.3)+
    (C.aer.4.eff*V.4)+(C.aer.5.eff*V.5)
  
  #part2<-(F.1*DE.1)+(F.2*DE.2)+(F.3*DE.3)+(F.4*DE.4)+(F.5*DE.5)+
  #  (F.6*DE.6)+(F.7*DE.7)+(F.8*DE.8)+(F.9*DE.9)+(F.10*DE.10)
  
  part2<-(F.1*DE.1)+(F.2*DE.2)+(F.3*DE.3)+(F.4*DE.4)+(F.5*DE.5)
  
  #Dose (conventional fixture)
  Dose.fixture.conv<-C.leg*B*t.shower*part1.conv*part2
  
  #Dose (water efficient fixture)
  Dose.fixture.eff<-C.leg*B*t.shower*part1.eff*part2
  
  #Dose response parameter
  r<-rlnorm(iterations,-2.93,0.49)
  
  P.infection.conv<-1-exp(-r*Dose.fixture.conv)
  
  P.infection.eff<-1-exp(-r*Dose.fixture.eff)
  
  model.conv<<-data.frame(B=B,C.aer.1.conv,C.aer.2.conv,C.aer.3.conv,C.aer.4.conv,C.aer.5.conv,
                          DE.1,DE.2,DE.3,DE.4,DE.5,r,Dose.fixture.conv,
                          P.infection.conv)
  
  model.eff<<-data.frame(B=B,C.aer.1.eff,C.aer.2.eff,C.aer.3.eff,C.aer.4.eff,C.aer.5.eff,
                         DE.1,DE.2,DE.3,DE.4,DE.5,r,Dose.fixture.eff,
                         P.infection.eff)
  require(ggplot2)
  require(ggpubr)
  require(corrplot)
  require(gridExtra)
  require(grid)
  require(gridGraphics)
  
  mydata.cor.conv=cor(model.conv,method=c("spearman"))
  spearman.conv<-mydata.cor.conv[,14]
  inputvar<-c("B","C.aer.1.conv","C.aer.2.conv","C.aer.3.conv","C.aer.4.conv","C.aer.5.conv",
              "DE.1","DE.2","DE.3","DE.4","DE.5","k","Dose.fixture.conv",
              "P.infect.conv")
  outputvar<-"Infection Risk"
  
  frame.conv<-data.frame(spearman.conv=spearman.conv,inputvar=inputvar,outputvar=outputvar)
  frame.conv<-frame.conv[frame.conv$inputvar!="P.infect.conv",]
  
  plotB<<-ggplot(frame.conv)+geom_tile(aes(x=outputvar,y=inputvar,fill=spearman.conv))+
    geom_text(aes(label=signif(spearman.conv,2),x=outputvar,y=inputvar),size=7)+
    scale_fill_gradient2(low="99CCCC",mid="white",high="#6666FF",name="",midpoint=0)+
    theme_bw()+
    theme(axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.key.width = unit(1, "cm"),
          plot.title = element_text(hjust = 0.5,size=16),
          legend.position = "none")+
    scale_x_discrete(name="")+
    scale_y_discrete(name="")+ggtitle("Hamilton et al. Conventional")
  
  
  mydata.cor.eff=cor(model.eff,method=c("spearman"))
  spearman.eff<-mydata.cor.eff[,14]
  inputvar<-c("B","C.aer.1.eff","C.aer.2.eff","C.aer.3.eff","C.aer.4.eff","C.aer.5.eff",
              "DE.1","DE.2","DE.3","DE.4","DE.5","k","Dose.fixture.eff",
              "P.infect.eff")
  outputvar<-"Infection Risk"
  
  frame.conv<-data.frame(spearman.eff=spearman.eff,inputvar=inputvar,outputvar=outputvar)
  frame.conv<-frame.conv[frame.conv$inputvar!="P.infect.eff",]
  
  plotC<<-ggplot(frame.conv)+geom_tile(aes(x=outputvar,y=inputvar,fill=spearman.eff))+
    geom_text(aes(label=signif(spearman.eff,2),x=outputvar,y=inputvar),size=7)+
    scale_fill_gradient2(low="99CCCC",mid="white",high="#6666FF",name="",midpoint=0)+
    theme_bw()+
    theme(axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          legend.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.key.width = unit(1, "cm"),
          plot.title = element_text(hjust = 0.5,size=16),
          legend.position="none")+
    scale_x_discrete(name="")+
    scale_y_discrete(name="")+ggtitle("Hamilton et al. Water Efficient")
  
  
  
  combinedinfection<-data.frame(infectionrisk=c(P.infection.eff,P.infection.conv),
                                type=c(rep("Water Efficient",iterations),rep("Conventional",iterations)))
  
  ggplot(data=combinedinfection)+geom_boxplot(aes(y=infectionrisk,x=type,group=type))+
    scale_y_continuous(trans="log10",name="Infection Risk")+theme_bw()+scale_x_discrete(name="Shower Type")
  
}

#checking app
#hamilton(10000,11,100000)
#mean(model.conv$P.infection.conv)
hamilton(10000,8,100)

#comparison

comparison<-function(iterations,showerduration,C.water){
  schoen.ashbolt(iterations,showerduration,C.water)
  hamilton(iterations,showerduration,C.water)
  
  infectionrisk.all<-c(model$infection.risk,model.conv$P.infection.conv,model.eff$P.infection.eff)
  infectionrisk.all<-c(infectionrisk.all,infectionrisk.all)
  model<-c(rep("Schoen & Ashbolt",iterations),rep("Hamilton",iterations*2),rep("Combined",iterations*3))
  showertype<-c(rep("Unspecified",iterations),rep("Conventional",iterations),rep("Water Efficient",iterations),rep("combined",iterations*3))
  
  frameall<<-data.frame(infection.risk=infectionrisk.all,model=model,showertype=showertype)
  
}

comparison(10000,7.8,100)


windows()
frameall<-frameall[frameall$model!="Combined",]

frameall$showertype <- factor(frameall$showertype, levels = c("Conventional","Water Efficient","Unspecified"))
tiff("figure2.tiff", units="in",width=8,height=6,res=600)
ggplot(data=frameall,aes(x=showertype,y=infection.risk,group=showertype))+geom_point(aes(colour=showertype),position=position_jitterdodge(),alpha=0.1,size=2)+scale_y_continuous(trans="log10",name="Infection Risk")+
  scale_x_discrete(name="")+
  scale_colour_manual(values=c("#0066CC","#99CCFF","light blue"),name="")+
  stat_summary(fun = median, fun.min = median, fun.max = median,
               geom = "crossbar", width = 0.2,colour="black",position=position_dodge(width=0.9))+
  theme_pubr()+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=16),
        strip.text = element_text(size=16),legend.text = element_text(size=16))+
  guides(colour = guide_legend(override.aes = list(size=3,alpha=1)))
dev.off()

mean(frameall$infection.risk[frameall$showertype=="Conventional"])
sd(frameall$infection.risk[frameall$showertype=="Conventional"])

mean(frameall$infection.risk[frameall$showertype=="Water Efficient"])
sd(frameall$infection.risk[frameall$showertype=="Water Efficient"])

mean(frameall$infection.risk[frameall$showertype=="Unspecified"])
sd(frameall$infection.risk[frameall$showertype=="Unspecified"])

concentrations<-c(1E0,1E1,1E2,1E3,1E4,1E5,1E6,1E7)

mean.schoen<-rep(NA,length(concentrations))
sd.schoen<-rep(NA,length(concentrations))

mean.ham.eff<-rep(NA,length(concentrations))
sd.ham.eff<-rep(NA,length(concentrations))

mean.ham.conv<-rep(NA,length(concentrations))
sd.ham.conv<-rep(NA,length(concentrations))

for (i in 1:length(concentrations)){
  comparison(10000,7.8,concentrations[i])
  
  if (i==1){
    frame.total<-frameall
    frame.total$conc<-concentrations[i]
  }else{
    frameall$conc<-concentrations[i]
    frame.total<-rbind(frame.total,frameall)
  }
  
  mean.schoen[i]<-mean(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$model=="Schoen & Ashbolt"])
  sd.schoen[i]<-sd(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$model=="Schoen & Ashbolt"])
  
  mean.ham.eff[i]<-mean(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient"])
  sd.ham.eff[i]<-sd(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient"])
  
  mean.ham.conv[i]<-mean(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$model=="Hamilton" & frame.total$showertype!="Water Efficient"])
  sd.ham.conv[i]<-sd(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$model=="Hamilton" & frame.total$showertype!="Water Efficient"])
  
}

mean<-c(mean.schoen,mean.ham.eff,mean.ham.conv)
sd<-c(sd.schoen,sd.ham.eff,sd.ham.conv)
model<-c(rep("Model 2",length(mean.schoen)),rep("Model 1",length(c(mean.ham.eff,mean.ham.conv))))
type<-c(rep("Unspecified",length(mean.schoen)),rep("Water Efficient",length(mean.ham.eff)),rep("Conventional",length(mean.ham.conv)))
conc<-rep(concentrations,3)
frame.conc.compare<-data.frame(mean=mean,sd=sd,model=model,type=type,conc=conc)

frame.conc.compare$type <- factor(frame.conc.compare$type, levels = c("Conventional","Water Efficient","Unspecified"))


windows()


tiff("test.tiff", units="in",width=20,height=5,res=600)

ggplot(frame.conc.compare)+geom_line(aes(x=conc/1000,y=mean,group=interaction(type,model),linetype=model,color=type),size=1.5)+
  geom_point(aes(x=conc/1000,y=mean,group=interaction(type,model),color=type),size=6)+
  #geom_ribbon(aes(x=conc/1000,ymax=mean+1.962*sd/sqrt(1000),ymin=mean-sd*1.962/sqrt(1000),group=interaction(type,model),fill=type),alpha=0.8)+
  geom_errorbar(aes(x=conc/1000,ymin=mean-sd,ymax=mean+sd,group=interaction(type,model),color=type),size=1,width=.2)+
  scale_y_continuous(trans="log10",name="Infection Risk",breaks=10^seq(-8,0,1),
                     labels=c("1/100,000,000","1/10,000,000","1/1,000,000","1/100,000","1/10,000","1/1,000","1/100","1/10","1"))+
  scale_x_continuous(trans="log10",name="CFU/mL",
                     labels=c("0.001","0.01","0.1","1","10","100","1,000","10,000"),breaks=10^seq(-3,4,1))+
  scale_colour_manual(values=c("#0066CC","#99CCFF","light blue"),name="Shower Type")+
  scale_linetype_discrete(name="Model")+
  geom_hline(yintercept=1/10000,linetype="dotted",color="black",size=2)+
  annotate("text",label=c("1/10,000 Risk Threshold"),x=1e3,y=2e-04,size=5)+
  theme_bw()+
  theme(axis.title = element_text(size=18),axis.text=element_text(size=18),
        legend.title=element_text(size=18),legend.text=element_text(size=18),
        legend.box="vertical",legend.position="top")

dev.off()


windows()
ggarrange(plotA,plotB,plotC,nrow=1)

#--------------------- previous plotting -----------------------------------

#windows()
#ggarrange(plotA,plotB,plotC,common.legend=TRUE,ncol=3,legend="right")

#A<-ggplot(frameall)+geom_density(aes(x=infection.risk,group=model,fill=model),alpha=0.3)+scale_x_continuous(trans="log10")
#B<-ggplot(frameall)+geom_density(aes(x=infection.risk,group=showertype,fill=showertype),alpha=0.3)+scale_x_continuous(trans="log10")

#ggarrange(A,B)

#ggplot(frameall)+geom_density(aes(x=infection.risk,group=interaction(showertype,model),fill=interaction(showertype,model)),alpha=0.3)+scale_x_continuous(trans="log10")