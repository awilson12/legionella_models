# Amanda Wilson (AW)
# Environmental Health Sciences PhD student
# University of Arizona

#--------------------------Notes-----------------------------------------------------------

# 1/15/2020 - Began stochastic adjustments to Schoen & Ashbolt (2011) paper -AW



#------------------------------------------------------------------------------------------



#---------PART 1 - STOCHASTIC APPROACH TO SCHOEN & ASHBOLT (2011) MODEL--------------------



# This model is based on one discussed by Schoen & Ashbolt (2011) An in-premise model for
# Legionella exposure during showering events

schoen.ashbolt<-function(iterations,showerduration,C.water){
  
  #model function inputs
        #1-iterations
        #2-shower duration (minutes)
        #3-C.water (concentration of L. pneumophila in water - CFU/mL)
  
  #-----------model input parameters--------------------------
  
  #flow rate (L/hr)
  FR<-runif(iterations,350,370)
  #min = best estimate - 10 L/hr
  #max = best estimate + 10 L/hr
  #ARBITRARY BOUNDS RIGHT NOW
  
  #inhalation rate (m^3/hr)
  IR<-runif(iterations,0.72,1.5)
  #min = best estimate value
  #max = high value
  
  #partitioning coefficient
  PC<-runif(iterations,10^-6,1.9e-05)
  #min = low value
  #max = (Best estimate value - low value) + Best estimate value
  
  #-------aerosolizing parameters--------------------------------
  
  #----------aerosol size 1 - 5 um--------------
  
  #fraction of total aerosolized organisms in aeorosol size 1-5
  F1.15<-runif(iterations,0.75,1)
  #min = best estimate value
  #max = high value
  
  #fraction of total aerosols of size range 1-5um deposited at the alveoli
  F2.15<-runif(iterations,0.2,0.54)
  #min = best estimate value
  #max = high value
  
  #---------aerosol size 5 - 6 um---------------
  
  #fraction of total aerosolized organisms in aeorosol size 5-6
  F1.56<-(1-F1.15)*0.36
  #12% of aerosols other than 1-5
  
  #fraction of total aerosols of size range 5-6 deposited at the alveoli
  F2.56<-(1-F2.15)*0.125
  
  #--------aerosol size 6 - 10 um----------------
  
  #fraction of total aerosolized organisms in aerosol size 6-10
  F1.610<-(1-F1.15)*0.56
  
  #fraction of total aerosols of size range 6-10 deposited at the alveoli
  F2.610<-(1-F2.15)*0.0125
  
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
  
    #convert minutes to hour by dividing by 60
    exposure<-showerduration/60
    
    #volume of water used in shower  
    V.water<-FR*exposure
    
    V.air<-IR*exposure
    
    C.water.converted<-C.water*1000 #convert CFU/mL to CFU/L
    
    #concentration of legionella in air (CFU/m^3)
    C.air<-C.water.converted*PC
    
    #calculating deposited dose
    DD<-C.air*V.air*((F1.15*F2.15)+(F1.56*F2.56)+(F1.610*F2.610))
    
    #calculating infection risk
    infection.risk<-1-exp(-k*(DD))
    
    #saving inputs and outputs
    
    model<<-data.frame(FR=FR,IR=IR,PC=PC,F1.15=F1.15,F2.15=F2.15,F1.56=F1.56,F2.56=F2.56,F1.610=F1.610,
                       F2.610=F2.610,k=k,DD=DD,infection.risk=infection.risk)
    require(ggplot2)
    require(ggpubr)
    require(corrplot)
    require(gridExtra)
    require(grid)
    require(gridGraphics)
    
    mydata.cor=cor(model,method=c("spearman"))

    corrplot(mydata.cor,method="number")
    grid.echo()
    plot1<-grid.grab()
    #grid.draw(plot1)
    
    plot2<-ggplot(data=model)+geom_boxplot(aes(y=infection.risk))+ggtitle("Infection Risk")+
      scale_y_continuous(trans="log10")+theme_bw()
    
    windows()
    grid.arrange(plot1,plot2,nrow=1,ncol=2)
    
  }
  

schoen.ashbolt(10000,8,.1)


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

  #Breathing rate (m^3/min)
  B<-runif(iterations,0.013,0.017)
  
  #Exposure duration (min)
  t.shower<-showerduration
  
  #Legionella concentration
  C.leg<-C.water*1000
  
  #CONVENTIONAL
  #concentration of aerosols at diameter 1-2
  C.aer.12.conv<-rlnorm(iterations,17.5,0.30)
  
  #concentration of aerosols at diameter 2-3
  C.aer.23.conv<-rlnorm(iterations,17.5,0.17)
  
  #concentration of aerosols at diameter 3-6
  C.aer.36.conv<-rlnorm(iterations,19.5,0.35)
  
  #concentration of aerosols at diameter 6-10
  C.aer.610.conv<-rlnorm(iterations,20,0.31)
  
  #WATER EFFICIENT
  #concentration of aerosols at diameter 1-2
  C.aer.12.eff<-rlnorm(iterations,18.1,0.57)
  #concentration of aerosols at diameter 2-3
  C.aer.23.eff<-rlnorm(iterations,17.9,0.64)
  #concentration of aerosols at diameter 3-6
  C.aer.36.eff<-rlnorm(iterations,18.7,0.52)
  #concentration of aerosols at diameter 6-10
  C.aer.610.eff<-rlnorm(iterations,18.3,0.14)
  
  #deposition efficiencies
  DE.1<-runif(iterations,0.23,0.25)
  DE.2<-runif(iterations,0.40,0.53)
  DE.3<-runif(iterations,0.36,0.62)
  DE.4<-runif(iterations,0.29,0.61)
  DE.5<-runif(iterations,0.19,0.52)
  DE.6<-runif(iterations,0.10,0.4)
  DE.7<-runif(iterations,0.06,0.29)
  DE.8<-runif(iterations,0.03,0.19)
  DE.9<-runif(iterations,0.01,0.12)
  DE.10<-runif(iterations,0.01,0.06)
  
  #percent aerosolized
  F.1<-17.50
  F.2<-16.39
  F.3<-15.56
  F.4<-6.67
  F.5<-3.89
  F.6<-2.50
  F.7<-2.78
  F.8<-5.00
  F.9<-5.28
  F.10<-3.89
  
  #volume of aerosol for size bin i
  V.1<-(4/3)*pi*(1*10^(-6))^3
  V.2<-(4/3)*pi*(2*10^(-6))^3
  V.3<-(4/3)*pi*(3*10^(-6))^3
  V.4<-(4/3)*pi*(4*10^(-6))^3
  V.5<-(4/3)*pi*(5*10^(-6))^3
  V.6<-(4/3)*pi*(6*10^(-6))^3
  V.7<-(4/3)*pi*(7*10^(-6))^3
  V.8<-(4/3)*pi*(8*10^(-6))^3
  V.9<-(4/3)*pi*(9*10^(-6))^3
  V.10<-(4/3)*pi*(10*10^(-6))^3
  
  sum.calculation.conv<-(C.aer.12.conv*DE.1*F.1*V.1)+(C.aer.23.conv*DE.2*F.2*V.2)+(C.aer.36.conv*DE.3*F.3*V.3)+
    (C.aer.36.conv*DE.4*F.1*V.4)+(C.aer.36.conv*DE.5*F.1*V.5)+(C.aer.610.conv*DE.6*F.6*V.6)+
    (C.aer.610.conv*DE.7*F.7*V.7)+(C.aer.610.conv*DE.8*F.8*V.8)+(C.aer.610.conv*DE.9*F.9*V.9)+
    (C.aer.610.conv*DE.10*F.10*V.10)
    
  
  sum.calculation.eff<-(C.aer.12.eff*DE.1*F.1*V.1)+(C.aer.23.eff*DE.2*F.2*V.2)+(C.aer.36.eff*DE.3*F.3*V.3)+
    (C.aer.36.eff*DE.4*F.1*V.4)+(C.aer.36.eff*DE.5*F.1*V.5)+(C.aer.610.eff*DE.6*F.6*V.6)+
    (C.aer.610.eff*DE.7*F.7*V.7)+(C.aer.610.eff*DE.8*F.8*V.8)+(C.aer.610.eff*DE.9*F.9*V.9)+
    (C.aer.610.eff*DE.10*F.10*V.10)
  
  
  #Dose (conventional fixture)
  Dose.fixture.conv<-C.leg*B*t.shower*sum.calculation.conv
  
  #Dose (water efficient fixture)
  Dose.fixture.eff<-C.leg*B*t.shower*sum.calculation.eff
  
  #Dose response parameter
  r<-rlnorm(iterations,-2.93,0.49)
  
  P.infection.conv<-1-exp(-r*Dose.fixture.conv)
  
  P.infection.eff<-1-exp(-r*Dose.fixture.eff)
  
  model.conv<<-data.frame(B=B,C.aer.12.conv,C.aer.23.conv,C.aer.36.conv,C.aer.610.conv,DE.1,
                          DE.2,DE.3,DE.4,DE.5,DE.6,DE.7,DE.8,DE.9,DE.10,r,Dose.fixture.conv,
                          P.infection.conv)
  
  model.eff<<-data.frame(B=B,C.aer.12.eff,C.aer.23.eff,C.aer.36.eff,C.aer.610.eff,DE.1,
                         DE.2,DE.3,DE.4,DE.5,DE.6,DE.7,DE.8,DE.9,DE.10,r,Dose.fixture.eff,
                         P.infection.eff)
  require(ggplot2)
  require(ggpubr)
  require(corrplot)
  require(gridExtra)
  require(grid)
  require(gridGraphics)
  
  mydata.cor.conv=cor(model.conv,method=c("spearman"))
  corrplot(mydata.cor.conv,method="number")
  grid.echo()
  plot1.conv<-grid.grab()
  #grid.draw(plot1)
  
  mydata.cor.eff=cor(model.eff,method=c("spearman"))
  corrplot(mydata.cor.eff,method="number")
  grid.echo()
  plot1.eff<-grid.grab()
  
  combinedinfection<-data.frame(infectionrisk=c(P.infection.eff,P.infection.conv),
                                type=c(rep("Water Efficient",iterations),rep("Conventional",iterations)))
  
    ggplot(data=combinedinfection)+geom_boxplot(aes(y=infectionrisk,x=type,group=type))+
    scale_y_continuous(trans="log10",name="Infection Risk")+theme_bw()+scale_x_discrete(name="Shower Type")

  }

hamilton(10000,8,.1)

#comparison

comparison<-function(iterations,showerduration,C.water){
  schoen.ashbolt(iterations,showerduration,C.water)
  hamilton(iterations,showerduration,C.water)
  
  infectionrisk.all<-c(model$infection.risk,model.conv$P.infection.conv,model.eff$P.infection.eff)
  infectionrisk.all<-c(infectionrisk.all,infectionrisk.all)
  model<-c(rep("Schoen & Ashbolt",iterations),rep("Hamilton",iterations*2),rep("Combined",iterations*3))
  showertype<-c(rep("conventional or unspecified",iterations*2),rep("water efficient",iterations),rep("combined",iterations*3))
  
  frameall<-data.frame(infection.risk=infectionrisk.all,model=model,showertype=showertype)
  
  ggplot(data=frameall)+geom_boxplot(aes(x=model,y=infectionrisk.all,colour=showertype))+scale_y_continuous(trans="log10",name="Infection Risk")+
  scale_x_discrete(name="Model Source")+scale_colour_discrete(name="Shower Type")
}

comparison(10000,8,.1)
