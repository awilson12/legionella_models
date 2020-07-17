# Amanda Wilson (AW)
# Environmental Health Sciences PhD student
# University of Arizona


require(ggplot2)
require(ggpubr)
require(corrplot)
require(gridExtra)
require(grid)
require(gridGraphics)



#---------PART 1 - STOCHASTIC APPROACH TO SCHOEN & ASHBOLT (2011) MODEL--------------------



# This model is based on one discussed by Schoen & Ashbolt (2011) An in-premise model for
# Legionella exposure during showering events

schoen.ashbolt<-function(iterations,C.air,IR){
  
  #model function inputs
        #1-iterations
        #2-flow rate (L/hr)
        #3-C.air (concentration of L. pneumophila in air - CFU/m^3)
        #4-IR (inhalation rate in m^3/min)
  
  #-----------model input parameters--------------------------
  
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

  #calculating deposited dose
  DD<-C.air*IR*((F1.15*F2.15)+(F1.56*F2.56)+(F1.610*F2.610))
    
    
  #saving inputs and outputs
    
  model<<-data.frame(IR=IR,PC=PC,F1.15=F1.15,F2.15=F2.15,F1.56=F1.56,F2.56=F2.56,F1.610=F1.610,
                       F2.610=F2.610,DD=DD)
    
    
}
  


#--------PART 2 - HAMILTON ET AL. (2019) MODEL----------------------------------------------

# This model is based on one discussed by Hamilton et al. (2019) Risk-based critical levels
# of Legionella pneumophila for indoor water uses. Environmental Science & Technology.

# "volume of aerosols of various size diameters that are large enough to hold
# L. pneumophila bacteria but small enough to deposit at the alveoli (1 um < diameter < 10um)
# were considered" - equation 2 from paper
  

hamilton<-function(iterations,IR){
  
  
  #model function inputs
  #1-iterations
  #2-shower duration (minutes)
  #3-C.water (concentration of L. pneumophila in water - CFU/mL)


  #truncnorm dist
  require(truncnorm)
  
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
  C.aer.6.conv<-rlnorm(iterations,20,0.31)
  C.aer.7.conv<-rlnorm(iterations,20,0.31)
  C.aer.8.conv<-rlnorm(iterations,20,0.31)
  C.aer.9.conv<-rlnorm(iterations,20,0.31)
  C.aer.10.conv<-rlnorm(iterations,20,0.31)
  
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
  C.aer.6.eff<-rlnorm(iterations,18.3,0.14)
  C.aer.7.eff<-rlnorm(iterations,18.3,0.14)
  C.aer.8.eff<-rlnorm(iterations,18.3,0.14)
  C.aer.8.eff<-rlnorm(iterations,18.3,0.14)
  C.aer.8.eff<-rlnorm(iterations,18.3,0.14)
  C.aer.9.eff<-rlnorm(iterations,18.3,0.14)
  C.aer.10.eff<-rlnorm(iterations,18.3,0.14)
  
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
  V.1<-(4/3)*pi*(1*10^(-6)/2)^3
  V.2<-(4/3)*pi*(2*10^(-6)/2)^3
  V.3<-(4/3)*pi*(3*10^(-6)/2)^3
  V.4<-(4/3)*pi*(4*10^(-6)/2)^3
  V.5<-(4/3)*pi*(5*10^(-6)/2)^3
  V.6<-(4/3)*pi*(6*10^(-6)/2)^3
  V.7<-(4/3)*pi*(7*10^(-6)/2)^3
  V.8<-(4/3)*pi*(8*10^(-6)/2)^3
  V.9<-(4/3)*pi*(9*10^(-6)/2)^3
  V.10<-(4/3)*pi*(10*10^(-6)/2)^3
  
  #conventional
  part1.conv<-(C.aer.1.conv*V.1)+(C.aer.2.conv*V.2)+(C.aer.3.conv*V.3)+
              (C.aer.4.conv*V.4)+(C.aer.5.conv*V.5)+(C.aer.6.conv*V.6)+
              (C.aer.7.conv*V.7)+(C.aer.8.conv*V.8)+(C.aer.9.conv*V.9)+
              (C.aer.10.conv*V.10)
  
  #efficient
  part1.eff<-(C.aer.1.eff*V.1)+(C.aer.2.eff*V.2)+(C.aer.3.eff*V.3)+
             (C.aer.4.eff*V.4)+(C.aer.5.eff*V.5)+(C.aer.6.eff*V.6)+
             (C.aer.7.eff*V.7)+(C.aer.8.eff*V.8)+(C.aer.9.eff*V.9)+
             (C.aer.10.eff*V.10)
  
  part2<-(F.1*DE.1)+(F.2*DE.2)+(F.3*DE.3)+(F.4*DE.4)+(F.5*DE.5)+
              (F.6*DE.6)+(F.7*DE.7)+(F.8*DE.8)+(F.9*DE.9)+(F.10*DE.10)
  
  C.leg<-3.3e4
  
  #Dose (conventional fixture)
  Dose.fixture.conv<-C.leg*IR*part1.conv*part2
  
  #Dose (water efficient fixture)
  Dose.fixture.eff<-C.leg*IR*part1.eff*part2
  
  model.conv<<-data.frame(IR=IR,C.aer.1.conv,C.aer.2.conv,C.aer.3.conv,C.aer.4.conv,C.aer.5.conv,
                          C.aer.6.conv,C.aer.7.conv,C.aer.8.conv,C.aer.9.conv,C.aer.10.conv,DE.1,
                          DE.2,DE.3,DE.4,DE.5,DE.6,DE.7,DE.8,DE.9,DE.10,Dose.fixture.conv)
  
  model.eff<<-data.frame(IR=IR,C.aer.1.eff,C.aer.2.eff,C.aer.3.eff,C.aer.4.eff,C.aer.5.eff,
                         C.aer.6.eff,C.aer.7.eff,C.aer.8.eff,C.aer.9.eff,C.aer.10.eff,DE.1,
                         DE.2,DE.3,DE.4,DE.5,DE.6,DE.7,DE.8,DE.9,DE.10,Dose.fixture.eff)
  

  }
