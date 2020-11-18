# Amanda Wilson (AW)
# Environmental Health Sciences PhD student
# University of Arizona


require(ggplot2)
require(ggpubr)
require(corrplot)
require(gridExtra)
require(grid)
require(gridGraphics)

set.seed(37)


#---------PART 1 - STOCHASTIC APPROACH TO SCHOEN & ASHBOLT (2011) MODEL--------------------



# This model is based on one discussed by Schoen & Ashbolt (2011) An in-premise model for
# Legionella exposure during showering events

schoen.ashbolt<-function(iterations,C.air,IR){
  
  
  #-------aerosolizing parameters--------------------------------
  
  #----------aerosol size 1 - 5 um--------------
  
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
    
  model<<-data.frame(IR=IR,F1.15=F1.15,F2.15=F2.15,F1.56=F1.56,F2.56=F2.56,F1.610=F1.610,
                       F2.610=F2.610,DD=DD)
    
    
}
  


#--------PART 2 - HAMILTON ET AL. (2019) MODEL----------------------------------------------

# This model is based on one discussed by Hamilton et al. (2019) Risk-based critical levels
# of Legionella pneumophila for indoor water uses. Environmental Science & Technology.

# "volume of aerosols of various size diameters that are large enough to hold
# L. pneumophila bacteria but small enough to deposit at the alveoli (1 um < diameter < 10um)
# were considered" - equation 2 from paper
  

hamilton<-function(iterations,C.air,IR){

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
  
  #fraction aerosolized
  F.1<-.1750
  F.2<-.1639
  F.3<-.1556
  F.4<-0.0667
  F.5<-0.0389
  F.6<-0.0250
  F.7<-0.0278
  F.8<-0.0500
  F.9<-0.0528
  F.10<-0.0389
  
  
  part2<-(F.1*DE.1)+(F.2*DE.2)+(F.3*DE.3)+(F.4*DE.4)+(F.5*DE.5)+
              (F.6*DE.6)+(F.7*DE.7)+(F.8*DE.8)+(F.9*DE.9)+(F.10*DE.10)
  
  
  #Dose (conventional fixture)
  DD<-C.air*IR*part2
  
  
  model<<-data.frame(IR=IR,DE.1,DE.2,DE.3,DE.4,DE.5,DE.6,DE.7,DE.8,DE.9,DE.10,DD)
  
  }
