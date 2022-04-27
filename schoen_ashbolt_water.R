
schoen.ashbolt.water<-function(iterations,C.water,IR,FL){
  require(triangle)
  
  #model function inputs
  #1-iterations
  #2-flow rate (L/hr)
  #3-C.air (concentration of L. pneumophila in air - CFU/m^3)
  #4-IR (inhalation rate in m^3/min)
  
  #-----------model input parameters--------------------------
  
  #partitioning coefficient
  PC<-rtriangle(iterations,a=1E-5,c=1E-4,b=1.9e-05)
  #min = low value
  #max = (Best estimate value - low value) + Best estimate value
  
  #-------aerosolizing parameters--------------------------------
  
  #----------aerosol size 1 - 5 um--------------
  
  #fraction of total aerosolized organisms in aeorosol size 1-5
  F1.15<-rtriangle(iterations,a=0.5,b=1,c=0.75)
  #min = best estimate value
  #max = high value
  
  #fraction of total aerosols of size range 1-5um deposited at the alveoli
  F2.15<-rtriangle(iterations,a=0,b=0.54,c=0.2)
  #min = best estimate value
  #max = high value
  
  #---------aerosol size 5 - 6 um---------------
  
  #fraction of total aerosolized organisms in aeorosol size 5-6
  #F1.56<-(1-F1.15)*0.36
  #12% of aerosols other than 1-5
  
  #fraction of total aerosols of size range 5-6 deposited at the alveoli
  #F2.56<-(1-F2.15)*0.125
  
  #--------aerosol size 6 - 10 um----------------
  
  #fraction of total aerosolized organisms in aerosol size 6-10
  #F1.610<-(1-F1.15)*0.56
  
  #fraction of total aerosols of size range 6-10 deposited at the alveoli
  #F2.610<-(1-F2.15)*0.0125
  
  ###########################################################
  
  C.air<-C.water*PC
  
  #calculating deposited dose
  DD<-C.air*IR*(F1.15*F2.15)
  
  
  #saving inputs and outputs
  
  model2<<-data.frame(IR=IR,PC=PC,F1.15=F1.15,F2.15=F2.15,DD=DD)
  
  
}
