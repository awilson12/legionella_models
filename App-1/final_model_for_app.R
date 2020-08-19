require(truncdist)

hamilton<-function(showerduration,C.water,type="conventional"){
  
  
  iterations<-10000
  
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
  C.leg<-C.water*1000*1000 #convert CFU/mL to CFU/L and then CFU/m^3
  
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
  
  #fraction aerosolized
  F.1<-.1750
  F.2<-.1639
  F.3<-.1556
  F.4<-.0667
  F.5<-.0389
  F.6<-.0250
  F.7<-.0278
  F.8<-.0500
  F.9<-.0528
  F.10<-.0389
  
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
  
  
  #Dose (conventional fixture)
  Dose.fixture.conv<-C.leg*B*t.shower*part1.conv*part2
  
  #Dose (water efficient fixture)
  Dose.fixture.eff<-C.leg*B*t.shower*part1.eff*part2
  
  #Dose response parameter
  r<-rlnorm(iterations,-2.93,0.49)
  
  P.infection.conv<-1-exp(-r*Dose.fixture.conv)
  
  P.infection.eff<-1-exp(-r*Dose.fixture.eff)

  if(type=="conventional"){
    mean<-signif(mean(P.infection.conv),2)
    sd<-signif(sd(P.infection.conv),2)
  }else{
    mean<-signif(mean(P.infection.eff),2)
    sd<-signif(sd(P.infection.eff),2)
  }
  
  x<-paste("Infection Risk:",mean,"\u00b1",sd)
  Encoding(x)<-"UTF-8"
  
  x.print<<-x
  
  
  frame.temp<-data.frame(mean=mean,C.water=C.water)
  frame.run<<-frame.temp
  
  #windows()
  

}



