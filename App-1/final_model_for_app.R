require(truncdist)
require(triangle)
require(ggplot2)
require(ggpubr)

hamilton<-function(showerduration,C.water,type="Conventional",age=11,sex="Male"){
  
  set.seed(34)
  
  
  iterations<<-10000
  
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
  #used tables 6-14 and -15 from exposure factors handbook
  #SD where SD ~ (95th percentile-mean)/2
  #left-truncation point = 5th percentile
  #right-truncation point = maximum
  
  if (sex=="Male"){
    
    if(age>=11 & age<16){
      percentile.5<-11.40
      percentile.95<-21.21
      mean.B<-15.32
      max.B<-28.54
      
    }else if (age>=16 & age<21){
      percentile.5<-12.60
      percentile.95<-23.37
      mean.B<-17.21
      max.B<-39.21
      
    }else if (age>=21 & age<31){
      percentile.5<-12.69
      percentile.95<-27.13
      mean.B<-18.82
      max.B<-43.42
      
    }else if (age>=31 & age<41){
      percentile.5<-14.00
      percentile.95<-28.90
      mean.B<-20.29
      max.B<-40.72
      
    }else if (age>=41 & age<51){
      percentile.5<-14.66
      percentile.95<-28.37
      mean.B<-20.94
      max.B<-45.98
      
    }else if (age>=51 & age<61){
      percentile.5<-14.99
      percentile.95<-29.09
      mean.B<-20.91
      max.B<-38.17
      
    }else if (age>=61 & age<71){
      percentile.5<-13.91
      percentile.95<-23.50
      mean.B<-17.94
      max.B<-28.09
      
    }else if (age>=71 & age<81){
      percentile.5<-13.10
      percentile.95<-20.42
      mean.B<-16.34
      max.B<-24.52
      
    }else if (age>=81){
      percentile.5<-11.95
      percentile.95<-18.69
      mean.B<-15.15
      max.B<-22.64
      
    }
    
  }else{ #female
    
    if(age>=11 & age<16){
      percentile.5<-10.47
      percentile.95<-17.41
      mean.B<-13.44
      max.B<-26.58
      
    }else if (age>=16 & age<21){
      percentile.5<-9.86
      percentile.95<-18.29
      mean.B<-13.59
      max.B<-30.11
      
    }else if (age>=21 & age<31){
      percentile.5<-10.15
      percentile.95<-21.14
      mean.B<-14.57
      max.B<-30.23
      
    }else if (age>=31 & age<41){
      percentile.5<-11.07
      percentile.95<-20.45
      mean.B<-14.98
      max.B<-28.28
      
    }else if (age>=41 & age<51){
      percentile.5<-12.11
      percentile.95<-21.34
      mean.B<-16.20
      max.B<-35.88
      
    }else if (age>=51 & age<61){
      percentile.5<-12.33
      percentile.95<-21.21
      mean.B<-16.19
      max.B<-25.70
      
    }else if (age>=61 & age<71){
      percentile.5<-10.40
      percentile.95<-16.14
      mean.B<-12.99
      max.B<-20.33
      
    }else if (age>=71 & age<81){
      percentile.5<-9.89
      percentile.95<-15.19
      mean.B<-12.04
      max.B<-17.70
      
    }else if (age>=81){
      percentile.5<-9.19
      percentile.95<-13.94
      mean.B<-11.15
      max.B<-16.93
      
    }
    
  }
  
  sd.B<-(percentile.95-mean.B)/2
  B<-rtrunc(iterations,"norm",a=percentile.5,b=max.B,mean=mean.B,sd=sd.B)
  
  
  #adjust by converting m^3/day to m^3/min
  B<-B/(24*60)
 
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
  #            (C.aer.4.conv*V.4)+(C.aer.5.conv*V.5)+(C.aer.6.conv*V.6)+
  #            (C.aer.7.conv*V.7)+(C.aer.8.conv*V.8)+(C.aer.9.conv*V.9)+
  #            (C.aer.10.conv*V.10)
  
  part1.conv<-(C.aer.1.conv*V.1)+(C.aer.2.conv*V.2)+(C.aer.3.conv*V.3)+
    (C.aer.4.conv*V.4)+(C.aer.5.conv*V.5)
  
  #efficient
  #part1.eff<-(C.aer.1.eff*V.1)+(C.aer.2.eff*V.2)+(C.aer.3.eff*V.3)+
  #           (C.aer.4.eff*V.4)+(C.aer.5.eff*V.5)+(C.aer.6.eff*V.6)+
  #           (C.aer.7.eff*V.7)+(C.aer.8.eff*V.8)+(C.aer.9.eff*V.9)+
  #           (C.aer.10.eff*V.10)
  
  part1.eff<-(C.aer.1.eff*V.1)+(C.aer.2.eff*V.2)+(C.aer.3.eff*V.3)+
    (C.aer.4.eff*V.4)+(C.aer.5.eff*V.5)
  
  #part2<-(F.1*DE.1)+(F.2*DE.2)+(F.3*DE.3)+(F.4*DE.4)+(F.5*DE.5)+
  #            (F.6*DE.6)+(F.7*DE.7)+(F.8*DE.8)+(F.9*DE.9)+(F.10*DE.10)
  
  part2<-(F.1*DE.1)+(F.2*DE.2)+(F.3*DE.3)+(F.4*DE.4)+(F.5*DE.5)
  
  
  #Dose (conventional fixture)
  Dose.fixture.conv<-C.leg*B*t.shower*part1.conv*part2
  
  #Dose (water efficient fixture)
  Dose.fixture.eff<-C.leg*B*t.shower*part1.eff*part2
  
  #population doses
  #used table 6-2 from exposure factors handbook
  #mean of 1.2e-2 and used 1.7e-2 95th percentile to estimate
  #SD where SD ~ (95th percentile-mean)/2
  sd.breathing<-((1.7E-2)-(1.2E-2))/2
  B.pop<-rtrunc(iterations,"norm",a=0,mean=1.2E-2,sd=sd.breathing)
  
  #inhalation rates for whole population
    
  Dose.fixture.conv.pop<-C.leg*B.pop*t.shower*part1.conv*part2
  
  Dose.fixture.eff.pop<-C.leg*B.pop*t.shower*part1.eff*part2
  
  #Dose response parameter
  r<-rlnorm(iterations,-2.93,0.49)
  
  P.infection.conv<-1-exp(-r*Dose.fixture.conv)
  P.infect.conv<<-P.infection.conv
  
  P.infection.eff<-1-exp(-r*Dose.fixture.eff)
  P.infect.eff<<-P.infection.eff
  
  #calculating population level infection risks
  P.infection.conv.pop<-1-exp(-r*Dose.fixture.conv.pop)
  P.infection.eff.pop<-1-exp(-r*Dose.fixture.eff.pop)
  
  #globally saving population level infection risks
  P.infect.conv.pop<<-P.infection.conv.pop
  P.infect.eff.pop<<-P.infection.eff.pop
  

  if(type=="Conventional"){
    mean<-signif(mean(P.infection.conv),2)
    sd<-signif(sd(P.infection.conv),2)
    
    mean.pop<-mean(P.infect.conv.pop)
  }else{
    mean<-signif(mean(P.infection.eff),2)
    sd<-signif(sd(P.infection.eff),2)
    
    mean.pop<-mean(P.infect.eff.pop)
  }
  
  x<-paste("Infection Risk:",mean,"\u00b1",sd)
  Encoding(x)<-"UTF-8"
  
  x.print<<-x
  
  
  frame.temp<-data.frame(mean=mean,C.water=C.water)
  frame.run<<-frame.temp
  
  #windows()
  
  AR<-0.05
  
  IR.P<-2.29/100000
  
  male.matrix<-matrix(ncol=4,nrow=4)
  
  female.matrix<-matrix(ncol=4,nrow=4)
  
  colnames(male.matrix)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
  colnames(female.matrix)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
  rownames(male.matrix)<-c("55-64","65-74","75-84","85+")
  rownames(female.matrix)<-c("55-64","65-74","75-84","85+")
  
  male.matrix[1,]<-c(NA,1.72,17.22,6.20)/100000
  male.matrix[2,]<-c(NA,3.22,16.59,8.29)/100000
  male.matrix[3,]<-c(NA,NA,15.34,10.26)/100000
  male.matrix[4,]<-c(NA,NA,15.65,11.89)/100000
  
  female.matrix[1,]<-c(NA,NA,10.24,3.24)/100000
  female.matrix[2,]<-c(NA,NA,9.31,4.20)/100000
  female.matrix[3,]<-c(NA,NA,9.68,4.63)/100000
  female.matrix[4,]<-c(NA,NA,10.10,5.84)/100000
  
  P.illness.male<-matrix(nrow=4,ncol=4)
  P.illness.female<-matrix(nrow=4,ncol=4)
  
  colnames(P.illness.male)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
  colnames(P.illness.female)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
  
  rownames(P.illness.male)<-c("55-64","65-74","75-84","85+")
  rownames(P.illness.female)<-c("55-64","65-74","75-84","85+")
  
  collength<-4
  rowlength<-4
  
  for (j in 1:collength){
    for (i in 1:rowlength){
      MR.G.M<-AR * (male.matrix[i,j]/IR.P)
      #now converted to per 100,000 people
      P.illness.male[i,j]<-mean.pop*MR.G.M*100000
      
      MR.G.F<-AR * (female.matrix[i,j]/IR.P)
      P.illness.female[i,j]<-mean.pop*MR.G.F*100000
    }
  }
  
  require(reshape2)
  
  mat.male<<-melt(P.illness.male)
  mat.female<<-melt(P.illness.female)
  

}




