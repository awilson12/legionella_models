#Illness risks for different groups
set.seed(42)

AR<-0.05

IR.P<-2.29/100000
  
IR.age.20.29<-0.34/100000
IR.age.30.39<-0.97/100000
IR.age.40.49<-1.94/100000
IR.age.50.59<-3.88/100000
IR.age.60.69<-5.21/100000
IR.age.70.79<-6.57/100000
IR.age.80.85<-7.69/100000
IR.age.85.plus<-9.24/100000

IR.female<-1.73/100000
IR.male<-2.86/100000

IR.american.indian.alaskan.native<-0.60/100000
IR.asian.pacific.islander<-0.49/100000
IR.african.american.black<-3.44/100000
IR.white<-1.85/100000

IR.hispanic<-0.74/100000
IR.nonhispanic<-2.06/100000

IRgroups<-c(IR.age.20.29,IR.age.30.39,IR.age.40.49,IR.age.50.59,IR.age.60.69,IR.age.70.79,IR.age.80.85,IR.age.85.plus,
            IR.female,IR.male,
            IR.american.indian.alaskan.native,IR.asian.pacific.islander,IR.african.american.black,IR.white,
            IR.hispanic,IR.nonhispanic)


mean.infect.schoen.ashbolt<-mean(frameall$infection.risk[frameall$model=="Schoen & Ashbolt"])

mean.infect.hamilton.conv<-mean(frameall$infection.risk[frameall$model=="Hamilton" & frameall$showertype=="Conventional"])

mean.infect.hamilton.eff<-mean(frameall$infection.risk[frameall$model=="Hamilton" & frameall$showertype=="Water Efficient"])

infect<-c(mean.infect.schoen.ashbolt,mean.infect.hamilton.conv,mean.infect.hamilton.eff)

P.illness<-matrix(nrow=16,ncol=3)
colnames(P.illness)<-c("Schoen & Ashbolt","Hamilton et al., Conventional","Hamilton et al., Water Efficient")
rownames(P.illness)<-c("Ages 20-29","Ages 30-39","Ages 40-49","Ages 50-59","Ages 60-69","Ages 70-79","Ages 80-85","Ages 85+",
                       "Female","Male",
                       "American Indian/Alaskan Native","Asian/Pacific Islander","African American/Black","White",
                       "Hispanic","Nonhispanic")

groups<-c("Ages 20-29","Ages 30-39","Ages 40-49","Ages 50-59","Ages 60-69","Ages 70-79","Ages 80-85","Ages 85+",
          "Female","Male",
          "American Indian/Alaskan Native","Asian/Pacific Islander","African American/Black","White",
          "Hispanic","Nonhispanic")

for (j in 1:length(infect)){
  for (i in 1:length(IRgroups)){
    
    MR.G<-AR * (IRgroups[i]/IR.P)
    P.illness[i,j]<-infect[j]*MR.G
  }
}

illness<-c(c(P.illness[,1]),c(P.illness[,2]),c(P.illness[,3]))
group<-rep(groups,3)
model<-c(rep("Schoen & Ashbolt",length(groups)),
           rep("Hamilton et al.,Conventional              ",length(groups)),
           rep("Hamilton et al.,Water Efficient", length(groups)))
frame.plot<-data.frame(illness=illness,group=group,model=model)

frame.plot$group<-factor(frame.plot$group,levels=groups)

windows()
ggplot(frame.plot)+geom_tile(aes(x=model,y=group,fill=log10(illness)))+
  geom_text(aes(label=signif(illness,2),x=model,y=group),size=7)+
  scale_fill_gradient2(low="white",mid="#99CCFF",high="#0066CC",midpoint=median(log10(frame.plot$illness)),name=expression("Log"[10]*phantom(x)*"Illness Risk"))+
  scale_x_discrete(name="")+
  scale_y_discrete(name="")+
  theme_bw()+
  theme(axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))



