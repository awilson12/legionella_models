#Illness risks for different groups

mean<-1e-3

AR<-0.05

IR.P<-2.29/100000

male.matrix<-matrix(ncol=4,nrow=3)

female.matrix<-matrix(ncol=4,nrow=3)

colnames(male.matrix)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
colnames(female.matrix)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
rownames(male.matrix)<-c("55-64","65-74","75-84")
rownames(female.matrix)<-c("55-64","65-74","75-84")

male.matrix[1,]<-c(NA,1.72,17.22,6.20)/100000
male.matrix[2,]<-c(NA,3.22,16.59,8.29)/100000
male.matrix[3,]<-c(NA,15.34,10.26)/100000

female.matrix[1,]<-c(NA,NA,10.24,3.24)/100000
female.matrix[2,]<-c(NA,NA,9.31,4.20)/100000
female.matrix[3,]<-c(NA,NA,9.68,4.63)/100000

P.illness.male<-matrix(nrow=3,ncol=4)
P.illness.female<-matrix(nrow=3,ncol=4)

colnames(P.illness.male)<-c("Native American","Asian/Pacific Islander","African American/Black","White")
colnames(P.illness.female)<-c("Native American","Asian/Pacific Islander","African American/Black","White")

rownames(P.illness.male)<-c("55-64","65-74","75-84")
rownames(P.illness.female)<-c("55-64","65-74","75-84")

collength<-4
rowlength<-3

for (j in 1:collength){
  for (i in 1:rowlength){
    MR.G.M<-AR * (male.matrix[i,j]/IR.P)
    P.illness.male[i,j]<-mean*MR.G.M
    
    MR.G.F<-AR * (female.matrix[i,j]/IR.P)
    P.illness.female[i,j]<-mean*MR.G.F
  }
}
  
require(reshape2)

mat.male<-melt(P.illness.male)
mat.female<-melt(P.illness.female)

A<-ggplot(mat.male)+geom_tile(aes(x=Var1,y=Var2,fill=value))+
  geom_text(aes(label=signif(value,2),x=Var1,y=Var2),size=7)+
  scale_fill_gradient2(low="white",mid="#99CCFF",high="#0066CC",name=expression("# LD Cases/100,000"))+
  scale_x_discrete(name="Age Range")+
  scale_y_discrete(name="Race")+
  theme_bw()+
  theme(axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title=element_text(size=14),
        title=element_text(size=16),
        legend.text=element_text(size=14))+ggtitle("Male")+
  guides(fill = guide_colourbar(barwidth = 12, barheight = 1))

B<-ggplot(mat.female)+geom_tile(aes(x=Var1,y=Var2,fill=value*100))+
  geom_text(aes(label=signif(value,2),x=Var1,y=Var2),size=7)+
  scale_fill_gradient2(low="white",mid="#99CCFF",high="#0066CC",name=expression("# LD Cases/100,000"))+
  scale_x_discrete(name="Age Range")+
  scale_y_discrete(name="Race")+
  theme_bw()+
  theme(axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title=element_text(size=14),
        title=element_text(size=16),
        legend.text=element_text(size=14))+ggtitle("Female")

windows()
ggarrange(A,B,common.legend = TRUE)

