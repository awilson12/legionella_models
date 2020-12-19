y<-c("Male","Female",
     "African American/Black","White","Asian/Pacific Islander","American Indian/Alaskan Native",
     "Hispanic","Non-Hispanic",
     "20-29","30-39","40-49","50-59","60-69","70-79","80-85","85+")

x<-c("Sex","Sex",
     "Race","Race","Race","Race",
     "Ethnicity","Ethnicity",
     "Age","Age","Age","Age","Age","Age","Age","Age")

choices_table<-as.data.frame(cbind(x,y))
colnames(choices_table)<-c("category","items")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Calculator Inputs"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      sliderInput(inputId = "C.water",
                  label = "Log10 CFU/mL",
                  min = -3,
                  max = 4,
                  value=1),
      sliderInput(inputId = "showerduration",
                  label = "Shower duration (min)",
                  min = 1,
                  max = 30,
                  value = 30),
      selectInput(inputId = "threshold",
                  label = "Risk Threshold",
                  choices=c("1/1,000",
                            "1/10,000",
                            "1/100,000",
                            "1/1,000,000")),
      selectInput(inputId = "type",
                  label = "Shower Type",
                  choices=c("Conventional","Water Efficient")),
      
      sliderInput(inputId = "age",
                  label="Infection Risk Input: Age (yrs)",
                  min=11,
                  max=81 ,
                  value=1),
      
      selectInput(inputId = "sex",
                  label = "Infection Risk Input: Sex",
                  choices=c("Male","Female")),
    
  ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Infection Risks and Expected # LD Cases/100,000",align="center"),
      
      plotOutput("infection",width = "80%"),
      br(),
      plotOutput("illness",width="100%")
    )
  )
)

server <- function(input, output,session) {

  output$infection <-renderPlot({
    
    source("final_model_for_app.R")
    
    concentrations<-c(1E-3,1E-2,1E-1,1E0,1E1,1E2,1E3,1E4)
    
    #vectors for saving mean and sd of infection risks
    mean.ham.eff<-rep(NA,length(concentrations))
    sd.ham.eff<-rep(NA,length(concentrations))
    
    mean.ham.conv<-rep(NA,length(concentrations))
    sd.ham.conv<-rep(NA,length(concentrations))
    
    mean.ham.eff.pop<-rep(NA,length(concentrations))
    sd.ham.eff.pop<-rep(NA,length(concentrations))
    
    mean.ham.conv.pop<-rep(NA,length(concentrations))
    sd.ham.conv.pop<-rep(NA,length(concentrations))

    
    for (i in 1:length(concentrations)){
      
      hamilton(showerduration=input$showerduration,C.water=concentrations[i],type=input$type,sex=input$sex,age=input$age)
      
      risk.all<-c(P.infect.conv,P.infect.eff,P.infect.conv.pop,P.infect.eff.pop)
      model<-c(rep("Hamilton",iterations*4))
      showertype<-rep(c(rep("Conventional",iterations),rep("Water Efficient",iterations)),2)
      runtype<-c(rep("Demographic-specific",iterations*2),rep("Population level",iterations*2))
      
      frameall<-data.frame(risk=risk.all,model=model,showertype=showertype,runtype=runtype)
      
      if (i==1){
        frame.total<-frameall
        frame.total$conc<-concentrations[i]
      }else{
        frameall$conc<-concentrations[i]
        frame.total<-rbind(frame.total,frameall)
      }
      
      #demographic-specific infection risks
      mean.ham.eff[i]<-mean(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient" & runtype=="Demographic-specific"])
      sd.ham.eff[i]<-sd(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient" & runtype=="Demographic-specific"])
      
      mean.ham.conv[i]<-mean(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$model=="Hamilton" & frame.total$showertype!="Water Efficient" & runtype=="Demographic-specific"])
      sd.ham.conv[i]<-sd(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$model=="Hamilton" & frame.total$showertype!="Water Efficient" & runtype=="Demographic-specific"])
      
      #population level infection risks
      mean.ham.eff.pop[i]<-mean(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient" & runtype=="Population level"])
      sd.ham.eff.pop[i]<-sd(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient" & runtype=="Population level"])
      
      mean.ham.conv.pop[i]<-mean(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype!="Water Efficient" & runtype=="Population level"])
      sd.ham.conv.pop[i]<-sd(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype!="Water Efficient" & runtype=="Population level"])
    }
    
    mean<-c(mean.ham.eff,mean.ham.conv,mean.ham.eff.pop,mean.ham.conv.pop)
    sd<-c(sd.ham.eff,sd.ham.conv,sd.ham.eff.pop,sd.ham.conv.pop)
    
    model<-c(rep("Hamilton",length(c(mean.ham.eff,mean.ham.conv,mean.ham.eff.pop,mean.ham.conv.pop))))
    
    type<-rep(c(rep("Water Efficient",length(mean.ham.eff)),rep("Conventional",length(mean.ham.conv))),4)
    conc<-rep(concentrations,4)
    runtype.2<-c(rep("Demographic-specific",length(c(mean.ham.eff,mean.ham.conv))),rep("Population level",length(c(mean.ham.eff.pop,mean.ham.conv.pop))))
    risktype<-c(rep("Infection Risk",length(c(mean.ham.eff,mean.ham.conv,mean.ham.eff.pop,mean.ham.conv.pop))))
    frame.conc.compare<-data.frame(mean=mean,sd=sd,model=model,type=type,conc=conc,runtype.2=runtype.2,risktype=risktype)
    
    frame.conc.compare$type <- factor(frame.conc.compare$type, levels = c("Conventional","Water Efficient"))
    
    hamilton(showerduration=input$showerduration,C.water=10^input$C.water,type=input$type,sex=input$sex,age=input$age)
  
    if(input$threshold=="1/1,000"){
      threshold=1/1000
    }else if (input$threshold=="1/10,000"){
      threshold=1/10000
    }else if (input$threshold=="1/100,000"){
      threshold=1/100000
    }else{
      threshold=1/1000000
    }
    
    if(frame.run$mean>threshold){
      choice<-"red"
    }else{
      choice<-"green"
    }
    
    A<-ggplot(data=frame.conc.compare[frame.conc.compare$runtype.2=="Demographic-specific",])+
      geom_line(aes(x=conc,y=mean,group=type,color=type),size=1.5)+
      geom_point(aes(x=conc,y=mean,group=type,color=type),size=6)+
      geom_errorbar(aes(x=conc,ymin=mean-sd,ymax=mean+sd,group=type,color=type),size=1,width=.2)+
      geom_point(data=frame.run,aes(C.water,y=mean),fill=choice,size=7,shape=23)+
      scale_y_continuous(trans="log10",name="Infection Risk",breaks=10^seq(-8,0,1),
                         labels=c("1/100,000,000","1/10,000,000","1/1,000,000","1/100,000","1/10,000","1/1,000","1/100","1/10","1"))+
      scale_x_continuous(trans="log10",name="CFU/mL",
                         labels=c("0.001","0.01","0.1","1","10","100","1,000","10,000"),breaks=10^seq(-3,4,1))+
      scale_colour_manual(values=c("#0066CC","#99CCFF"),name="Shower Type")+
      geom_hline(yintercept=threshold,linetype="dotted",color="black",size=2)+
      #annotate("text",label=c("1/10,000 Risk Threshold"),x=1e3,y=2e-04,size=5)+
      theme_bw()+
      theme(axis.title = element_text(size=18),axis.text=element_text(size=18),
            legend.title=element_text(size=20),legend.text=element_text(size=18),
            legend.box="vertical",legend.position="top")+
      annotate("text",x=1,y=1,label=x.print,size=6)
       
    B<-ggplot(mat.male)+geom_tile(aes(x=Var1,y=Var2,fill=value))+
      geom_text(aes(label=signif(value,2),x=Var1,y=Var2),size=7)+
      scale_fill_gradient2(low="white",mid="#99CCFF",high="#0066CC",name=expression("# LD Cases/100,000"))+
      scale_x_discrete(name="Age Range")+
      scale_y_discrete(name="Race")+
      theme_bw()+
      theme(axis.text.x=element_text(size=16),
            axis.text.y=element_text(size=16),
            axis.title=element_text(size=16),
            legend.title=element_text(size=20),
            title=element_text(size=16),
            legend.text=element_text(size=14))+ggtitle("Male")+
      guides(fill = guide_colourbar(barwidth = 12, barheight = 1))
    
    C<-ggplot(mat.female)+geom_tile(aes(x=Var1,y=Var2,fill=value))+
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
  
    A
    
  })
  
  output$illness <-renderPlot({
    
    source("final_model_for_app.R")
    hamilton(showerduration=input$showerduration,C.water=10^input$C.water,type=input$type,sex=input$sex,age=input$age)
    
  
  B<-ggplot(mat.male)+geom_tile(aes(x=Var1,y=Var2,fill=value))+
    geom_text(aes(label=signif(value,2),x=Var1,y=Var2),size=7)+
    scale_fill_gradient2(low="white",mid="#99CCFF",high="#0066CC",name=expression("#LD Cases/100,000"))+
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
  
  C<-ggplot(mat.female)+geom_tile(aes(x=Var1,y=Var2,fill=value))+
    geom_text(aes(label=signif(value,2),x=Var1,y=Var2),size=7)+
    scale_fill_gradient2(low="white",mid="#99CCFF",high="#0066CC",name=expression("#LD Cases/100,000"))+
    scale_x_discrete(name="Age Range")+
    scale_y_discrete(name="Race")+
    theme_bw()+
    theme(axis.text.x=element_text(size=16),
          axis.text.y=element_text(size=16),
          axis.title=element_text(size=16),
          legend.title=element_text(size=14),
          title=element_text(size=16),
          legend.text=element_text(size=14))+ggtitle("Female")
  ggarrange(B,C,common.legend = TRUE)
  })

}
  
  shinyApp(ui = ui, server = server)