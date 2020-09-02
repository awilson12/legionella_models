
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
                            "1,10,000",
                            "1/100,000",
                            "1/1,000,000")),
      selectInput(inputId = "type",
                  label = "Shower Type",
                  choices=c("Conventional","Water Efficient")),

      sliderInput(inputId = "age",
                  label="Age (yrs)",
                  min=11,
                  max=81 ,
                  value=1),
      
      selectInput(inputId = "sex",
                  label = "Sex",
                  choices=c("All","Male","Female")),
      
      selectInput(inputId = "race",
                  label = "Race",
                  choices=c("All","African American/Black","White","Asian/Pacific Islander","American Indian/Alaskan Native")),
      
      selectInput(inputId = "ethnicity",
                  label = "Ethnicity",
                  choices=c("All","Hispanic","Non-Hispanic")),
      
      
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Infection Risk",align="center"),
      
      plotOutput("test")
    )
  )
)

server <- function(input, output) {
  
  frame.conc.compare<-reactive({
    frame.conc.compare<-read.csv('frame.plot.csv')
    
  })

  output$test <-renderPlot({
    
    source("final_model_for_app.R")
    

    concentrations<-c(1E-3,1E-2,1E-1,1E0,1E1,1E2,1E3,1E4)
    
    mean.ham.eff<-rep(NA,length(concentrations))
    sd.ham.eff<-rep(NA,length(concentrations))
    
    mean.ham.conv<-rep(NA,length(concentrations))
    sd.ham.conv<-rep(NA,length(concentrations))
    
    for (i in 1:length(concentrations)){
      
      hamilton(showerduration=input$showerduration,C.water=concentrations[i],type=input$type,sex=input$sex,age=input$age)
      
      infectionrisk.all<-c(P.infect.conv,P.infect.eff)
      model<-c(rep("Hamilton",iterations*2))
      showertype<-c(rep("Conventional",iterations),rep("Water Efficient",iterations))
      
      frameall<-data.frame(infection.risk=infectionrisk.all,model=model,showertype=showertype)
      
      if (i==1){
        frame.total<-frameall
        frame.total$conc<-concentrations[i]
      }else{
        frameall$conc<-concentrations[i]
        frame.total<-rbind(frame.total,frameall)
      }
      
      mean.ham.eff[i]<-mean(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient"])
      sd.ham.eff[i]<-sd(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient"])
      
      mean.ham.conv[i]<-mean(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$model=="Hamilton" & frame.total$showertype!="Water Efficient"])
      sd.ham.conv[i]<-sd(frame.total$infection.risk[frame.total$conc==concentrations[i] & frame.total$model=="Hamilton" & frame.total$showertype!="Water Efficient"])
      
    }
    
    mean<-c(mean.ham.eff,mean.ham.conv)
    sd<-c(sd.ham.eff,sd.ham.conv)
    model<-c(rep("Hamilton",length(c(mean.ham.eff,mean.ham.conv))))
    type<-c(rep("Water Efficient",length(mean.ham.eff)),rep("Conventional",length(mean.ham.conv)))
    conc<-rep(concentrations,2)
    frame.conc.compare<-data.frame(mean=mean,sd=sd,model=model,type=type,conc=conc)
    
    frame.conc.compare$type <- factor(frame.conc.compare$type, levels = c("Conventional","Water Efficient"))
    
    hamilton(showerduration=input$showerduration,C.water=10^input$C.water,type=input$type,age=input$age)
  
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
    
    
    ggplot(data=frame.conc.compare)+geom_line(aes(x=conc,y=mean,group=type,color=type),size=1.5)+
      geom_point(aes(x=conc,y=mean,group=type,color=type),size=6)+
      geom_errorbar(aes(x=conc,ymin=mean-sd,ymax=mean+sd,group=type,color=type),size=1,width=.2)+
      geom_point(data=frame.run,aes(C.water,y=mean),fill=choice,size=7,shape=23)+
      scale_y_continuous(trans="log10",name="Infection Risk",breaks=10^seq(-8,0,1),
                         labels=c("1/100,000,000","1/10,000,000","1/1,000,000","1/100,000","1/10,000","1/1,000","1/100","1/10","1"))+
      scale_x_continuous(trans="log10",name="CFU/mL",
                         labels=c("0.001","0.01","0.1","1","10","100","1,000","10,000"),breaks=10^seq(-3,4,1))+
      scale_colour_manual(values=c("#0066CC","#99CCFF","light blue"),name="Shower Type")+
      geom_hline(yintercept=threshold,linetype="dotted",color="black",size=2)+
      annotate("text",label=c("1/10,000 Risk Threshold"),x=1e3,y=2e-04,size=5)+
      theme_bw()+
      theme(axis.title = element_text(size=18),axis.text=element_text(size=18),
            legend.title=element_text(size=18),legend.text=element_text(size=18),
            legend.box="vertical",legend.position="top")+
      annotate("text",x=1,y=1,label=x.print,size=6)
    
  })

}
  
  shinyApp(ui = ui, server = server)