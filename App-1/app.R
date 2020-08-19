
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
                  max = 8,
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

      selectInput(inputId = "age",
                  label = "Age range",
                  choices=c("20-29","30-39","40-49",
                            "50-59","60-69","70-79",
                            "80-85","85+")),
      
      selectInput(inputId = "sex",
                  label = "Sex",
                  choices=c("Male","Female")),
      
      selectInput(inputId = "race",
                  label = "Race",
                  choices=c("African American/Black","White","Asian/Pacific Islander","American Indian/Alaskan Native")),
      
      selectInput(inputId = "ethnicity",
                  label = "Ethnicity",
                  choices=c("Hispanic","Non-Hispanic")),
      
      
      
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
    
    if(input$type=="Conventional"){
      type="conventional"
    }else{
      type="water efficient"
    }
    
    
    source("final_model_for_app.R")
    hamilton(showerduration=input$showerduration,C.water=10^input$C.water,type=type)
    frame.conc.compare<-read.csv('C:/Users/wilso/Documents/legionella_models/App-1/frame.conc.compare.csv')
    
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
    
    
    
    ggplot(frame.conc.compare)+geom_line(aes(x=conc/1000,y=mean,group=type,color=type),size=1.5)+
      geom_point(aes(x=conc/1000,y=mean,group=type,color=type),size=6)+
      geom_errorbar(aes(x=conc/1000,ymin=mean-sd,ymax=mean+sd,group=type,color=type),size=1,width=.2)+
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