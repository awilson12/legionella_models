y<-c("Male","Female",
     "15-24","25-34","35-44","45-54","55-64","65-74","75-84","85+")

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
      
      selectInput(inputId = "choose_category",
                  label="Illness Risk: Demographic Category",
                  choices=unique(choices_table$category),
                  selected=unique(choices_table$category[1])),
      
      selectInput(inputId ="choose_item",
                  label="Select",
                  choices=unique(choices_table$items),
                  selected=unique(choices_table$items)[1])
    
  ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Infection and Illness Risks",align="center"),
      
      plotOutput("infection",width = "100%")
    )
  )
)

server <- function(input, output,session) {
  
  #frame.conc.compare<-reactive({
    #frame.conc.compare<-read.csv('frame.plot.csv')
    
  #})
  
  observe({ updateSelectInput(session=session,inputId = "choose_item",
                             choices=unique(choices_table[choices_table$category==input$choose_category,"items"]
                                            
                             ))})  

  output$infection <-renderPlot({
    
    
    if(input$choose_category=="Sex"){
      
      if (input$choose_item=="Male"){
        
        IR<-2.86/100000 #male
        
      }else{
        
        IR<-1.73/100000 #female
      }
      
    }else if (input$choose_item=="Maale" && "American Indian/Alaskan Native" && input$choose_category=="Age"){
      
      if (input$choose_item=="15-24"){
        
        IR<-0.00/100000
        
      }else if (input$choose_item=="25-34"){
        
        IR<-0.26/100000
        
      }else if (input$choose_item=="35-44"){
        
        IR<-0.31/100000
        
      }else if (input$choose_item=="45-54"){
        
        IR<-1.80/100000
        
      }else if (input$choose_item=="55-64"){
        
        IR<-4.33/100000
        
      }else if (input$choose_item=="65-74"){
        
        IR<-6.26/100000
        
      }else if (input$choose_item=="75-84"){
        
        IR<-6.18/100000
        
      }else{
        #85+ selection
        IR<-0.00/100000
      }
        
    }else if (input$choose_item=="Maale" && "Asian/Pacific Islander" && input$choose_category=="Age"){
        
      if (input$choose_item=="15-24"){
          
          IR<-0.00/100000
          
      }else if (input$choose_item=="25-34"){
          
          IR<-0.17/100000
          
      }else if (input$choose_item=="35-44"){
          
          IR<-0.31/100000
          
      }else if (input$choose_item=="45-54"){
          
          IR<-0.75/100000
          
      }else if (input$choose_item=="55-64"){
          
          IR<-1.72/100000
          
      }else if (input$choose_item=="65-74"){
          
          IR<-3.22/100000
          
      }else if (input$choose_item=="75-84"){
          
          IR<-2.53/100000
          
       }else{
          #85+ selection
          IR<-6.50/100000
       }
    }else if (input$choose_item=="Maale" && "African American/Black" && input$choose_category=="Age"){
         
         if (input$choose_item=="15-24"){
           
           IR<-0.45/100000
           
         }else if (input$choose_item=="25-34"){
           
           IR<-2.72/100000
           
         }else if (input$choose_item=="35-44"){
           
           IR<-6.94/100000
           
         }else if (input$choose_item=="45-54"){
           
           IR<-12.32/100000
           
         }else if (input$choose_item=="55-64"){
           
           IR<-17.22/100000
           
         }else if (input$choose_item=="65-74"){
           
           IR<-16.59/100000
           
         }else if (input$choose_item=="75-84"){
           
           IR<-15.34/100000
           
         }else{
           #85+ selection
           IR<-15.65/100000
         }
    }else if (input$choose_item=="Maale" && "White" && input$choose_category=="Age"){
           
           if (input$choose_item=="15-24"){
             
             IR<-0.14/100000
             
           }else if (input$choose_item=="25-34"){
             
             IR<-0.59/100000
             
           }else if (input$choose_item=="35-44"){
             
             IR<-1.69/100000
             
           }else if (input$choose_item=="45-54"){
             
             IR<-3.54/100000
             
           }else if (input$choose_item=="55-64"){
             
             IR<-6.20/100000
             
           }else if (input$choose_item=="65-74"){
             
             IR<-8.29/100000
             
           }else if (input$choose_item=="75-84"){
             
             IR<-10.26/100000
             
           }else{
             #85+ selection
             IR<-11.89/100000
           }
    }else if (input$choose_item=="Female" && "American Indian/Alaskan Native" && input$choose_category=="Age"){
             
             if (input$choose_item=="15-24"){
               
               IR<-0.00/100000
               
             }else if (input$choose_item=="25-34"){
               
               IR<-0.28/100000
               
             }else if (input$choose_item=="35-44"){
               
               IR<-0.33/100000
               
             }else if (input$choose_item=="45-54"){
               
               IR<-1.81/100000
               
             }else if (input$choose_item=="55-64"){
               
               IR<-1.60/100000
               
             }else if (input$choose_item=="65-74"){
               
               IR<-3.44/100000
               
             }else if (input$choose_item=="75-84"){
               
               IR<-3.20/100000
               
             }else{
               #85+ selection
               IR<-12.45/100000
             }   
     }else if (input$choose_item=="Femaale" && "Asian/Pacific Islander" && input$choose_category=="Age"){
               
               if (input$choose_item=="15-24"){
                 
                 IR<-0.00/100000
                 
               }else if (input$choose_item=="25-34"){
                 
                 IR<-0.00/100000
                 
               }else if (input$choose_item=="35-44"){
                 
                 IR<-0.22/100000
                 
               }else if (input$choose_item=="45-54"){
                 
                 IR<-0.13/100000
                 
               }else if (input$choose_item=="55-64"){
                 
                 IR<-0.80/100000
                 
               }else if (input$choose_item=="65-74"){
                 
                 IR<-1.16/100000
                 
               }else if (input$choose_item=="75-84"){
                 
                 IR<-1.97/100000
                 
               }else{
                 #85+ selection
                 IR<-4.92/100000
               }
    }else if (input$choose_item=="Femaale" && "African American/Black" && input$choose_category=="Age"){
                 
                 if (input$choose_item=="15-24"){
                   
                   IR<-0.29/100000
                   
                 }else if (input$choose_item=="25-34"){
                   
                   IR<-0.83/100000
                   
                 }else if (input$choose_item=="35-44"){
                   
                   IR<-2.79/100000
                   
                 }else if (input$choose_item=="45-54"){
                   
                   IR<-6.03/100000
                   
                 }else if (input$choose_item=="55-64"){
                   
                   IR<-10.24/100000
                   
                 }else if (input$choose_item=="65-74"){
                   
                   IR<-9.31/100000
                   
                 }else if (input$choose_item=="75-84"){
                   
                   IR<-9.68/100000
                   
                 }else{
                   #85+ selection
                   IR<-10.10/100000
                 }
    }else if (input$choose_item=="Maale" && "White" && input$choose_category=="Age"){
                   
                   if (input$choose_item=="15-24"){
                     
                     IR<-0.11/100000
                     
                   }else if (input$choose_item=="25-34"){
                     
                     IR<-0.27/100000
                     
                   }else if (input$choose_item=="35-44"){
                     
                     IR<-0.83/100000
                     
                   }else if (input$choose_item=="45-54"){
                     
                     IR<-1.86/100000
                     
                   }else if (input$choose_item=="55-64"){
                     
                     IR<-3.24/100000
                     
                   }else if (input$choose_item=="65-74"){
                     
                     IR<-4.20/100000
                     
                   }else if (input$choose_item=="75-84"){
                     
                     IR<-4.63/100000
                     
                   }else{
                     #85+ selection
                     IR<-5.84/100000
                   }
    }
    
    AR<-0.05
    
    IR.P<-2.29/100000
    
    MR.G<-AR * (IR/IR.P)
    
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
    
    mean.ham.conv.illness<-rep(NA,length(concentrations))
    sd.ham.conv.illness<-rep(NA,length(concentrations))
    
    mean.ham.eff.illness<-rep(NA,length(concentrations))
    sd.ham.eff.illness<-rep(NA,length(concentrations))
    
    
    for (i in 1:length(concentrations)){
      
      hamilton(showerduration=input$showerduration,C.water=concentrations[i],type=input$type,sex=input$sex,age=input$age)

      P.illness.conv<-P.infect.conv.pop*MR.G
      P.illness.eff<-P.infect.eff.pop*MR.G

      
      risk.all<-c(P.infect.conv,P.infect.eff,P.infect.conv.pop,P.infect.eff.pop,P.illness.conv,P.illness.eff)
      model<-c(rep("Hamilton",iterations*6))
      showertype<-rep(c(rep("Conventional",iterations),rep("Water Efficient",iterations)),3)
      runtype<-c(rep("Demographic-specific",iterations*2),rep("Population level",iterations*2),rep("Demographic-specific illness",iterations*2))
      
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
      
      #population level illness risks
      mean.ham.eff.illness[i]<-mean(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient" & runtype=="Demographic-specific illness"])
      sd.ham.eff.illness[i]<-sd(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype=="Water Efficient" & runtype=="Demographic-specific illness"])
      
      mean.ham.conv.illness[i]<-mean(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype!="Water Efficient" & runtype=="Demographic-specific illness"])
      sd.ham.conv.illness[i]<-sd(frame.total$risk[frame.total$conc==concentrations[i] & frame.total$showertype!="Water Efficient" & runtype=="Demographic-specific illness"])
      
    }
    
    mean<-c(mean.ham.eff,mean.ham.conv,mean.ham.eff.pop,mean.ham.conv.pop,mean.ham.eff.illness,mean.ham.conv.illness)
    sd<-c(sd.ham.eff,sd.ham.conv,sd.ham.eff.pop,sd.ham.conv.pop,sd.ham.eff.illness,sd.ham.conv.illness)
    
    model<-c(rep("Hamilton",length(c(mean.ham.eff,mean.ham.conv,mean.ham.eff.pop,mean.ham.conv.pop,mean.ham.eff.illness,mean.ham.conv.illness))))
    
    type<-rep(c(rep("Water Efficient",length(mean.ham.eff)),rep("Conventional",length(mean.ham.conv))),6)
    conc<-rep(concentrations,6)
    runtype.2<-c(rep("Demographic-specific",length(c(mean.ham.eff,mean.ham.conv))),rep("Population level",length(c(mean.ham.eff.pop,mean.ham.conv.pop))),
                 rep("Demographic-specific Illness",length(c(mean.ham.eff.illness,mean.ham.conv.illness))))
    risktype<-c(rep("Infection Risk",length(c(mean.ham.eff,mean.ham.conv,mean.ham.eff.pop,mean.ham.conv.pop))),
              rep("Illness Risk",length(c(mean.ham.eff.illness,mean.ham.conv.illness))))
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
            legend.title=element_text(size=18),legend.text=element_text(size=18),
            legend.box="vertical",legend.position="top")+
      annotate("text",x=1,y=1,label=x.print,size=6)
    
    frametemp<-frame.conc.compare[frame.conc.compare$conc==input$C.water  
                                  & frame.conc.compare$risktype=="Illness Risk" 
                                  & frame.conc.compare$type=="Conventional",]
    
    frametemp2<-frame.conc.compare[frame.conc.compare$conc==input$C.water  
                                  & frame.conc.compare$risktype=="Illness Risk" 
                                  & frame.conc.compare$type=="Water Efficient",]
    
    x.2.print<-paste("Illness Risk, Conventional:",signif(frametemp$mean,2),"\u00b1",signif(frametemp$sd,2))
    Encoding(x.2.print)<-"UTF-8"
    
    x.3.print<-paste("Illness Risk, Water Efficient:",signif(frametemp2$mean,2),"\u00b1",signif(frametemp2$sd,2))
    Encoding(x.3.print)<-"UTF-8"
    
    
    B<-ggplot(data=frame.conc.compare[frame.conc.compare$risktype=="Illness Risk",])+
      geom_line(aes(x=conc,y=mean,group=type,color=type),size=1.5)+
      geom_point(aes(x=conc,y=mean,group=type,color=type),size=6)+
      geom_errorbar(aes(x=conc,ymin=mean-sd,ymax=mean+sd,group=type,color=type),size=1,width=.2)+
      geom_point(data=frame.conc.compare[frame.conc.compare$conc==input$C.water & frame.conc.compare$risktype=="Illness Risk",],
                 aes(conc,y=mean),fill="black",size=7,shape=23)+
      scale_y_continuous(trans="log10",name="Illness Risk",breaks=10^seq(-8,0,1),
                         labels=c("1/100,000,000","1/10,000,000","1/1,000,000","1/100,000","1/10,000","1/1,000","1/100","1/10","1"))+
      scale_x_continuous(trans="log10",name="CFU/mL",
                         labels=c("0.001","0.01","0.1","1","10","100","1,000","10,000"),breaks=10^seq(-3,4,1))+
      scale_colour_manual(values=c("#0066CC","#99CCFF"),name="Shower Type")+
      geom_hline(yintercept=threshold,linetype="dotted",color="black",size=2)+
      #annotate("text",label=c("1/10,000 Risk Threshold"),x=1e3,y=2e-04,size=5)+
      theme_bw()+
      theme(axis.title = element_text(size=18),axis.text=element_text(size=18),
            legend.title=element_text(size=18),legend.text=element_text(size=18),
            legend.box="vertical",legend.position="top")+
      annotate("text",x=1,y=1,label=x.2.print,size=6)+
      annotate("text",x=1,y=0.3,label=x.3.print,size=6)
      
      
    ggarrange(A,B)
    
  })

}
  
  shinyApp(ui = ui, server = server)