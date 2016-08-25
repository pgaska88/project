library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(sqldf)
require(reshape2)
library(plotly)
library(ggiraph)
library(RCurl)
library(googleVis)
library(plyr)
library(rCharts)
library(rjson)


shinyServer <- function(input, output, session) {
  
  
  
  #input for analysis Graph
  analysisUnit<-reactive({input$analysisUnit})
  analysisRegion<-reactive({input$analysisGeo})
  analysisYear<- reactive({input$analysisYear})
  
  
  
  #making input reactive
  division<-reactive({input$division})
  mydataset<-reactive({input$dataset})
  myYear <- reactive({input$Year})
  myUnit<-reactive({input$unit})
  myUnit2<-reactive({input$unit2})
  myCountry <-reactive({input$country})
  map<- reactive({input$map})
  all<-reactive({input$all})
  
  
#testing output
  output$unit <- renderText({
  })
  
  #for reactive table
  data<-reactive({
  #input tourism
    if(input$rb1=="tourism"){
  #table querying, if inputSelect not empty
        if(!is.null(input$regions) & !is.null(input$unitTable)){
        data<-tourism1[(tourism1$region %in% input$regions)&(tourism1$unit %in% input$unitTable),]
  #if empty then show the full table      
        }else{
        data<- tourism1
      }
    }
  #input gdpRegions
    else{
  #table querying, if inputSelects not empty 
        if(!is.null(input$regions)& !is.null(input$unit2Table)){
          data<-  gdpRegions1[(gdpRegions1$region %in% input$regions)&(gdpRegions1$unit %in% input$unit2Table),]
  #if input empty show full data      
        }else{
        data<-   gdpRegions1
      }
    }
    data
  })
  
  action <- dataTableAjax(session, tourism1)
  widget <- datatable(gdpRegions1, 
                      class = 'display cell-border compact hover order-column',
                      filter = list(position='top',clear=TRUE,plain=TRUE),
                      options = list(ajax = list(url = action))
  )
  
#Table output, using DT:: to make sure the latest version is used 
  output$tbl2 <- DT::renderDataTable({
    DT::datatable(data(),filter = 'bottom', options = list(pageLength = 5, autoWidth=TRUE))
  })
  
  
  
  
  
  
 # getPlot<-reactive({
#   
 #   region = input$selRegGDP3
  #  region2 = input$selRegGDP33
  #  if(region==region2){}else{region<-paste(region,' & ',region2)}
  #  description = nuts[nuts$nutsCode == region]

  #  df <- rbind(tourism1[,c(input$slider3[1]:input$slider3[2],10,11,12)], gdpRegions1[,c(input$slider3[1]:input$slider3[2],10,11,12)])
    
  #  df <- melt(df, id.vars=c("region", "unit", "df"))
  #  ggplot(df[(df$region==input$selRegGDP3& df$unit==input$selUniGDP3)|(df$region==input$selRegGDP33 & df$unit ==input$selUniGDP3),],
  #         aes(variable,value,tooltip=variable))
  #         +geom_bar(aes(fill=factor(region)),position="dodge",stat="identity")
  #         + scale_colour_gradient(low = "blue")
  #         +xlab("year")
  #         +ylab("Value")
  #         +ggtitle(paste("GDP Regions comparison, unit: ",input$selUniGDP3, ". Region: ", region))
    
#  })
 
  
  
  
   
#First plot GDP / TOURISM
  output$plot1 = renderggiraph({
    #changing MIO_EUR to THOUSAND_EUR
    gdpRegions1[gdpRegions1$unit=="MIO_EUR",1:8]<-gdpRegions1[gdpRegions1$unit=="MIO_EUR",1:8]*1000
    
    #Getting regions for title
    region = input$selRegGDP
    region2 = input$selRegTour
    if(region==region2){}else{region<-paste(region,' & ',region2)}
    
    description = nuts1[nuts1$nutsCode == region]
    
    df <- rbind(tourism1[,c(input$slider1[1]:input$slider1[2],9,10,11)], gdpRegions1[,c(input$slider1[1]:input$slider1[2],9,10,11)])
    
    df <- melt(df, id.vars=c("region", "unit", "df"))
    
    
    #for bar chart output
    if(input$rb2=="bar")
      {
            gg_<-ggplot(df[(df$region==input$selRegTour & df$unit == input$selUniTour) | (df$region == input$selRegGDP & df$unit == input$selUniGDP),],aes(variable, value,tooltip=paste0("Region: ",region,"<br/>","Value: ",value), data_id=region, fill=factor(unit)))+geom_bar_interactive(stat="identity",position=position_dodge(),colour="black")+xlab("year")+ylab("Value")
      
      }
    #if line graph output
    else
      {
            gg_ <- ggplot(df[(df$region==input$selRegTour&df$unit==input$selUniTour)|(df$region==input$selRegGDP&df$unit==input$selUniGDP),], 
                      aes(variable, value, tooltip=  paste(region ,value)   , data_id=region ,group=region ))+
              geom_line_interactive()+geom_point_interactive(size=4)
      
      }
    ggiraph(code = print(gg_ +  labs(title = paste("GDP/Tourism comparison, unit: ",input$selUniTour,", ", input$selUniGDP, " Region: ", region))),
            width = 1,
            zoom_max=5,
            selection_type = "single"
    )  
  })
  
  #Second plot 
  output$plot2 = renderggiraph({ 
    
    region = input$selRegTour2
    region2 = input$selRegTour22
    if(region==region2){}else{region<-paste(region,' & ',region2)}
    description = nuts1[nuts1$nutsCode == region]
    
    df <- rbind(tourism1[,c(input$slider2[1]:input$slider2[2],9,10,11)], gdpRegions1[,c(input$slider2[1]:input$slider2[2],9,10,11)])
    df <- melt(df, id.vars=c("region", "unit", "df"))
      
    #bar graph
    if(input$rb3=="bar")
    {
    gg_<-ggplot(df[(df$region==input$selRegTour2 & df$unit==input$SelUniTour2)|(df$region==input$selRegTour22 & df$unit ==input$SelUniTour2),],aes(variable, value,tooltip=paste0("Region: ",region,"<br/>","Value: ",value), data_id=region, fill=factor(region))) + geom_bar_interactive(stat="identity",position=position_dodge(),colour="black")+
      xlab("year")+ylab("Value")+ ggtitle(paste("Tourism comparison, unit: ",input$SelUniTour2, ". Region: ", region))+scale_fill_brewer(palette="Set1")
    }
    #line graph
    else{
      gg_<-ggplot(df[(df$region==input$selRegTour2 & df$unit==input$SelUniTour2)|(df$region==input$selRegTour22 & df$unit ==input$SelUniTour2),],aes(variable, value,tooltip=paste0("Region: ",region,"<br/>","Value: ",value), data_id=region, color= region,group=factor(region))) + geom_line_interactive()+geom_point_interactive(size=4)+
        xlab("year")+ylab("Value")+ ggtitle(paste("Tourism comparison, unit: ",input$SelUniTour2, ". Region: ", region))
      
    }
    ggiraph(code = print(gg_),
            width = 1,
            zoom_max=5,
            selection_type = "single")
    
    })
  
  #Third plot
  output$plot3 = renderggiraph({ 
    s= input$tbl2_rows_selected
    #if(length(s)){
    # region = tourism[tourism$X == s]
    #  description = nuts[nuts$nutsCode = region]
    #}
    region = input$selRegGDP3
    region2 = input$selRegGDP33
    if(region==region2){}else{region<-paste(region,' & ',region2)}
    description = nuts1[nuts1$nutsCode == region]
    
    df <- rbind(tourism1[,c(input$slider3[1]:input$slider3[2],9,10,11)], gdpRegions1[,c(input$slider3[1]:input$slider3[2],9,10,11)])
  
    df <- melt(df, id.vars=c("region", "unit", "df"))
    
    gg_<- ggplot(df[(df$region==input$selRegGDP3& df$unit==input$selUniGDP3)|(df$region==input$selRegGDP33 & df$unit ==input$selUniGDP3),],aes(variable, value,tooltip=paste0("Region: ",region,"<br/>","Value: ",value), data_id=region, fill=factor(region))) + geom_bar_interactive(stat="identity",position=position_dodge(), colour="black")+
      xlab("year")+ylab("Value")+ggtitle(paste("GDP Regions comparison, unit: ",input$selUniGDP3, ". Region: ", region))+scale_fill_manual(values=c("#999999", "#E69F00"))
    ggiraph(code = print(gg_),
            width = 1,
            zoom_max=5,
            hover_css = "fill:#FF3333;stroke:black;cursor:pointer;",
            selected_css = "fill:#FF3333;stroke:black;",
            selection_type = "single"
            )
    
  })
  #summary for plot GDP
  output$info2 <- renderPrint({
    h4("GDP Selected Region Summary")
    selected<-input$plot3_selected
    print("GDP summary for selected region and unit:",quote = FALSE )
    summary(df2[df2$region==selected & df2$unit==input$selUniGDP,5])
  })
  
  #summary for TOURISM
  output$info1 <- renderPrint({
    h4("Tourism Selected Region Summary")
    selected<-input$plot2_selected
    print("GDP summary for selected region and unit:",quote = FALSE )
    summary(df2[df2$region==selected & df2$unit==input$SelUniTour2,5])
    })
 
  output$gvis <- renderGvis({
    
    #divison variable
    z<-division()
    #map view variable
    map<-map()
    
    #selecting unit for particular dataset
    if(mydataset()==1){uni<-myUnit()}else{uni<-myUnit2()}
    
    #variable for search country% like codes
    country<-paste(myCountry(),"%",sep="",collapse=NULL)
    
    #year to be displayed
    year<-myYear()
    #one or all countries
    all<-all()
    
    
    #all regions for given country
    tc<- fn$sqldf("Select * from t where region like '$country'")
    #all regions for given unit
    tcu<- fn$sqldf("Select * from tc where unit like '$uni'")
    
    
    #z - divison
    if(z=="countries"){
      tcuy<-fn$sqldf("Select Remark,AVG(value) as value from tcu where variable like $year")
      tcuy$Remark <- myCountry()
      #if regions
    }else{
      tcuy<- fn$sqldf("Select * from tcu where variable like $year")
    }
    #if Europe view
    if(map=="Europe"){
      
      #all countries by region
      tu<-fn$sqldf("Select * from t where unit like '$uni'")
      #averages for all countries
      tca<- fn$sqldf("Select AVG(value) as value, country,unit,variable from tu where variable like $year group by country,variable ")
      
      tca<-rename(tca,c("country"="Remark"))
      reg="150"
      
    }
    #if view country
    else{
      tca<- fn$sqldf("Select AVG(value) as value, country,unit,variable from t  where variable like $year  group by unit, country,variable")
      tca<-rename(tca,c("country"="Remark"))
      reg=myCountry()
    }
    
    if(all==TRUE){
      data<-tca
    }
    else{
      data<-tcuy
    }
    map<-    gvisGeoChart(data,
                          locationvar="Remark", colorvar="value",
                          options=list(region=reg, displayMode="regions", 
                                       resolution=z,
                                       magnifyingGlass="{enable: true, zoomFactor: 7.5}",
                                       useMapTypeControl=TRUE,
                                       backgroundColor='lightblue',
                                       enableScrollWheel=TRUE,
                                       enableRegionInteractivity=TRUE,
                                       forcelFrame=TRUE,
                                       width=1000, height=800,
                                       colorAxis="{colors:['blue', 'green']}"
                          ))
    table <- gvisTable(data[with(data, order(-value)),],
                       options=list(width='automatic',height='automatic',colors="['#cbb69d', '#603913', '#c69c6e']"))
    gvisMerge(map,table ,horizontal=TRUE)
  })
  
  
#ANALYSIS
  
 
  typeOfGraph<-reactive({
    input$rbAnalysis
  })
  
  output$analysisPlot2<-renderChart2({
#    "Vertical Bar graph"="vb", "Horizontal Bar graph"="hb", 
#    "Line Chart graph"="lc","Scatter Plot"="sp",
#    "Stacked Bar Chart Graph"="sbc","Boxplot"="bp"),selected="vb"    
    
    if(input$rbAnalysisData=="t"){
      two <- subset(dat2m, df == 2 & year %in% input$analysisYear & region %in% input$analysisGeo & unit %in% input$analysisUnit)
    }else{
      two <- subset(dat2m, df == 1 & year %in% input$analysisYear & region %in% input$analysisGeo & unit %in% input$analysisUnit2)
      
    }
    
    
    if(input$rbAnalysis=="vb"){
      p1<- dPlot(
        x= c(input$x,"year"),
        y=input$y,
        groups=input$Groups,
        data=two,
        type="bar"
        #tooltip="function(item){return item.region +'\n' + item.value + '\n' + item.year+ '\n' +item.unit}"
      )
      p1$set(dom="analysisPlot2")
      
    }
    if(input$rbAnalysis=="hb"){
      p1<- dPlot(
        y= c(input$x,"year"),
        x=input$y,
        groups=input$Groups,
        data=two,
        type="bar")
        p1$xAxis(type="addMeasureAxis")
        p1$yAxis(type="addCategoryAxis")
        
    }
      
      if(input$rbAnalysis=="vsb"){
      p1<- dPlot(
        x<-c(input$x,"year"),
        y=input$y,
        groups=input$Groups,
        data=two,
        type="bar"
      )
      }
    if(input$rbAnalysis=="bc"){
       p1<-dPlot(x=c(input$x,"unit"),y=input$y,z="value",groups=input$Groups,type="bubble",data=two)
     p1$xAxis(type="addCategoryAxis")
     p1$yAxis(type="addCategoryAxis")
     p1$zAxis(type="addMeasureAxis")
       }
    
    if(input$rbAnalysis=="psb"){
      p1<-dPlot(
        x=c(input$x,"year"),
        y=input$y,
        groups=input$Groups,
        data=two,
        type="bar",
        name="Percentage Stacked Bar"
        )
      p1$yAxis(type="addPctAxis")
      #p1$plotOptions(bar = list(cursor = 'pointer', point = list(events = list(click = "#! function() { alert ('Category: '+ this.category +', value: '+ this.y); } !#"))))
      
    }
    
    if(input$rbAnalysis=="sp"){
      p1<-dPlot(
        y=input$y,x=input$x,groups=input$Groups,data=two, type="bubble"
      )
      p1$yAxis(type="addCategoryAxis", orderRula="Cell")
      p1$xAxis(type="addMeasureAxis")
      p1$colorAxis(
        type="addColorAxis",
        colorSries="value",
        pallette=c("red","yellow","blue")
      )
    }
    
    if(input$rbAnalysis=="bp"){
      test<-dat2m[(dat2m$region %in% input$analysisGeo) & dat2m$unit %in% input$analysisUnit,c(2,6)]
      test$region<-as.character(test$region)
      test$region<-factor(test$region)
      test$value<-as.numeric(test$value)
      
      bwstats = setNames(
        as.data.frame(boxplot(value ~ region, data = test, plot = F)$stats),
        nm = NULL
      )
      
      p1 <- Highcharts$new()
      p1$set(series = list(list(
        name = 'BoxPlot',
        data = bwstats
      )))
      
      # set xaxis/yaxis titles and labels
      p1$xAxis(
        categories = levels(test$region),
        title = list(text = 'Region Distribution')
      )
      p1$yAxis(
        title = list(text = 'value')  
      )
      p1$chart(type = 'boxplot')
    }
    

   if(input$rbAnalysis=="lc"){
     p1<- dPlot(
       value~year,
       groups="region",
       data=two,
       type="line"
     )
     
   }
#Setting Options from Analysis
  if(input$rbAxis==FALSE){}else{
           if(input$rbAnalysis=="hb"){
    p1$xAxis(overrideMin=input$Min, overrideMax=input$Max) 
  }       else{
  p1$yAxis(overrideMin=input$Min, overrideMax=input$Max)
  }
  }
        
    if(input$Animate==FALSE){}else{
      p1$set(storyboard = paste(input$Animate))
    }
    if(input$rbAnalysis=="hb"){
      p1$yAxis(orderRule=input$Sortby)
    }else{p1$xAxis(orderRule=input$Sortby) }
    p1$set(width=input$width, height=input$height)
    
      if(input$rbAnalysis=="bp"){
      p1$set(dom="analysisPlot2")
      }else{
        p1$set(dom="analysisPlot3")
      }
    if(input$rbAnalysis=="psb"){
      p1$yAxis(type="addPctAxis")
    }else{
    if(input$xtype=="n"){
      p1$xAxis(type="addMeasureAxis")    
    }else{
      p1$xAxis(type="addCategoryAxis")
    }
    
    if(input$ytype=="c"){
      p1$yAxis(type="addCategoryAxis")
    }
    else{
      p1$yAxis(type="addMeasureAxis")
    }
    }
    
    p1$legend(
      x = 60,
      y = 10,
      width = 700,
      height = 20,
      horizontalAlign = "right"
    )
    
    p1
    
  })
  
 
  
  
  
  output$test <-renderText({
  
  
    
  })
  
}