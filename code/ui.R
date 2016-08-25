library(shiny)
library(shinydashboard)
library(rCharts)
library(shinythemes)
options(RCHART_LIB= 'dimple')
options(RCHART_LIB="highcharts")

fluidPage(theme=shinytheme("united"),
  
  titlePanel(title = "Visualizations with R Shiny"),
  
  wellPanel(
    tags$style(type="text/css", '#sidebar { background: DarkGrey; width:300px; float:left;}'),
    id = "sidebar",
    
       tabsetPanel(id="menu1",
      
               tabPanel("Table", tabName = "db", icon=icon("table")),
    
               tabPanel("Map", tabName = "db2",  icon=icon("map-o")),
    
               tabPanel("Analysis", tabName = "db3", icon=icon("bar-chart"))
                 ),
       
#TABLE MENU
           conditionalPanel(
                  condition="input.menu1=='Table'",
                           
                            radioButtons("rb1", label = "Select data", 
                            choices = list("Tourism" = "tourism", "GDP Regions" = "gdpRegions"),selected = "tourism"),
                            tabsetPanel(
                                        tabPanel("GDP/T", value="gdpt", 
                                                radioButtons("rb2",label="Type of Graph", choices = list("Line"="line", "Bar"="bar"),selected="bar"),
                                                selectInput("selRegTour",label="Reg. Tourism",choices=as.list(unique(tourism1$region))), 
                                                selectInput("selUniTour", label="U. Tourism",choices=as.list(unique(tourism1$unit))),
                                               selectInput("selRegGDP",label="Reg. GDP",choices=as.list(unique(gdpRegions1$region))),
                                                selectInput("selUniGDP", label="U. GDP",choices=as.list(unique(gdpRegions1$unit))),
                                                sliderInput("slider1",label="Dates",min=1,max=8,step=1, value=c(1,8))
                                                ),
                                        tabPanel("T/T",value="tt",
                                                radioButtons("rb3",label="Type of Graph", choices = list("Line"="line", "Bar"="bar"),selected="bar"),
                                                selectInput("selRegTour2",label="Region 1",choices=as.list(unique(tourism1$region))), 
                                                selectInput("selRegTour22", label="Region 2",choices=as.list(unique(tourism1$region))),
                                                selectInput("SelUniTour2",label="Unit",choices=as.list(unique(tourism1$unit))),
                                                sliderInput("slider2",label="Dates",min=1,max=8,step=1, value=c(1,8))
                                                ),
                                        tabPanel("GDP/GDP",value="gdpgdp",
                                                radioButtons("rb4",label="Type of Graph", choices = list("Line"="line", "Bar"="bar"),selected="bar"),
                                                selectInput("selRegGDP3",label="Region GDP 1",choices=as.list(unique(gdpRegions1$region))), 
                                                selectInput("selRegGDP33", label="Region GDP 2",choices=as.list(unique(gdpRegions1$region))),
                                                selectInput("selUniGDP3",label="GDP Unit",choices=as.list(unique(gdpRegions1$unit))),
                                                sliderInput("slider3",label="Dates",min=1,max=8,step=1, value=c(1,8))
             
                                                                                  
              )
                                      
                           )),
#MAP MENU
          conditionalPanel(
                 condition="input.menu1=='Map'",
                 selectInput("dataset",label="Select data",choices = c("tourism"=1,"gdp"=2)),
                 selectInput("country",label="Select country",choices = as.list(unique(sort(countries$NUTScode)))),
                 conditionalPanel(
                   condition = "input.dataset == 1",
                   selectInput("unit","Select Unit", choices = list("Resident"="B004","Non-Resident"="B005","Total"="B006"),selected="B004")
                   
                 ),
                 conditionalPanel(
                   condition = "input.dataset == 2",
                   selectInput("unit2","Select Unit", choices=as.list(unique(gdpRegions1$unit)))
                   
                 ),
                 selectInput("division","Select Divison", choices = list("Country"="countries","Regions"="provinces"),selected="provinces"),
                 selectInput("map","Map Zoom", choices = list("Country"="Country","Europe"="Europe"),selected="provinces"),
                 conditionalPanel(
                   condition = "input.map=='Europe'",
                   selectInput("all","Display",choices=list("All Countries"=TRUE,"One Country"=FALSE),selected=FALSE)
                 ),
                 sliderInput("Year", "Year to be displayed:", 
                             min=2004, max=2011, value=2004,  step=1,
                             animate=TRUE)
                 
          ),
#ANALYSIS MENU
          conditionalPanel(
            condition="input.menu1=='Analysis'",
            tabsetPanel(
              tabPanel("Type", value= "gt",
                       radioButtons("rbAnalysis",label="Type of Graph", choices = list("Vertical Bar graph"="vb", "Horizontal Bar graph"="hb", "Vertical Stacked Bar"="vsb","Scatter Plot"="sp","Bubble Chart"="bc","Percentage Stacked Bar"="psb","Line Chart"="lc","Box Plot"="bp"),selected="vb"),
                       h4("Set up horizontal line"),
                       radioButtons("rbAxisLine",label="Select Axis", choices = list("xAxis"="x", "yAxis"="y"),selected="y"),
                       checkboxInput("analysisCheckBox", "Horizontal Line Yes/No", value=FALSE,width=NULL),
                       numericInput("horizontalLine", label="Set up value",value=1000000, min=1000000, max=50000000)
                                 
                       
                       ),
              tabPanel("Data", value="data",
                       radioButtons("rbAnalysisData",label="Select the dataset", choices = list("T"='t',"GDP"="gdp"),inline = TRUE),
                       selectInput("analysisGeo","Select Region(s)", choices = as.list(unique(tourism1$region)),selected="AT11",multiple=TRUE),
                        selectInput("analysisYear","Select Year(s)", choices = list(2004,2005,2006,2007,2008,2009,2010,2011),selected=2004,multiple=TRUE),
                       
                       conditionalPanel(
                         condition = "input.rbAnalysisData == 't'",
                         selectInput("analysisUnit","Select Unit(s)", choices = list("Non-Residents"="B005","Residents"="B004","Total"="B006"),selected="B004",multiple=TRUE)
                         
                       ),
                       conditionalPanel(
                         condition = "input.rbAnalysisData == 'gdp'",
                         selectInput("analysisUnit2","Select Unit(s)", choices = as.list(unique(gdpRegions1$unit)),selected="EUR_HAB",multiple=TRUE)
                         
                       ),
                       
                       
                      
                       selectInput("x","X",choices = list("region"="region","year"="year","unit"="unit","value"="value"),selected="region"),
                       selectInput("xtype","Type of data",choices = list("Categorical"="c","Numeric"="n"),selected="c"),
                       selectInput("y","Y",choices = list("region"="region","year"="year","unit"="unit","value"="value"),selected="value"),
                       selectInput("ytype","Type of data",choices = list("Categorical"="c","Numeric"="n"),selected="n"),
                       selectInput("Groups","Groups",choices = list("region"="region","year"="year","unit"="unit","value"="value"),selected="unit")
                       
                       
              ),
              tabPanel("Options", value="options",
                       h4("Appearence"),
                       numericInput("width","Width",min = 300,max=1600,step=50,value=800),
                       numericInput("height","Height",min=300, max=1200,step=50,value=400),
                       h4("Storyboard"),
                       selectInput("Animate","Animate",choices=list("Region"="region","Year"="year","Unit"="unit","Value"="value", "None"=FALSE),selected="None"),
                       selectInput("Sortby", "Sort by",choices=list("Region"="region","Value"="value","Year"="year","Unit"="unit", "Automatic"=FALSE)),
                       h4("Axes"),
                       radioButtons("rbAxis",label="Scaling", choices = list("Automatic"=FALSE, "Manual"=TRUE),selected="Automatic"),
                       numericInput("Max","Max",value=1000000,step=1000),
                       numericInput("Min","Min",value=0,step=1000)
                      
                       )
              
            )
          )
                   
       
),


  mainPanel(
#TABLE BODY
    tabsetPanel(
          tabPanel('Table',
            fixedPage(
              fixedRow(
              column(4,selectInput("regions","Region(s)",choices=as.list(unique(tourism1$region)),multiple = TRUE)),
              
              conditionalPanel(
                               condition="input.rb1=='tourism'",
              column(4, selectInput("unitTable","Unit(s) Tourism",choices=as.list(unique(tourism1$unit)),multiple = TRUE))                 
                               ),
              conditionalPanel(
                               condition="input.rb1=='gdpRegions'",
                               column(4,selectInput("unit2Table","Unit(s) GDP",choices=as.list(unique(gdpRegions1$unit)),multiple = TRUE))                 
                               )),
             
              fixedRow(DT::dataTableOutput('tbl2')),
              fixedRow(class="myRow2",
                       column(6,verbatimTextOutput("info1")),
                       column(6,verbatimTextOutput("info2"))
                       
                       
                       
              ),
              fixedRow( class="myRow1",
             column(4, ggiraph::ggiraphOutput('plot1')),
              column(4, ggiraph::ggiraphOutput('plot2')),
              column(4, ggiraph::ggiraphOutput('plot3')))
             )),
#MAP BODY
tabPanel('Map',
              htmlOutput("gvis"),
              htmlOutput("table")
              
                 ),
#ANALYSIS BODY
tabPanel('Analysis',
             showOutput("analysisPlot2", "dimple"),
             showOutput("analysisPlot3","highcharts")
             
                 )
      
    )
)
)
  
               
