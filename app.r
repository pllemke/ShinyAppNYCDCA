# 2011 Vegetable data from https://stateofobesity.org/lists/lowest-rates-fruits-vegetables/ These numbers represent percent of people consuming less than one per day
# http://eric.clst.org/wupl/Stuff/gz_2010_us_040_00_500k.json is the source of the shapefile geojson
# https://en.wikipedia.org/wiki/Urbanization_in_the_United_States is the source of urban and rural population
PackageLoader <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#  Then try/install packages...
PackageLoader( c("dplyr", "reshape2", "lubridate", "tidyr", "rgdal", "ggplot2", "grid", "ggpubr", "leaflet", "maps", "rgeos", "htmltools", "htmlwidgets", "shiny", "shinyjs", "data.table") )

# library(maps)



# Create Function to turn text into Proper nouns
firstCap <- function(x) { gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE) }



# access the fruit data and merge it with some arrest data. I select out the arrest data here, but still want to be able to use it in the future if needed.
plants <- data.frame(read.csv('data/Plants.csv', stringsAsFactors=FALSE)) %>% mutate(Stateabv = state.abb[match(State,state.name)], Fruit = (1-Fruits)*100, UrbanPopulation = as.numeric(UrbanPopulation),	RuralPopulation = as.numeric(RuralPopulation), Population = as.numeric(gsub(',', '', Population)))  %>% select('Stateabv','Fruit', 'UrbanPopulation', 'RuralPopulation', 'Population') %>% filter(Stateabv != 'AK' & Stateabv != 'HI')
mydata <- data.frame(state = firstCap(rownames(USArrests)), USArrests) %>% mutate(Stateabv = state.abb[match(state,state.name)]) %>% filter(Stateabv != 'AK' & Stateabv != 'HI')
mydata <- inner_join(mydata, plants, by='Stateabv') %>% mutate(Population = Population/1000000) %>% select('Stateabv', 'state', 'Fruit', 'Population', 'UrbanPopulation', 'RuralPopulation')

# access the UFO data and rebucket the shape types.
UFO_All <- data.frame(read.csv("data/UFO.csv", stringsAsFactors=FALSE)) %>% mutate(Year=year(as.Date(datetime, format = "%d/%m/%Y")), Stateabv=toupper(state)) %>%
  mutate(Shape = replace(shape, shape %in% c('', ' ', 'unknown', 'other', 'changing', 'formation'), 'Other')) %>%
  mutate(Shape = replace(Shape, Shape %in% c('light', 'fireball', 'flash'), 'Lights')) %>% 
  mutate(Shape = replace(Shape, Shape %in% c('rectangle', 'diamond', 'triangle', 'chevron', 'cross'), 'Edged')) %>% 
  mutate(Shape = replace(Shape, Shape %in% c('circle', 'sphere', 'oval', 'cylinder', 'cigar', 'egg', 'teardrop', 'cone', 'disk'), 'Rounded')) %>% 
  filter(country == 'us' & Year == 2011 & Stateabv != 'AK' & Stateabv != 'HI') %>% mutate(seconds = as.numeric(duration_seconds), latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>% select('Stateabv', 'Shape', 'seconds', 'latitude', 'longitude')

# pivot and aggregate the UFO data to add it to the fruit data.
UFOPivot_All <- UFO_All %>% group_by(Stateabv) %>% summarize(Total=n()) %>% select('Stateabv', 'Total')
UFOPivot_shape <- UFO_All %>% group_by(Stateabv, Shape) %>% summarize(Shape_Total=n()) %>% dcast(Stateabv ~ Shape, value.var = 'Shape_Total', fill=0) # %>% mutate_all(funs(replace(., is.na(.), 0)))
UFOPivot_All <- inner_join(UFOPivot_All, UFOPivot_shape, by='Stateabv') %>% mutate(Edged = round(Edged/Total, 3), Lights = round(Lights/Total, 3), Other = round(Other/Total, 3), Rounded = round(Rounded/Total, 3))

mydata <- inner_join(mydata, UFOPivot_All, by='Stateabv') %>% mutate(Sightings_per_MM = Total/Population)
# Make Terciles for the bivariate choropleth
f.v <- quantile(mydata$Fruit, c(0.33, 0.66, 1))
p.v <- quantile(mydata$Sightings_per_MM, c(0.33, 0.66, 1))
mydata <- mydata %>% mutate(p = ifelse(Sightings_per_MM<p.v[1], 'pa', ifelse(Sightings_per_MM<p.v[2], 'pb', 'pc')), f = ifelse(Fruit<f.v[1], 'fa', ifelse(Fruit<f.v[2], 'fb', 'fc'))) %>% mutate(BINS_letters = paste(p, f, sep = '_')) %>%
  mutate(GRID = case_when(BINS_letters == 'pa_fa' ~ 1, BINS_letters == 'pa_fb' ~ 2, BINS_letters == 'pa_fc' ~ 3, BINS_letters == 'pb_fa' ~ 4, BINS_letters == 'pb_fb' ~ 5, BINS_letters == 'pb_fc' ~ 6, BINS_letters == 'pc_fa' ~ 7, BINS_letters == 'pc_fb' ~ 8, BINS_letters == 'pc_fc' ~ 9)) %>%
  mutate(FruitRank = case_when(f == 'fa' ~ '(Low)', f == 'fb' ~ '(Medium)', f == 'fc' ~ '(High)'), SightingsRank = case_when(p == 'pa' ~ '(Low)', p == 'pb' ~ '(Medium)', p == 'pc' ~ '(High)'))

# bucket rural population as above/below average
rur <- quantile(mydata$RuralPopulation, c(0.33, 0.66, 1))
mydata <- mydata %>% mutate(RuralComparison = ifelse(RuralPopulation<rur[1], 'Low', ifelse(RuralPopulation<rur[2], 'Medium', 'High')))
# Get Shape file
url <- "http://eric.clst.org/wupl/Stuff/gz_2010_us_040_00_500k.json"
fil <- "gz_2010_us_040_00_500k.json"
if (!file.exists(fil)) download.file(url, fil, cacheOK=TRUE)
states_m <- readOGR("gz_2010_us_040_00_500k.json")
states_m <- subset(states_m, !NAME %in% c("Alaska", "Hawaii", "Puerto Rico", "District of Columbia")) # filter out anything which isn't the lower 48

# merge the fruit and UFO data into the shape file
states_m@data$rn <- row.names(states_m) # save the rownames as an explicit variable to use for reattachment. This is needed because we must preserve the row order.
states_data_temp <- data.table(states_m@data, stringsAsFactors = FALSE) # create a temporary table of the shapefile's data attribute
mydata_temp <- data.table(mydata, stringsAsFactors = FALSE)
setkey(states_data_temp, NAME) # set the state NAME as the key in the temporary data table
setkey(mydata_temp, state) # set the state column as the key in the data to be added. this will allow the tables to be merged based on state name.
states_data_temp <- mydata_temp[states_data_temp] # add in the new columns
states_data_temp <- data.table(states_data_temp %>% mutate(SightingsPerMile = Sightings_per_MM/CENSUSAREA))

setkey(states_data_temp, rn) # set the key in the updated table
states_m@data <- states_data_temp[row.names(states_m)] # attach the new data to the shape file

# Convert some data from long to wide for the Shape box Plots; reorder by mean
melted_data <- gather(data.frame(states_m@data %>% select('state', 'Other', 'Lights', 'Edged', 'Rounded')), Shape, Percent, Other:Rounded, factor_key=TRUE)
melted_data$Shape <- with(melted_data, reorder(Shape, Percent, mean))

# Colors for map
PALLETTE <- c("#e8e8e8","#ace4e4","#5ac8c8","#dfb0d6", "#a5add3", "#5698b9", "#be64ac", "#8c62aa", "#3b4994")
BINS_number <- c(0,1.4,2.4,3.4,4.4,5.4,6.4,7.4,8.4, Inf)
pal <- colorBin(PALLETTE, domain=states_m$GRID, bins=BINS_number)
states_m@data$RuralComparison <- factor(states_m@data$RuralComparison, levels=c("High", "Medium", "Low"), labels=c("High", "Medium", "Low"))

# some nice extras for the reactive plots
Fruitbp_grob <- ggplotGrob(ggboxplot(states_m@data$Fruit, width = 0.5, fill = "transparent", outlier.shape=NA) + rotate() + theme_transparent() + theme(plot.margin=unit(c(0,0,0,0), "cm"))); xmin <- min(data.frame(states_m@data$Fruit)); xmax <- max(data.frame(states_m@data$Fruit)) 
Sightingsbp_grob <- ggplotGrob(ggboxplot(states_m@data$Sightings_per_MM, width = 0.5, fill = "transparent", outlier.shape=NA) + theme_transparent() + theme(plot.margin=unit(c(0,0,0,0), "cm"))); ymin <- min(states_m@data$Sightings_per_MM); ymax <- max(states_m@data$Sightings_per_MM)
Fruitplot_grob <- ggplotGrob(ggdensity(states_m@data, "Fruit", fill = "RuralComparison") + clean_theme() + theme(legend.position="none", plot.margin=unit(c(0,0,-1,0), "cm")) + theme(plot.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()))
Sightingsplot_grob <- ggplotGrob(ggdensity(states_m@data, "Sightings_per_MM", fill = "RuralComparison") + annotate("text", x = 14.5, y = 0.14, label = "Rural Population\nDistribution", angle = -90, size=3) + rotate() + clean_theme() + theme(legend.position="none", plot.margin=unit(c(0,0,0,-1), "cm")) + theme(plot.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()))
shape_grob <- ggplotGrob(ggdensity(data=melted_data, "Percent", fill = "Shape", alpha=0.6) + rotate() + annotate("text", x = 0.6, y = 3.3, label = "Shape Distribution\n(USA Total)", angle = -90, size=3) + clean_theme() + theme(legend.position="none", plot.margin=unit(c(0,0,-1,0), "cm")) + theme(plot.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()))


# Build fancy hover popup
states_m@data$hoverText <- mapply(
  function(st, Fruit, FruitRank, sightings, SightingsRank, urban, rural, RuralComparison) {
    htmltools::HTML(
      sprintf(
        "<div style='font-size:12px;width:200px;padding-left: 5px; padding-right: 5px; padding-top: 5px; padding-bottom: 5px;background-color: rgba(255,255,255,0.75);;float:left'>
        <span style='font-size:22px;opacity: 1;font-weight:bold'>%s</span><br/>
        <span style='font-size:12px;opacity: 1;font-weight:bold'>  %s eat fruit daily %s</span><br/>
        <span style='font-size:12px;opacity: 1;font-weight:bold'>  %s sightings/MM People %s</span><br/>
        <span style='font-size:16px;opacity: 1;font-weight:bold;float:left'>Population Makeup:</span><br/>
        <span style='font-size:14px;opacity: 1;font-weight:bold;float:left'>Urban </span>
        <span style='font-size:14px;opacity: 1;font-weight:bold;float:right'>Rural </span><br/>
        <span style='color:#2aa1ec;opacity: 1;float:left'>%s%%</span>
        <span style='color:#fe6a59;opacity: 1;float:right'>%s%%</span><br clear='all'/>
        <span style='background:#2aa1ec;opacity: 1;width:%s%%;float:left'>&nbsp;</span>
        <span style='background:#fe6a59;opacity: 1;width:%s%%;float:right'>&nbsp;</span><br/>
        <span style='font-size:12px;opacity: 1;font-weight:bold'>%s rural percentage.</span><br/>
        </div>",
        st,
        Fruit, FruitRank,
        sightings, SightingsRank,
        urban, rural,
        urban, rural,
        RuralComparison
        
      )
    )
  },
  states_m@data$state, # st
  paste(states_m@data$Fruit, '%'), states_m@data$FruitRank, # fruit
  format(round(states_m@data$Sightings_per_MM, 0), nsmall = 0), states_m@data$SightingsRank, #sightings
  states_m@data$UrbanPopulation, #urban
  states_m@data$RuralPopulation, #rural
  states_m@data$RuralComparison, # Rural Comparison
  SIMPLIFY = F) %>%  set_names(states_m@data$state)

# javascript to make map full screen
jscode <- '$(document).on("shiny:connected", function(e) {
              var jsHeight = window.innerHeight;
              Shiny.onInputChange("GetScreenHeight",jsHeight);
              });'

server <- function(input, output, session) {
  # Build the Map
  output$map <- renderLeaflet({
    leaflet(data=states_m) %>% setView(-96, 37.8, 4) %>% addProviderTiles("Hydda.Base") %>% addTerminator() %>%
      addPolygons(data=states_m, layerId=states_m@data$state, stroke=TRUE, weight=2, opacity=0.6, dashArray = "3", fill=TRUE, fillColor = ~pal(GRID), fillOpacity = 0.8, smoothFactor=0.5, highlight = highlightOptions(weight = 10, color = "#666", dashArray = "4",  fillOpacity = 1,  bringToFront = FALSE), label = ~hoverText,
              labelOptions = labelOptions(
                style = list(
                  'background'='rgba(255,255,255,0.95)',
                  'border-color' = 'rgba(0,0,0,1)',
                  'border-radius' = '4px',
                  'border-style' = 'solid',
                  'border-width' = '4px'),
                textsize = "15px",
                direction = "auto")) %>% 
      htmlwidgets::onRender("function(el, t) {var myMap = this; myMap._container.style['background'] = '#ffffff'; }") %>%
      addCircles(data=UFO_All, lng=~longitude, lat=~latitude, radius=5000, color="black", weight=3, opacity=1, fillColor="black", fillOpacity=2)
  })

# Use the full screen rebuild of the leaflet map
    output$fullleaflet <- renderUI({
    if(!is.null(input$GetScreenHeight)){
      width  <- session$clientData$output_image1_width
      print(session$clientData)
      height <- session$clientData$output_image1_height
      leafletOutput("map", width = "100%", height = (input$GetScreenHeight)*0.9)
    }
  })

    
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedShape=NULL)
  # store the click
  observeEvent(input$map_shape_click,{
    data_of_click$clickedshape <- input$map_shape_click$id
    print(data_of_click$clickedshape)
  })

 
    
# Build Map Legend
  output$legend <- renderPlot({ggplot(data.frame(states_m@data), aes(x=f,y=p,fill=factor(GRID), alpha=0.9,width=1, height=1)) + 
    geom_tile() +
    scale_fill_manual(values = PALLETTE) +
    theme_void() + 
    theme(legend.position="none", panel.background=element_blank(), panel.border=element_blank(), plot.margin=margin(t=0, r=0, b=, l=0, unit = "pt"), plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(plot.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) +
    geom_segment(aes(x=0.5, xend = 3.5 , y=0.5, yend = 0.5), size=2, arrow = arrow(length = unit(0.3,"cm"))) +
    geom_segment(aes(x=0.5, xend = 0.5 , y=0.5, yend = 3.5), size=2, arrow = arrow(length = unit(0.3,"cm"))) +
    coord_fixed(ratio=1)
})
# Build Scatter Plot
  output$scatter <- renderPlot({ 
    clicked_state <- data_of_click$clickedshape
    ggplot(data=data.frame(states_m@data), aes(x=Fruit, y=Sightings_per_MM, color=RuralComparison)) + guides(color=guide_legend(title="Rural Population:"))+theme(legend.direction="horizontal", legend.position="bottom", legend.title=element_text(size=12, face='bold'), legend.text=element_text(size=10, face='bold'), legend.key=element_rect(color='white', fill='white')) +
      annotation_custom(grob = Fruitbp_grob, xmin = xmin, xmax = xmax, ymin = ymin-1.75, ymax = ymin) +
      annotation_custom(grob = Sightingsbp_grob, xmin = xmin-1.75, xmax = xmin, ymin = ymin, ymax = ymax) +
      annotation_custom(grob = Fruitplot_grob, xmin = xmin-1.25, xmax = xmax+1.25, ymin = ymax+5, ymax = ymax+5) +
      annotation_custom(grob = Sightingsplot_grob, xmin = xmax+5, xmax = xmax+5, ymin = ymax+1.25, ymax = ymin-1.25) +
    geom_point(alpha=1, size = 2, position = position_jitter(width=0, height=0)) +
      geom_point(data=subset(states_m@data, states_m@data$state == clicked_state), aes(x = Fruit, y=Sightings_per_MM), alpha=1, size = 7, shape=23, position = position_jitter(width=0, height=0), color='black', fill = 'red') +
    geom_smooth(method = "lm", color = "black", alpha=0.5, size = 1, se=FALSE) +
    coord_fixed() +
      geom_text(data=subset(states_m@data, states_m@data$state == clicked_state), aes(x = Fruit, y=Sightings_per_MM, label=state), vjust="inward", hjust="inward", position = position_dodge(0.5), size = 5, color='black', fontface = "bold") + 
    theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) +
    theme(axis.title.x=element_text(size=15, face='bold'), axis.title.y=element_text(size=15, face='bold'), axis.text=element_text(size=15, face='bold')) +
     theme(plot.background = element_rect(size = 5), panel.background=element_blank(), plot.margin=margin(t=60, r=40, b=0, l=0, unit = "pt")) +
      labs(x = "People Eating Fruit Daily", y="UFOs / Million People")
      
  })
# render click text
output$info <- renderText({paste0(input$map_shape_click$id)
  })
# Build Box Plot
  output$box <- renderPlot({ 
    clicked_state <- data_of_click$clickedshape
    ggplot(data=melted_data, aes(x=Shape,y=Percent*100, fill = Shape, color=Shape)) + 
      theme(axis.text.x=element_text(angle=45, hjust = 1, size=15, face="bold"), axis.text.y=element_text(size=15, face="bold"), axis.title=element_text(size=15, face='bold'), plot.background = element_blank(), panel.background=element_blank(), legend.position="") + 
      labs(x = "UFO Shape", y=paste0(input$map_shape_click$id," Sightings % \n", ifelse(input$map_shape_click$id == "", "", "vs. Rest of US"))) +
      geom_boxplot(width = 0.5, fill = "transparent", outlier.stroke=1, outlier.alpha=1, alpha=1, outlier.size=1, outlier.shape = 1, lwd = 1) +
      geom_bar(data=subset(melted_data, melted_data$state == clicked_state), aes(x=Shape,y=Percent*100, fill = Shape), stat = "summary", fun.y = "median", alpha=0.4, width=0.75) +
      annotation_custom(grob = shape_grob, xmin = 4.75, xmax = 7, ymin = 5, ymax = 85) +
      theme(plot.margin=margin(t=0, r=75, b=0, l=0, unit = "pt")) +
      coord_fixed(ratio=0.065)
  })
# Build MyInfo leaflet
  output$Myinfoleaflet <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 9, maxZoom = 9, dragging = FALSE)) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addProviderTiles(providers$Stamen.Toner, options = providerTileOptions(opacity = 0.4)) %>%
      setView(-95.36, 29.76, 9) %>%
      addCircles(data=UFO_All, lng=~longitude, lat=~latitude, radius=2000, color="black", weight=2, opacity=0.5, fillColor="black", fillOpacity=2)
  })
# Build MyInfo leaflet to fill screen
  output$Myinfofullleaflet <- renderUI({
    if(!is.null(input$GetScreenHeight)){
      width  <- session$clientData$output_image1_width
      print(session$clientData)
      height <- session$clientData$output_image1_height
      leafletOutput("Myinfoleaflet", width = "100%", height = (input$GetScreenHeight)*0.9)
    }
  })
  

}


ui <- fluidPage(
  navbarPage("UFOs vs. Fruit Eaters", id="nav",
             
             tabPanel("Sightings Map",
  
  tags$script(jscode),
  uiOutput("fullleaflet"),
  absolutePanel(tags$h3("Click a State to Update Plots", style = "background-color: #FFFFFF;border: 3px solid #FFFFFF;border-radius: 10px;padding: 0px;height: 27px;margin: auto;text-align: center;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;"),
                tags$h5("Points Indicate UFO Sighting Location. All Data From 2011", style = "background-color: #FFFFFF;border: 3px solid #FFFFFF;border-radius: 10px;padding: 0px;height: 20px;margin: auto;text-align: center;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;"),
                class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 10, left = 0, right = 0, bottom = 600,
                width = 450, height = 50, style = "padding-top: 2px;padding-right: 2px;padding-bottom: 2px;padding-left: 2px;opacity: 0.9;margin: auto;background-color: rgba(250,250,250);border: 0px solid;"
                ),
  absolutePanel(tags$h3("State Color Key", style = "border: 3px solid #FFFFFF;background-color:#FFFFFF;border-radius: 10px;height: 30px;width: 260px;text-align: center;margin-top: 5px;margin-bottom: 5px;margin-right: 5px;margin-left: 0px;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;"), 
                tags$h4("UFOs per Million People", style = "border: 3px solid #FFFFFF;background-color:#FFFFFF;border-radius: 10px;text-align: center;margin: 1px;height: 27px;padding: 1px;width: 260px;position: absolute;left: -150px;top: 155px;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;-ms-transform:rotate(-90deg);-moz-transform:rotate(-90deg);-webkit-transform:rotate(-90deg);-o-transform:rotate(-90deg);"),
                tags$div(plotOutput("legend", height = 250, width = 250), class="container", style = "background-color: #FFFFFF;height: 260px;width: 260px;padding: 0px;margin: 0px;opacity: 1;border: 5px solid #FFFFFF;border-radius: 10px;"), 
                tags$h4("People Eating Fruit Daily", style = "background-color: #FFFFFF;border: 3px solid #FFFFFF;border-radius: 10px;text-align: center;margin-top: 5px;margin-bottom: 5px;margin-right: 5px;margin-left: 1px;padding: 1px;height: 27px;width: 260px;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;"),
                class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = "auto", left = 80, right = "auto", bottom = 90,
                width = 260, height = 260, style = "background-color: rgba(250,250,250,0);opacity: 0.9;border: 0px solid;"
                ),
  absolutePanel(tags$h3("% Sightings by UFO Shape", style="margin-bottom: 5px;border: 3px solid #FFFFFF;background-color:#FFFFFF;border-radius: 10px;height: 30px;width: 355px;text-align: center;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;"),
                tags$div(plotOutput("box", height = 280, width = 350), style ="align-items: center;height: 285px;width: 355px;border: 3px solid #FFFFFF;background-color:#FFFFFF;border-radius: 10px;margin: 0px;padding: 0px;"), 
                class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = "auto", left = "auto", right = 40, bottom = 440,
                width = 360, height = 290, style = "background-color: rgba(250,250,250,0);margin: auto;opacity: 0.9;border: 0px solid;"
  ),
  absolutePanel(tags$h3("UFOs vs. Fruit Eaters", style="margin-bottom: 5px;border: 3px solid #FFFFFF;background-color:#FFFFFF;border-radius: 10px;height: 30px;width: 355px;text-align: center;font-weight:bold;text-shadow: -1px 0 #000000,0 1px #000000,1px 0 #000000,0 -1px #000000;"),
                tags$div(plotOutput("scatter", height = 280, width = 350), style ="height: 285px;width: 355px;border: 3px solid #FFFFFF;background-color:#FFFFFF;border-radius: 10px;margin-top: 0px;margin-bottom: 0px;margin-right: 0px;margin-left: 0px;"), 
                class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = "auto", left = "auto", right = 40, bottom = 80,
                width = 360, height = 290, style = "background-color: rgba(250,250,250,0);opacity: 0.9;border: 0px solid;"
                )
    
                          
      )
             
            
           
  
           
  )
  )
  

    
shinyApp(ui = ui, server = server, options = list(launch.browser=TRUE))


