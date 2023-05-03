library(shiny)
library(csasAtlPhys)
data("station2PlotLimits")
library(oce)
library(ocedata)
data("ctd")
ghostCtd <- ctd
fakeYear <- 2020
# load a function for gridding the climatology
source('./functions/gridClimatologyDailyFn.R') 

# 1. load data
load('ctd.rda')

# 2. load climatology and format it 
load('./climatologyData/sgscatter1991to2020Climatology.rda')

station2Climatology <- dailyClimatologyDf
# have to 'fudge' the month to do a monthly average and other calculations below
# 2020 is ok to use, not a leap year, so should be fine
fakeDate <- seq(as.POSIXct(paste(fakeYear, '01-01', sep = '-'), tz = 'UTC'), 
                as.POSIXct(paste(fakeYear, '12-31', sep = '-'), tz = 'UTC'), by = 'day')
fakeDateDf <- data.frame(date = fakeDate, 
                         month = as.POSIXlt(fakeDate)$mon + 1,
                         yearDay = as.POSIXlt(fakeDate)$yday + 1)
# merge it together to make it easy to use
mclimdf <- merge(station2Climatology, fakeDateDf)
mclimdf <- mclimdf[with(mclimdf, order(yearDay, depth)), ]

# create CTD objects for easy plotting of section
uyday <- unique(mclimdf[['yearDay']])
ctdClimatology <- vector(mode = 'list', length = length(uyday))
for(iy in 1:length(uyday)){
  ok <- mclimdf[['yearDay']] %in% uyday[iy]
  cctd <- as.ctd(salinity = mclimdf[['salinity']][ok],
                temperature = mclimdf[['temperature']][ok],
                pressure = as.numeric(paste(mclimdf[['depth']][ok])),
                startTime = mclimdf[['date']][ok][1])
  # add sigmaThetaCalculated
  cctd <- oceSetData(cctd,
                    name = 'sigmaThetaCalculated',
                    value = mclimdf[['sigmaTheta']][ok])
  ctdClimatology[[iy]] <- cctd
}
sg <- gridClimatologyDaily(ctdClimatology)
# a bit redudant, but get it for use below for setting plot limits
climatologyStartTime <- as.POSIXct(unlist(lapply(ctdClimatology, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')

# set up various plotting parameters for climatology section plot
xlim <- c(as.POSIXct(paste(fakeYear, '01', '01', sep = '-'), tz = 'UTC'),
          as.POSIXct(paste(fakeYear, '12', '31', sep = '-'), tz = 'UTC'))
ylim <- c(150, 0)

# 3. get some useful information out of the ctd objects to use in the app
# function for making it easier to subset objects to a specific month-day
getFakeTime <- function(x, fakeYear){ 
  as.POSIXct(paste(fakeYear, format(x, '%m-%d %H:%M:%S'), sep = '-'), origin = '1970-01-01', tz = 'UTC')
}
startTime <- as.POSIXct(unlist(lapply(ctd, '[[', 'startTime')), origin = '1970-01-01', tz = 'UTC')
years <- as.POSIXlt(startTime)$year + 1900
fakeStartTime <- getFakeTime(x = startTime, fakeYear = fakeYear)

# 4. define some things for initializing reactive state variable
initialDeploymentStart <- Sys.Date() - 2
initialDeploymentEnd <- Sys.Date() + 2
initialRecoveryStart <- Sys.Date() + 19
initialRecoveryEnd <- Sys.Date() + 23
initialCtdYearRange <- c(as.numeric(format(Sys.Date(), '%Y')) - 2,
                         as.numeric(format(Sys.Date(), '%Y')))
deploymentFakeTime <- getFakeTime(x = c(initialDeploymentStart,
                                        initialDeploymentEnd),
                                  fakeYear = fakeYear)
initialDeploymentCtd <- ctd[fakeStartTime %in% deploymentFakeTime[1]:deploymentFakeTime[2]]
recoveryFakeTime <- getFakeTime(x = c(initialRecoveryStart,
                                      initialRecoveryEnd),
                                fakeYear = fakeYear)
initialRecoveryCtd <- ctd[fakeStartTime %in% recoveryFakeTime[1]:recoveryFakeTime[2] & years %in% initialCtdYearRange]
initialClimatologyCtdDep <- ctdClimatology[climatologyStartTime %in% recoveryFakeTime[1]:recoveryFakeTime[2]]
initialClimatologyCtdRec <- ctdClimatology[climatologyStartTime %in% deploymentFakeTime[1]:deploymentFakeTime[2]]

allInitialCtd <- c(initialDeploymentCtd, 
                   initialClimatologyCtdDep,
                   initialRecoveryCtd,
                   initialClimatologyCtdRec)
initialTlim <- range(unlist(lapply(allInitialCtd, '[[', 'temperature')), na.rm = TRUE)
initialSlim <- range(unlist(lapply(allInitialCtd, '[[', 'salinity')), na.rm = TRUE)
initialSTlim <- range(unlist(lapply(allInitialCtd, '[[', 'sigmaTheta')), na.rm = TRUE)
initialplim <- range(unlist(lapply(allInitialCtd, '[[', 'pressure')), na.rm = TRUE)

initialSectionPlotVariable <- 'sigmaTheta'

# define some plot variables
col.temperature <- 'darkred'
col.temperature2 <- 'red'
col.salinity <- 'darkgreen'
col.salinity2 <- 'green'
col.sigmaTheta <- 'darkblue'
col.sigmaTheta2 <- 'blue'
col.grid <- 'lightgrey'
lty.grid <- 'dotted'
axisNameLoc <- 2 # par('mgp')[1]
mar <- c(3, 3.5, 3.5, 2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('Glider Ballasting for Halifax line'),
  fluidRow(
    column(3, wellPanel(
      dateRangeInput(inputId = 'deploymentDateRange',
                     start = initialDeploymentStart, end = initialDeploymentEnd,
                     label = '1. Choose a deployment date range'
      ),
      dateRangeInput(inputId = 'recoveryDateRange',
                     start = initialRecoveryStart, end = initialRecoveryEnd,
                     label = '2. Choose a recovery date range'
      ),
      selectInput(inputId = 'sectionPlotVariable',
                  choices = c('temperature',
                              'salinity',
                              'sigmaTheta'),
                  selected = initialSectionPlotVariable,
                  label = '3. Choose variable for section plot'
                  ),
      sliderInput(inputId = 'ctdYearRange',
                  min = 1999, # constrain it to this - beginning of real sampling
                  max = max(years, na.rm = TRUE),
                  value = initialCtdYearRange,
                  step = 1,
                  label = '4. Choose year range of profiles to show'),
      h5(strong('5. Show most recent profile ?')) ,
      checkboxInput(inputId = 'showRecentProfile',
                    label = ''),
      uiOutput(outputId = 'recentProfileDate')
      
    ) # closes wellPanel
    ), # closes column
    column(9, 
           fluidRow(
             column(6, align = 'center',
                    h4(strong('Deployment'))),
             column(6, align = 'center',
                    h4(strong('Recovery')))
           ),
           fluidRow(
             column(3,
                    textOutput(outputId = 'TSDepText')
                    ),
             column(3,
                    htmlOutput(outputId = 'sigmaThetaDepText') # html to handle superscript
                    ),
             column(3,
                    textOutput(outputId = 'TSRecText')
                    ),
             column(3,
                    htmlOutput(outputId = 'sigmaThetaRecText') # html to handle superscript
                    )
            ), # closes first fluid row in column 9
           # first row - text and plots
           fluidRow(
             column(3,
                  plotOutput(outputId = 'TSDep',
                             dblclick = 'resetLimits',
                             brush = brushOpts(id = "TSDepPlotBrush",
                                               direction = "xy",
                                               resetOnNew = TRUE)
                  )
           ), # closes column for TSDep
           column(3, 
                  plotOutput(outputId = 'sigmaThetaDep',
                             dblclick = 'resetLimits',
                             brush = brushOpts(id = "STDepPlotBrush",
                                               direction = "xy",
                                               resetOnNew = TRUE))
           ), # closes column for sigmaThetaDep
           column(3,
                  plotOutput(outputId = 'TSRec',
                             dblclick = 'resetLimits',
                             brush = brushOpts(id = "TSRecPlotBrush",
                                               direction = "xy",
                                               resetOnNew = TRUE))
           ), # closes column for TSRec
           column(3,
                  plotOutput(outputId = 'sigmaThetaRec',
                             dblclick = 'resetLimits',
                             brush = brushOpts(id = "STRecPlotBrush",
                                               direction = "xy",
                                               resetOnNew = TRUE))
           ) # closes column for sigmaThetaRec
           ), # closes fluid row for second column 
           # second row - section plot of climatology
           fluidRow(
             column(12, align = 'center',
                    h4(strong('Climatology section plot')))
           ),
           fluidRow(
           column(12,
                  plotOutput(outputId = 'climatologySection')
           ) # closes column for climatologySection
    ) # closes fluidRow for third row in column 9
    ) # closes column 9
  )# closes fluidRow
)

server <- function(input, output) {
  state <- reactiveValues(deploymentCtd = initialDeploymentCtd,
                          deploymentCtdClimatology = initialClimatologyCtdDep,
                          recoveryCtd = initialRecoveryCtd,
                          recoveryCtdClimatology = initialClimatologyCtdRec,
                          yearRange = initialCtdYearRange,
                          deploymentDateRange = c(initialDeploymentStart,
                                                  initialDeploymentEnd),
                          recoveryDateRange = c(initialRecoveryStart,
                                                initialRecoveryEnd),
                          Tlim = initialTlim,
                          Slim = initialSlim,
                          STlim = initialSTlim,
                          plim = initialplim,
                          sectionPlotVariable = initialSectionPlotVariable,
                          recentProfile = NULL)
  # update deployment ctd, climatology, and plot limits when deployment range adjusted
  observeEvent(input$deploymentDateRange,{
    state$deploymentDateRange <- input$deploymentDateRange
    # cat(paste('deployment date range', paste(state$deploymentDateRange, collapse = ' to ')), sep = '\n')
    # cat(paste('year range', paste(state$yearRange, collapse = ' to ')), sep = '\n')
    fakeDeploymentDate <- getFakeTime(x = input$deploymentDateRange, 
                                      fakeYear = fakeYear)
    state$deploymentCtd <- ctd[fakeStartTime >= fakeDeploymentDate[1] & fakeStartTime <= fakeDeploymentDate[2] & years %in% state$yearRange[1]:state$yearRange[2]]
    state$deploymentCtdClimatology <- ctdClimatology[climatologyStartTime >= fakeDeploymentDate[1] & climatologyStartTime <= fakeDeploymentDate[2]]
    # cat(paste('found', length(state$deploymentCtd), 'ctd after updating date range'), sep = '\n')
    allPlotCtd <- c(state$deploymentCtd,
                    state$deploymentCtdClimatology,
                    state$recoveryCtd,
                    state$recoveryCtdClimatology)
    state$Tlim <- range(unlist(lapply(allPlotCtd, '[[', 'temperature')), na.rm = TRUE)
    state$Slim <- range(unlist(lapply(allPlotCtd, '[[', 'salinity')), na.rm = TRUE)
    state$STlim <- range(unlist(lapply(allPlotCtd, '[[', 'sigmaTheta')), na.rm = TRUE)
    state$plim <- range(unlist(lapply(allPlotCtd, '[[', 'pressure')), na.rm = TRUE)
    })
  # update recovery ctd when deployment range adjusted
  observeEvent(input$recoveryDateRange,{
    state$recoveryDateRange <- input$recoveryDateRange
    fakeRecoveryDate <- getFakeTime(x = input$recoveryDateRange,
                                    fakeYear = fakeYear)
    state$recoveryCtd <- ctd[fakeStartTime %in% fakeRecoveryDate[1]:fakeRecoveryDate[2] & years %in% state$yearRange[1]:state$yearRange[2]]
    state$recoveryCtdClimatology <- ctdClimatology[climatologyStartTime >= fakeRecoveryDate[1] & climatologyStartTime <= fakeRecoveryDate[2]]
    allPlotCtd <- c(state$deploymentCtd,
                    state$deploymentCtdClimatology,
                    state$recoveryCtd,
                    state$recoveryCtdClimatology)
    state$Tlim <- range(unlist(lapply(allPlotCtd, '[[', 'temperature')), na.rm = TRUE)
    state$Slim <- range(unlist(lapply(allPlotCtd, '[[', 'salinity')), na.rm = TRUE)
    state$STlim <- range(unlist(lapply(allPlotCtd, '[[', 'sigmaTheta')), na.rm = TRUE)
    state$plim <- range(unlist(lapply(allPlotCtd, '[[', 'pressure')), na.rm = TRUE)
  })
  # update deployment and recovery ctd when year range adjusted
  observeEvent(input$ctdYearRange, {
    state$yearRange <- input$ctdYearRange
    #cat(paste('deployment date range', paste(state$deploymentDateRange, collapse = ' to ')), sep = '\n')
    #cat(paste('year range', paste(state$yearRange, collapse = ' to ')), sep = '\n')
    fakeDeploymentDate <- getFakeTime(x = state$deploymentDateRange,
                                      fakeYear = fakeYear)
    state$deploymentCtd <- ctd[fakeStartTime >= fakeDeploymentDate[1] & fakeStartTime <= fakeDeploymentDate[2] & years %in% input$ctdYearRange[1]:input$ctdYearRange[2]]
    #cat(paste('found', length(state$deploymentCtd), 'ctd after updating year range'), sep = '\n')
    fakeRecoveryDate <- getFakeTime(x = state$recoveryDateRange,
                                    fakeYear = fakeYear)
    state$recoveryCtd <- ctd[fakeStartTime %in% fakeRecoveryDate[1]:fakeRecoveryDate[2] & years %in% input$ctdYearRange[1]:input$ctdYearRange[2]]
    allPlotCtd <- c(state$deploymentCtd,
                    state$deploymentCtdClimatology,
                    state$recoveryCtd,
                    state$recoveryCtdClimatology)
    state$Tlim <- range(unlist(lapply(allPlotCtd, '[[', 'temperature')), na.rm = TRUE)
    state$Slim <- range(unlist(lapply(allPlotCtd, '[[', 'salinity')), na.rm = TRUE)
    state$STlim <- range(unlist(lapply(allPlotCtd, '[[', 'sigmaTheta')), na.rm = TRUE)
    state$plim <- range(unlist(lapply(allPlotCtd, '[[', 'pressure')), na.rm = TRUE)
  })
  # update section plot variable
  observeEvent(input$sectionPlotVariable, {
    state$sectionPlotVariable <- input$sectionPlotVariable
  })
  # checkbox to show most recent profile
  observeEvent(input$showRecentProfile, {
    if(input$showRecentProfile){
      ok <- which.min(Sys.time() - startTime)
      state$recentProfile <- ctd[[ok]]
    } else {
      state$recentProfile <- NULL
    }
    if(input$showRecentProfile){
      output$recentProfileDate <- renderText(paste('The most recent profile was taken on', format(ctd[[ok]][['startTime']], '%Y-%m-%d'))) 
    } else {
      output$recentProfileDate <- renderText('')
    }
    
  })
  
  # plot and text output
  output$TSDepText <- renderText({
    paste('The climatological near surface (p <= 5m) average temperature is',
          sprintf('%.2f', mean(unlist(lapply(state$deploymentCtdClimatology, function(k) {sub <- subset(k, pressure <= 5) ; sub[['temperature']]})), na.rm = TRUE)),
          '\u00B0C', # degree C
          'and salinity is',
          sprintf('%.2f', mean(unlist(lapply(state$deploymentCtdClimatology, function(k) {sub <- subset(k, pressure <= 5) ; sub[['salinity']]})), na.rm = TRUE)),
          '.',
          ifelse(!is.null(state$recentProfile),
                 paste('The most recent profile near surface (p <= 5m) average temperature is',
                       sprintf('%.2f', mean(unlist(lapply(list(state$recentProfile), function(k) {sub <- subset(k, pressure <= 5) ; sub[['temperature']]})), na.rm = TRUE)),
                       '\u00B0C', # degree C
                       'and salinity is',
                       sprintf('%.2f', mean(unlist(lapply(list(state$recentProfile), function(k) {sub <- subset(k, pressure <= 5) ; sub[['salinity']]})), na.rm = TRUE)),
                       '.'),
                 ''))
    })
  output$TSDep <- renderPlot({
    # # for developing purposes
    # state <- list()
    # state$Tlim <- c(0,20)
    # state$plim <- c(0, 200)
    # plot temperature
    plotProfile(ghostCtd,
                xtype = 'temperature',
                col = 'white',
                Tlim = state$Tlim, plim = rev(state$plim),
                xlab = '',
                mar = mar,
                grid = FALSE)
    axis(3, col=col.temperature, col.axis=col.temperature, col.lab=col.temperature)
    mtext(resizableLabel("T", "x"),
            side=3, line=axisNameLoc, col=col.temperature)
    lapply(state$deploymentCtdClimatology, function(k) lines(k[['temperature']], k[['pressure']], col = col.temperature))
    lapply(state$deploymentCtd, function(k) lines(k[['temperature']], k[['pressure']], col = col.temperature2))
    if(!is.null(state$recentProfile)){
      lines(state$recentProfile[['temperature']], state$recentProfile[['pressure']], col = col.temperature2, lwd = 2)
    }
    box()
    # now add salinity
    par(new = TRUE)
    plot(ghostCtd[['salinity']], ghostCtd[['pressure']],
         col = 'white',         
         xlim = state$Slim, ylim = rev(state$plim),
         xlab = '', ylab = '', axes = FALSE,
         type = 'n')
    axis(1, col=col.salinity, col.axis=col.salinity, col.lab=col.salinity)
    mtext(resizableLabel("S", "x"),
          side=1, line=axisNameLoc, col=col.salinity, cex=par("cex"))
    lapply(state$deploymentCtdClimatology, function(k) lines(k[['salinity']], k[['pressure']], col = col.salinity))
    lapply(state$deploymentCtd, function(k) lines(k[['salinity']], k[['pressure']], col = col.salinity2)) 
    if(!is.null(state$recentProfile)){
      lines(state$recentProfile[['salinity']], state$recentProfile[['pressure']], col = col.salinity2, lwd = 2)
    }
    box()
    at <- par("yaxp")
    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
    
  })
  output$sigmaThetaDepText <- renderText({
    paste('The climatological near surface (p <= 5m) average density is',
          sprintf('%.2f', mean(unlist(lapply(state$deploymentCtdClimatology, function(k) {sub <- subset(k, pressure <= 5) ; sub[['sigmaThetaCalculated']]})), na.rm = TRUE)),
          "kg/m <sup>3</sup>",
          '.',
          ifelse(!is.null(state$recentProfile),
                 paste('The most recent profile near surface (p <= 5m) average density is',
                       sprintf('%.2f', mean(unlist(lapply(list(state$recentProfile), function(k) {sub <- subset(k, pressure <= 5) ; sub[['sigmaTheta']]})), na.rm = TRUE)),
                       "kg/m <sup>3</sup>",
                       '.'),
                 ''))
  })
  output$sigmaThetaDep <- renderPlot({
    plotProfile(ghostCtd,
                xtype = 'sigmaTheta',
                col = 'white',
                densitylim = state$STlim, plim = rev(state$plim),
                mar = mar,
                xlab = '')
    axis(3, col=col.sigmaTheta, col.axis=col.sigmaTheta, col.lab=col.sigmaTheta)
    mtext(resizableLabel("sigmaTheta"), side=3, line=axisNameLoc, col=col.sigmaTheta)
    lapply(state$deploymentCtdClimatology, function(k) lines(k[['sigmaTheta']], k[['pressure']], col = col.sigmaTheta))
    lapply(state$deploymentCtd, function(k) lines(k[['sigmaTheta']], k[['pressure']], col = col.sigmaTheta2))
    if(!is.null(state$recentProfile)){
      lines(state$recentProfile[['sigmaTheta']], state$recentProfile[['pressure']], col = col.sigmaTheta2, lwd = 2)
    }
    box()
  })
  output$TSRecText <- renderText({
    paste('The climatological near surface (p <= 5m) average temperature is',
          sprintf('%.2f', mean(unlist(lapply(state$recoveryCtdClimatology, function(k) {sub <- subset(k, pressure <= 5) ; sub[['temperature']]})), na.rm = TRUE)),
          '\u00B0C', # degree C
          'and salinity is',
          sprintf('%.2f', mean(unlist(lapply(state$recoveryCtdClimatology, function(k) {sub <- subset(k, pressure <= 5) ; sub[['salinity']]})), na.rm = TRUE)),
          '.')
  })
  output$TSRec <- renderPlot({
    # # # for developing purposes
    # state <- list()
    # state$Tlim <- c(0,20)
    # state$plim <- c(0, 200)
    # plot temperature
    plotProfile(ghostCtd,
                xtype = 'temperature',
                col = 'white',
                Tlim = state$Tlim, plim = rev(state$plim),
                xlab = '',
                mar = mar,
                grid = FALSE)
    axis(3, col=col.temperature, col.axis=col.temperature, col.lab=col.temperature)
    mtext(resizableLabel("T", "x"),
          side=3, line=axisNameLoc, col=col.temperature)
    lapply(state$recoveryCtdClimatology, function(k) lines(k[['temperature']], k[['pressure']], col = col.temperature))
    lapply(state$recoveryCtd, function(k) lines(k[['temperature']], k[['pressure']], col = col.temperature2))
    box()
    # now add salinity
    par(new = TRUE)
    plot(ghostCtd[['salinity']], ghostCtd[['pressure']],
         col = 'white',         
         xlim = state$Slim, ylim = rev(state$plim),
         xlab = '', ylab = '', axes = FALSE,
         type = 'n')
    axis(1, col=col.salinity, col.axis=col.salinity, col.lab=col.salinity)
    mtext(resizableLabel("S", "x"),
          side=1, line=axisNameLoc, col=col.salinity, cex=par("cex"))
    lapply(state$recoveryCtdClimatology, function(k) lines(k[['salinity']], k[['pressure']], col = col.salinity))
    lapply(state$recoveryCtd, function(k) lines(k[['salinity']], k[['pressure']], col = col.salinity2))    
    box()
    at <- par("yaxp")
    abline(h=seq(at[1], at[2], length.out=at[3]+1), col=col.grid, lty=lty.grid)
    
  })
  output$sigmaThetaRecText <- renderText({
    paste('The climatological near surface (p <= 5m) average density is',
          sprintf('%.2f', mean(unlist(lapply(state$recoveryCtdClimatology, function(k) {sub <- subset(k, pressure <= 5) ; sub[['sigmaThetaCalculated']]})), na.rm = TRUE)),
          "kg/m <sup>3</sup>",
          '.')
  })
  output$sigmaThetaRec <- renderPlot({
    plotProfile(ghostCtd,
                xtype = 'sigmaTheta',
                col = 'white',
                densitylim = state$STlim, plim = rev(state$plim),
                mar = mar,
                xlab = '')
    axis(3, col=col.sigmaTheta, col.axis=col.sigmaTheta, col.lab=col.sigmaTheta)
    mtext(resizableLabel("sigmaTheta"), side=3, line=axisNameLoc, col=col.sigmaTheta)
    lapply(state$recoveryCtdClimatology, function(k) lines(k[['sigmaTheta']], k[['pressure']], col = col.sigmaTheta))
    lapply(state$recoveryCtd, function(k) lines(k[['sigmaTheta']], k[['pressure']], col = col.sigmaTheta2))
    box()
  })
  
  output$climatologySection <- renderPlot({
    # # for debugging
    # state <- list()
    # state$sectionPlotVariable <- 'sigmaTheta'
    # modified code from climatology/station2/09fclimatologyDifferences.*.R
    zlim <- station2PlotLimits[['limits']][[gsub('\\d+', '', state$sectionPlotVariable)]]
    levels <- station2PlotLimits[['contourLevels']][[gsub('\\d+', '', state$sectionPlotVariable)]]
    levelLimits <- station2PlotLimits[['contourLevelLimits']][[gsub('\\d+', '', state$sectionPlotVariable)]]
    zcol <- switch(state$sectionPlotVariable,
                   'temperature' = oceColorsJet,
                   'salinity' = oceColorsJet,
                   'sigmaTheta' = oceColorsJet)
    # warming at 0.13degC/decade, use this for breaks and limits for both temp and salinity ??
    zbreaks <- switch(state$sectionPlotVariable,
                      'temperature'= NULL, #seq(Tlim[1], Tlim[2],1),
                      'salinity'= NULL, #seq(Slim[1], Slim[2]),
                      'sigmaTheta' = NULL)
    levelLimits <- switch(state$sectionPlotVariable,
                          'temperature' = levelLimits,
                          'salinity' = levelLimits,
                          'sigmaTheta' = levelLimits)
    levels <- switch(state$sectionPlotVariable,
                     'temperature' = levels,
                     'salinity' = levels,
                     'sigmaTheta' = levels,
                     'sigmaTheta2' = levels)
    plot(sg, which = state$sectionPlotVariable, ztype = 'image', 
         zlim = zlim, ylim = ylim, xlim = xlim, # x and y lim define is pre-amble
         xtype = 'time', zcol = zcol, zbreaks = zbreaks,
         axes = FALSE, ylab = '',
         mar = mar,
         stationTicks = FALSE, drawPalette = FALSE)
    clx <- sg[['startTime']]
    cly <- sg[['station',1]][['pressure']]
    clz <- matrix(sg[[state$sectionPlotVariable]], byrow = TRUE, nrow = length(sg[['station']]))
    contour(clx, cly, clz, levels = levels[levels > levelLimits[1] & levels < levelLimits[2]], 
            col = 'black', add = TRUE, labcex = 0.8, 
            vfont = c('sans serif', 'bold'))
    contour(clx, cly, clz, levels = levels[levels <= levelLimits[1] | levels >= levelLimits[2]],
            col = 'white', add = TRUE, labcex = 0.8, 
            vfont = c('sans serif', 'bold'))
    clxlab <- as.POSIXct(paste(fakeYear, 1:12, '01', sep = '-'), tz = 'UTC')
    # axis, side = 1
    mlab <- substr(month.abb, 1, 1)
    axis.POSIXct(side = 1, at = clxlab, labels = mlab)
    # axis, side = 2
    axis(side = 2, at = pretty(ylim))
    mtext(side = 2, text = resizableLabel('depth'), line = 2, cex = 4/5)
    # axis, side = 4
    axis(side = 4, at = pretty(ylim), labels = FALSE)
    # add deployment and recovery times
    par(xpd = NA)
    points(getFakeTime(state$deploymentDateRange, fakeYear), rep(-3, length(state$deploymentDateRange)), pch = 25, bg = 'black', col = 'black', cex = 9/10)
    points(getFakeTime(state$recoveryDateRange, fakeYear), rep(-3, length(state$recoveryDateRange)), pch = 25, bg = 'black', col = 'black', cex = 9/10)
    par(xpd = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
