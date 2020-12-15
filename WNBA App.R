### SHINY APP ###

#install packages
library(rsconnect)
library(shinycssloaders)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(grid)
library(jpeg)
library(ggExtra)
library(scales)
library(patchwork)
library(paletteer)
library(jpeg)
library(RCurl)
library(tidyverse)
library(ggfortify)
library(randomForest)
library(shiny)
library(gt)
library(data.table)
library(shinythemes)

wnba2 <- readRDS('wnba2.rdata')
playershooting1 <- readRDS('playershooting.rdata')
playershooting <- playershooting1[!duplicated(playershooting1$Player), ]
halfcourt <- rasterGrob(readJPEG("halfcourt.jpg"),
                        width=unit(1,"npc"), height=unit(1,"npc"))

ui <- fluidPage(theme = themeSelector(),
  #Create Title
  titlePanel("WNBA Player Shooter Profiles"),
  actionButton("about", "About"),
  #Generate sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "userplayer", 
                  label = "Player:",
                  choices = unique(sort(as.character(wnba2$player, decreasing = TRUE)))),
      actionButton("action1", "Submit", class = "btn-primary"),
      hr()),

    mainPanel(width = 5,
      tabsetPanel(type = "tabs",
        tabPanel("Shot Density", plotOutput("courtplot") %>% withSpinner(color="#0dc5c1"),
                 tableOutput("locals")),
        tabPanel("Visual Percentages", plotOutput("madeplot") %>% withSpinner(color="#0dc5c1")),
        tabPanel("Shot Chart", plotOutput("pointplot") %>% withSpinner(color="#0dc5c1"),
                 tableOutput("areapercent")),
        tabPanel("Player Summary", tableOutput("playersummary") %>% withSpinner(color="#0dc5c1"))
      )
    )
  )
)

server <- function(input, output){
  
  heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1))(10)
  
  userselect <- eventReactive(input$action1, {
                              wnba2 %>% filter(player == input$userplayer)
                              })
  observeEvent(input$about, {
    showModal(modalDialog(
      title = "About the Data",
      HTML("The purpose of this app is to allow the user easily see where different WNBA players shoot best
      on the floor, as well as provide summary statistics for each player. This data can be used to create both
      offensive and defense game plans, indentify places where players should spend more time improving their shot,
      and be compared with in-game data to identify players that are playing better/worse than they normally have. With more 
      data the app can be improved to identify how close a player should be guarded, as well as what type of shot (i.e.
      Catch and Shoot, One-Dribble Shot, Fade Away, etc...) a player shoots best. The data retreived didn't have that
      kind of data, nor could I find that data, but it would be useful since those variables effect whether a player makes or misses a shot.<br><br>
      Data was taken from:<br>
                SportRadar (https://developer.sportradar.com/docs/read/basketball/WNBA_v7)<br>
                SportReference (https://www.basketball-reference.com/wnba/players/)<br><br>
                Created by: Billy Stewart Dec-2020<br>
                Contact Information: sbillystewartsw@gmail.com"),
      easyClose = TRUE
    ))
  })
  
  # playerlist <- unique(sort(as.character(wnba2$player, decreasing = TRUE)))
  # playershooting <- NULL
  # for(i in 1:length(playerlist)){
  #   temp <- wnba2 %>% 
  #     filter(player == playerlist[i])
  #   
  #   tempshot <- sum(as.logical(temp$made))/length(temp$made)
  #   
  #   tempthree <- NULL
  #   
  #   for(j in 1:length(temp$player)){
  #     if(grepl("three", temp$event[j])){
  #       tempthree <- rbind(tempthree, temp[j,])
  #     }
  #   }
  #       
  #   threeadd <- sum(as.logical(tempthree$made))/length(tempthree$made)
  #   tempadd <- cbind("Player" = playerlist[i], "Position" = temp$position[1], "EFGP" = tempshot, "3PtP" = threeadd)
  #   playershooting <- rbind(playershooting, tempadd)
  # }
  # 
  # playershooting <- data.frame(playershooting)
  
  usertable <- eventReactive(input$action1, {
    tbl <- wnba2 %>% 
      filter(player == input$userplayer) %>% 
      group_by(area) %>% 
      summarise(sum(as.logical(made))/length(as.logical(made)), length(made))
    
    tbl <- data.frame(tbl)
    names(tbl)[names(tbl) == "area"] = "Area"
    names(tbl)[names(tbl) == "sum.as.logical.made...length.as.logical.made.."] = "Shooting %"
    names(tbl)[names(tbl) == "length.made."] = "# of Shots"
    for(i in 1:length(tbl[,1])){
      temp <- if(grepl("out", tbl[i,1])){3} else {2}
      tbl[i,4] <- round((tbl[i,2] * temp), 2)
    }
    names(tbl)[names(tbl) == "V4"] = "ExpPts"
    tbl[ ,2] <- percent(tbl[ ,2], accuracy = 1)
    tbl <- tbl[order(-tbl$ExpPts), ]
    
    return(tbl)
  })
  
  output$courtplot <- renderPlot({
    ggplot(data = userselect(), mapping = aes(x = newy, y = newx)) +
      annotation_custom(halfcourt, 0, 600, 0, 600) +
      geom_density_2d_filled(aes(fill = ..level.., color = ..level..),
                             contour_var = "ndensity", alpha = .8) +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      xlim(0, 600) +
      ylim(0, 600) +
      xlab("") +
      ylab("") +
      ggtitle("Shot Density") +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
  })
  
  userselect2 <- eventReactive(input$action1, {
    test <- wnba2 %>% 
      filter(player == input$userplayer) %>% 
      group_by(area) %>% 
      summarise("Shooting %" = percent(sum(as.logical(made))/length(made), accuracy = 1), "Shots" = length(made))
    
    x <- c(300, 300, 450, 450, 150, 150, 300, 300, 500, 575, 100, 25, 300)
    y <- c(550, 300, 225, 50, 225, 50, 150, 400, 350, 50, 350, 50, 50)
    area <- c('backcourt', 'insidecenter', 'insideleft', 'insideleftwing', 'insideright', 'insiderightwing', 'inthepaint', 'outsidecenter',
              'outsideleft', 'outsideleftwing', 'outsideright', 'outsiderightwing', 'underbasket')
    
    temp <- data.frame(cbind(area, x, y))
    temp$x <- as.numeric(temp$x)
    temp$y <- as.numeric(temp$y)
    
    locals <- merge(temp, test, by.x = "area", by.y = "area")
    
    return(locals)
  })
  
  output$madeplot <- renderPlot({
    ggplot(data = userselect2(), mapping = aes(x = x, y = y)) +
      annotation_custom(halfcourt, 0, 600, 0, 600) +
      geom_point(aes(colour = area, size = 4, alpha = 0.8), size = 4) +
      geom_text(aes(colour = area, label = `Shooting %`), vjust = -1.2, size = 7) +
      xlim(0, 600) +
      ylim(0, 600) +
      xlab("") +
      ylab("") +
      ggtitle("Visual Shooting Percentage") +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
  })
  
  usertable2 <- eventReactive(input$action1, {
    tble <- wnba2 %>% 
      filter(player == input$userplayer, made == TRUE) %>% 
      group_by(area) %>% 
      summarise(length(made))
    
    tble <- data.frame(tble)
    tble <- tble[order(-tble$length.made.), ]
    names(tble)[names(tble) == "area"] = "Area"
    names(tble)[names(tble) == "length.made."] = "# of Shots"
    tble <- setDT(tble)
    
    return(tble)
  })
  
  output$pointplot <- renderPlot({
    ggplot(data = userselect(), mapping = aes(x = newy, y = newx, color=made)) +
      annotation_custom(halfcourt, 0, 600, 0, 600) +
      geom_point() +
      xlim(0, 600) +
      ylim(0, 600) +
      xlab("") +
      ylab("") +
      ggtitle("Shot Chart") +
      labs(colour = "Made/Missed Shot") +
      scale_color_discrete(labels = c("Missed Shot", "Made Shot")) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
  })
  
  areaperc <- eventReactive(input$action1, {
    tbl <- wnba2 %>% 
      filter(player == input$userplayer) %>% 
      group_by(area) %>% 
      summarise(sum(as.logical(made))/length(as.logical(made)))
    
    tbl <- data.frame(tbl)
    names(tbl)[names(tbl) == "area"] = "Area"
    names(tbl)[names(tbl) == "sum.as.logical.made...length.as.logical.made.."] = "Shooting %"
    for(i in 1:length(tbl[,1])){
      temp <- if(grepl("out", tbl[i,1])){3} else {2}
      tbl[i,3] <- round((tbl[i,2] * temp), 2)
    }
    names(tbl)[names(tbl) == "V3"] = "ExpPts"
    tbl[ ,2] <- percent(tbl[ ,2], accuracy = 1)
    tbl <- tbl[order(-tbl$ExpPts), ]
    
    
    tbl <- setDT(tbl)
    
    return(tbl)
  })
  
  playsum <- eventReactive(input$action1, {
      tempdata <- wnba2 %>% 
        filter(player == input$userplayer)

      playersum <- data.frame()
      shootingperc <- percent(sum(as.logical(tempdata$made))/length(tempdata$made),2)
      pos <- tempdata$position[1]
      
      temp <- playershooting %>% 
        filter(Position == pos)
      
      playernums <- na.omit(playershooting[playershooting$Player == input$userplayer,])
      threepoint <- NULL
      for(i in 1:length(tempdata$player)){
        if(grepl("three", tempdata$event[i])){
          threepoint <- rbind(threepoint, tempdata[i,])
        }
      }
      
      twopoint <- NULL
      for(i in 1:length(tempdata$player)){
        if(grepl("two", tempdata$event[i])){
          twopoint <- rbind(twopoint, tempdata[i,])
        }
      }
      
      tempt <- sum(as.logical(threepoint$made))/length(threepoint$made)
      threeperc <- percent(tempt,2)
      
      qtle <- ecdf(temp$EFGP)
      percshots <- round(qtle(playernums$EFGP),2)
      qtle2 <- ecdf(temp$X3PtP)
      percthree <- round(qtle2(playernums$X3PtP),2)
      
      exptwo <- round(sum(as.logical(twopoint$made))/length(twopoint$made) * 2, 2)
      expthree <- round(tempt * 3, 2)
      
      
      playersum <- cbind("Name" = input$userplayer, "Pos" = pos, "Shooting Perc" = shootingperc,
                         "Pos Percentile" = percshots, "Three Perc." = threeperc, "Pos. Percentile" = percthree,
                         "Exp2" = exptwo, "Exp3" = expthree)
      playersum <- data.frame(playersum)
      playersum <- setDT(playersum)
      return(playersum)
  })
  
  output$locals <- renderTable({
    setDT(usertable())
  })
  
  output$areapercent <- renderTable({
    setDT(areaperc())
  })
  
  output$playersummary <- renderTable({
    setDT(playsum())
  })
}

shinyApp(ui = ui, server = server)


#Change quantile to percentile
#Column names for tables
#Create "about" tab with information