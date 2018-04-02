#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.'
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer( function(input, output) {
   
  output$goalPlot <- renderPlot({
      #ce n'est pas dynamique
      # exemple avec Paris
    
    paris <- filter( Team_home_viz,team_long_name=="Paris Saint-Germain")
    
    paris4<-melt(paris[,c("nb_goal_scored_2008/2009","nb_goal_scored_2009/2010",
                          "nb_goal_scored_2010/2011","nb_goal_scored_2011/2012",
                          "nb_goal_scored_2012/2013","nb_goal_scored_2013/2014",
                          "nb_goal_scored_2014/2015",
                          "nb_goal_conceded_2008/2009","nb_goal_conceded_2009/2010",
                          "nb_goal_conceded_2010/2011","nb_goal_conceded_2011/2012",
                          "nb_goal_conceded_2012/2013","nb_goal_conceded_2013/2014",
                          "nb_goal_conceded_2014/2015"
    )])
    
    paris4$season <- substr(as.character(paris4$variable),nchar(as.character(paris4$variable))-8,nchar(as.character(paris4$variable)) )
    
    paris4$variable <- substr(as.character(paris4$variable),1,nchar(as.character(paris4$variable))-10 )
    
      
      ggplot(paris4)+ aes(x =season, y=value, group=variable, colour=variable) +
      geom_point(size=2) +
      geom_line(size=1) + 
      theme_classic() +
      geom_text(aes(label=value), nudge_y = 4, size=3 ) +
      labs(title="Nombre de buts marqués et encaissés par saison", x= "Saison", y ="Nombre de buts") +
      theme(legend.position = "bottom") +
      scale_color_manual(values=c("red", "green"))
    
  })
  
  output$teamData <- renderDataTable({as.data.frame(t(paris [,51:62]))})
  
})