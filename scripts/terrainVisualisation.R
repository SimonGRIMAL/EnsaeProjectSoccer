terrainVisualisation <- function(Match,League_id,Match_id){
  
  # Description:
  # fonction de visualisation de la composition d'equipe sur le terrain :

  
  # Input:
  # - Match (data Frame issu de data_Import.R)
  # - League_id identifiant de la ligue choisi par l'utilisateur
  # - Match_id identifiant du match choisit par l'utilisateur via l'appli
  
  # Fonction :

library(magick)

#for debug  
Match_id <- 3000
  
#Preparation des objets R qui seront en arguments (A mettre hors de la fonction une fois l'objet R crée)
Match_light<-Match[,(1:55)]

#plot(t(Match_light[Match,34:44]),t(Match_light[Match,12:22]),pch=15,col="blue",xlab="",ylab="", main=paste("Home team game system :",Matchs_Config$game_syst_home[Num_Match]))
#Match_light[1000,12]

#new method The image_draw() function opens a graphics device to draw on top of an existing image using pixel coordinates.
terrain <- image_read("img/terrain2.jpg")
drawing <- image_draw(terrain, pointsize = 20,antialias = FALSE)
drawing
width<-330
height<-640  #essayer de trouver comment sortie la resolution de drawing

X_home<-width/10*t(Match_light[Match_id,12:22])  #calcul coord team home
Y_home<-height/2/12*t(Match_light[Match_id,34:44])

X_away <- width/10*t(Match_light[Match_id,23:33])  #calcul coord team away
Y_away <- height/2/12*t(Match_light[Match_id,45:55])+2*(height/2-height/2/12*t(Match_light[Match_id,45:55]))

points(X_home,Y_home,pch=6,col="blue",bg="blue") #on trace
points(X_away,Y_away,pch=2,col="red",bg="red")

#dev.off()



# #tests
# library(ggplot2)
# 
# #create overlay image
# terrain <- image_read("img/soccer-145794_640.png")
# terrain
# overlay <- image_transparent(terrain,"white",fuzz = 10)
# 
# #create plot
# par(mfrow =c(1,2))
# Num_Match <- 3000
# plotHome<-image_graph(res=96)
# plot(t(Matchs_Config[Num_Match,14:24]),t(Matchs_Config[Num_Match,36:46]),pch=15,col="blue",xlab="",ylab="", main=paste("Home team game system :",Matchs_Config$game_syst_home[Num_Match]))
# dev.off()
# #composite
# image_composite(plotHome,overlay,operator = "atop", offset="+0+0")
# 
# 
# 
# #new method The image_draw() function opens a graphics device to draw on top of an existing image using pixel coordinates.
# terrain <- image_read("img/soccer-145794_640.png")
# drawing <- image_draw(terrain, pointsize = 20,antialias = FALSE)
# points(x=100,y=20)
# text(100, 30, "name")
# 
# 
# rect(20, 20, 200, 100, border = "red", lty = "dashed", lwd = 5)
# abline(h = 300, col = 'blue', lwd = '10', lty = "dotted")
# text(10, 250, "Hoiven-Glaven", family = "courier", cex = 4, srt = 90)
# palette(rainbow(11, end = 0.9))
# symbols(rep(200, 11), seq(0, 400, 40), circles = runif(11, 5, 35),
#         bg = 1:11, inches = FALSE, add = TRUE)
# 
# image_browse(drawing)
# 
# 
# 
# 
}
