#' Esta funcion dibuja el campo de futbol y viene de la libreria soccermatics
#'
#' @examples
#' OptaMAPcampofutbol()
#' @export
#'
#'
#'
OptaMAPcampofutbol <- function(){

  #Creamos la plantilla del tema del gráfico (esto es de soccermatics con algunos cambios)
  theme_blankPitch = function(size=12) {
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.length=unit(0, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="#538032", colour=NA),
      legend.key=element_rect(colour="#538032",fill="#538032"),
      legend.key.size=unit(1.2, "lines"),
      legend.text=element_text(size=size),
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = "#538032", fill = "#538032", size = .5),
      panel.background=element_rect(fill="#538032",colour="#538032"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.spacing=element_blank(),
      plot.background=element_blank(),
      plot.margin=unit(c(0, 0, 0, 0), "lines"),
      plot.title=element_text(size=size*1.2),
      strip.text.y=element_text(colour="#538032",size=size,angle=270),
      strip.text.x=element_text(size=size*1))}

  ymin <- 0
  xmin <- 0

  # Defining dimensions
  GoalWidth <- 732
  penspot <- 1100
  boxedgeW <- 4032
  boxedgeL <- 1650
  box6yardW <- 1832
  box6yardL <- 550

  ## dimensions calculations
  # The 18 Yard Box
  TheBoxWidth <- c(((7040 / 2) + (boxedgeW / 2)),((7040 / 2) - (boxedgeW / 2)))
  TheBoxHeight <- c(boxedgeL,10600-boxedgeL)
  GoalPosts <- c(((7040 / 2) + (GoalWidth / 2)),((7040 / 2) - (GoalWidth / 2)))

  # The 6 Yard Box
  box6yardWidth <- c(((7040 / 2) + (box6yardW / 2)),((7040 / 2) - (box6yardW / 2)))
  box6yardHeight <- c(box6yardL,10600-box6yardL)

  ## Centre circle dimensions
  centreCirle_d <- 1830

  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create leftD arc ####
  Dleft <- circleFun(c((penspot),(7040/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]

  ## create rightD arc  ####
  Dright <- circleFun(c((10600-(penspot)),(7040/2)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  Dright <- Dright[which(Dright$x <= (10600-(boxedgeL))),]

  #### create center circle ####
  center_circle <- circleFun(c((10600/2),(7040/2)),centreCirle_d,npoints = 100)

  ## create corner flag radius ####
  TopLeftCorner <- circleFun(c(xmin,7040),200,npoints = 1000)
  TopRightCorner <- circleFun(c(10600,7040),200,npoints = 1000)
  BottomLeftCorner <- circleFun(c(xmin,ymin),200,npoints = 1000)
  BottomRightCorner <- circleFun(c(10600,ymin),200,npoints = 1000)

  p <- ggplot() +

    xlim(c(-10,10600+10)) + ylim(c(-10,7040+10)) +
    theme_blankPitch() +
    geom_rect(aes(xmin=0, xmax=10600, ymin=0, ymax=7040), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=TheBoxHeight[2], xmax=10600, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), fill = "#538032", colour = "#ffffff") +
    geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#538032", colour = "#ffffff")  +
    geom_rect(aes(xmin=box6yardHeight[2], xmax=10600, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), fill = "#538032", colour = "#ffffff")  +
    geom_segment(aes(x = 10600/2, y = ymin, xend = 10600/2, yend = 7040),colour = "#ffffff") +
    geom_path(data=Dleft, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=Dright, aes(x=x,y=y), colour = "#ffffff") +
    geom_path(data=center_circle, aes(x=x,y=y), colour = "#ffffff") +
    geom_point(aes(x = penspot , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600-(penspot)) , y = 7040/2), colour = "#ffffff") +
    geom_point(aes(x = (10600/2) , y = 7040/2), colour = "#ffffff") +
    geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "#000000", size = 1) +
    geom_segment(aes(x = 10600, y = GoalPosts[1], xend = 10600, yend = GoalPosts[2]),colour = "#000000", size = 1)+
    theme(legend.position="none")

  return(p)

}
