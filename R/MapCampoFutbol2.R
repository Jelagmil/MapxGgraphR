#' Esta funcion crea un mapa con diferentes disparos, muestra su ubicación y calcula su xG.
#'
#' @param No hay parámetros
#' @return El mapa para localizar coordenadas
#' @examples
#' OptaMAPcampofutbol2()
#' @export
#'
OptaMAPcampofutbol2 <- function(){

#Creamos la plantilla del tema del gráfico (esto es de soccermatics con algunos cambios)
theme_blankPitch = function(size=12) {
  theme(
    # axis.text.x=element_text(size=size),
    # axis.text.y=element_blank(),
    # axis.ticks.length=unit(0, "lines"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    # legend.background=element_rect(fill="#538032", colour=NA),
    # legend.key=element_rect(colour="#538032",fill="#538032"),
    # legend.key.size=unit(1.2, "lines"),
    # legend.text=element_text(size=size),
    # legend.title=element_text(size=size, face="bold",hjust=0),
    # strip.background = element_rect(colour = "#538032", fill = "#538032", size = .5),
    # panel.background=element_rect(fill="#538032",colour="#538032"),
    panel.grid.major = element_line(colour = "red",linetype = 'dotted'),
    panel.grid.minor = element_line(colour = "red",linetype = 'dotted'),
    panel.spacing=element_blank(),
    plot.background=element_blank(),
    plot.margin=unit(c(0, 0, 0, 0), "lines"),
    plot.title=element_text(size=size*1.2),
    strip.text.y=element_text(colour="#538032",size=size,angle=270),
    strip.text.x=element_text(size=size*1))}

ymin <- 0
xmin <- 0

# Defining dimensions
GoalWidth <- 7.32
penspot <- 11.00
boxedgeW <- 40.32
boxedgeL <- 16.50
box6yardW <- 18.32
box6yardL <- 5.50

## dimensions calculations
# The 18 Yard Box
TheBoxWidth <- c(((70.40 / 2) + (boxedgeW / 2)),((70.40 / 2) - (boxedgeW / 2)))
TheBoxHeight <- c(boxedgeL,106.00-boxedgeL)
GoalPosts <- c(((70.40 / 2) + (GoalWidth / 2)),((70.40 / 2) - (GoalWidth / 2)))

# The 6 Yard Box
box6yardWidth <- c(((70.40 / 2) + (box6yardW / 2)),((70.40 / 2) - (box6yardW / 2)))
box6yardHeight <- c(box6yardL,106.00-box6yardL)

## Centre circle dimensions
centreCirle_d <- 18.30

## define the circle function
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#### create leftD arc ####
Dleft <- circleFun(c((penspot),(70.40/2)),centreCirle_d,npoints = 1000)
## remove part that is in the box
Dleft <- Dleft[which(Dleft$x >= (boxedgeL)),]

## create rightD arc  ####
Dright <- circleFun(c((106.00-(penspot)),(70.40/2)),centreCirle_d,npoints = 1000)
## remove part that is in the box
Dright <- Dright[which(Dright$x <= (106.00-(boxedgeL))),]

#### create center circle ####
center_circle <- circleFun(c((106.00/2),(70.40/2)),centreCirle_d,npoints = 100)

## create corner flag radius ####
TopLeftCorner <- circleFun(c(xmin,70.40),2.00,npoints = 1000)
TopRightCorner <- circleFun(c(106.00,70.40),2.00,npoints = 1000)
BottomLeftCorner <- circleFun(c(xmin,ymin),2.00,npoints = 1000)
BottomRightCorner <- circleFun(c(106.00,ymin),2.00,npoints = 1000)

p <- ggplot() +

  # xlim(c(50,106.00+0.10)) + ylim(c(-0.10,70.40+0.10)) +
  theme_blankPitch() +
  geom_rect(aes(xmin=42, xmax=106.00, ymin=0, ymax=70.40), colour = "black", alpha = 0) +
  # geom_rect(aes(xmin=0, xmax=TheBoxHeight[1], ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]),colour = "black",alpha = 0) +
  geom_rect(aes(xmin=TheBoxHeight[2], xmax=106.00, ymin=TheBoxWidth[1], ymax=TheBoxWidth[2]), colour = "black", alpha = 0) +
  # geom_rect(aes(xmin=0, xmax=box6yardHeight[1], ymin=box6yardWidth[1], ymax=box6yardWidth[2]), colour = "black", alpha = 0)  +
  geom_rect(aes(xmin=box6yardHeight[2], xmax=106.00, ymin=box6yardWidth[1], ymax=box6yardWidth[2]), colour = "black", alpha = 0)  +
  geom_segment(aes(x = 106.00/2, y = ymin, xend = 106.00/2, yend = 70.40),colour = "black") +
  # geom_path(data=Dleft, aes(x=x,y=y), colour = "black") +
  geom_path(data=Dright, aes(x=x,y=y), colour = "black") +
  geom_path(data=center_circle, aes(x=x,y=y), colour = "black") +
  # geom_point(aes(x = penspot , y = 70.40/2), colour = "black") +
  geom_point(aes(x = (106.00-(penspot)) , y = 70.40/2), colour = "black") +
  geom_point(aes(x = (106.00/2) , y = 70.40/2), colour = "black") +
  # geom_segment(aes(x = xmin, y = GoalPosts[1], xend = xmin, yend = GoalPosts[2]),colour = "black", size = 1) +
  geom_segment(aes(x = 106.00, y = GoalPosts[1], xend = 106.00, yend = GoalPosts[2]),colour = "black", size = 1)+
  theme(legend.position="none") +
  scale_x_continuous(minor_breaks = seq(42 , 106, 2), breaks = seq(42, 106, 4),sec.axis = dup_axis()) +
  scale_y_continuous(minor_breaks = seq(0 , 70.4, 2), breaks = seq(0, 70.4, 4),sec.axis = dup_axis()) +
  xlab("Eje X") + ylab("Eje Y")
#scale_x_continuous(minor_breaks = seq(50, 106, 1))

return(p)

}
