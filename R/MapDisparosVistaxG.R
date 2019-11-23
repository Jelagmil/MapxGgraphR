#' Esta funcion crea una visualizacion a partir de datos de entrada de disparos introducidos de manera manual.
#'
#' @return El mapa de tiros y acumulado en un partido para los equipos con el xG calculado
#' @examples
#' xGgraph()
#' @export
#'
#' @import ggrepel
#' @import caret
#' @import gbm
#' @import pdp
#' @import ggplot2
#' @import ggpubr
#' @import dplyr
#' @importFrom magrittr %>%
#'
xGgraph <- function(){

  directorio_modelo <- paste(system.file(package="MapxGgraphR"),"/model/xG_model.rda",sep="")

  load(directorio_modelo)

  for(j in 1:2){

    if(j==1){
              partido <- readline("Escribe un identificador para el partido [P.Ej. 20190101_VLCRMA: ")
              equipo <- readline("Escribe el nombre del equipo local: ")
              numerotiros <- readline("Cuantos tiros vas a incluir del equipo local?: ")
              local<-'1'
            }else if(j==2){
              equipo <- readline("Escribe el nombre del equipo visitante: ")
              numerotiros <- readline("Cuantos tiros vas a incluir del equipo visitante?: ")
              local<-'2'
            }

    for(h in 1:numerotiros){

          x <- readline("Localiza en el plano la coordenada X y escribela aqui: ")
          y <- readline("Localiza en el plano la coordenada Y y escribela aqui: ")

          x <- as.numeric(unlist(strsplit(x, ",")))
          y <- as.numeric(unlist(strsplit(y, ",")))

          min <- readline("En que minuto se hizo el disparo? [Total minutos] ")
          min <- as.numeric(unlist(strsplit(min, ",")))

          distancia<-((106-x)^2+((35)-(y))^2)^0.5

          if(y==35){
            angulo<-round(asin(0)*57.2974694,digits=2)
          }else if(y>35){
            angulo<-round(asin((y-35)/distancia)*57.2974694,digits=2)
          }else if(y<35){
            angulo<-round(asin((35-y)/distancia)*57.2974694,digits=2)
          }

          parte_cuerpo<-menu(c("cabeza", "pied","piei","otraparte"), title="Selecciona una parte del cuerpo:")

          if(parte_cuerpo==1){
            parte_cuerpo<-c('cabeza')
          }else if(parte_cuerpo==2){
            parte_cuerpo<-c('pied')
          }else if(parte_cuerpo==3){
            parte_cuerpo<-c('piei')
          }else if(parte_cuerpo==4){
            parte_cuerpo<-c('otraparte')
          }

          situacion<-menu(c("juegoestatico", "contra","faltaind","corner",'faltadir','penalty','banda'), title="Selecciona una parte del cuerpo:")

          if(situacion==1){
            situacion<-c('juegoestatico')
          }else if(situacion==2){
            situacion<-c('contra')
          }else if(situacion==3){
            situacion<-c('faltaind')
          }else if(situacion==4){
            situacion<-c('corner')
          }else if(situacion==5){
            situacion<-c('faltadir')
          }else if(situacion==6){
            situacion<-c('penalty')
          }else if(situacion==7){
            situacion<-c('banda')
          }

          penal<-menu(c("Si", "No"), title="Es un tiro de penalty?:")
          if(penal==1){
            penal<-"-1"
          }else if(penal==2){
            penal<-"0"
          }

          caracara<-menu(c("Si", "No"), title="Es un uno contra uno contra el portero?:")
          if(caracara==1){
            caracara<-"-1"
          }else if(caracara==2){
            caracara<-"0"
          }

          gol<-menu(c("Si", "No"), title="La jugada acabo en gol?:")
          if(gol==1){
            gol<-1
            jugador<-readline("Quien marco el gol?: ")

          }else if(gol==2){
            gol<-0
            jugador<-"0"
          }

          prueba<-data.frame(list(h,distancia,angulo,caracara,penal,situacion,parte_cuerpo,equipo,min,gol,jugador,local))
          names(prueba)<-c('id','distancia','angulo','one2one','penalty','situacion_juego','parte_cuerpo','equipo','min','gol','jugador','local')
          prueba$distancia <- as.numeric(as.character(prueba$distancia))
          prueba$angulo <- as.numeric(as.character(prueba$angulo))
          prueba2<-dplyr::select(prueba,distancia,angulo,one2one,penalty,situacion_juego,parte_cuerpo)
          prueba$GBM <- gbm::predict(xG_model, prueba2, na.action = na.pass, type = "prob")[,"1"]

          prueba$x<-x
          prueba$y<-y

          if(h==1 & j==1){
            Mapa_disparos1<-prueba
          }else if (h>1 & j==1){
            Mapa_disparos1<-rbind(Mapa_disparos1,prueba)
          }else if (h==1 & j==2){
            Mapa_disparos2<-prueba
          }else if (h>1 & j==2){
            Mapa_disparos2<-rbind(Mapa_disparos2,prueba)}




          if(j==1){
            Goles_xg_local<-sum(Mapa_disparos1$GBM)
            Goles_real_local<-sum(Mapa_disparos1$gol)
            Mapa_disparos1$equipo<-as.character(Mapa_disparos1$equipo)
            equipo1<-dplyr::distinct(Mapa_disparos1,equipo)
          }else{
            Goles_xg_visitante<-sum(Mapa_disparos2$GBM)
            Goles_real_visitante<-sum(Mapa_disparos2$gol)
            Mapa_disparos2$equipo<-as.character(Mapa_disparos2$equipo)
            equipo2<-dplyr::distinct(Mapa_disparos2,equipo)
          }
  }
}



  ojo <- c("#a50044", "#000000")
  names(ojo) <- c('1','2')


    h <- OptaMAPcampofutbol()
    p <- h +

      # ggtitle(paste("\nMapa de disparo")) +
      # Aqui dibujamos el mapa de calor de los corners
      #stat_density2d(data=polar2,aes(x=f*106,y=g*70,fill = ..level..,alpha=..level..), geom="polygon",show.legend = FALSE) +
      #scale_fill_gradient(low="yellow", high="red",aesthetics = "fill") +
      # dejo comentado la linea siguiente para el futuro, dado que con ella dibujamos la flecha del lanzamiento de corner
      # geom_segment(data=Mapa_disparos,aes(x=x*100, y=y*100, xend = 10600, yend = 3500),arrow = arrow(length = unit(0.01, "npc")))+
      #geom_point(data = shots,aes(x = ((a)),y = ((b)/15.57)*1.31,color=Tipo_tiro,size=-Dist_Shoot,shape=Remate,stroke = 1)) +
      #Dibujamos los remates
      geom_point(data=Mapa_disparos1,aes(x = x*100, y=y*100,size=GBM,shape=factor(gol)),stroke = 1,color="#a50044") +
      geom_point(data=Mapa_disparos2,aes(x = 10600-x*100, y=y*100,size=GBM,shape=factor(gol)),stroke = 1,color="#000000") +
      scale_shape_manual(values=c(19,1)) +
      annotate(geom="text", x=2650, y=5500, label=format(Goles_xg_local,digits=2,nsmall=2), color="#a50044",size=10) +
      annotate(geom="text", x=2650, y=6500, label=equipo1, color="#a50044",size=8) +
      annotate(geom="text", x=7950, y=5500, label=format(Goles_xg_visitante,digits=2,nsmall=2), color="#000000",size=10) +
      annotate(geom="text", x=7950, y=6500, label=equipo2, color="#000000",size=8)
      #metemos la leyenda abajo
      #theme(legend.position="bottom")


    # names(prueba)<-c('id','distancia','angulo','one2one','penalty','situacion_juego','parte_cuerpo','equipo','min','gol','jugador')

    Mapa<-rbind(Mapa_disparos1,Mapa_disparos2)
    Mapa$local<-as.character(Mapa$local)
    Mapa$jugador<-as.character(Mapa$jugador)
    saveRDS(Mapa,file=paste(partido,".rds",sep=""))

    t1<-data.frame(list(local=1:2))
    t2<-data.frame(list(min=1:100))
    t3<-merge(t1,t2)
    t3$local<-as.character(t3$local)
    Mapa1<-left_join(t3,Mapa)

    Mapa1$GBM[is.na(Mapa1$GBM)] <- 0

    clasico_rollsum <- Mapa1 %>%
      group_by(min, local) %>%
      summarize(sumxg = sum(GBM)) %>%
      ungroup() %>%
      group_by(local) %>%
      mutate(rollsum = lag(cumsum(sumxg)),
             rollsum = if_else(is.na(rollsum), 0, rollsum)) %>%
      select(local, min, rollsum, sumxg) %>%
      mutate(rollsum = case_when(
        row_number() == n() & sumxg != 0 ~ rollsum + sumxg,
        TRUE ~ rollsum
      ))

    clasico_rollsum <- clasico_rollsum %>%
      left_join(Mapa1 %>% filter(gol == '1') %>% select(min, gol, local, jugador),
                by = c("min", "local")) %>%
      mutate(rollsum_goal = rollsum + sumxg,
             minute_goal = min + 1,
             player_label = case_when(
               gol == '1' ~ as.character(glue::glue("{jugador}: {sumxg %>% signif(digits = 2)} xG")),
               TRUE ~ ""))


    #glimpse(clasico_rollsum)

    clasico_1112_xg <- Mapa1 %>%
      group_by(local,equipo) %>%
      summarize(tot_xg = sum(GBM) %>% signif(digits = 2)) %>%
      mutate(team_label = as.character(glue::glue("{equipo}: {tot_xg} xG")))

    tot_clasico_df <- clasico_1112_xg %>%
      pull(tot_xg)


    clasico_rollsumxg_plot <- clasico_rollsum %>%
      ggplot(aes(x = min, y = rollsum,
                 group = local, color = local)) +
      geom_line(size = 2.5) +
      geom_label_repel(data = clasico_rollsum %>% filter(gol == '1'),
                       aes(x = minute_goal, y = rollsum_goal,
                           color = local, label = player_label),
                       nudge_x = 6, nudge_y = 0.15, family = "mono",
                       show.legend = FALSE,size=3) +
      geom_point(data = clasico_rollsum %>% filter(gol == '1'),
                 aes(x = minute_goal, y = rollsum_goal, color = local), show.legend = FALSE,
                 size = 5, shape = 21, fill = "white", stroke = 0.6) +
      scale_color_manual(values = ojo,
                         labels = c(equipo1,
                                    equipo2)) +
      scale_fill_manual(values = ojo)+

      scale_x_continuous(breaks = c(seq(0, 100, by = 5)),
                         labels = c(seq(0, 100, by = 5)),
                         expand = c(0.01, 0),
                         limits = c(0, 110)) +
      scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tot_clasico_df)) +
      labs(title = paste(equipo1," (",Goles_real_local,") - ",equipo2," (",Goles_real_visitante,")",sep=''),
           subtitle = "Acumulado de xG",
           x = NULL,
           y = "Expected Goals") +
      theme_minimal() +
      theme(text = element_text(family = "mono"),
            plot.title = element_text(size = 18, family = "mono"),
            plot.subtitle = element_text(size = 14, family = "mono",
                                         color = "grey20"),
            axis.title = element_text(size = 8, color = "grey20"),
            axis.text = element_text(size = 8, face = "bold"),
            panel.grid.minor = element_blank(),
            legend.text = element_text(size = 8),
            legend.position = c(0.2, 2),
            legend.direction = "horizontal",
            legend.title = element_blank())


    figure <- ggarrange(clasico_rollsumxg_plot,p,ncol = 1, nrow = 2)
    return(figure)

    ggsave(paste(partido,".png",sep=''),plot=figure,
           width = 20, height = 8, dpi = 300, units = "in", device='png')

  }





