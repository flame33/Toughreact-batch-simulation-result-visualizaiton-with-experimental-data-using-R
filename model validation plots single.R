setwd('C:/¸ÆÖÊ½º½á×éLN/Case6 Di')
rm(list = ls())
gc()
library('tidyverse')
library('cowplot')
library('ggplot2')

dat_fun <- function(filename){
  filename1 <- paste('./', filename, sep = '')
  dat1 <- read.table(filename1, fill = TRUE, row.names = NULL, header = F, skip = 11)
  # dat1<-read.table(filename1,colClass=c('NULL','NULL','Numeric'),header = F,skip = 1)
  # dat1 %>% select(-1)
  col_name <- dat1[1,-ncol(dat1)] %>% c('B',.) %>% unlist()
  # dat1<-dat1[-1,]
  colnames(dat1) <- col_name
  return(dat1[-1,-c(1,2)])
}
test_dat <- dat_fun(filename = 'co2d_tim.dat')

legend_plot <- ggplot()+
  coord_cartesian(xlim = c(0,2.5), ylim = c(0,2), expand = F)+
  geom_point(data = data.frame(x = 1, y = 1.4), aes(x = x, y = y), color = 'blueviolet',size = 1)+
  geom_line(data = data.frame(x = c(0.5,1,1.5), y = 1.1), aes(x = x, y = y), color = '#63B8FF', size = 0.6)+
  annotate(geom = 'text', x = 2, y = c(1.4,1.1),label = c('measured','simulated'), size = 2)+
  theme_void()

plot_fun <- function(filename, ind, y_lab){
  filename1 <- paste('./', filename, sep = '')
  dat1 <- read.table(filename1, fill = TRUE, row.names = NULL, header = F, skip = 11)
  col_name <- dat1[1,-ncol(dat1)] %>% c('A',.) %>% unlist()
  # dat1<-dat1[-1,]
  colnames(dat1) <- col_name
  dat2 <- dat1[-1,-c(1,2)]
  
  df <- data.frame(time = as.numeric(dat2[,1]), val = as.numeric(dat2[,ind]))
  
  if(substr(y_lab, 1,4) == 't_ca'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0,0.020289875,0.0218255,0.021275563,0.020725625,0.01734675))
  }else if(substr(y_lab, 1,2) == 'pH'){
    df1 <- data.frame(time = seq(0,15,3), val = c(6.8,6.47,	6.57,6.46,6.56,6.75))
  }else if(substr(y_lab, 1,4) == 't_mg'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0,0.007367083,0.011208125,0.012409271,0.013610417,0.012370417))
  }else if(substr(y_lab, 1,4) == 't_na'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0,0.009531957,0.009849348	,0.009721957,0.009594565,0.012084783))
  }else if(substr(y_lab, 1,4) == 't_sio2'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0,0.002709464,0.003220714,0.003150893,0.003081071,0.003964821))
  }else if(substr(y_lab, 1,4) == 't_fe'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0,0.000242384,0.000179122,0.000169534,0.000159946,0.000121237))
  }else{
    df1 <- data.frame(time = seq(0,15,3), val = 0)
  }
  
  one_plot <- ggplot(data = df, aes(x = time*365, y = val))+
    ylab(label = y_lab)+
    xlab(label = "Time (Days)")+
    geom_line(size = 1, color = '#63B8FF', show.legend = T)+
    #geom_point(size = 2, color = '#FF6347')+
    geom_point(data = df1, aes(x = time, y = val), size = 2, color = 'blueviolet')+
    theme_bw() +
    theme(legend.position = c(1,0),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.justification = c(1,0),
          legend.text = element_text(face = 'bold',size = 9),
          legend.key.size = unit(8,"pt"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(size = 1),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.title = element_text(size=10.2,face='bold',color = 'black'),
          axis.text = element_text(size=10.2,face='bold',color = 'black'))
  draw <- ggdraw()+
    draw_plot(one_plot, 0, 0, width = 1, height = 1)+
    draw_plot(legend_plot, 0.85, 0.85, width = 0.12, height = 0.15)
  return(draw)
}


p1 <- plot_fun(filename = './co2d_tim.dat', ind = 5, y_lab = 'pH')
save_plot('./pH.jpg', p1, ncol = 1, nrow = 1, base_asp = 1.618)
p2 <- plot_fun(filename = './co2d_tim.dat', ind = 10, y_lab = 't_ca')
save_plot('./t_ca.jpg', p2, ncol = 1, nrow = 1, base_asp = 1.618)
p3 <- plot_fun(filename = './co2d_tim.dat', ind = 11, y_lab = 't_mg')
save_plot('./t_mg.jpg', p3, ncol = 1, nrow = 1, base_asp = 1.618)
p4 <- plot_fun(filename = './co2d_tim.dat', ind = 12, y_lab = 't_na')
save_plot('./na.jpg', p4, ncol = 1, nrow = 1, base_asp = 1.618)
p5 <- plot_fun(filename = './co2d_tim.dat', ind = 14, y_lab = 't_fe')
save_plot('./fe.jpg', p1, ncol = 1, nrow = 1, base_asp = 1.618)
p6 <- plot_fun(filename = './co2d_tim.dat', ind = 15, y_lab = 't_sio2')
save_plot('./sio2.jpg', p1, ncol = 1, nrow = 1, base_asp = 1.618)




