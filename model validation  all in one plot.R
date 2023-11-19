setwd('D:/ÂÛÎÄmodel/case6/case6 2022.1.20 change/2cell')
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
  col_name <- dat1[1,-ncol(dat1)] %>% c() %>% unlist()
  # dat1<-dat1[-2,]
  colnames(dat1) <- col_name
  dat2=dat1[-c(1),-c(1)]
  
  return(dat2)
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
  col_name <- dat1[1,-ncol(dat1)] %>% c() %>% unlist()
  # dat1<-dat1[-1,]
  colnames(dat1) <- col_name
  dat2=dat1[-c(1,2,4,5),-c(1)]
  
 
  
  df <- data.frame(time = as.numeric(dat2[,1]), val = as.numeric(dat2[,ind]))
  
  if(substr(y_lab, 1,6) == 't_ca+2'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0.198325,0.2098375,0.222185	,0.203005,0.2091775,0.2244225))
  }else if(substr(y_lab, 1,2) == 'pH'){
    df1 <- data.frame(time = seq(0,15,3), val = c(6.8,6.47,	6.57,6.46,6.56,6.75))
  }else if(substr(y_lab, 1,6) == 't_mg+2'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0.045182917,0.048233333,0.053695833,0.0529625,0.0586125,0.068358333))
  }else if(substr(y_lab, 1,5) == 't_na+'){
    df1 <- data.frame(time = seq(0,15,3), val = c(1.827425166,2.745295652,2.975786957,2.754017391,2.912430435,3.165004348))
  }else if(substr(y_lab, 1,10) == 't_sio2(aq)'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0.001785714,0.008282143,0.005103571,0.004457143,0.005385714,0.005471429))
  }else if(substr(y_lab, 1,6) == 't_fe+2'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0.000896057,0.00611828,0.003100358,0.001783154,0.001306452,0.000946237))
  }else if(substr(y_lab, 1,4) == 't_k+'){
    df1 <- data.frame(time = seq(0,15,3), val = c(0.016042244,0.022028205,0.02184359,0.017576923,0.018135897,0.019869231))
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
# save_plot('./pH.jpg', p1, ncol = 1, nrow = 1, base_asp = 1.618)
p2 <- plot_fun(filename = './co2d_tim.dat', ind = 10, y_lab = 't_ca+2')
# save_plot('./t_ca.jpg', p2, ncol = 1, nrow = 1, base_asp = 1.618)
p3 <- plot_fun(filename = './co2d_tim.dat', ind = 11, y_lab = 't_mg+2')
# save_plot('./t_mg.jpg', p3, ncol = 1, nrow = 1, base_asp = 1.618)
p4 <- plot_fun(filename = './co2d_tim.dat', ind = 12, y_lab = 't_na+')
# save_plot('./na.jpg', p4, ncol = 1, nrow = 1, base_asp = 1.618)
p5 <- plot_fun(filename = './co2d_tim.dat', ind = 14, y_lab = 't_fe+2')
# save_plot('./fe.jpg', p1, ncol = 1, nrow = 1, base_asp = 1.618)
p6 <- plot_fun(filename = './co2d_tim.dat', ind = 15, y_lab = 't_sio2(aq)')
# save_plot('./sio2.jpg', p1, ncol = 1, nrow = 1, base_asp = 1.618)
p7 <- plot_fun(filename = './co2d_tim.dat', ind = 13, y_lab = 't_k+')

all_one<-plot_grid(p2,p3,p4,p5,p6,p7,labels=c('b','c','d','e','f','g'))
# all_one<-plot_grid(p1,p2,p3,p4,p5,p6,p7,labels=c('a','b','c','d','e','f','g'))


save_plot('./co2d_tim.jpg', all_one, ncol = 1, nrow = 1, base_asp = 1.618)
all_one



