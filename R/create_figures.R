
library("rprojroot")
library(readr)
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(patchwork)
library(readr)
library(readxl)
library(lubridate)
library(qqplotr)
library(timeDate)
library(gridExtra)
library(grid)
set1 <- RColorBrewer::brewer.pal(7, "Set1")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")

# load datasets

d <- read_csv("Data/daily_births.csv")
kd <- read_csv("Data/caesarian.csv")

# parameters

height1 <- 5
width1 <- 7

# pic 1

p1 <- kd %>%  
  pivot_longer(cols = c("natural","caesarian")) %>% 
  ggplot(aes(year,value,color=name)) +
  geom_line(linewidth=1)+
  labs(x="Dagsetning", y="Fjöldi") + 
  scale_y_continuous(limits = c(0,NA),
                     expand = expansion(mult = c(0,0.1))) +
  scale_color_manual(values = c(set1[1],set1[2]),
                     labels = c("natural"="Eðlileg","caesarian"="Keisari")) +
  ggtitle(bquote("Árlegar fæðingar á Íslandi 1970-2020")) +
  guides(color=guide_legend(title="Gerð fæðingar:")) +
  theme_classic()  +
  theme(legend.position = "bottom")

ggsave(
  p1,
  filename = "Figures/p1.png",
  height = height1,
  width = width1
)

# pic 2

p2 <- kd %>% 
  ggplot(aes(year,ratio)) +
  geom_ribbon(aes(ymin = 10, ymax = 15,fill = set1[2]),alpha=.2) +
  geom_line(color=set1[3],size=1) +
  labs(x="Dagsetning", y="Hlutfall (%)") +
  scale_y_continuous(limits = c(0,NA),
                     expand = expansion(mult = c(0,0.1))) +
  scale_fill_identity(name = '', guide = 'legend',labels = c('10%-15% viðmið Alþjóðaheilbrigðismálastofnunarinnar')) +
  ggtitle(bquote("Hlutfall keisaraskurða við fæðingar á Íslandi 1970-2020"))+
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(
  p2,
  filename = "Figures/p2.png",
  height = height1,
  width = width1
)

# pic 3

p3 <- d %>% 
  ggplot(aes(x=date, y=births)) + 
  geom_point(color=set1[2],alpha=.2) +
  labs(x="Dagsetning", y="Fjöldi fæðinga") +
  ggtitle(bquote("Daglegar fæðingar á Íslandi 1990-2021"))+
  theme_classic()

ggsave(
  p3,
  filename = "Figures/p3.png",
  height = height1,
  width = width1
)

# pic 4

p4 <- d %>%
  group_by(day_of_year2) %>%
  summarise(meanbirths=mean(births)) %>%
  ggplot(aes(x=as.Date("2021-12-31")+day_of_year2, y=meanbirths)) +
  geom_point(color=set1[2],alpha=.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ggtitle(bquote("Meðalfjöldi fæðinga eftir dögum ársins"))+
  labs(x="Dagur árs", y="Meðalfjöldi fæðinga") +
  theme_classic()

ggsave(
  p4,
  filename = "Figures/p4.png",
  height = height1,
  width = width1
)

# pic 5

p5 <- d %>%
  group_by(day_of_week) %>%
  summarise(meanbirths=mean(births)) %>%
  ggplot(aes(x=day_of_week, y=meanbirths)) +
  geom_point(color=set1[2], size=4) +
  geom_line(color=set1[2], size=1) +
  geom_point( size=4,shape=1, color="black",alpha=.6) +
  scale_x_continuous(breaks = 1:7, 
                     labels=c('Mán','Þri','Mið','Fim','Fös','Lau','Sun')) +
  labs(x="Dagur vikunnar", y="Meðalfjöldi fæðinga") +
  ggtitle(bquote("Meðalfjöldi fæðinga eftir vikudögum"))+
  theme_classic()

ggsave(
  p5,
  filename = "Figures/p5.png",
  height = height1,
  width = width1
)

# pic 6

p6 <- d %>%
  mutate(`Tímabil`=case_when(year<=1999~"1990-1999",
                             year>=2000 & year<=2009~"2000-2009",
                             year>=2010 & year<=2021~"2010-2021")) %>% 
  group_by(day_of_week,`Tímabil`) %>%
  summarise(meanbirths=mean(births)) %>%
  ggplot(aes(x=day_of_week, y=meanbirths,color=`Tímabil`)) +
  geom_point( size=4) +
  geom_line(size=1) +
  geom_point( size=4,shape=1, color="black",alpha=.6) +
  scale_x_continuous(breaks = 1:7, 
                     labels=c('Mán','Þri','Mið','Fim','Fös','Lau','Sun')) +
  labs(x="Dagur vikunnar", y="Meðalfjöldi fæðinga") +
  scale_color_brewer()+
  ggtitle(bquote("Meðalfjöldi fæðinga eftir vikudögum yfir þrjú tímabil"))+
  guides(color=guide_legend(title="Tímabil:")) +
  theme_classic() +
  theme(legend.position="bottom")

ggsave(
  p6,
  filename = "Figures/p6.png",
  height = height1,
  width = width1
)

# run stan model

source("R/run_model.R")

scale_y <- c(0.02,0.02)

# pic 7 - 12

p7 <- d %>%
  mutate(Ef = Ef) %>%
  ggplot(aes(x=date, y=births_relative100)) +
  geom_point(color=set1[2], alpha=0.03) +
  geom_line(aes(y=Ef), color=set1[1], alpha=0.85) +
  geom_hline(yintercept=100, color='gray') +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = scale_y))+
  labs(x="",
       y="Hlutf. fjöldi fæðinga") +
  ggtitle("Allir þættir líkansins (f1+f2+f3+f4)") +
  theme_classic()

p8 <- d %>%
  mutate(Ef1 = Ef1) %>%
  ggplot(aes(x=date, y=births_relative100)) +
  geom_point(color=set1[2], alpha=0.03) +
  geom_line(aes(y=Ef1), color=set1[1],size=1,alpha=0.85) +
  geom_hline(yintercept=100, color='gray') +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = scale_y))+
  labs(x="",
       y="Hlutf. fjöldi fæðinga") +
  ggtitle("Langtímahneigð (f1)") +
  theme_classic()

p9 <- d %>%
  mutate(Ef2 = Ef2) %>%
  group_by(day_of_year2) %>%
  summarise(meanbirths=mean(births_relative100), meanEf2=mean(Ef2)) %>%
  ggplot(aes(x=as.Date("1991-12-31")+day_of_year2, y=meanbirths)) +
  geom_point(color=set1[2], alpha=0.3) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = scale_y))+
  scale_x_continuous(breaks=c(as.Date("1992-01-01"),as.Date("1992-02-01"),
                              as.Date("1992-03-01"),as.Date("1992-04-01"),
                              as.Date("1992-05-01"),as.Date("1992-06-01"),
                              as.Date("1992-07-01"),as.Date("1992-08-01"),
                              as.Date("1992-09-01"),as.Date("1992-10-01"),
                              as.Date("1992-11-01"),as.Date("1992-12-01"),
                              as.Date("1993-01-01")),
                     labels = c("Jan","Feb","Mar","Apr","Maí","Jún",
                                "Júl","Ágú","Sep","Okt","Nóv","Des","Jan")) +
  geom_line(aes(y=meanEf2), color=set1[1], size=1,alpha=0.85) +
  geom_hline(yintercept=100, color='gray') +
  labs(x="",
       y="Hlutf. fjöldi fæðinga") +
  ggtitle("Árstíðaráhrif (f2)") +
  theme_classic()

p10 <- ggplot(data=data.frame(x=rep(1:7,6),
                              y=Ef_day_of_week,
                              period=rep(1:6,each=7)),
              aes(period,y,color=as.factor(x),group=as.factor(x))) +
  scale_color_brewer(name="",
                     labels=c('Mán','Þri','Mið','Fim','Fös','Lau','Sun'),
                     palette = "Set2") +
  scale_x_continuous(breaks = 1:6,
                     labels=c("'90-'94","'95-'99","'00-'04",
                              "'05-'09","'10-'14","'15-'21"),
                     limits = c(.5,6.4)) +
  geom_line() +
  geom_point() +
  geom_point(shape=1, color="black",alpha=.6) +
  geom_hline(yintercept=100, color='gray') +
  annotate("text",
           x=6.4,
           y=Ef_day_of_week[36:42]+c(0,0,0,1,3,0,0),
           label=c('Mán','Þri','Mið','Fim','Fös','Lau','Sun')) +
  annotate("text",
           x=0.6,
           y=Ef_day_of_week[1:7]+c(-1,1,2.5,0.5,-1,0,0),
           label=c('Mán','Þri','Mið','Fim','Fös','Lau','Sun')) +
  labs(x="",
       y="Hlutf. fjöldi fæðinga") +
  theme_classic() +
  ggtitle("Vikudagaáhrif (f3)") +
  theme(legend.position = "none")

p11 <- data.frame(x=as.Date("1992-01-01")+0:365, y=Ef4float) %>%
  ggplot(aes(x=x,y=y)) + geom_line(color=set1[1]) +
  scale_x_continuous(breaks=c(as.Date("1992-01-01"),as.Date("1992-02-01"),
                              as.Date("1992-03-01"),as.Date("1992-04-01"),
                              as.Date("1992-05-01"),as.Date("1992-06-01"),
                              as.Date("1992-07-01"),as.Date("1992-08-01"),
                              as.Date("1992-09-01"),as.Date("1992-10-01"),
                              as.Date("1992-11-01"),as.Date("1992-12-01"),
                              as.Date("1993-01-01")),
                     labels = c("Jan","Feb","Mar","Apr","Maí","Jún",
                                "Júl","Ágú","Sep","Okt","Nóv","Des","Jan")) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0.05,0.02)))+
  scale_fill_manual() +
  geom_hline(yintercept=100, color='gray') +
  labs(x="",
       y="Hlutf. fjöldi fæðinga") +
  geom_point(data=dagar,aes(x,y,shape=type,colour=type),size= 2)+
  scale_shape_manual(name = "Sérstakir dagar:",
                     labels = c("Stórhátíðardagar",
                                "Sérstakir frídagar",
                                "Aðrir dagar"),
                     values = c(8, 4, 5)) +
  scale_colour_manual(name="Sérstakir dagar:",
                      labels=c("Stórhátíðardagar",
                               "Sérstakir frídagar",
                               "Aðrir dagar"),
                      values=c(set1[2], set1[3], "black")) +
  annotate("text",x=as.Date("1992-01-01"),y=Ef4float[1]-2,label="Nýársd.",size=dfs)+
  annotate("text",x=as.Date("1992-02-29"),y=Ef4float[60]-3,label="29. feb.",size=dfs)+
  annotate("text",x=as.Date("1992-04-19"),y=Ef4float[110]-2,label="Páskad.",size=dfs)+
  annotate("text",x=as.Date("1992-06-07"),y=Ef4float[159]-2,label="Hvítasunnud.",size=dfs)+
  annotate("text",x=as.Date("1992-05-12"),y=Ef4float[114],label="Sumard.f.",size=dfs)+
  annotate("text",x=as.Date("1992-05-15"),y=Ef4float[122],label="1. maí",size=dfs)+
  annotate("text",x=as.Date("1992-05-22"),y=Ef4float[149]-2,label="Uppst.d.",size=dfs)+
  annotate("text",x=as.Date("1992-06-17"),y=Ef4float[169]+4,label="17. júní",size=dfs)+
  annotate("text",x=as.Date("1992-08-03"),y=Ef4float[216]-2,label="Fríd. verzlunarm.",size=dfs)+
  annotate("text",x=as.Date("1992-11-20"),y=Ef4float[336]+4,label="Fullveldisd.",size=dfs)+
  annotate("text",x=as.Date("1992-12-04"),y=Ef4float[359],label="Aðfangad.",size=dfs) +
  annotate("text",x=as.Date("1992-12-24"),y=Ef4float[360]-2,label="Jólad.",size=dfs) +
  annotate("text",x=as.Date("1993-01-01"),y=Ef4float[366]+4,label="Gamlársd.",size=dfs) +
  theme_classic() +
  ggtitle("Áhrif daga ársins og fljótandi hátíðardaga (f4)") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 0, unit='cm'))

ggsave(
  p11+ggtitle("Áhrif daga ársins og fljótandi hátíðardaga"),
  filename = "Figures/p11.png",
  height = height1,
  width = width1
)

p12 <- grid.arrange(arrangeGrob(p7,p8,ncol=2),
                    arrangeGrob(p9,p10,ncol=2),
                    p11, 
                    nrow=3,
                    heights=c(1,1,1))

ggsave(
  p12,
  filename = "Figures/p12.png",
  height = 10,
  width = width1
)
  


