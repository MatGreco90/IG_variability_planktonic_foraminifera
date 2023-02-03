library(tidyverse)

#subset species of interest

clades<-c('Benthic Rotaliids','Microperforates','Non-Spinose','Spinose')

#import and plot data 

read_csv('pol_sites_data.csv') %>% 
  filter(!grepl('V9|45', region)) %>% 
  select(1:3,6,8) %>% 
  distinct() %>%
  filter(clade %in% clades) %>% 
  group_by(clade) %>% 
  mutate(median_pol_len=median(length_polymorph,na.rm = TRUE)) %>% 
  ungroup() %>%  
  ggplot(aes(reorder(clade,mean_pol_len),length_polymorph,fill=clade))+
  geom_boxplot(width=0.5,col='black',size=0.8)+
  scale_fill_manual(values=c( '#f28026','#76B7B2','#EDC948','#4E79A7'))+
  coord_flip()+
  xlab('')+
  ylab('n polymorphic sites')+
  theme_classic()+
  theme(axis.text.y = element_text(face='bold', size=12),
        axis.title.x = element_text(face='bold', size=12),
        legend.position = 'none')
