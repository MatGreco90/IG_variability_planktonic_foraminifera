library(patchwork)
library(ggstatsplot)
library(tidyverse)

#read n poymorphism sites data

n_poly_dat<-read_csv('pol_sites_data.csv')

# Panel A -----------------------------------------------------------------

#subset data excluding the benthic 
dat_cons_poly<-n_poly_dat %>% 
  select(clade,taxon_id, length_polymorph, region) %>%  
  distinct() %>%    
  filter(!clade=='Benthic Rotaliids') 

#store in ggplot object
pA<-ggbetweenstats(
  data  = dat_cons_poly,
  x     = clade,
  y     = length_polymorph,
  plot.type = "box",
  type = "np",
  centrality.plotting = FALSE,
  ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
  xlab='',
  ylab = 'n polymorphic sites',
  ggtheme = ggplot2::theme_classic(),
  ggplot.component =list(ggplot2::scale_color_manual(values=c('#76B7B2', '#BAB0AC','#EDC948','#B07AA1','#4E79A7')),
                         ggplot2::theme(
                           axis.text.x=element_text(face = 'bold',size = 12),
                           axis.title.y = element_text(face='bold', size=12)
                         ))
)

# Panel B -----------------------------------------------------------------

#store in ggplot object

pB<-n_poly_dat %>% 
  select(clade,taxon_id, length_polymorph, region) %>%  
  distinct() %>%
  filter(!clade=='Benthic Rotaliids') %>%  
  mutate(pol_len_class=cut(length_polymorph, breaks=c(0,1,3,5,10,112),
                           labels=c('1 bp','2-3 bp','3-5 bp','5-10 bp','>10 bp'))) %>% 
  group_by(clade, pol_len_class) %>% 
  add_count(name='clade_class_len') %>%  
  ungroup() %>% 
  select(clade, clade_class_len, pol_len_class) %>%
  distinct() %>% 
  ggplot(aes(pol_len_class,clade_class_len,fill=clade))+
  geom_col(col="black", width=0.5)+
  scale_fill_manual(values=c('#76B7B2', '#BAB0AC','#EDC948','#B07AA1','#4E79A7'))+
  theme_classic()+
  xlab('')+
  ylab('mutation events')+
  theme(axis.text.x=element_text(face = 'bold',size = 12, angle = 30, vjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face='bold', size=12),
        axis.title.y = element_text(face='bold', size=12),
        legend.position = 'none')

# Panel C -----------------------------------------------------------------

#read stem-loop ratio data

stem_loop_ratio_dat<-read_csv('slr_data.csv')

#store in ggplot object
pC<-ggbetweenstats(
  data  = stem_loop_ratio_dat
  ,
  x     = clade,
  y     = sl_ratio,
  plot.type = "box",
  type = "np",
  centrality.plotting = FALSE,
  ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
  xlab='',
  ylab = 'Stem-Loop ratio',
  ggtheme = ggplot2::theme_classic(),
  ggplot.component =list(ggplot2::scale_color_manual(values=c('#76B7B2', '#BAB0AC','#EDC948','#B07AA1','#4E79A7')),
                         ggplot2::theme(
                           axis.text.x=element_text(face = 'bold',size = 12),
                           axis.title.y = element_text(face='bold', size=12)
                         ))
)


# compose final plot ------------------------------------------------------

layout_a <- "
AABB
AABB
##CC
##CC"

pA+pB+pC+plot_layout(design = layout_a)
