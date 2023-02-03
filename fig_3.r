library(phytools)
library(ggtree)
library(patchwork)
library(tidyverse)

n_poly_dat<-read_csv('pol_sites_data.csv')

incidence_by_region<-read_csv('ig_incidence_by_region.csv')


# ancestral state reconstruction in Microperforate -------------------------

#read tree 
m_tree<-read.tree('Microperforate_tree.tre')

#subset data

m_phy_dat<-n_poly_dat %>%
  select(clade,taxon_id, length_polymorph, region) %>%
  distinct() %>%
  group_by(taxon_id) %>%
  summarise(median_pol_sites=median(length_polymorph)) %>%
  filter(taxon_id %in% m_tree$tip.label) %>%
  column_to_rownames('taxon_id') %>%
  as.data.frame()

#prepare data for ancestral state analysis

m_svl<-m_phy_dat%>%
  select(1) %>%
  as.matrix(.)

m_fit<-fastAnc(m_tree,m_svl,vars=TRUE,CI=TRUE)

m_td <- data.frame(node = nodeid(m_tree, row.names(m_svl)),
                      trait = m_svl)
m_nd <- data.frame(node = names(m_fit$ace), median_pol_sites = m_fit$ace)

m_df <- rbind(m_td, m_nd)

m_df$node <- as.numeric(m_df$node)

m_tree_dat <- full_join(m_tree, m_df, by = 'node')

#store results in ggtree object

m_ggtree<-ggtree(m_tree_dat,size=2.2) +
  geom_tree(aes(color=median_pol_sites), continuous = 'colour', size=1.5) +
  xlim(0,2)+
  scale_color_distiller(palette = "Spectral",limits=c(0, 11))+
  geom_tiplab( fontface='italic') +
  labs(col='Mps')+
  theme(legend.direction = "vertical") 


# ancestral state reconstruction in Spinose --------------------------------

#spinose tree
sp_tree<-read.tree('Spinose_tree.tre')

#subset data

sp_phy_dat<-n_poly_dat %>%
  select(clade,taxon_id, length_polymorph,
         region) %>% #add data for G. conglobatus
  add_row(clade = 'Spinose',
          taxon_id = 'Globigerinoides_conglobatus',
          length_polymorph=0,
          region='37f') %>%
  add_row(clade = 'Spinose',
          taxon_id = 'Globigerinoides_conglobatus',
          length_polymorph=0,
          region='41f') %>% 
  add_row(clade = 'Spinose',
          taxon_id = 'Globigerinoides_conglobatus',
          length_polymorph=0,
          region='43e') %>% 
  add_row(clade = 'Spinose',
          taxon_id = 'Globigerinoides_conglobatus',
          length_polymorph=0,
          region='45e_47f') %>% 
add_row(clade = 'Spinose',
        taxon_id = 'Globigerinoides_conglobatus',
        length_polymorph=0,
        region='49e (V9)') %>% 
  filter(taxon_id %in% sp_tree$tip.label) %>% 
  distinct() %>% 
  mutate(length_polymorph=ifelse(is.na(length_polymorph),0, length_polymorph)) %>%   
  group_by(taxon_id) %>%
  summarise(median_pol_sites=median(length_polymorph)) %>%  
  column_to_rownames('taxon_id') %>%
  as.data.frame()

#prepare data for ancestral state analysis

sp_svl<-sp_phy_dat%>% 
  select(1) %>% 
  as.matrix(.)

sp_fit<-fastAnc(sp_tree,sp_svl,vars=TRUE,CI=TRUE)

sp_td<- data.frame(node = nodeid(sp_tree, row.names(sp_svl)),
                   trait = sp_svl)
sp_nd<- data.frame(node = names(sp_fit$ace), median_pol_sites = sp_fit$ace)

sp_df <- rbind(sp_td, sp_nd)
sp_df$node <- as.numeric(sp_df$node)

sp_tree_dat <- full_join(sp_tree, sp_df, by = 'node')

#store results in ggtree object

sp_ggtree<-ggtree(sp_tree_dat, size=2.2) + 
  geom_tree(aes(color=median_pol_sites), continuous = 'colour', size=1.5) +
  xlim(NA, 90)+
  scale_color_distiller(palette = "Spectral",limits=c(0, 11))+
  geom_tiplab( fontface='italic') +
  labs(col='Mps')+
  theme(legend.position = "none") 


# ancestral state reconstruction in Non-Spinose ----------------------------

#read tree
ns_tree<-read.tree('Non_Spinose_tree.tre')

#subset data

ns_phy_dat<-n_poly_dat %>%
  select(clade,taxon_id, length_polymorph, region) %>%
  distinct() %>%
  group_by(taxon_id) %>%
  summarise(median_pol_sites=median(length_polymorph)) %>%
  filter(taxon_id %in% ns_tree$tip.label) %>%
  column_to_rownames('taxon_id') %>%
  as.data.frame()

#prepare data for ancestral state analysis

ns_svl<-ns_phy_dat %>% 
  select(1) %>% 
  as.matrix(.)

ns_fit<-fastAnc(ns_tree,ns_svl,vars=TRUE,CI=TRUE)

ns_td <- data.frame(node = nodeid(ns_tree, row.names(ns_svl)),
                     trait = ns_svl)

ns_nd <- data.frame(node = names(ns_fit$ace), median_pol_sites = ns_fit$ace)

ns_df<- rbind(ns_td, ns_nd)

ns_df$node <- as.numeric(ns_df$node)
ns_tree_dat <- full_join(ns_tree, ns_df, by = 'node')

#store results in ggtree object

ns_ggtree<-ggtree(ns_tree_dat, size=2.2) + 
  geom_tree(aes(color=median_pol_sites), continuous = 'colour', size=1.5) +
  xlim(NA, 90)+
  scale_color_distiller(palette = "Spectral",limits=c(0, 11))+
  geom_tiplab( fontface='italic') +
  labs(col='Length polymorphism')+
  theme(legend.position = "none") 


# Compose plot ------------------------------------------------------------


m_ggtree+ns_ggtree+m_ggtree+
  plot_layout(widths = c(1.5, 1.5,0.5),guides = 'collect')

