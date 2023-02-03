library(ggtree)
library(patchwork)
library(tidyverse)


#read incidence data 

incidence_by_region<-read_csv('ig_incidence_by_region.csv')

#read cloning effort data
cloning_effort<-read_csv('cloning_effort.csv')

# Non-Spinose panel -------------------------------------------------------

#read tree 
ns_tree<-read.tree('Non_Spinose_tree.tree')

# store ggtree object
ns_ggtree<-ggtree(ns_tree)+
  geom_tree(size=1.5)

#subset tree to sampled species
ns_vals<-get_taxa_name(ns_ggtree)

#subset incidence data
ns_incidence<-incidence_by_region %>% 
  select(ig_incidence,taxon_id,region, clade) %>% 
  filter(taxon_id %in% ns_vals) %>% 
  distinct() %>% 
  rename('var'='ig_incidence')

#subset cloning effort data
ns_cloning_effort<-cloning_effort %>% 
  filter(taxon_id %in% ns_vals) %>% 
  select(cloning_effort, taxon_id) %>% 
  mutate(region='Cloning effort',
         clade='Non-Spinose') %>% 
  rename('var'='cloning_effort')

#merge data

ns_merged_dat<-rbind(ns_incidence,ns_cloning_effort)

#store plot in ggplot object

ns_panel<-ggplot(ns_merged_dat,aes(var,taxon_id, fill=clade))+
  geom_vline(xintercept = 50,linetype='dotted')+
  geom_col(width=0.3, color='black')+
  scale_fill_manual(values = c('#EDC948','#4E79A7'))+
  scale_y_discrete(limits=rev(ns_vals))+
  facet_grid(~region)+
  xlab('')+
  ylab('')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.y = element_text(face='italic'))


# Spinose panel -----------------------------------------------------------

#read tree

sp_tree<-read.tree('Spinose_tree.tre')

#store in ggtree object
sp_ggtree<-ggtree(sp_tree)+
  geom_tree(size=1.5)

#subset tree to sampled species
sp_vals<-get_taxa_name(sp_ggtree)

#subset incidence dataset 
sp_incidence<-incidence_by_region %>% 
  select(ig_incidence,taxon_id,region, clade) %>% 
  filter(taxon_id %in% sp_vals) %>% 
  distinct() %>% 
  rename('var'='ig_incidence')

#subset cloning effort data
sp_cloning_effort<-cloning_effort %>% 
  filter(taxon_id %in% sp_vals) %>% 
  select(cloning_effort, taxon_id) %>% 
  mutate(region='Cloning effort',
         clade='Spinose') %>% 
  rename('var'='cloning_effort')

#merge data
sp_merged_dat<-rbind(sp_incidence,sp_cloning_effort) 

#store plot in ggplot object

sp_panel<-ggplot(sp_merged_dat,aes(var,taxon_id, fill=clade))+
  geom_vline(xintercept = 50,linetype='dotted')+
  geom_col(width=0.3, color='black')+
  scale_fill_manual(values = c('#4E79A7'))+
  scale_y_discrete(limits=rev(sp_vals))+
  facet_grid(~region)+
  xlab('')+
  ylab('')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.y = element_text(face='italic'))

# Microperforate panel ----------------------------------------------------

#read tree 
m_tree<-read.tree('Microperforate_tree.tre')

# store ggtree object
m_ggtree<-ggtree(m_tree)+
  geom_tree(size=1.5)

m_vals<-get_taxa_name(m_ggtree)

#subset incidence dataset 
m_incidence<-incidence_by_region %>% 
  select(ig_incidence,taxon_id,region, clade) %>% 
  filter(taxon_id %in% m_vals) %>% 
  distinct() %>% 
  rename('var'='ig_incidence')

#subset cloning effort data
m_cloning_effort<-cloning_effort %>% 
  filter(taxon_id %in% m_vals) %>% 
  select(cloning_effort, taxon_id) %>% 
  mutate(region='Cloning effort',
         clade='Microperforate') %>% 
  rename('var'='cloning_effort')

#merge data
m_merged_dat<-rbind(m_incidence,m_cloning_effort) 

#store plot in ggplot object
m_panel<-ggplot(m_merged_dat,aes(var,taxon_id, fill=clade))+
  geom_vline(xintercept = 50,linetype='dotted')+
  geom_col(width=0.3, color='black')+
  scale_fill_manual(values = '#76B7B2')+
  scale_y_discrete(limits=rev(m_vals))+
  facet_grid(~region)+
  xlab('')+
  ylab('')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.y = element_text(face='italic'))


# Bottom panel ------------------------------------------------------------

#extract other species
other_species<-c("Hastigerina_pelagica","Bolivina_variabilis", 
                 "Globigerinita_minuta", "Dentigloborotalia_anfracta")


#subset incidence dataset 
other_incidence<-incidence_by_region %>% 
  select(ig_incidence,taxon_id,region, clade) %>% 
  filter(taxon_id %in% other_species) %>% 
  distinct() %>%  
  rename('var'='ig_incidence')

#subset cloning effort data
other_cloning_effort<-cloning_effort %>% 
  filter(taxon_id %in% other_species) %>% 
  select(cloning_effort, taxon_id) %>% 
  mutate(region='Cloning effort',
         clade=ifelse(grepl('Bolivin|Dentiglobo', taxon_id),'Other',
                      ifelse(grepl('Hastigerina', taxon_id),
                             'Monolamellar','Microperforate'))) %>% 
  rename('var'='cloning_effort')

#merge data
other_merged_dat<-rbind(other_incidence,other_cloning_effort) 


#store plot in ggplot object
other_panel<-ggplot(other_merged_dat,aes(var,taxon_id, fill=clade))+
  geom_vline(xintercept = 50,linetype='dotted')+
  geom_col(width=0.3, color='black')+
  scale_fill_manual(values = c('#76B7B2','#BAB0AC','#B07AA1'))+
  scale_x_continuous(limits = c(0,100))+
  facet_grid(~region)+
  xlab('')+
  ylab('')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.y = element_text(face='italic'))


# compose final plot ------------------------------------------------------

layout_a <- "
ABBBBB
ABBBBB
CDDDDD
CDDDDD
EFFFFF
#GGGGG
"

sp_ggtree+sp_panel+ns_ggtree+ns_panel+
  m_ggtree+m_panel+ other_panel+
  plot_layout(design = layout_a)
