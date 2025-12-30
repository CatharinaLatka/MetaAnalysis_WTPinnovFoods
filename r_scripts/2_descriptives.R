#################################
# "Descriptives"
#author: "Catharina Latka"
#################################

#load packages
library(glue)
library(tidyverse)
library(readxl)
library(gtsummary)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#read data
data_ori<-read_csv(glue("data/processed/data.csv"))


data_ori<-data_ori%>%mutate(product_type_nice=case_when(product_type=="plant_based"~"plant-based",
                                                product_type=="improved_animal_welfare"~"improved animal welfare",
                                                product_type=="improved_health"~"improved health",
                                                product_type=="improved_environmental_footprint"~"improved environment",
                                                product_type=="lab_gene_engin"~"lab/gene-engineered",
                                                TRUE~product_type))%>%
  mutate(continent_nice=case_when(continent=="North_America"~"North America",
                                  continent=="South_America"~"South America",
                                  TRUE~continent
                                  ))

length(unique(data_ori$study_ID))
#71

#drop missing years
data_ori<-data_ori%>%filter(!(is.na(first_year)))
length(unique(data_ori$study_ID))
#67 - 4 studies no first year dropped

data_ori<-data_ori%>%filter(!(is.na(relative_difference)))
length(unique(data_ori$study_ID))
#67


testmeta<-data_ori%>%filter(!(is.na(relative_SE_WTP)))
length(unique(testmeta$study_ID))
#59

#descriptives- consumer types
tbl_summary(data_ori%>%dplyr::select(consumer_type)%>%filter(consumer_type!="average")) %>% # build gtsummary table
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "data/output/html_out/descriptive_data_consumertypes.html"
  )



cons<-data_ori%>%filter(consumer_type!="average")
length(unique(cons$study_ID))
#13

#filter for averaged
data<-data_ori%>%filter(consumer_type=="average")
length(unique(data$study_ID))
#67



#write out numbers of studies with several estimates
test<-data%>%group_by(reference)%>%mutate(count=n())%>%ungroup()%>%filter(count>1)
length(unique(test$study_ID))
#30


test2<-data%>%dplyr::select(study_ID,reference,product,product_type,country,consumer_group_paper)%>%group_by(reference,product)%>%mutate(count=n())%>%ungroup()%>%filter(count>1)
#6 studies with different study regions (within country, within EU)

test3<-data%>%dplyr::select(study_ID,reference,product_type)%>%distinct()%>%group_by(reference)%>%mutate(count=n())%>%ungroup()%>%filter(count>1)
unique(test3$study_ID)
#14 show multiple product categories


#there are also studies with different products in same product category
test4<-data%>%dplyr::select(study_ID,reference,product)%>%distinct()%>%group_by(reference)%>%mutate(count=n())%>%ungroup()%>%filter(count>1)
unique(test4$study_ID)
#26 show multiple products




test5<-data%>%filter(is.na(SE_WTP))
unique(test5$study_ID)
#8 studies




#by continent
test<-data%>%dplyr::select(study_ID, continent)%>%distinct()%>%group_by(continent)%>%mutate(count=n())%>%ungroup()

#when
test<-data%>%dplyr::select(study_ID, first_year)%>%distinct()%>%mutate(after2010=if_else(first_year>2010,1,0))%>%group_by(after2010)%>%mutate(count=n())%>%ungroup()
#check count: 6 before/ in 2010, 61 after
61/67
#91%



#remove some unnstudy_ID#remove some unnecessary variables
data<-data%>%dplyr::select(-article,-product,-period_data_collection,-dependent_variable,-consumer_group_paper,-unit,-reference_product,
                          -group_share,-WTP_estimate,-WTP_adjusted,-WTP_OR_logit_coefficient,-SE_OR_SD,-price_WTP_reference,-study_ID,
                           -Price_estimate,-SE_price,-SE_WTP)




#descriptives
tbl_summary(data%>%dplyr::select(-consumer_type)) %>% # build gtsummary table
  as_gt() %>% # convert to gt table
  gt::gtsave( # save table as image
    filename = "data/output/html_out/descriptive_data.html"
  )

##list for appendix: products in groups
data_products<-data_ori%>%
  dplyr::select(product_type,product_detail,product)%>%
  distinct()%>%
  arrange(product_type,product_detail)

write_csv(data_products,file=glue("data/output/tables/product_mapping.csv"),col_names = TRUE)



#create indicator if SE present
data<-data%>%
  mutate(no_SE=if_else(is.na(relative_SE_WTP),"no SE estimate","with SE estimate"))

#create figures
product_figure<-ggplot(data,
                       aes(x=first_year,y=relative_difference,color=continent_nice))+
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1)+
  geom_point(aes(alpha= 1-scales::rescale(relative_SE_WTP),shape=no_SE,size=sample_size))+
  scale_shape_manual(values=c(3, 19)) +
  scale_alpha_continuous(range = c(0.1, 0.6))+ 
  theme_bw()+facet_wrap(~product_type_nice) +
  labs(x="Year",y="Marginal willingness to pay compared to conventional product",alpha="Estimate precision",shape="SE presence",size="sample size",color="Continent")

ggsave(glue("data/output/figures/descriptives_product_types_WTP.png"),product_figure,height=6,width=9,unit="in")


#motivation groups
data<-data%>%mutate(motivation=case_when(sustainability==1&health==0&animal_welfare==0~"sustainability",
                                         sustainability==0&health==1&animal_welfare==0~"health",
                                         sustainability==0&health==0&animal_welfare==1~"animal welfare",
                                         TRUE~"various"),
                    motivation_full=case_when(sustainability==1&health==0&animal_welfare==0~"sustainability",
                                         sustainability==0&health==1&animal_welfare==0~"health",
                                         sustainability==0&health==0&animal_welfare==1~"animal welfare",
                                         sustainability==1&health==1&animal_welfare==0~"sustainability:health",
                                         sustainability==0&health==1&animal_welfare==1~"health:animal welfare",
                                         sustainability==1&health==0&animal_welfare==1~"sustainability:animal welfare",
                                         TRUE~"all"))



motiv_WTP<-ggplot(data=data#%>%mutate(relative_difference=if_else(relative_difference<(-5),(-5),relative_difference))
                  ,
                  aes(x=motivation,y=relative_difference))+geom_boxplot()+geom_point(alpha=0.6, aes(color=product_type_nice))+
  theme_bw()+scale_color_viridis_d()+
  labs(x="Motivation",y="Marginal willingness to pay compared to conventional product",color="Product type")+ guides(color = guide_legend(reverse = TRUE))

ggsave(glue("data/output/figures/motivation_WTP_boxplot.png"),motiv_WTP,height=6,width=8,unit="in")


#add table with estimate
####complete references column and use references instead
data_studies<-data_ori%>%filter(consumer_type=="average")%>%dplyr::select(reference,relative_difference,product_type,relative_SE_WTP,product_detail,product_type_nice)%>%distinct()%>%
  group_by(reference)%>%
  mutate(group_id=row_number(),
         max_id=max(group_id))%>%
  ungroup()%>%
  mutate(group_id=as.character(group_id),group_id=if_else(max_id==1,"",paste0("_",group_id)))%>%
  mutate(study_estimate=paste0(reference,group_id))%>%
  mutate(study_estimate2=paste0(reference,group_id,"_",product_detail))%>%
  mutate(no_SE=if_else(is.na(relative_SE_WTP),"no SE estimate","with SE estimate"))
  
#order by product AND add product name
data_studies<-data_studies%>%
  group_by(product_type)%>%
  mutate(study_estimate=reorder(study_estimate,relative_difference))%>%
  mutate(study_estimate2=reorder(study_estimate2,relative_difference))%>%
  ungroup()


study_plot<-ggplot(data_studies,aes(x=relative_difference,y=study_estimate2,color=product_type_nice#,alpha=relative_SE_WTP
))+
  geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin = relative_difference - relative_SE_WTP, xmax = relative_difference + relative_SE_WTP))+
  #geom_point()+
  geom_point(size=2,alpha=0.5,aes(#alpha=relative_SE_WTP,
    shape=no_SE))+
  scale_shape_manual(values=c(3, 19)) +
  scale_color_viridis_d()+theme_bw()+#facet_wrap(~product_type)+
  labs(y="Study & estimate # & conventional animal product",x="Marginal willingness to pay compared to conventional product",color="Product type",shape="SE presence")+ guides(color = guide_legend(reverse = TRUE))

ggsave(glue("data/output/figures/study_plot.png"),study_plot,height=12,width=10,unit="in")


#figure for consumer types
cons<-cons%>%group_by(consumer_type)%>%mutate(count=n())%>%filter(count>5)%>%
  group_by(reference)%>%
  arrange(reference,consumer_type,product_type,,relative_difference)%>%
  mutate(group_id=row_number(),
         max_id=max(group_id))%>%
  ungroup()%>%
  mutate(group_id=as.character(group_id),group_id=if_else(max_id==1,"",paste0("_",group_id)))%>%
  mutate(study_estimate=paste0(reference,group_id))%>%
  mutate(study_estimate2=paste0(reference,group_id,"_",product_detail))%>%
  mutate(product_short=case_when(product_type=="insects"~"ins",
                                 product_type=="lab_gene_engin"~"lab",
                                 product_type=="plant_based"~"pla",
                                 product_type=="improved_animal_welfare"~"ani",
                                 product_type=="improved_environmental_footprint"~"env",
                                 product_type=="improved_health"~"hea"
  ))%>%
  #mutate(study_estimate2=paste0(reference,group_id,"_",product_short))%>%
  mutate(no_SE=if_else(is.na(relative_SE_WTP),"no SE estimate","with SE estimate"))
  
cons<-cons%>%
  group_by(product_type)%>%
  mutate(study_estimate=reorder(study_estimate,relative_difference))%>%
  mutate(study_estimate2=reorder(study_estimate2,relative_difference))%>%
  ungroup()

cons<-cons%>%
  mutate(consumer_type=case_when(
    consumer_type=="animal_welfare"~"animal welfare",
    consumer_type=="price_sensitive"~"price-sensitive",
    consumer_type=="technology_open"~"technology-open",
    TRUE~consumer_type ))
  
#for consumer groups with at least 6 observations
study_plot<-ggplot(cons,aes(x=relative_difference,y=study_estimate2,color=product_type_nice#,alpha=relative_SE_WTP
))+
  geom_vline(xintercept = 0)+
  geom_errorbarh(aes(xmin = relative_difference - relative_SE_WTP, xmax = relative_difference + relative_SE_WTP))+
  #geom_point()+
  geom_point(size=2,alpha=0.5,aes(#alpha=relative_SE_WTP,
    shape=no_SE))+
  scale_shape_manual(values=c(3, 19)) +
  scale_color_viridis_d()+theme_bw()+facet_wrap(~consumer_type,scales="free")+
  labs(y="Study & estimate # & conventional animal product",x="Marginal willingness to pay compared to conventional product",color="Product type",shape="SE presence")+
  theme(legend.position = "top")

ggsave(glue("data/output/figures/study_plot_consgroups.png"),study_plot,height=8,width=12,unit="in")

rm(list=ls())
gc()
