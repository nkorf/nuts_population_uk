rm(list=ls())
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(scales)
library(purrr)

pyramid_data_all <- readxl::read_excel("../data/unidemopublishing.xlsx",sheet = 4,skip = 9) 

colnames(pyramid_data_all) <- c("age","total","male","female")

pyramid_data_all |> 
  select(age, male,female) |> 
  filter(age!="TOTAL") |> 
  mutate(age = gsub("Y","",age)) |> 
  mutate(age = gsub("_MAX","",age)) |> 
  mutate(age = as.numeric(age)) |> 
  mutate(age_band = cut(age,breaks = seq(0, 100, 5), right = FALSE)) |> 
  select(age_band,male,female) |> 
  pivot_longer(-age_band,names_to="type",values_to="total_pop") |> 
  group_by(age_band,type) |> 
  summarise(total=sum(total_pop),.groups = "drop") |> 
  na.omit()  -> plotdata


levels_format <- levels(plotdata$age_band)
levels_format <- gsub("\\[|\\)","",levels_format)
levels_format <- gsub(",","-",levels_format)

plotdata$age_band <- factor(plotdata$age_band,
                            levels = levels(plotdata$age_band),
                            labels=levels_format)

# ggplot plot 

ggplot(plotdata,aes(x=age_band,y=total,fill=type))+
    geom_bar(data=subset(plotdata,type=="male"),stat="identity")+
    geom_bar(data=subset(plotdata,type=="female"),
             aes(x=age_band,y=-total),stat="identity")+
      coord_flip()+
    scale_fill_manual(name="",values=c("darkred","steelblue"),labels = c("Female","Male"))+
    scale_y_continuous(n.breaks = 8,labels = unit_format(unit = "M", scale = 1e-6))+
    geom_vline(xintercept = 0,linetype="solid")+
    cowplot::theme_cowplot()+
    labs(x="Totals",y="Age Band",title = "UK Population Pyramid",
         subtitle = paste("Total Population: ",format(sum(plotdata$total),big.mark = ",")),
         caption = "ONS Data - 2020\nhttp://github.com/nkorf")+
      theme(legend.position = "top",panel.grid = element_blank(),
          axis.line.x = element_line(),
          axis.text.x = element_text(vjust = .5),
          panel.grid.major.y = element_line(color = "lightgrey",linetype = "dashed"),
          legend.justification = "centre")->this_plot

#ggsave(plot = this_plot,filename = file.path("plots","all_uk.jpeg"), width = 6,height = 5,units = "in")


pyramid_data_nuts <- readxl::read_excel("../data/unidemopublishing.xlsx",
                                        sheet = 6,skip = 10) %>% 
  janitor::clean_names() %>% 
  rename(nuts_code = nuts2016, 
         nuts_label = x2) %>% 
  pivot_longer(-c(nuts_code,nuts_label),names_to="age_band",values_to="population") 

pyramid_data_nuts <- pyramid_data_nuts %>% 
 tidyr::separate(age_band,into = c("gender","sequence")) %>% 
  mutate(gender = case_when(
    gender=="t"~"total", 
    gender=="f"~"female", 
    gender=="m"~"male", 
    TRUE ~ NA
  )) %>% 
  mutate(sequence = as.integer(sequence)) %>% 
  group_by(nuts_code,nuts_label,gender) %>% 
  nest()

year_bands <- c("TOTAL","Y0_4","Y5_9",
                "Y10_14","Y15_19","Y20_24","Y25_29","Y30_34","Y35_39",
                "Y40_44",
                "Y45_49","Y50_54","Y55_59","Y60_64","Y65_69","Y70_74",
                "Y75_79","Y80_84","Y85_89","Y90_94","Y95_99","Y100_MAX")

pyramid_data_nuts <- pyramid_data_nuts %>% 
  mutate(data = map(data,~.x %>% 
                      mutate(age_band = year_bands) %>% 
                      select(age_band,population)
                    )) %>% 
  unnest()

# set factor levels 
pyramid_data_nuts$age_band <- factor(pyramid_data_nuts$age_band,levels=year_bands,
                                     labels = c("total","0-4","5-9","10-14","15-19","20-24",
                                                "25-29","30-34","35-39","40-44",
                                                "45-49","50-54","55-59","60-64","65-69",
                                                "70-74","75-79","80-84","85-89","90-94",
                                                "95-99",
                                                "100-Max"))

 pyramid_data_nuts %>% 
  filter(nuts_code =="UKH15") %>% 
  filter(gender !="total") %>% 
  filter(age_band!="total") -> norwich_data 
norwich_pyramid <-  ggplot()+
  geom_bar(data=subset(norwich_data,gender=="female"),
           aes(x=age_band,y=-population,fill=gender),stat="identity",position = "dodge")+
  geom_bar(data=subset(norwich_data,gender=="male"),
           aes(x=age_band,y=population,fill=gender),stat="identity",position = "dodge")+
    coord_flip()+
    scale_fill_manual(name="",values=c("darkred","steelblue"),labels = c("Female","Male"))+
 cowplot::theme_cowplot()+
    labs(x="Totals",y="Age Band",title = "Norwich Population Pyramid",
         subtitle = paste("Total Population: ",format(sum(norwich_data$population),big.mark = ",")),
         caption = "ONS Data - 2020\nhttp://github.com/nkorf")+
      theme(legend.position = "top",panel.grid = element_blank(),
          axis.line.x = element_line(),
          axis.text.x = element_text(vjust = .5),
          panel.grid.major.y = element_line(color = "lightgrey",linetype = "dashed"),
          legend.justification = "centre")


cowplot::save_plot("../plots/norwich_pyramid.jpeg",norwich_pyramid,base_width = 6,
                   base_height = 5)

plot_pyramid_nuts <- function(pyramid_data,nuts_code_filter,nuts_region_label){
   this_nuts_data <- pyramid_data %>% 
    filter(nuts_code ==nuts_code_filter) %>% 
    filter(gender !="total") %>% 
    filter(age_band!="total")   

   pyramid_plot <-  ggplot()+
        geom_bar(data=subset(this_nuts_data,gender=="female"),
           aes(x=age_band,y=-population,fill=gender),stat="identity",position = "dodge")+
        geom_bar(data=subset(this_nuts_data,gender=="male"),
           aes(x=age_band,y=population,fill=gender),stat="identity",position = "dodge")+
        coord_flip()+
        scale_fill_manual(name="",values=c("darkred","steelblue"),
                      labels = c("Female","Male"))+
        cowplot::theme_cowplot()+
        labs(y="Totals",x="Age Band",
            title = nuts_region_label,
            subtitle = paste("Total Population: ",format(sum(this_nuts_data$population),
                                                      big.mark = ",")),
         caption = "ONS Data - 2020\nhttp://github.com/nkorf")+
      theme(legend.position = "top",panel.grid = element_blank(),
          axis.line.x = element_line(),
          axis.text.x = element_text(vjust = .5),
          panel.grid.major.y = element_line(color = "lightgrey",linetype = "dashed"),
          legend.justification = "centre")
   
     return(pyramid_plot)
}

data_crawl <- pyramid_data_nuts %>% 
  ungroup() %>% 
  select(nuts_code,nuts_label) %>%
  unique() %>% 
  mutate(nuts_type = case_when(
    nchar(nuts_code) == 2 ~ 0,
    nchar(nuts_code) == 3 ~ 1,
    nchar(nuts_code) == 4 ~ 2,
    nchar(nuts_code) == 5 ~ 3,
    TRUE ~ NA
  ))


for(i in 1:nrow(data_crawl)){
  this_nuts_label <- data_crawl$nuts_label[i]
  this_nuts_code <- data_crawl$nuts_code[i]
  this_nuts_type <- data_crawl$nuts_type[i]
  print(paste("Fetching for ",data_crawl$nuts_label[i]))
  this_pyramid <- plot_pyramid_nuts(pyramid_data = pyramid_data_nuts,
                    nuts_code_filter = this_nuts_code, 
                    nuts_region_label = this_nuts_label)
  nuts_dir <- paste0("nuts_",this_nuts_type)
  plot_dir <- paste0("../plots/",nuts_dir,"/",this_nuts_code,".jpeg")
  
  cowplot::save_plot(plot_dir,
                     plot = this_pyramid,base_width = 6,base_height = 5)
  
  
  
}