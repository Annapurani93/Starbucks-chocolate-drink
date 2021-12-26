library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(png)
tuesdata <- tidytuesdayR::tt_load('2021-12-21')
tuesdata$starbucks->starbucks
glimpse(starbucks)

starbucks%>%
  filter(product_name=="White Chocolate Mocha"|
           product_name=="Iced White Chocolate Mocha"|
           product_name=="Chocolate Smoothie"|
           product_name=="Double Chocolaty Chip Crème Frappuccino Blended"|
           product_name=="Hot Chocolate"|
           product_name=="Skinny Hot Chocolate"|
           product_name=="White Hot Chocolate")%>%
  select(product_name,size,milk,whip,calories,total_fat_g,saturated_fat_g,trans_fat_g,
         cholesterol_mg,sodium_mg,total_carbs_g,fiber_g,sugar_g,caffeine_mg)%>%
  filter(whip==1&
           milk==5)->data

data$trans_fat_g<-as.numeric(data$trans_fat_g)
data$fiber_g<-as.numeric(data$fiber_g)

data%>%
  select(-c(milk,whip,saturated_fat_g,trans_fat_g))%>%
  rowwise()%>%
  mutate(Total=sum(total_fat_g,cholesterol_mg,sodium_mg, total_carbs_g,fiber_g,sugar_g,caffeine_mg))%>%
  rowwise()%>%
  mutate(fatp=round(total_fat_g/Total,3)*100, chp=round(cholesterol_mg/Total,3)*100,
         sp=round(sodium_mg/Total,3)*100, tcp=round(total_carbs_g/Total,3)*100,
         fp=round(fiber_g/Total,3)*100,sugp=round(sugar_g/Total,3)*100,
         cfp=round(caffeine_mg/Total,3)*100)%>%
  select(-c(total_fat_g,cholesterol_mg,sodium_mg, total_carbs_g,fiber_g,sugar_g,caffeine_mg, Total))%>%
  rowwise()%>%
  mutate(Others=sum(fatp,fp,sugp,cfp))%>%
  select(-c(fatp,fp,sugp,cfp))%>%
  gather("Component","Share",4:7)->data1
  
data1%>%
  filter(size=="grande")->data11

hsize=2.5

ggplot(data11, aes(x = hsize, y = Share, fill = Component)) +
  geom_col(color="black") +
  geom_text(aes(label = paste0(Share,"%")),size=2.5,colour="white",fontface="bold",
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5))+
  facet_wrap(~product_name, nrow = 1,labeller = label_wrap_gen())+
  scale_fill_manual(values = c("#0b421a", "#eac784",
                               "#362415", "#604c4c"), 
                    labels=c("Cholestrol","Others (fat, fiber, sugar, caffeine)","Sodium","Carbohydrates"))+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="white"),
        plot.background=element_rect(fill="white"),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1.5,1,1.5),"cm"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour="black",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.background = element_rect(fill="white"),
        legend.key = element_rect(fill="white"),
        legend.text=element_text(colour="black",face="bold"),        
        legend.title = element_blank(),
        legend.position = "top",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=18,face="bold",color="black",margin=margin(b=15)),
        plot.subtitle = element_text(size=14, colour="black",margin=margin(b=21)),
        plot.caption = element_text(size=12, colour="black",hjust=0,margin=margin(t=30)))+  
  theme(panel.spacing = unit(3, "lines"))+
  labs(title="WHAT ARE YOUR FAVORITE CHOCOLATE DRINKS AT STARBUCKS MADE OF?",
       subtitle = str_wrap("The below visualization shows the share of caffeine, carbohydrates, cholestrol, fat, fiber, sodium and sugar, in a whole milk, with whip, grande mug",120),
       caption="Data: PythonCoderUnicorn via Tidy Tuesday| Design and Analysis: @annapurani93")->plot
       
       
readPNG("C:/Users/Annapurani/Desktop/starbucks logo.png")->logo


ggdraw(plot) +
  draw_image(logo, x = -0.382, y =-0.09, scale = .12) +
  draw_image(logo, x = -0.19, y =-0.09, scale = .12)+
  draw_image(logo, x = 0.001, y =-0.09, scale = .12)+
  draw_image(logo, x = 0.193, y =-0.09, scale = .12)+
  draw_image(logo, x = 0.385, y =-0.09, scale = .12)->plot1
  
ggsave("Starbuckschoc.png",plot1,width=15,height=9)




