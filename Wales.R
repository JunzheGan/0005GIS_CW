library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(geojsonio)

download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/Wales_lad_2011_gen_clipped.zip",
              destfile = "data/Welsh.zip")

listfiles<-dir_info(here::here("data")) %>%
  dplyr::filter(str_detect(path, ".zip")) %>%
  dplyr::select(path)%>%
  pull()%>%
  #print out the .gz file
  print()%>%
  as.character()%>%
  utils::unzip(exdir=here::here("data"))

WS<-dir_info(here::here("data"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "wales_lad_2011_gen_clipped.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()

qtm(WS)

Population_density <- read_csv("population_density.csv",
                         col_names = TRUE,
                         na = c("", "NA", "n/a"),
                         locale = locale(encoding = 'Latin1'))

#check all of the columns have been read in correctly
Datatypelist <- Population_density %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

Population_density <- WS%>%
  left_join(.,
            Population_density, 
            by = c("name" = "LAD/UA name"))

tmap_mode("view")
qtm(Population_density, 
    fill = "Density", 
    borders = NULL,  
    fill.palette = "Blues")




Income <- read_csv("totalannualincome2018.csv",
                   col_names = TRUE,
                   na = c("", "NA", "n/a"),
                   locale = locale(encoding = 'Latin1'))

Datatypelist3 <- Income %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist3

Income <- WS%>%
  left_join(.,
            Income, 
            by = c("name" = "Local authority name"))

tmap_mode("view")
qtm(Income, 
    fill = "Total annual income", 
    borders = NULL,  
    fill.palette = "Blues")

Income_qual<- read_csv("edu.csv",
                       col_names = TRUE,
                       na = c("", "NA", "n/a"))

Datatypelist4 <- Income_qual %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist4

Income_qual<- WS%>%
  left_join(.,
            Income_qual, 
            by = c("name" = "Area name"))



q <- qplot(x = `Level 4 and above`, 
           y = `Total annual income`, 
           data=Income_qual)
#plot with a regression line 
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


#run the linear regression model and store its outputs in an object called model1
Regressiondata<- Income_qual%>%
  clean_names()%>%
  dplyr::select(total_annual_income, 
                level_4_and_above)

#now model
model1 <- Regressiondata %>%
  lm(total_annual_income ~
       level_4_and_above,
     data=.)

summary(model1)

library(broom)
tidy(model1)
glance(model1)

install.packages("tidypredict")
library(tidypredict)
Regressiondata %>%
  tidypredict_to_column(model1)

#Bootstrap
Bootstrapdata<- Income_qual%>%
  clean_names()%>%
  dplyr::select(total_annual_income, 
                level_4_and_above)


library(rsample)
set.seed(99)

Income_boot <-st_drop_geometry(Bootstrapdata) %>%
  bootstraps(times = 1000, apparent = TRUE)


slice_tail(Income_boot, n=5)

Income_models <- Income_boot %>%
  #make new column
  mutate(
    #column name is model that contains...
    model = map(splits, ~ lm(total_annual_income ~ level_4_and_above, 
                             data = .)))

# let's look at the first model results
Income_models$model[[1]]
Income_models$model

Income_models_tidy <- Income_models %>%
  mutate(
    coef_info = map(model, tidy))

Income_coef <- Income_models_tidy %>%
  unnest(coef_info)
Income_coef

coef <- Income_coef %>% 
  filter(term == "level_4_and_above")
coef

#histogram
coef %>%
  ggplot(aes(x=estimate)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="lightblue2", col="lightblue3")+
  geom_vline(aes(xintercept=mean(estimate)),
             color="blue",
             linetype="dashed")+
  labs(title="Bootstrap resample estimates",
       x="Coefficient estimates",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

library(rsample)
int_pctl(Income_models_tidy, coef_info, alpha = 0.05)

Income_aug <- Income_models_tidy %>%
  #sample_n(5) %>%
  mutate(augmented = map(model, augment))%>%
  unnest(augmented)

length(Income_qual$`Total annual income`)

firstboot<-filter(Income_aug,id=="Bootstrap0001")

firstbootlength <- firstboot %>%
  dplyr::select(total_annual_income)%>%
  pull()%>%
  length()

firstboot$coef_info

uniquecoefficent <- firstboot %>%
  
  dplyr::select(coef_info)%>%
  unnest(coef_info)%>%
  distinct()

uniquecoefficent

ggplot(Income_aug, aes(level_4_and_above,
                     total_annual_income))+
  # we want our lines to be from the fitted column grouped by our bootstrap id
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "cyan3") +  
  # remember out apparent data is the original within the bootstrap
  geom_point(data=filter(Income_aug,id=="Apparent"))+
  #add some labels to x and y
  labs(x="education qualification at level 4 and above",
       y="total annual income")

Income_qual <- Income_qual %>%
  clean_names()

#let's check the distribution of these variables first

ggplot(Income_qual, aes(x=level_4_and_above)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(Income_qual, aes(x=total_annual_income)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 5000) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)

#residual check
#save the residuals into the dataframe

model_data <- model1 %>%
  augment(., Regressiondata)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram(binwidth = 2000)

#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model1)

DW <- durbinWatsonTest(model1)
tidy(DW)

Income_qual <- Income_qual %>%
  mutate(model1resids = residuals(model1))

tmap_mode("view")


tm_shape(Income_qual) +
  tm_polygons("model1resids",
              palette = "RdYlBu")

coordsW <- Income_qual%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)

L_nb <- Income_qual %>%
  poly2nb(., queen=T)

knn_s <-coordsW %>%
  knearneigh(., k=4)

L_knn <- knn_s %>%
  knn2nb()

#plot them
plot(L_nb, st_geometry(coordsW), col="red")
plot(L_knn, st_geometry(coordsW), col="blue")
plot(Income_qual)



Lward.knn_4_weight <- L_knn %>%
  nb2listw(., style="C")



Nearest_neighbour <- Income_qual %>%
  st_drop_geometry()%>%
  dplyr::select(model1resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()
Nearest_neighbour

model1 <- lm(total_annual_income ~ level_4_and_above + 
               data = Income_qual)

tidy(model1)