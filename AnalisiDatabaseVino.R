library(tidyverse)
library(ggplot2)
library(highcharter)
library(plotly)
library(viridisLite)
library(cowplot)
library(treemap)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(maps)
library(countrycode)
library(purrr)
library(htmltools)
library(knitr)


df <- read.csv('winemag-data-130k-v2.csv') #importo database


df %>% glimpse() #trasporre riche con colonne

group_category <- function(name, tops=FALSE, limit=30){    
  temp_df <- df
  
  if(tops){
    temp_df <- df %>% filter(points >= 90)
  }
  
  var_ = temp_df[ , (names(temp_df) %in% c(name))]
  
  value_data = temp_df %>% 
    select(name) %>%
    filter(name != '' & name != ' ') %>%
    mutate(x_var = var_) %>%
    group_by(x_var) %>%
    summarise(count = n())%>%
    arrange(desc(count)) %>%
    head(limit)
  
  return(value_data)
}

get_summary <- function(name){
  X_var = df[ , (names(df) %in% c(name))]
  
  
  summarise_df <- df %>% select(name) %>% summarise(
    mean_ = mean(X_var),
    median_ = median(X_var), 
    std = sd(X_var),
    min_ = min(X_var), 
    max_ = max(X_var),
    Q1_ = quantile(X_var, 0.25), 
    Q3_ = quantile(X_var, 0.75),
    iqr_range = Q3_ - Q1_
  )
  
  summarise_df <- summarise_df %>% select(min_, Q1_, iqr_range, mean_, median_, Q3_, max_, std) 
  return(summarise_df)
}

hpolar <- function(x, a, c, z, type_) { 
  highchart() %>% 
    hc_title(text = x) %>%     
    hc_xAxis(categories = a,
             tickmarkPlacement = "on",
             lineWidth = 0
    ) %>% 
    
    hc_yAxis(
      gridLineInterpolation = "polygon",
      lineWidth = 0
    ) %>% 
    
    hc_series(
      list(
        name = z,
        data = c,
        pointPlacement = "on",
        type = type_
      ),
      list(
        name = z,
        data = c,
        pointPlacement = "on",
        type = "line",
        color= 'black'
      )
      
    )
  
}                   

draw_tree_map <- function(value_data){
  tree_chart <- value_data %>%
    hchart(type="treemap", hcaes(x = x_var, value = count, color=count)) %>%
    hc_colorAxis(minColor = minColor, maxColor = maxColor, type = "logarithmic")
  
  htmltools::tagList(tree_chart)
}

draw_column_chart = function(name, title, bins){
  keeps <- c(name, 'price')
  X_var = df[ , (names(df) %in% c(name))]
  
  value_df <- df %>%
    select(name) %>%
    arrange(desc(name))
  
  
  measure_points <- value_df %>% mutate(
    var_ = as.numeric(X_var),
    count = length(var_),
    points_mean = mean(var_),
    points_median = median(var_), 
    std = sd(var_),
    points_min = min(var_), 
    points_max = max(var_)
  ) 
  
  measure_points <- measure_points %>% select(var_, count, points_min, points_mean, points_median, points_max, std)
  
  normal_chart <- measure_points %>%        
    ggplot(aes(x = var_))+
    geom_histogram(aes(y = ..density..), bins = bins)+
    labs(title = title, x = name, y = 'density')+
    geom_vline(aes(xintercept = points_mean),color = 'red', linetype = 'dashed')+
    geom_vline(aes(xintercept = points_median),color = 'blue', linetype = 'dashed')+
    stat_function(fun = dnorm, color = 'black', args = list(mean = mean(measure_points$var_), sd = sd(measure_points$var_)))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5))
  
  
  return(normal_chart)
  
}

##
knitr::kable(get_summary('points'))
draw_column_chart('points', 'Wine Points', bins=40)



## grafuco a barre come progetto

kable(get_summary('price'))  

price_df <- df %>%
  select(price) %>%
  count(price)
hchart(price_df$price, type = "column") 

## grafico a barre con modello ALTERNATIVO

x <- price_df$price
hchart(x)

price_states <- df %>%
  select(price,  ) %>%
  count(price)

###
data("citytemp")

x <- price_df$price
hchart(x)


prezzoNazione = df %>% 
  select(price, country) %>%               
  group_by(country)%>%
  summarise(Count = n())

prezzoNazioneItalia = df %>% 
  select(price, country) %>%               
  group_by(country = "Italy")%>%
  summarise(Count = n())

prezzoNazioneGermania = df %>% 
  select(price, country) %>%               
  group_by(country = "Germany")%>%
  
  
  value_data = df %>% 
  select(price, country) %>%               
  group_by(country) %>%
  summarise(Count = n(),
            Avg_Price = mean(price))


FR <- function(df){
  getState <- function(types){
    new_char_list = c(1, length(types))
    for (i in 1:length(types)) {
      type_ = types[i]
      
      new_char = 'other provinces'
      
      if(type_ == 'Burgundy'){
        new_char = 'Bourgogne-Franche-Comté'
      }  
      
      if(type_ == 'Loire Valley'){
        new_char = "Centre-Val de Loire"
      }
      
      if(type_ == "Languedoc-Roussillon"){
        new_char = 'Occitanie'   
      }      
      
      if(type_ == 'Bordeaux' | type_ == 'Southwest France'){
        new_char = "Nouvelle-Aquitaine"
      }
      
      if(type_ == 'Champagne' | type_ == 'Alsace'){
        new_char = "Grand Est"
      }
      
      if(type_ == 'Beaujolais' | type_ == 'Rhône Valley'){
        new_char = "Auvergne-Rhône-Alpes"
      }      
      
      new_char_list[i] = new_char 
      
    }
    
    return(new_char_list)
  }
  
  value_data <- df %>% 
    select(province, country) %>%
    filter(country == 'France') %>%
    mutate(province = getState(province)) %>%
    group_by(province) %>% 
    summarise(count = n())%>%
    arrange(desc(count))
  
  
}

FR(df)

##

value_data = df %>%
  select(title, price, points) %>%
  filter(title != '' & points == 100) %>%
  arrange(price) %>%
  head(15)

##

hpolar('Vini di alta qualita con punteggio pari a 100, ordinati dal meno caro al piu costoso', value_data$title, value_data$price, 'price', "column")

##

make_string_rating <- function(bool_var){
  interval_list = c(1, length(bool_var))
  for (i in 1:length(bool_var)) {
    var = as.numeric(bool_var[i])
    interval = "0"
    
    if(var >= 90){
      interval = "90 - 100 points"
    }
    
    else{
      interval = "80 - 90 points"
    }
    
    interval_list[i] = interval
  }
  
  return(interval_list)
}


blox_plot <- function(dati, var, y_title, x_title){
  max_y = nrow(var)
  dati <- data.frame(dati)
  varx = data.frame(var)
  varx2 = data.frame(var)
  plot <- data_to_boxplot(dati, varx, varx2, add_outliers = FALSE) %>%
    hc_chart(type = "bar") %>%
    hc_yAxis(title = list(text = y_title),
             labels = list(format = "$ {value}"), min = 0) %>%
    hc_xAxis(title = list(text = x_title),
             labels = list(format = "{value}")) %>%
    hc_add_theme(hc_theme_google())
  
  htmltools::tagList(plot)
}


blox_plot(df$price,df$points, 'Price', 'Points')




##

price <- scale(price_df, scale = TRUE)
points <- scale(points_df, scale = TRUE)


ggplot(df, aes(y=price, x=points)) + 
  geom_point(alpha = .5,  col = '#f52f32', size=2) + 
  stat_smooth(geom = "smooth", method = "lm", formula = y ~ poly(x,3),  col = 'darkgrey')+
  theme_minimal()                      


## top vini

library(recommenderlab)


rating_df <- df %>% select(taster_name, title, points) %>% filter(taster_name != '' & title != '') %>% mutate(points = as.numeric(points)) %>% drop_na()    
rating_df <- rating_df[!duplicated(rating_df[,c("taster_name", "title")]),]

rating_matrix <- rating_df
n_user_ratings = nrow(rating_matrix)


rating_matrix <- rating_matrix[n_user_ratings > 100]
set.seed(73)                                
pop_rec <- Recommender(data = rating_matrix, method = "POPULAR")
model_details <- getModel(pop_rec)

pop_predicted <- predict(pop_rec, rating_matrix, n = 60)
cat('Top 50 vini')                

#### plto

ggp <- ggplot(data = df, aes(x = price, y = df$points))

print(ggp)
  

(ggp <- ggplot(df, aes(x = price , fill = points) ) +
    geom_density(alpha = .5))

  

prezzo <- 500

#valori di punti


intervalloPunti1 <- 80
intervalloPunti2 <- 90 
intervalloPunti3 <- 90
intervalloPunti4 <- 100

valore_massimo <- df


dati_filtrati_intervallo1_2 <- subset(df, df$p< Lower )
dati_filtrati_intervallo3_4 <- subset(df, df$points> intervalloPunti3 & df$points< intervalloPunti4  )

dim(data_no_outlier)

ggp <- ggplot(dati_filtrati_intervallo3_4,  outliers=FALSE) +
  geom_boxplot(aes(x = "", y = price)) +
  coord_flip() +
  xlab("Intervallo di analisi dei punti 90 - 100")
print(ggp)


## peggiori vini con costo maggiore

peggiori_vini = df %>%
  select(title, price, points) %>%
  filter(title != '' & points <81) %>%
  arrange(desc(price)) %>%
  head(15)
kable(peggiori_vini)


## migori vini prezzo minore

migori_vini = df %>%
  select(title, price, points) %>%
  filter(price != 'NA' & title != '' & points >98) %>%
  arrange( price)%>%
head(15)
kable(migori_vini)

##miglori vini 
migori_vini = df %>%
  select(title, price, points) %>%
  filter(price != 'NA' & title != '' & points >99) %>%
  head(15)
kable(migori_vini)

##migloiri vini prezzo maggiore

peggiori_vini = df %>%
  select(title, price, points) %>%
  filter(price != 'NA' & title != '' & points >99) %>%
  arrange(desc(price)) %>%
  head(15)
kable(peggiori_vini)

## vini piu costosi

peggiori_vini = df %>%
  select(title, price, points) %>%
  filter(price != 'NA'  & title != '') %>%
  arrange(desc(price)) %>%
  head(15)
kable(peggiori_vini)


## vini meno costosi


peggiori_vini = df %>%
  select(title, price, points) %>%
  filter(price != 'NA'  & title != '') %>%
  arrange(price) %>%
  head(15)
kable(peggiori_vini)



# luogo con vino piu costo 


peggiori_vini = df %>%
  select(title, price, points, country) %>%
  filter(price != 'NA'  & title != '')

peggiori_vini %>%
  group_by(country) %>%
  summarise(media = median(price))%>%
  arrange(desc(media))




## fasce di prezzo o di punti

vini_piu_costosi = df %>%
  select(price, country) %>%
  filter(price != 'NA'& country !='') %>%
  arrange(desc(price))%>%
head(15)


  vini_piu_costosi %>%
  group_by(country)%>%
  summarise(media = mean(price))%>%
  arrange(desc(media))


  
  
##
  library(ggridges) 
  
  temp_df <- df %>% filter(points >=80)
  temp_df$points = as.character(temp_df$points)        
  ggplot(temp_df, 
         aes(y = points,
             x = price,
             fill= points,
             color=points)) +
    xlim(0, 500) +
    geom_density_ridges() + 
    theme_minimal() + 
    theme(legend.position = "none")

## punteggio top
  
  
  value_data = df %>%
    select(title, price, points) %>%
    filter(title != '' & points == 100) %>%
    arrange(price) %>%
    head(15)
  
  suppressWarnings(
    hpolar('Vini di alta qualità con punteggio pari a 100, ordinati in modo crescente rispetto al prezzo', value_data$title, value_data$price, 'Prezzo', "column")
  )

  ## Grafico varietà di vini
  
  value_data <- group_category('variety') 
  
  vini_piu_presenti = value_data %>%
    select(x_var, count)%>%
    filter(count > 2750)
  
  pie3D(vini_piu_presenti$count, mar = rep(0.7, 4),
        labels = vini_piu_presenti$x_var,
        explode = 0,
        labelcex = 0.5)
  
  
##
  
  
  
  
  price_df <- df %>%
    select(price) %>%
    count(price) 
  
  custom_colours = c("#20B2AA", "#008080" )
  
  x <- price_df$price
  hchart(x)%>%
    hc_colors(custom_colours)
  

