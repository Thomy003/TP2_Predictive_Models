#cargamos los dataframes para trabajar
load("tp2.Rdata")


#LIBRERIAS ----

#instalamos la libreria partree y remotes desde github
#install.packages("remotes")
#remotes::install_github("grantmcdermott/parttree")
library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(parttree)
library(class)
library(readr)
library(knitr)
library(kableExtra)

#variable ----
v_dias_feriados <- c("2022-01-01", "2022-02-28", "2022-03-01", "2022-03-24", "2022-04-02", "2022-04-14", "2022-04-15", "2022-04-16", "2022-04-17", "2022-04-22", "2022-04-23", "2022-04-24", "2022-05-01", "2022-05-02", "2022-05-18", "2022-05-25", "2022-06-17", "2022-06-20", "2022-07-09", "2022-07-30", "2022-08-15", "2022-09-26", "2022-09-27", "2022-10-05", "2022-10-07", "2022-10-10", "2022-11-20", "2022-11-21", "2022-12-08", "2022-12-09", "2022-12-25" )
v_dias_no_laborales <- c("Saturday", "Sunday")

#DATAFRAMES ----
#agregar feriados
clima_ecobici_lluvia_dialaboral <- clima_ecobici %>% 
  mutate(llovio = (prcp > 0), 
         dia_feriado = (as.character(date) %in% v_dias_feriados), 
         dia_finde = (weekdays(date) %in% v_dias_no_laborales),
         dia_laboral = !(dia_feriado | dia_finde),
         tavg_cuadrado = tavg**2,
         tmin_cuadrado = tmin**2)


#FUNCIONES ----
observations_bike <- sample(x = nrow(clima_ecobici_lluvia_dialaboral), 
                            size = nrow(clima_ecobici_lluvia_dialaboral) * 0.8,
                            replace = F)

df_train_bikes <- clima_ecobici_lluvia_dialaboral[observations_bike,]
df_test_bikes <- clima_ecobici_lluvia_dialaboral[-observations_bike,]

observations_news <- sample(x = nrow(fake_news), 
                            size = nrow(fake_news) * 0.8,
                            replace = F)

df_train_news <- fake_news[observations_news,]
df_test_news <- fake_news[-observations_news,]


function_df_train_test_bike <- function(se){
  set.seed(se)
  observations_bike <- sample(x = nrow(clima_ecobici_lluvia_dialaboral), 
                              size = nrow(clima_ecobici_lluvia_dialaboral) * 0.8,
                              replace = F)
  
  df_train_bikes <- clima_ecobici_lluvia_dialaboral[observations_bike,]
  df_test_bikes <- clima_ecobici_lluvia_dialaboral[-observations_bike,]
}


function_df_train_test_fake_news <- function(df, se){
  set.seed(se)
  observations_news <- sample(x = nrow(fake_news), 
                              size = nrow(fake_news) * 0.8,
                              replace = F)
  
  df_train_news <- fake_news[observations_news,]
  df_test_news <- fake_news[-observations_news,]
}


#ANALISIS EXPLORATORIO REGRESION ----

#uso de bicis segun temp promedio
gr_bike_uses_tavg_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = tavg, y = n,  colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_y_continuous( breaks = c(2000,4000,6000,8000,10000,12000,14000),
                      labels = c(2000,4000,6000,8000,10000,12000,14000)) +
  scale_colour_manual(values = c("#ffa69e", "#6096ba"), labels= c('Dia no Laboral', 'Dia Laboral')) +
  labs(title = "Viajes según temperatura promedio",
       x = "Temperatura Promedio",
       y = "Cantidad de viajes",
       colour = "Tipo de Día:")

gr_bike_uses_tmin_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = tmin, y = n,  colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_y_continuous( breaks = c(2000,4000,6000,8000,10000,12000,14000),
                      labels = c(2000,4000,6000,8000,10000,12000,14000)) +
  scale_colour_manual(values = c("#ffa69e", "#6096ba"), labels= c('Dia no Laboral', 'Dia Laboral')) +
  labs(title = "Viajes según temperatura mínima",
       x = "Temperatura Mínima",
       y = "Cantidad de viajes",
       colour = "Tipo de Día:")

gr_bike_uses_tmax_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = tmax, y = n,  colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_y_continuous( breaks = c(2000,4000,6000,8000,10000,12000,14000),
                      labels = c(2000,4000,6000,8000,10000,12000,14000)) +
  scale_colour_manual(values = c("#ffa69e", "#6096ba"), labels= c('Dia no Laboral', 'Dia Laboral')) +
  labs(title = "Viajes según temperatura máxima",
       x = "Temperatura Máxima",
       y = "Cantidad de viajes",
       colour = "Tipo de Día:")


#uso de bicis segun precipitacion promedio         
ggplot(clima_ecobici_lluvia_dialaboral[-58,], mapping = aes(x = prcp, y = n, colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F)

#uso de bicis segun si llovio
gr_bike_uses_rain_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = llovio, y = n, col = dia_laboral)) +
  geom_boxplot() + 
  scale_color_discrete(labels= c('Dia no Laboral', 'Dia Laboral')) +
  scale_x_discrete(labels = c("No llovió", "llovió")) +
  labs(title = "Viajes durante días de lluvia",
       x = "",
       y = "Cantidad de viajes",
       col = "Tipo de Día:")


#uso de bicis segun presion
ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = pres, y = n, colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F)


#uso de bicis segun velocidad del viento
ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = wspd, y = n, colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F)


#uso de bicis segun dia laborable
gr_bike_uses_by_workdays <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = dia_laboral, y = n)) +
  geom_boxplot(col = c("#606c38", "#dda15e")) +
  scale_x_discrete(labels = c("Dia no laboral", "Dia laboral")) +
  scale_y_continuous( breaks = c(2000,4000,6000,8000,10000,12000,14000),
    labels = c(2000,4000,6000,8000,10000,12000,14000)) +
  labs(title = "Viajes de bicis - Días laborales VS Días no laborales",
       y = "Cantidad de viajes",
       x = "") 





#MODELO DE REGRESION ----

#Apartir del analisis exploratorio las 3 varaibles que nos resultaron interesantes de analisar fueron temperatura, lluvia y dia laboral

linear_model_bikes_1 <- lm(data = df_train_bikes, formula = n ~ llovio * dia_laboral)
coef_linear_model_bikes_1 <- coef(linear_model_bikes_1)

linear_model_bikes_4 <- lm(data = df_train_bikes, formula = n ~ tavg + tavg_cuadrado + dia_laboral)
coef_linear_model_bikes_4 <- coef(linear_model_bikes_4)

linear_model_bikes_5 <- lm(data = df_train_bikes, formula = n ~ tmin + tmin_cuadrado + dia_laboral)
coef_linear_model_bikes_5 <- coef(linear_model_bikes_5)

linear_model_bikes_6 <- lm(data = df_train_bikes, formula = n ~ tmax + tavg_cuadrado + dia_laboral)
coef_linear_model_bikes_6 <- coef(linear_model_bikes_6)

linear_model_bikes_7 <- lm(data = df_train_bikes, formula = n ~ tavg * tavg_cuadrado * dia_laboral)
coef_linear_model_bikes_7 <- coef(linear_model_bikes_7)

#buscar alguna visualizacion que me permita ver si debo usar tavg + tavg_cuadrado + dia_laboral, tavg * tavg_cuadrado + dia_laboral, tavg + tavg_cuadrado * dia_laboral o tavg * tavg_cuadrado * dia_laboral

#calculamos el error cuadratico medio de cada modelo para distintos tests

df_errores_all <- data.frame("type" = c(), "val" = c())

for(i in 1:500){
  set.seed(i)
  observations_bike <- sample(x = nrow(clima_ecobici_lluvia_dialaboral), 
                              size = nrow(clima_ecobici_lluvia_dialaboral) * 0.8,
                              replace = F)
  
  df_train_bikes <- clima_ecobici_lluvia_dialaboral[observations_bike,]
  df_test_bikes <- clima_ecobici_lluvia_dialaboral[-observations_bike,]
  
  
  df_test_bikes$predicciones <- coef_linear_model_bikes_1[1] + 
    as.numeric(df_test_bikes$llovio == TRUE) * coef_linear_model_bikes_1[2] + 
    coef_linear_model_bikes_1[3] * as.numeric(df_test_bikes$dia_laboral == T) +
    coef_linear_model_bikes_1[4] * as.numeric(df_test_bikes$dia_laboral == T) * as.numeric(df_test_bikes$llovio == TRUE)
  
  error_cuadratico_medio_1 <- sum((df_test_bikes$n - df_test_bikes$predicciones)**2) / nrow(df_test_bikes)
  v_error_cuadratico_medio_1 <- c(error_cuadratico_medio_1)
  
  
  df_test_bikes$predicciones_4 <- coef_linear_model_bikes_4[1] + 
    df_test_bikes$tavg * coef_linear_model_bikes_4[2] + 
    coef_linear_model_bikes_4[3] *  (df_test_bikes$tavg)**2 +
    coef_linear_model_bikes_4[4] * as.numeric(df_test_bikes$dia_laboral == T)
  
  error_cuadratico_medio_4 <- sum((df_test_bikes$n - df_test_bikes$predicciones_4)**2) / nrow(df_test_bikes)
  
  
  df_test_bikes$predicciones_5 <- coef_linear_model_bikes_5[1] + 
    df_test_bikes$tmin * coef_linear_model_bikes_5[2] + 
    coef_linear_model_bikes_5[3] *  (df_test_bikes$tmin)**2 +
    coef_linear_model_bikes_5[4] * as.numeric(df_test_bikes$dia_laboral == T)
    
    
  error_cuadratico_medio_5 <- sum((df_test_bikes$n - df_test_bikes$predicciones_5)**2) / nrow(df_test_bikes)
  
  
  df_test_bikes$predicciones_6 <- coef_linear_model_bikes_6[1] + 
    df_test_bikes$tmax * coef_linear_model_bikes_6[2] + 
    coef_linear_model_bikes_6[3] *  (df_test_bikes$tmax)**2 +
    coef_linear_model_bikes_6[4] * as.numeric(df_test_bikes$dia_laboral == T)
  
  
  error_cuadratico_medio_6 <- sum((df_test_bikes$n - df_test_bikes$predicciones_6)**2) / nrow(df_test_bikes)
  
  df_test_bikes$predicciones_7 <- coef_linear_model_bikes_7[1] + 
    df_test_bikes$tavg * coef_linear_model_bikes_7[2] + 
    coef_linear_model_bikes_7[3] *  (df_test_bikes$tavg)**2 +
    coef_linear_model_bikes_7[4] * as.numeric(df_test_bikes$dia_laboral == T) +
    coef_linear_model_bikes_7[5] * (df_test_bikes$tavg)**2 * (df_test_bikes$tavg) +
    coef_linear_model_bikes_7[6] * as.numeric(df_test_bikes$dia_laboral == T) * (df_test_bikes$tavg) +
    coef_linear_model_bikes_7[7] * as.numeric(df_test_bikes$dia_laboral == T) * (df_test_bikes$tavg)**2 +
    coef_linear_model_bikes_7[8] * as.numeric(df_test_bikes$dia_laboral == T) * (df_test_bikes$tavg)**2 * (df_test_bikes$tavg)
  
  error_cuadratico_medio_7 <- sum((df_test_bikes$n - df_test_bikes$predicciones_7)**2) / nrow(df_test_bikes)
  
  
  df_errores_all <- rbind(df_errores_all, 
                      data.frame("type" = c("lluvia-dialab", 
                                            "tavg-dialab", 
                                            "tmin-dialab", 
                                            "tmax-dialab"),
                                 "val" = c(error_cuadratico_medio_1,
                                           error_cuadratico_medio_4,
                                           error_cuadratico_medio_5,
                                           error_cuadratico_medio_6)))
}

gr_mean_squared_error_by_model <- ggplot(df_errores_all, mapping = aes(x = type, y = val, colour = type)) +
  geom_boxplot() +
  scale_colour_discrete(labels = c("Lluvia-\nEs_DiaLaboral",
                                   "TempProm-\nEs_DiaLaboral",
                                   "TempMáx-\nEs_DiaLaboral",
                                   "TempMín-\nEs_DiaLaboral")) +
  theme(axis.text.x = element_blank(), #removes the labels
        axis.ticks.x = element_blank()) + #removes de ticks in the x axis
  labs(title = "Error Cuadratico Medio por Modelo",
       x = "Modelo",
       y = "Error Medido (MSE)",
       colour = "Modelo:")

#notamos que tavg es levemente mejor


#pero podemos mejorar la ecuacion del tavg
df_errores <- data.frame("type" = c(), "val" = c())

for(i in 1:500){
  set.seed(i)
  observations_bike <- sample(x = nrow(clima_ecobici_lluvia_dialaboral), 
                              size = nrow(clima_ecobici_lluvia_dialaboral) * 0.8,
                              replace = F)
  
  df_train_bikes <- clima_ecobici_lluvia_dialaboral[observations_bike,]
  df_test_bikes <- clima_ecobici_lluvia_dialaboral[-observations_bike,]
  
  
  
  
  df_test_bikes$predicciones_4 <- coef_linear_model_bikes_4[1] + 
    df_test_bikes$tavg * coef_linear_model_bikes_4[2] + 
    coef_linear_model_bikes_4[3] *  (df_test_bikes$tavg)**2 +
    coef_linear_model_bikes_4[4] * as.numeric(df_test_bikes$dia_laboral == T)
  
  error_cuadratico_medio_4 <- sum((df_test_bikes$n - df_test_bikes$predicciones_4)**2) / nrow(df_test_bikes)
  
  
  df_test_bikes$predicciones_7 <- coef_linear_model_bikes_7[1] + 
    df_test_bikes$tavg * coef_linear_model_bikes_7[2] + 
    coef_linear_model_bikes_7[3] *  (df_test_bikes$tavg)**2 +
    coef_linear_model_bikes_7[4] * as.numeric(df_test_bikes$dia_laboral == T) +
    coef_linear_model_bikes_7[5] * (df_test_bikes$tavg)**2 * (df_test_bikes$tavg) +
    coef_linear_model_bikes_7[6] * as.numeric(df_test_bikes$dia_laboral == T) * (df_test_bikes$tavg) +
    coef_linear_model_bikes_7[7] * as.numeric(df_test_bikes$dia_laboral == T) * (df_test_bikes$tavg)**2 +
    coef_linear_model_bikes_7[8] * as.numeric(df_test_bikes$dia_laboral == T) * (df_test_bikes$tavg)**2 * (df_test_bikes$tavg)
  
  error_cuadratico_medio_7 <- sum((df_test_bikes$n - df_test_bikes$predicciones_7)**2) / nrow(df_test_bikes)
  
  
  df_errores <- rbind(df_errores, 
                      data.frame("type" = c("tavg-dialab", 
                                            "tavg-dialab_mejorado"),
                                 "val" = c(error_cuadratico_medio_4,
                                           error_cuadratico_medio_7)))
}


gr_mean_squared_error_tavg_upgrade_model <- ggplot(df_errores, mapping = aes(x = type, y = val, colour = type)) +
  geom_boxplot() +
  scale_colour_discrete(labels = c("TempProm-\nEs_DiaLaboral",
                                   "TempPromMejorado-\nEs_DiaLaboral")) +
  theme(axis.text.x = element_blank(), #removes the labels
        axis.ticks.x = element_blank()) + #removes de ticks in the x axis
  labs(title = "Error Cuadratico Medio por Modelo",
       x = "Modelo",
       y = "Error Medido (MSE)",
       colour = "Modelo:")



#Ahora creo un dataframe para usarlo como visualizacion de las ecuaciones del modelo 4 y 7

df_cuadratic <- data.frame("tavg" = rep(5:35, 2), "dia_laboral" = rep(c(TRUE, FALSE), 31))

df_cuadratic$predicciones <- coef_linear_model_bikes_4[1] + 
  df_cuadratic$tavg * coef_linear_model_bikes_4[2] + 
  coef_linear_model_bikes_4[3] *  (df_cuadratic$tavg)**2 +
  coef_linear_model_bikes_4[4] * as.numeric(df_cuadratic$dia_laboral == T)

gr_visualizacion_modelotavg <- ggplot(data = df_cuadratic, mapping = aes(x = tavg, y = predicciones, col = dia_laboral)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#ffa69e", "#6096ba"), labels = c("",""), guide = guide_legend(title = NULL)) +
  theme_void()

df_cuadratic$predicciones2 <- coef_linear_model_bikes_7[1] + 
  df_cuadratic$tavg * coef_linear_model_bikes_7[2] + 
  coef_linear_model_bikes_7[3] *  (df_cuadratic$tavg)**2 +
  coef_linear_model_bikes_7[4] * as.numeric(df_cuadratic$dia_laboral == T) +
  coef_linear_model_bikes_7[5] * (df_cuadratic$tavg)**2 * (df_cuadratic$tavg) +
  coef_linear_model_bikes_7[6] * as.numeric(df_cuadratic$dia_laboral == T) * (df_cuadratic$tavg) +
  coef_linear_model_bikes_7[7] * as.numeric(df_cuadratic$dia_laboral == T) * (df_cuadratic$tavg)**2 +
  coef_linear_model_bikes_7[8] * as.numeric(df_cuadratic$dia_laboral == T) * (df_cuadratic$tavg)**2 * (df_cuadratic$tavg)

gr_visualizacion_modelotavg_mejorado <- ggplot(data = df_cuadratic, mapping = aes(x = tavg, y = predicciones2, col = dia_laboral)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("#ffa69e", "#6096ba"), labels = c("",""), guide = guide_legend(title = NULL)) +
  theme_void()

gr_quadratic_ecuation_model <- df_cuadratic %>% 
  ggplot(mapping = aes(x = tavg, y = predicciones2, col = dia_laboral)) + 
  geom_line(linewidth = 1.2) +
  geom_point(data = clima_ecobici_lluvia_dialaboral, aes(x = tavg, y = n)) +
  scale_y_continuous( breaks = c(0, 2000,4000,6000,8000,10000,12000,14000),
                      labels = c(0, 2000,4000,6000,8000,10000,12000,14000),
                      limits = c(0,15000)) + #el warning me sale por limits idk why
  scale_x_continuous( breaks = c(5,10,15,20,25,30,35),
                      labels = c(5,10,15,20,25,30,35)) +
  scale_colour_manual(values = c("#ffa69e", "#6096ba"), labels= c('Dia no Laboral', 'Dia Laboral')) +
  labs(title = "Viajes según temperatura promedio en base al modelo",
       x = "Temperatura Promedio",
       y = "Cantidad de viajes",
       colour = "Tipo de Día:")



  
#ANALISIS EXPLORATORIO CLASIFICACION ----

#real vs fake según title words y title exclamations
#gr_title_exclamations <- ggplot(data = fake_news, mapping = aes(y = title_words, x = type, col = title_has_excl)) +
  #geom_boxplot() +
  #labs("")


#real vs fake según negative y title exclamations
#gr_negative_exclamations <- ggplot(data = fake_news, mapping = aes(y = negative, x = type, col = title_has_excl)) +
  #geom_boxplot()

#real vs fake todas las variables (no usar)

#x <- fake_news %>% mutate(excl_type = case_when(title_has_excl == T & type == "real" ~ "real_excl",
                                                #title_has_excl == F & type == "real" ~ "real_noexcl",
                                                #title_has_excl == T & type == "fake" ~ "fake_excl",
                                                #.default =  "fake_noexcl"))

#ggplot(data = x, mapping = aes(x = title_words, y = negative , colour = excl_type)) +
  #geom_point()




#en lugar de utilizar el grafico para mostrar la relacion entre type y title_has_excl utilizar la siguiente tabla:
table_fakenews_excl <- table(fake_news$title_has_excl, fake_news$type)

#real vs fake según negative y title words
fit <- rpart(data = fake_news, type ~ negative + title_words)

gr_negative_title_words <- ggplot(data = fake_news, mapping = aes(y = negative, x = title_words, col = type)) +
  geom_parttree(data = fit, alpha = 0.1, aes(fill = type)) +
  geom_point(aes(col = type)) +
  scale_colour_manual(values = c("#ffa69e", "#6096ba"), labels= c('Fake', 'Real')) +
  scale_fill_manual(values = c("#ffa69e", "#6096ba"), labels= c('Fake', 'Real')) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Negatividad y cantidad de palabras",
       x = "Cantidad de palabras en el título",
       y = "Porcentaje de negatividad en el título", 
       col = "Tipo de noticia:",
       fill = "Tipo de noticia:")
  



#Veamos que valor para minsplit mejor determina el arbol
df_accuracy_tree_models <- data.frame("mins_val" = c(), "accuracy" = c())

for(seed_val in 150:209){
  
  set.seed(seed_val)
  observations_news <- sample(x = nrow(fake_news), 
                              size = nrow(fake_news) * 0.8,
                              replace = F)
  
  df_train_news <- fake_news[observations_news,]
  df_test_news <- fake_news[-observations_news,]
  
  
  for(mins_val in 1:50){
    
    tree_model <- rpart(data = df_train_news, formula = type ~ title_words + negative + title_has_excl, minsplit = mins_val)
    df_test_news$predictions <- predict(tree_model, df_test_news, type = "class")
    accuracy_tree_model <- mean(df_test_news$type == df_test_news$predictions)
    
    df_accuracy_tree_models <- rbind(df_accuracy_tree_models, 
                                     data.frame("mins_val" = mins_val, "accuracy" = accuracy_tree_model))
  }
}

#df_accuracy_tree_models %>% ggplot(mapping = aes(x = mins_val, y = accuracy)) + geom_point()

v_accuracy_prom_tree <- df_accuracy_tree_models %>% group_by(mins_val) %>% summarise(accuracy_prom = mean(accuracy)) %>% arrange(desc(accuracy_prom))

df_accuracy_tree_models %>% group_by(mins_val) %>% ggplot(mapping = aes(group = mins_val, x = mins_val, y = accuracy)) + geom_boxplot()

#modelo de arbol de decision 
tree_model <- rpart(data = fake_news, formula = type ~ title_words + negative + title_has_excl, minsplit = 28, )

rpart.plot(tree_model, shadow.col = "#C8B596", box.palette = "Browns", tweak = 1.2, extra = 8)

#matriz de confusión
df_test_news_2 <- df_test_news %>% mutate(predictions = case_when(predictions == "real" ~ "pred_real",
                                                .default = "pred_fake"))


table(df_test_news$type,df_test_news$predictions)
table_matriz_de_confusion_tree <- prop.table(table(df_test_news_2$type,df_test_news_2$predictions)) 


#modelo knn

#Veamos que valor para K mejor determina el KNN
df_accuracy_k_models <- data.frame("k_val" = c(), "accuracy" = c())

for(i in 150:209){
  
  set.seed(i)
  observations_news <- sample(x = nrow(fake_news), 
                              size = nrow(fake_news) * 0.8,
                              replace = F)
  
  df_train_news <- fake_news[observations_news,]
  df_test_news <- fake_news[-observations_news,]
  
  
  for(j in seq(from = 1, to = 120, by = 2)){
    
    knn_model <- knn(train = df_train_news[,c("title_words", "negative", "title_has_excl")], 
                     test = df_test_news[,c("title_words", "negative", "title_has_excl")], 
                     cl =  df_train_news$type, 
                     k = j)
    accuracy_knn <- mean(df_test_news$type == knn_model)
  
    
    df_accuracy_k_models <- rbind(df_accuracy_k_models, 
                                  data.frame("k_val" = j, "accuracy" = accuracy_knn))
  }
}

v_accuracy_prom_knn <- df_accuracy_k_models %>% group_by(k_val) %>% summarise(accuracy_prom = mean(accuracy)) %>% arrange(desc(accuracy_prom))

df_accuracy_tree_models %>% filter(mins_val < 60) %>% group_by(mins_val) %>% ggplot(mapping = aes(group = mins_val, x = mins_val, y = accuracy)) + geom_boxplot()

set.seed(34)
observations_news <- sample(x = nrow(fake_news), 
                            size = nrow(fake_news) * 0.8,
                            replace = F)

df_train_news <- fake_news[observations_news,]
df_test_news <- fake_news[-observations_news,]

knn_model <- knn(train = df_train_news[,c("title_words", "negative", "title_has_excl")], 
                 test = df_test_news[,c("title_words", "negative", "title_has_excl")], 
                 cl =  df_train_news$type, 
                 k = 31)

# matriz de confusión
table(df_test_news$type,knn_model)
table_matriz_de_confusion_knn <- prop.table(table(df_test_news$type,knn_model)) 


#grafico para visualizar las predicciones


ggplot(data = df_train_news, mapping = aes(y = negative, x = title_words, col = type)) +
  geom_point() +
  geom_parttree(data = fit, alpha = 0.1, aes(fill = type)) +
  geom_point(data = df_test_news_2, aes(y = negative, x = title_words, col = predictions)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Negatividad y cantidad de palabras",
       x = "Cantidad de palabras en el título",
       y = "Porcentaje de negatividad en el título", 
       col = "Tipo de noticia:")





#Probemos las probabilidades de que la nueva noticia sea falsa o verdadera
prediction_tree_model <- predict(tree_model, data.frame("title_words" = 15, "negative" = 6, "title_has_excl" = FALSE), type = "prob")

prediction_knn_model <- knn(train = df_train_news[,c("title_words", "negative", "title_has_excl")], 
    test = data.frame("title_words" = 15, "negative" = 6, "title_has_excl" = FALSE), 
    cl =  df_train_news$type, 
    k = 31,
    prob = T)


#para el dataframe de fake news saca todas las vaRIABLESA que no se utilizan osea todas menos las 3 del enunciado
#para los graficos hacer dos boxplot que relacionen las 2 catgoricas y una numerica
#para mostrar que k y que minsplit elegimos probar varios valores para ellos con distintos train/test y graficar

#k a mano dura
