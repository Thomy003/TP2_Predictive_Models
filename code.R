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
  geom_smooth(se = F)

gr_bike_uses_tmin_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = tmin, y = n,  colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F)

gr_bike_uses_tmax_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = tmax, y = n,  colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F)

#uso de bicis segun precipitacion promedio         
ggplot(clima_ecobici_lluvia_dialaboral[-58,], mapping = aes(x = prcp, y = n, colour = dia_laboral)) +
  geom_point() +
  geom_smooth(se = F)

#uso de bicis segun si llovio
gr_bike_uses_rain_workday <- ggplot(clima_ecobici_lluvia_dialaboral, mapping = aes(x = llovio, y = n, col = dia_laboral)) +
  geom_boxplot()


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
  geom_boxplot()





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

df_errores <- data.frame("type" = c(), "val" = c())

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
  
  
  df_errores <- rbind(df_errores, 
                      data.frame("type" = c("lluvia-dialab", 
                                            "tavg-dialab", 
                                            "tmin-dialab", 
                                            "tmax-dialab"),
                                 "val" = c(error_cuadratico_medio_1,
                                           error_cuadratico_medio_4,
                                           error_cuadratico_medio_5,
                                           error_cuadratico_medio_6)))
}

gr_mean_squared_error_by_model <- ggplot(df_errores, mapping = aes(x = type, y = val)) +
  geom_boxplot()

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


gr_mean_squared_error_tavg_upgrade <-df_errores %>% 
  filter(type %in% c("tavg-dialab", "tavg-dialab_mejorado")) %>% 
  ggplot(mapping = aes(x = type, y = val)) +
  geom_boxplot()



#Ahora creo un dataframe para usarlo como visualizacion de las ecuaciones del modelo 4 y 7

df_cuadratic <- data.frame("tavg" = rep(5:35, 2), "dia_laboral" = rep(c(TRUE, FALSE), 31))

df_cuadratic$predicciones <- coef_linear_model_bikes_4[1] + 
  df_cuadratic$tavg * coef_linear_model_bikes_4[2] + 
  coef_linear_model_bikes_4[3] *  (df_cuadratic$tavg)**2 +
  coef_linear_model_bikes_4[4] * as.numeric(df_cuadratic$dia_laboral == T)

df_cuadratic$predicciones2 <- coef_linear_model_bikes_7[1] + 
  df_cuadratic$tavg * coef_linear_model_bikes_7[2] + 
  coef_linear_model_bikes_7[3] *  (df_cuadratic$tavg)**2 +
  coef_linear_model_bikes_7[4] * as.numeric(df_cuadratic$dia_laboral == T) +
  coef_linear_model_bikes_7[5] * (df_cuadratic$tavg)**2 * (df_cuadratic$tavg) +
  coef_linear_model_bikes_7[6] * as.numeric(df_cuadratic$dia_laboral == T) * (df_cuadratic$tavg) +
  coef_linear_model_bikes_7[7] * as.numeric(df_cuadratic$dia_laboral == T) * (df_cuadratic$tavg)**2 +
  coef_linear_model_bikes_7[8] * as.numeric(df_cuadratic$dia_laboral == T) * (df_cuadratic$tavg)**2 * (df_cuadratic$tavg)

gr_quadratic_ecuation_model <- df_cuadratic %>% 
  ggplot(mapping = aes(x = tavg, y = predicciones2, col = dia_laboral)) + 
  geom_line() +
  geom_point(data = clima_ecobici_lluvia_dialaboral, aes(x = tavg, y = n))



  
#ANALISIS EXPLORATORIO CLASIFICACION ----

#real vs fake según title words y title exclamations
gr_title_exclamations <- ggplot(data = fake_news, mapping = aes(y = title_words, x = type, col = title_has_excl)) +
  geom_boxplot()


#real vs fake según negative y title exclamations
gr_negative_exclamations <- ggplot(data = fake_news, mapping = aes(y = negative, x = type, col = title_has_excl)) +
  geom_boxplot()


#real vs fake según negative y title words
gr_negative_title <- ggplot(data = fake_news, mapping = aes(y = negative, x = title_words, col = type)) +
  geom_point()


#real vs fake todas las variables (no usar)

x <- fake_news %>% mutate(excl_type = case_when(title_has_excl == T & type == "real" ~ "real_excl",
                                                title_has_excl == F & type == "real" ~ "real_noexcl",
                                                title_has_excl == T & type == "fake" ~ "fake_excl",
                                                .default =  "fake_noexcl"))

ggplot(data = x, mapping = aes(x = title_words, y = negative , colour = excl_type)) +
  geom_point()



#Veamos que valor para minsplit mejor determina el arbol
df_accuracy_tree_models <- data.frame("mins_val" = c(), "accuracy" = c())

for(i in 130:150){
  
  set.seed(i)
  observations_news <- sample(x = nrow(fake_news), 
                              size = nrow(fake_news) * 0.8,
                              replace = F)
  
  df_train_news <- fake_news[observations_news,]
  df_test_news <- fake_news[-observations_news,]
  
  
  for(j in 1:70){
    
    tree_model <- rpart(data = df_train_news, formula = type ~ title_words + negative + title_has_excl, minsplit = j)
    df_test_news$predictions <- predict(tree_model, df_test_news, type = "class")
    accuracy_tree_model <- mean(df_test_news$type == df_test_news$predictions)
    
    df_accuracy_tree_models <- rbind(df_accuracy_tree_models, 
                                     data.frame("mins_val" = j, "accuracy" = accuracy_tree_model))
  }
}

df_accuracy_tree_models %>% ggplot(mapping = aes(x = mins_val, y = accuracy)) + geom_point()

v_accuracy_prom_tree <- df_accuracy_tree_models %>% group_by(mins_val) %>% summarise(accuracy_prom = mean(accuracy)) %>% arrange(desc(accuracy_prom))

#modelo de arbol de decision 
tree_model <- rpart(data = df_train_news, formula = type ~ title_words + negative + title_has_excl, minsplit = 22)

rpart.plot(tree_model)



#modelo knn

#Veamos que valor para K mejor determina el KNN
df_accuracy_k_models <- data.frame("k_val" = c(), "accuracy" = c())

for(i in 3242:3260){
  
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



#Probemos las probabilidades de que la nueva noticia sea falsa o verdadera
prediction_tree_model <- predict(tree_model, data.frame("title_words" = 15, "negative" = 6, "title_has_excl" = FALSE), type = "prob")

prediction_knn_model <- knn(train = df_train_news[,c("title_words", "negative", "title_has_excl")], 
    test = data.frame("title_words" = 15, "negative" = 6, "title_has_excl" = FALSE), 
    cl =  df_train_news$type, 
    k = 25,
    prob = T)


#para el dataframe de fake news saca todas las vaRIABLESA que no se utilizan osea todas menos las 3 del enunciado
#para los graficos hacer dos boxplot que relacionen las 2 catgoricas y una numerica
#para mostrar que k y que minsplit elegimos probar varios valores para ellos con distintos train/test y graficar

#k a mano dura
