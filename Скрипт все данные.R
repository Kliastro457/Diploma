# библиотеки
library(xlsx)
library(dplyr)
library(stargazer)
library(tableone)
library(mosaic)
library(xtable)
library(glue)
library(factoextra)
library(ggplot2)
library(FactoMineR)
library(corrplot)
library(lavaan)
library(semPlot)
library(ltm)
library(lmtest)
library(skedastic)
library(foreign)
library(tidyverse)
library(margins)
library(nnet)
library(effects)
library(gridExtra)
library(tseries)
library(oglmx)
library(memisc)
library(pscl)
library(hmeasure)
library(writexl)
library(sandwich)
library(webr)
library(tidyr)
library(viridisLite)
library(DandEFA)
library(xtable)
library(MASS)
library(car)

################################################################################
#ПРЕДИССЛЕДОВАНИЕ
################################################################################
# считаем данные
data_pull1 <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/survey_melt_1.xlsx", sheetIndex = 1)
data_pull2 <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/survey_melt_2.xlsx", sheetIndex = 1)
data_pull3 <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/survey_melt_3.xlsx", sheetIndex = 1)
View(data_pull1)
View(data_pull2)
View(data_pull3)

# изменим тип переменных
data_pull1$feature <- as.factor(data_pull1$feature)
data_pull2$feature <- as.factor(data_pull2$feature)
data_pull3$feature <- as.factor(data_pull3$feature)


# датасет1
data_pictures <- list()
cnt = 0
for (i in 12:22) {
  cnt = cnt + 1
  data_pictures[[cnt]] <- filter(data_pull1, variable == i)
  test2<-lm(value~feature, data=data_pictures[[cnt]])
  hsd_i <- TukeyHSD(test2)
  hsd_name <- glue("hsd_{cnt}.html")
  print(xtable(hsd_i$feature), type="html", file=hsd_name)
  cat("<p></p>", file=hsd_name, append=TRUE)
}

# датасет2
data_pictures2 <- list()
cnt = 0
for (i in 1:11) {
  cnt = cnt + 1
  data_pictures2[[cnt]] <- filter(data_pull2, variable == i)
  test2<-lm(value~feature, data=data_pictures2[[cnt]])
  hsd_i <- TukeyHSD(test2)
  hsd_name <- glue("hsd2_{cnt}.html")
  print(xtable(hsd_i$feature), type="html", file=hsd_name)
  cat("<p></p>", file=hsd_name, append=TRUE)
}

# датасет3
data_pictures3 <- list()
cnt = 0
for (i in 23:31) {
  cnt = cnt + 1
  data_pictures3[[cnt]] <- filter(data_pull3, variable == i)
  test2<-lm(value~feature, data=data_pictures3[[cnt]])
  hsd_i <- TukeyHSD(test2)
  hsd_name <- glue("hsd3_{cnt}.html")
  print(xtable(hsd_i$feature), type="html", file=hsd_name)
  cat("<p></p>", file=hsd_name, append=TRUE)
}


################################################################################
#ДАННЫЕ короткий ОПРОСНИК
################################################################################
data_pict <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/diploma_demo.xlsx", sheetIndex = 5)
data_pict <- dplyr::select(data_pict, - NA.)
data_pict <- data_pict/3
#View(data_pict)

data_longquest <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/Diploma_short.xlsx", sheetIndex = 1)
#View(data_longquest)

data_lq_short <- dplyr::select(data_longquest, id, p1_prob:p10_good, tea_coffee:city, d_extravers:d_open)
#View(data_lq_short)

data_transf <-  dplyr::select(data_lq_short,id, p1_prob:p10_good)
#View(data_transf)
data_clean <- dplyr::select(data_lq_short,id, tea_coffee:city, d_extravers:d_open)
#View(data_clean)

# Первые 5 картинок
{# Разделим фрейм данных на отдельные фреймы данных по 5 столбцов в каждом
  df_list <- split.default(data_transf[, 2:26], rep(1:5, each = 5))
  
  for (i in 1:length(df_list)) {
    df_list[[i]]$id <- data_transf$id
    df_list[[i]] <- merge(df_list[[i]], data_clean, by = "id")
    names(df_list[[i]])[2] <- "prob"
    names(df_list[[i]])[3] <- "similar"
    names(df_list[[i]])[4] <- "often"
    names(df_list[[i]])[5] <- "like"
    names(df_list[[i]])[6] <- "good"
  }
  # Удалим строки с отсутствующими значениями из каждого фрейма данных
  df_list <- lapply(df_list, function(x) x[complete.cases(x), ])
  # Объединим все фреймы данных в один
  first_pictures <- bind_rows(df_list)
  # Добавим столбец с номером изображения
  first_pictures$pic_num <- rep(1:length(df_list), sapply(df_list, nrow))
}
# Вторые 5 картинок
{
  # Разделим фрейм данных на отдельные фреймы данных по 5 столбцов в каждом
  df_list <- split.default(data_transf[, 27:51], rep(1:5, each = 5))
  
  for (i in 1:length(df_list)) {
    df_list[[i]]$id <- data_transf$id
    df_list[[i]] <- merge(df_list[[i]], data_clean, by = "id")
    names(df_list[[i]])[2] <- "prob"
    names(df_list[[i]])[3] <- "similar"
    names(df_list[[i]])[4] <- "often"
    names(df_list[[i]])[5] <- "like"
    names(df_list[[i]])[6] <- "good"
  }
  # Удалим строки с отсутствующими значениями из каждого фрейма данных
  df_list <- lapply(df_list, function(x) x[complete.cases(x), ])
  # Объединим все фреймы данных в один
  second_pictures <- bind_rows(df_list)
  # Добавим столбец с номером изображения
  second_pictures$pic_num <- rep(1:length(df_list), sapply(df_list, nrow)) + 5
}
# Объединить все вместе
final_df <- bind_rows(list(first_pictures, second_pictures))
#View(final_df)


# создадим отдельные датасеты для каждой картинки
{
  data_pict1 <- dplyr::select(filter(data_longquest, between(p1_prob,1,5)),id, p1_prob:p1_good, tea_coffee:d_open)
  data_pict2 <- dplyr::select(filter(data_longquest, between(p2_prob,1,5)),id, p2_prob:p2_good, tea_coffee:d_open)
  data_pict3 <- dplyr::select(filter(data_longquest, between(p3_prob,1,5)),id, p3_prob:p3_good, tea_coffee:d_open)
  data_pict4 <- dplyr::select(filter(data_longquest, between(p4_prob,1,5)),id, p4_prob:p4_good, tea_coffee:d_open)
  data_pict5 <- dplyr::select(filter(data_longquest, between(p5_prob,1,5)),id, p5_prob:p5_good, tea_coffee:d_open)
  data_pict6 <- dplyr::select(filter(data_longquest, between(p6_prob,1,5)),id, p6_prob:p6_good, tea_coffee:d_open)
  data_pict7 <- dplyr::select(filter(data_longquest, between(p7_prob,1,5)),id, p7_prob:p7_good, tea_coffee:d_open)
  data_pict8 <- dplyr::select(filter(data_longquest, between(p8_prob,1,5)),id, p8_prob:p8_good, tea_coffee:d_open)
  data_pict9 <- dplyr::select(filter(data_longquest, between(p9_prob,1,5)),id, p9_prob:p9_good, tea_coffee:d_open)
  data_pict10 <- dplyr::select(filter(data_longquest, between(p10_prob,1,5)),id, p10_prob:p10_good, tea_coffee:d_open)
}

data_pict_trasp <- data.frame(t(data_pict))

# объединим с характеристиками картинки
{
  data_p1 <- t((data.frame(t(dplyr::select(data_pict1,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X1))))
  data_p2 <- t((data.frame(t(dplyr::select(data_pict2,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X2))))
  data_p3 <- t((data.frame(t(dplyr::select(data_pict3,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X3))))
  data_p4 <- t((data.frame(t(dplyr::select(data_pict4,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X4))))
  data_p5 <- t((data.frame(t(dplyr::select(data_pict5,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X5))))
  data_p6 <- t((data.frame(t(dplyr::select(data_pict6,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X6))))
  data_p7 <- t((data.frame(t(dplyr::select(data_pict7,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X7))))
  data_p8 <- t((data.frame(t(dplyr::select(data_pict8,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X8))))
  data_p9 <- t((data.frame(t(dplyr::select(data_pict9,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X9))))
  data_p10 <- t((data.frame(t(dplyr::select(data_pict10,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X10))))
}

#View(data_p1)

##### КАРТИНКА 1 ###############################################################
# евклидово расстояние 
data_p1 <- data.frame(data_p1, data.frame(mahalanobis(data_p1, t(data_pict_trasp$X1), diag(1,5))))
names(data_p1)[6] <- "dist"
data_p1$p <- pchisq(data_p1$dist, df = 3 , lower.tail=FALSE)
data_p1 <- head(data_p1,-1)
data_p1$id <- data_pict1$id
#View(data_p1)


##### КАРТИНКА 2 ###############################################################
# евклидово расстояние 
data_p2 <- data.frame(data_p2, data.frame(mahalanobis(data_p2, t(data_pict_trasp$X2), diag(1,5))))
names(data_p2)[6] <- "dist"
data_p2$p <- pchisq(data_p2$dist, df = 3 , lower.tail=FALSE)
data_p2 <- head(data_p2,-1)
data_p2$id <- data_pict2$id
#View(data_p2)

##### КАРТИНКА 3 ###############################################################
# евклидово расстояние 
data_p3 <- data.frame(data_p3, data.frame(mahalanobis(data_p3, t(data_pict_trasp$X3), diag(1,5))))
names(data_p3)[6] <- "dist"
data_p3$p <- pchisq(data_p3$dist, df = 3 , lower.tail=FALSE)
data_p3 <- head(data_p3,-1)
data_p3$id <- data_pict3$id
#View(data_p3)

##### КАРТИНКА 4 ###############################################################
# евклидово расстояние 
data_p4 <- data.frame(data_p4, data.frame(mahalanobis(data_p4, t(data_pict_trasp$X4), diag(1,5))))
names(data_p4)[6] <- "dist"
data_p4$p <- pchisq(data_p4$dist, df = 3 , lower.tail=FALSE)
data_p4 <- head(data_p4,-1)
data_p4$id <- data_pict4$id
#View(data_p4)

##### КАРТИНКА 5 ###############################################################
# евклидово расстояние 
data_p5 <- data.frame(data_p5, data.frame(mahalanobis(data_p5, t(data_pict_trasp$X5), diag(1,5))))
names(data_p5)[6] <- "dist"
data_p5$p <- pchisq(data_p5$dist, df = 3 , lower.tail=FALSE)
data_p5 <- head(data_p5,-1)
data_p5$id <- data_pict5$id
#View(data_p5)

##### КАРТИНКА 6 ###############################################################
# евклидово расстояние 
data_p6 <- data.frame(data_p6, data.frame(mahalanobis(data_p6, t(data_pict_trasp$X6), diag(1,5))))
names(data_p6)[6] <- "dist"
data_p6$p <- pchisq(data_p6$dist, df = 3 , lower.tail=FALSE)
data_p6 <- head(data_p6,-1)
data_p6$id <- data_pict6$id
#View(data_p6)

##### КАРТИНКА 7 ###############################################################
# евклидово расстояние 
data_p7 <- data.frame(data_p7, data.frame(mahalanobis(data_p7, t(data_pict_trasp$X7), diag(1,5))))
names(data_p7)[6] <- "dist"
data_p7$p <- pchisq(data_p7$dist, df = 3 , lower.tail=FALSE)
data_p7 <- head(data_p7,-1)
data_p7$id <- data_pict7$id
#View(data_p7)

##### КАРТИНКА 8 ###############################################################
# евклидово расстояние 
data_p8 <- data.frame(data_p8, data.frame(mahalanobis(data_p8, t(data_pict_trasp$X8), diag(1,5))))
names(data_p8)[6] <- "dist"
data_p8$p <- pchisq(data_p8$dist, df = 3 , lower.tail=FALSE)
data_p8 <- head(data_p8,-1)
data_p8$id <- data_pict8$id
#View(data_p8)

##### КАРТИНКА 9 ###############################################################
# евклидово расстояние 
data_p9 <- data.frame(data_p9, data.frame(mahalanobis(data_p9, t(data_pict_trasp$X9), diag(1,5))))
names(data_p9)[6] <- "dist"
data_p9$p <- pchisq(data_p9$dist, df = 3 , lower.tail=FALSE)
data_p9 <- head(data_p9,-1)
data_p9$id <- data_pict9$id
#View(data_p9)

##### КАРТИНКА 10 #########
# евклидово расстояние 
data_p10 <- data.frame(data_p10, data.frame(mahalanobis(data_p10, t(data_pict_trasp$X10), diag(1,5))))
names(data_p10)[6] <- "dist"
data_p10$p <- pchisq(data_p10$dist, df = 3 , lower.tail=FALSE)
data_p10 <- head(data_p10,-1)
data_p10$id <- data_pict10$id
#View(data_p10)


# объединим все в один датафрейм #######
pict_list <- list(data_p1, data_p2, data_p3, data_p4, data_p5, 
                  data_p6, data_p7, data_p8, data_p9, data_p10)
distance <- bind_rows(pict_list)
distance$pic_num <- rep(1:length(pict_list), sapply(pict_list, nrow))

data_pict_itog <- dplyr::select(merge(distance, final_df, by = c("id","pic_num")), -c(d_extravers.y:d_open.y))
#View(data_pict_itog)


################################################################################
#ДАННЫЕ БОЛЬШОЙ ОПРОСНИК
################################################################################
data_pict_sh <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/diploma_demo.xlsx", sheetIndex = 5)
data_pict_sh <- dplyr::select(data_pict_sh, - NA.)
data_pict_sh <- data_pict_sh/3
#View(data_pict_sh)

data_longquest_sh <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/Diploma_long - все ответы.xlsx", sheetIndex = 1)
data_longquest_sh <- data_longquest_sh[-c(52:69),]
#View(data_longquest_sh)

data_lq_short_sh <- dplyr::select(data_longquest_sh, id, p1_prob:p10_good, tea_coffee:city, d_extravers:d_open)
#View(data_lq_short)

data_transf_sh <-  dplyr::select(data_lq_short_sh,id, p1_prob:p10_good)
#View(data_transf)
data_clean_sh <- dplyr::select(data_lq_short_sh,id, tea_coffee:city, d_extravers:d_open)
#View(data_clean)

# Первые 5 картинок
{# Разделим фрейм данных на отдельные фреймы данных по 5 столбцов в каждом
  df_list <- split.default(data_transf_sh[, 2:26], rep(1:5, each = 5))
  
  for (i in 1:length(df_list)) {
    df_list[[i]]$id <- data_transf_sh$id
    df_list[[i]] <- merge(df_list[[i]], data_clean_sh, by = "id")
    names(df_list[[i]])[2] <- "prob"
    names(df_list[[i]])[3] <- "similar"
    names(df_list[[i]])[4] <- "often"
    names(df_list[[i]])[5] <- "like"
    names(df_list[[i]])[6] <- "good"
  }
  # Удалим строки с отсутствующими значениями из каждого фрейма данных
  df_list <- lapply(df_list, function(x) x[complete.cases(x), ])
  # Объединим все фреймы данных в один
  first_pictures <- bind_rows(df_list)
  # Добавим столбец с номером изображения
  first_pictures$pic_num <- rep(1:length(df_list), sapply(df_list, nrow))
}
# Вторые 5 картинок
{# Разделим фрейм данных на отдельные фреймы данных по 5 столбцов в каждом
  df_list <- split.default(data_transf_sh[, 27:51], rep(1:5, each = 5))
  
  for (i in 1:length(df_list)) {
    df_list[[i]]$id <- data_transf_sh$id
    df_list[[i]] <- merge(df_list[[i]], data_clean_sh, by = "id")
    names(df_list[[i]])[2] <- "prob"
    names(df_list[[i]])[3] <- "similar"
    names(df_list[[i]])[4] <- "often"
    names(df_list[[i]])[5] <- "like"
    names(df_list[[i]])[6] <- "good"
  }
  # Удалим строки с отсутствующими значениями из каждого фрейма данных
  df_list <- lapply(df_list, function(x) x[complete.cases(x), ])
  # Объединим все фреймы данных в один
  second_pictures <- bind_rows(df_list)
  # Добавим столбец с номером изображения
  second_pictures$pic_num <- rep(1:length(df_list), sapply(df_list, nrow)) + 5
}
# Объединить все вместе
final_df_sh <- bind_rows(list(first_pictures, second_pictures))
#View(final_df_sh)


# создадим отдельные датасеты для каждой картинки
{
  data_pict1_sh <- dplyr::select(filter(data_longquest_sh, between(p1_prob,1,5)),id, p1_prob:p1_good, tea_coffee:d_open)
  data_pict2_sh <- dplyr::select(filter(data_longquest_sh, between(p2_prob,1,5)),id, p2_prob:p2_good, tea_coffee:d_open)
  data_pict3_sh <- dplyr::select(filter(data_longquest_sh, between(p3_prob,1,5)),id, p3_prob:p3_good, tea_coffee:d_open)
  data_pict4_sh <- dplyr::select(filter(data_longquest_sh, between(p4_prob,1,5)),id, p4_prob:p4_good, tea_coffee:d_open)
  data_pict5_sh <- dplyr::select(filter(data_longquest_sh, between(p5_prob,1,5)),id, p5_prob:p5_good, tea_coffee:d_open)
  data_pict6_sh <- dplyr::select(filter(data_longquest_sh, between(p6_prob,1,5)),id, p6_prob:p6_good, tea_coffee:d_open)
  data_pict7_sh <- dplyr::select(filter(data_longquest_sh, between(p7_prob,1,5)),id, p7_prob:p7_good, tea_coffee:d_open)
  data_pict8_sh <- dplyr::select(filter(data_longquest_sh, between(p8_prob,1,5)),id, p8_prob:p8_good, tea_coffee:d_open)
  data_pict9_sh <- dplyr::select(filter(data_longquest_sh, between(p9_prob,1,5)),id, p9_prob:p9_good, tea_coffee:d_open)
  data_pict10_sh <- dplyr::select(filter(data_longquest_sh, between(p10_prob,1,5)),id, p10_prob:p10_good, tea_coffee:d_open)
}

data_pict_trasp_sh <- data.frame(t(data_pict_sh))

# объединим с характеристиками картинки
{
  data_p1_sh <- t((data.frame(t(dplyr::select(data_pict1_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X1))))
  data_p2_sh <- t((data.frame(t(dplyr::select(data_pict2_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X2))))
  data_p3_sh <- t((data.frame(t(dplyr::select(data_pict3_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X3))))
  data_p4_sh <- t((data.frame(t(dplyr::select(data_pict4_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X4))))
  data_p5_sh <- t((data.frame(t(dplyr::select(data_pict5_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X5))))
  data_p6_sh <- t((data.frame(t(dplyr::select(data_pict6_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X6))))
  data_p7_sh <- t((data.frame(t(dplyr::select(data_pict7_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X7))))
  data_p8_sh <- t((data.frame(t(dplyr::select(data_pict8_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X8))))
  data_p9_sh <- t((data.frame(t(dplyr::select(data_pict9_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X9))))
  data_p10_sh <- t((data.frame(t(dplyr::select(data_pict10_sh,d_open, d_consci, d_extravers, d_agreeabl,d_neuro)), data.frame(data_pict_trasp$X10))))
}

#View(data_p1)

##### КАРТИНКА 1 ###############################################################
# расстояние махаланобиса
data_p1_sh <- data.frame(data_p1_sh, data.frame(mahalanobis(data_p1_sh, t(data_pict_trasp_sh$X1), diag(1,5))))
names(data_p1_sh)[6] <- "mahal"
data_p1_sh$p <- pchisq(data_p1_sh$mahal, df = 3 , lower.tail=FALSE)
data_p1_sh <- data_p1_sh[-16,]
data_p1_sh$id <- data_pict1_sh$id
#View(data_p1)


##### КАРТИНКА 2 ###############################################################
# расстояние махаланобиса
data_p2_sh <- data.frame(data_p2_sh, data.frame(mahalanobis(data_p2_sh, data_p2_sh[11,], diag(1,5))))
names(data_p2_sh)[6] <- "mahal"
data_p2_sh$p <- pchisq(data_p2_sh$mahal, df = 3 , lower.tail=FALSE)
data_p2_sh <- data_p2_sh[-11,]
data_p2_sh$id <- data_pict2_sh$id
#View(data_p2)

##### КАРТИНКА 3 ###############################################################
# расстояние махаланобиса
data_p3_sh <- data.frame(data_p3_sh, data.frame(mahalanobis(data_p3_sh, data_p3_sh[8,], diag(1,5))))
names(data_p3_sh)[6] <- "mahal"
data_p3_sh$p <- pchisq(data_p3_sh$mahal, df = 3 , lower.tail=FALSE)
data_p3_sh <- data_p3_sh[-8,]
data_p3_sh$id <- data_pict3_sh$id
#View(data_p3)

##### КАРТИНКА 4 ###############################################################
# расстояние махаланобиса
data_p4_sh <- data.frame(data_p4_sh, data.frame(mahalanobis(data_p4_sh, data_p4_sh[9,], diag(1,5))))
names(data_p4_sh)[6] <- "mahal"
data_p4_sh$p <- pchisq(data_p4_sh$mahal, df = 3 , lower.tail=FALSE)
data_p4_sh <- data_p4_sh[-9,]
data_p4_sh$id <- data_pict4_sh$id
#View(data_p4)

##### КАРТИНКА 5 ###############################################################
# расстояние махаланобиса
data_p5_sh <- data.frame(data_p5_sh, data.frame(mahalanobis(data_p5_sh, data_p5_sh[12,], diag(1,5))))
names(data_p5_sh)[6] <- "mahal"
data_p5_sh$p <- pchisq(data_p5_sh$mahal, df = 3 , lower.tail=FALSE)
data_p5_sh <- data_p5_sh[-12,]
data_p5_sh$id <- data_pict5_sh$id
#View(data_p5)

##### КАРТИНКА 6 ###############################################################
# расстояние махаланобиса
data_p6_sh <- data.frame(data_p6_sh, data.frame(mahalanobis(data_p6_sh, data_p6_sh[12,], diag(1,5))))
names(data_p6_sh)[6] <- "mahal"
data_p6_sh$p <- pchisq(data_p6_sh$mahal, df = 3 , lower.tail=FALSE)
data_p6_sh <- data_p6_sh[-12,]
data_p6_sh$id <- data_pict6_sh$id
#View(data_p6)

##### КАРТИНКА 7 ###############################################################
# расстояние махаланобиса
data_p7_sh <- data.frame(data_p7_sh, data.frame(mahalanobis(data_p7_sh, data_p7_sh[10,], diag(1,5))))
names(data_p7_sh)[6] <- "mahal"
data_p7_sh$p <- pchisq(data_p7_sh$mahal, df = 3 , lower.tail=FALSE)
data_p7_sh <- data_p7_sh[-10,]
data_p7_sh$id <- data_pict7_sh$id
#View(data_p7)

##### КАРТИНКА 8 ###############################################################
# расстояние махаланобиса
data_p8_sh <- data.frame(data_p8_sh, data.frame(mahalanobis(data_p8_sh, data_p8_sh[12,], diag(1,5))))
names(data_p8_sh)[6] <- "mahal"
data_p8_sh$p <- pchisq(data_p8_sh$mahal, df = 3 , lower.tail=FALSE)
data_p8_sh <- data_p8_sh[-12,]
data_p8_sh$id <- data_pict8_sh$id
#View(data_p8)

##### КАРТИНКА 9 ###############################################################
# расстояние махаланобиса
data_p9_sh <- data.frame(data_p9_sh, data.frame(mahalanobis(data_p9_sh, data_p9_sh[12,], diag(1,5))))
names(data_p9_sh)[6] <- "mahal"
data_p9_sh$p <- pchisq(data_p9_sh$mahal, df = 3 , lower.tail=FALSE)
data_p9_sh <- data_p9_sh[-12,]
data_p9_sh$id <- data_pict9_sh$id
#View(data_p9)

##### КАРТИНКА 10 #########
# расстояние махаланобиса
data_p10_sh <- data.frame(data_p10_sh, data.frame(mahalanobis(data_p10_sh, data_p10_sh[10,], diag(1,5))))
names(data_p10_sh)[6] <- "mahal"
data_p10_sh$p <- pchisq(data_p10_sh$mahal, df = 3 , lower.tail=FALSE)
data_p10_sh <- data_p10_sh[-10,]
data_p10_sh$id <- data_pict10_sh$id
#View(data_p10)

# объединим все в один датафрейм #######
pict_list_sh <- list(data_p1_sh, data_p2_sh, data_p3_sh, data_p4_sh, data_p5_sh, 
                     data_p6_sh, data_p7_sh, data_p8_sh, data_p9_sh, data_p10_sh)
mahalanob <- bind_rows(pict_list_sh)
mahalanob$pic_num <- rep(1:length(pict_list_sh), sapply(pict_list_sh, nrow))
#View(mahalanob)
data_pict_itog_sh <- dplyr::select(merge(mahalanob, final_df_sh, by = c("id","pic_num")), -c(d_extravers.y:d_open.y))
#View(data_pict_itog_sh)

#проверим согласованность факторов #############################################
data_pict_itog_sh <- dplyr::select(merge(mahalanob, final_df, by = c("id","pic_num")), -c(d_extravers.y:d_open.y))
#View(data_pict_itog)

#проверим согласованность факторов
data_e_sh <- data.frame(data_longquest_sh$q1e, data_longquest_sh$q2e, 6 - data_longquest_sh$q3e, 
                        6 - data_longquest_sh$q4e, data_longquest_sh$q5e, 6-data_longquest_sh$q6e,
                        6-data_longquest_sh$q7e, 6-data_longquest_sh$q8e, data_longquest_sh$q9e,
                        data_longquest_sh$q10e, 6-data_longquest_sh$q11e, data_longquest_sh$q12e)

data_a_sh <- data.frame(data_longquest_sh$q1a, data_longquest_sh$q2a, 6 - data_longquest_sh$q3a, 
                        6 - data_longquest_sh$q4a, 6 - data_longquest_sh$q5a, data_longquest_sh$q6a,
                        data_longquest_sh$q7a, 6-data_longquest_sh$q8a, 6 - data_longquest_sh$q9a,
                        6 - data_longquest_sh$q10a, data_longquest_sh$q11a, data_longquest_sh$q12a)

data_c_sh <- data.frame(6-data_longquest_sh$q1c, 6-data_longquest_sh$q2c, data_longquest_sh$q3c, 
                        data_longquest_sh$q4c, 6-data_longquest_sh$q5c, 6-data_longquest_sh$q6c,
                        data_longquest_sh$q7c, data_longquest_sh$q8c, data_longquest_sh$q9c,
                        6-data_longquest_sh$q10c, data_longquest_sh$q11c, 6-data_longquest_sh$q12c, 6-data_longquest_sh$q12c)

data_n_sh <- data.frame(6-data_longquest_sh$q1n, 6-data_longquest_sh$q2n, data_longquest_sh$q3n, 
                        data_longquest_sh$q4n, 6-data_longquest_sh$q5n, 6-data_longquest_sh$q6n,
                        data_longquest_sh$q7n, data_longquest_sh$q8n, 6-data_longquest_sh$q9n,
                        6-data_longquest_sh$q10n, data_longquest_sh$q11n, data_longquest_sh$q12n)

data_o_sh <- data.frame(6-data_longquest_sh$q1o, data_longquest_sh$q2o, data_longquest_sh$q3o, 
                        data_longquest_sh$q4o, 6-data_longquest_sh$q5o, 6-data_longquest_sh$q6o,
                        data_longquest_sh$q7o, data_longquest_sh$q8o, 6-data_longquest_sh$q9o,
                        6-data_longquest_sh$q10o, 6-data_longquest_sh$q11o, data_longquest_sh$q12o)
cronbach.alpha(na.omit(data_e_sh))#хорошо
cronbach.alpha(na.omit(data_a_sh))#приемлемо
cronbach.alpha(na.omit(data_c_sh))#хорошо
cronbach.alpha(na.omit(data_n_sh))#хорошо
cronbach.alpha(na.omit(data_o_sh))#приемлемо

o_sh <- data_longquest_sh$d_open
c_sh <- data_longquest_sh$d_consci
e_sh <- data_longquest_sh$d_extravers
a_sh <- data_longquest_sh$d_agreeabl
n_sh <- data_longquest_sh$d_neuro

#### регрессии######
View(data_pict_itog_sh)
mod1_sh <- lm(prob ~ mahal + age + tea_coffee + like_boardgames + pic_num +
                d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x + d_neuro.x, data = data_pict_itog_sh)
summary(mod1_sh)
mod1_sh2 <- stepAIC(mod1_sh)
anova(mod1_sh, mod1_sh2)
summary(mod1_sh2)

#тест Рамсея
resettest(mod1_sh2)
#неплохо

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod1_sh2) #гетероскедастичность есть
#тест Уйта
white_lm(mod1_sh2) #есть
#использую робастные ошибки

# like -------------------------------------------------------------------------
mod2_sh <- lm(like ~ mahal + age + tea_coffee + like_boardgames  + pic_num 
              + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x + d_neuro.x, data = data_pict_itog_sh)
summary(mod2_sh)
mod2_sh2 <- stepAIC(mod2_sh)
anova(mod2_sh2, mod2_sh)
summary(mod2_sh2)

#тест Рамсея
resettest(mod2_sh2)
#неплохо

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod2_sh2) #гетероскедастичность есть
#тест Уйта
white_lm(mod2_sh2) #есть
#использую робастные ошибки

# like -------------------------------------------------------------------------
mod3_sh <- lm(good ~ mahal + age + tea_coffee + like_boardgames  + pic_num +
                + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x + d_neuro.x, data = data_pict_itog_sh)
summary(mod3_sh)
mod3_sh2 <- stepAIC(mod3_sh)
anova(mod3_sh2, mod3_sh)
summary(mod3_sh2)

#тест Рамсея
resettest(mod3_sh2)
#неплохо

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod3_sh2) #гетероскедастичность есть
#тест Уйта
white_lm(mod3_sh2) #нет
#использую робастные ошибки

#в одну таблицу
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod1_sh2, mod2_sh2, mod3_sh2,
          se=list(cse(mod1_sh2), cse(mod2_sh2), cse(mod3_sh2)),
          title="модели", type="text",
          df=FALSE, digits=3,out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mod1-mod3 длинный опрос.html")



################################################################################
#описательные статистики
################################################################################
#опрос 51 человек -------------------------------------------------------------
data_pict_sh <- read.xlsx("C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/diploma_demo.xlsx", sheetIndex = 5)
data_lquest_short_sh <- dplyr::select(data_longquest_sh, tea_coffee:age, d_extravers:d_open)
stargazer(data_lquest_short_sh, type = "text", median = TRUE, digits = 4,
          font.size = "tiny", 
          out = "C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/описательные статистики short.html")
corrplot(cor(data_lquest_short_sh))

#опрос 545 человек -------------------------------------------------------------
data_lquest_short <- dplyr::select(data_longquest, tea_coffee:age, d_extravers:d_open)
stargazer(data_lquest_short, type = "text", median = TRUE, digits = 4,
          font.size = "tiny", 
          out = "C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/описательные статистики.html")
data_lquest_fact <- dplyr::select(data_longquest, city, male)
city <- data.frame(table(data_lquest_fact$city))
male <- data.frame(table(data_lquest_fact$male))
corrplot(cor(data_lquest_short))
cor_opisat <- cor.mtest(data_lquest_short, conf.level = 0.95)
corrplot(cor(data_lquest_short), addgrid.col = TRUE,
         p.mat = cor_opisat$p, sig.level = 0.05, insig='blank', addCoef.col = TRUE)

quantile(data_lquest_short$age, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1))
hist(data_lquest_short$age)


#города ------------------------------------------------------------------------
for_donut_graph <- data_char
for_donut_graph <- for_donut_graph %>% dplyr::filter(city %in% c("Москва","Санкт-Петербург","Киров","Нижневартовск","Нижний Новгород","Уфа"))
for_donut_graph$like_criteria <- ifelse(for_donut_graph$mean_att > mean(for_donut_graph$mean_att), "Выше среднего", "Ниже среднего")
for_donut_graph1 <- as.data.frame(table(for_donut_graph$city, for_donut_graph$like_criteria))
colnames(for_donut_graph1) <- c("city", "like_criteria", "Amount")
PieDonut(for_donut_graph1, aes(city, like_criteria, count=Amount))

#пол ___------------------------------------------------------------------------
for_donut_graph <- data_char
for_donut_graph$like_criteria <- ifelse(for_donut_graph$mean_att > mean(for_donut_graph$mean_att), "Выше среднего", "Ниже среднего")
for_donut_graph2 <- as.data.frame(table(for_donut_graph$male, for_donut_graph$like_criteria))
colnames(for_donut_graph2) <- c("male", "like_criteria", "Amount")
PieDonut(for_donut_graph2, aes(male, like_criteria, count=Amount))

#гистограммы для черт ----------------------------------------------------------
par(mfrow=c(3,2))
hist(data_char$d_open.x, col = "lightblue")
hist(data_char$d_consci.x, col = "lightblue")
hist(data_char$d_extravers.x, col = "lightblue")
hist(data_char$d_agreeabl.x, col = "lightblue")
hist(data_char$d_neuro.x, col = "lightblue")

#одуванчики --------------------------------------------------------------------
names(data_longquest_sh)
data_pict_sh12 <- data_longquest_sh[,c(2:61)]
View(data_pict_sh12)
dandpal <- rev(rainbow(100, start = 0.2, end = 0.7))
facl <- factload(data_pict_sh12, nfac=5, method="prax", cormeth="spearman")
dandelion(facl,bound=0,mcex=c(2,1.5),palet=dandpal)

names(data_longquest)
library(paletteer)
data_pict_sh13 <- data_longquest[,c(2:31)]
View(data_pict_sh13)
facl2 <- factload(data_pict_sh13, nfac=5, method="prax", cormeth="spearman")
par(bg = "#f7f7f7")

dandelion(facl2,bound=0,mcex=c(2,1.2),palet = paletteer_c("grDevices::Red-Blue", 30))
?paletteer_c



################################################################################
####ЛИНЕЙНЫЕ РЕГРЕССИИ. ГИПОТЕЗА 1  
################################################################################
data_pict_itog$pic_num <- as.factor(data_pict_itog$pic_num)

# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod1_h1 <- lm(prob ~ d_open.x + d_consci.x + d_agreeabl.x + d_neuro.x + d_extravers.x + age + male + tea_coffee 
              + like_boardgames + pic_num, data = data_pict_itog)
summary(mod1_h1)

### чистка выбросов ############################
plot(mod1_h1, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod1_h1) #да будто все и нормально

crPlots(mod1_h1)
mod1_h12 <- update(mod1_h1, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                 + I(d_neuro.x^2) + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod1_h1)
crPlots(mod1_h12)

mod1_h11 <- stepAIC(mod1_h12)
#проверим не сделали ли хуже
anova(mod1_h12, mod1_h11)
#короткая лучше
mod1_h1 <- mod1_h11

influencePlot(mod1_h1)
data1_h1 <- data_pict_itog[-845,]
data1_h1 <- data1_h1[-407,]
data1_h1 <- data1_h1[-345,]
mod1_h1 <- update(mod1_h1, data = data1_h1)
plot(mod1_h1, which = 2)
influencePlot(mod1_h1) #хватит
#теперь расстояние Кука
cook1_h1 <- cooks.distance(mod1_h1)
plot(cook1_h1, type = "h") #думаю, достаточно
which.max(cook1_h1)
summary(mod1_h1)
influencePlot(mod1_h1)

#теперь подбираем спецификацию 
boxCox(mod1_h1) #непон

#тест Рамсея
resettest(mod1_h1)
#неплохо

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod1_h1) #кажется, гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod1_h1, order.by = data1_h1$age) #гетероскедастичность нет
#тест Уйта
white_lm(mod1_h1) #есть
#использую робастные ошибки

# МОДЕЛЬ (LIKE) ----------------------------------------------------------------
mod4_h1 <- lm(like ~ d_open.x + d_consci.x + d_agreeabl.x + d_neuro.x + d_extravers.x + male + age + tea_coffee 
              + like_boardgames + pic_num, data = data_pict_itog)
summary(mod4_h1)

crPlots(mod4_h1)
mod4_h11 <- update(mod4_h1, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                   + I(d_neuro.x^2) + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
crPlots(mod4_h11)

### чистка выбросов ############################
plot(mod4_h11, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod4_h11) #да будто все и нормально

mod44_h1 <- stepAIC(mod4_h11)
#проверим не сделали ли хуже
anova(mod4_h11, mod44_h1)
#короткая лучше 
summary(mod44_h1)

mod4_h1 <- mod44_h1

influencePlot(mod4_h1)
data4_h1 <- data_pict_itog[-362,]
data4_h1 <- data4_h1[-346,]
mod4_h1 <- update(mod4_h1, data = data4_h1)
plot(mod4_h1, which = 2)
influencePlot(mod4_h1) #хватит
#теперь расстояние Кука
cook4_h1 <- cooks.distance(mod4_h1)
plot(cook4_h1, type = "h") #думаю, достаточно
which.max(cook4_h1)
data4_h1 <- data4_h1[-264,]
mod4_h1 <- update(mod4_h1, data = data4_h1)
cook4_h1 <- cooks.distance(mod4_h1)
plot(cook4_h1, type = "h")
summary(mod4_h1)
influencePlot(mod4_h1)

#теперь подбираем спецификацию 
boxCox(mod4_h1) #log, но не будем

#тест Рамсея
resettest(mod4_h1)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod4_h1) #кажется, гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod4_h1, order.by = data4_h1$age) #гетероскедастичность есть
#тест Уайта
white_lm(mod4_h1) #есть
#в любом случае использую робастные ошибки

# МОДЕЛЬ (GOOD) ----------------------------------------------------------------
mod5_h1 <- lm(good ~ d_open.x + d_consci.x + d_agreeabl.x + d_neuro.x + d_extravers.x + male + age + tea_coffee 
              + like_boardgames + pic_num, data = data_pict_itog)
summary(mod5_h1)

crPlots(mod5_h1)
mod5_h11 <- update(mod5_h1, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                   + I(d_neuro.x^2) + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod5_h11)
crPlots(mod5_h11)

### чистка выбросов ############################
plot(mod5_h11, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod5_h11) #да будто все и нормально

mod55_h1 <- stepAIC(mod5_h11)
#проверим не сделали ли хуже
anova(mod5_h11, mod55_h1)
#короткая лучше 
mod5_h1 <- mod55_h1

influencePlot(mod5_h1)
data5_h1 <- data_pict_itog[-529,]
data5_h1 <- data5_h1[-453,]
data5_h1 <- data5_h1[-346,]
mod5_h1 <- update(mod5_h1, data = data5_h1)
plot(mod5_h1, which = 2)
influencePlot(mod5_h1) #хватит
#теперь расстояние Кука
cook5_h1 <- cooks.distance(mod5_h1)
plot(cook5_h1, type = "h")
which.max(cook5_h1)
summary(mod5_h1)

#теперь подбираем спецификацию 
boxCox(mod5_h1) #log, но не будем

#тест Рамсея
resettest(mod5_h1)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod5_h1) #кажется, гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod5_h1, order.by = data5_h1$age) #гетероскедастичность есть
#тест Уйта
white_lm(mod5_h1) #есть
#в любом случае использую робастные ошибки

#в одну таблицу ###############################################################
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod1_h1, mod4_h1, mod5_h1,
          se=list(cse(mod1_h1), cse(mod4_h1), cse(mod5_h1)),
          title="Линейные регрессии. Гипотеза 1", type="text",
          df=FALSE, digits=3,out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/H1 - mod1-mod5.html")


################################################################################
####ЛИНЕЙНЫЕ РЕГРЕССИИ. ГИПОТЕЗЫ 2,3 
################################################################################
data_pict_itog$pic_num <- as.factor(data_pict_itog$pic_num)

# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod1 <- lm(prob ~ dist + age + male + tea_coffee + like_boardgames + city + pic_num, data = data_pict_itog)
summary(mod1)

### чистка выбросов ##
dev.off()
plot(mod1, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod1) #да будто все и нормально

mod11 <- stepAIC(mod1)
#проверим не сделали ли хуже
anova(mod1, mod11)
#на 1% короткая лучше, но пусть будет просто без городов
mod1 <- lm(prob ~ dist + male + age + tea_coffee + like_boardgames + pic_num, data = data_pict_itog)
mod11 <- stepAIC(mod11)
anova(mod1, mod11)
mod1 <- mod11

influencePlot(mod1)
data1 <- data_pict_itog[-985,]
data1 <- data1[-621,]
data1 <- data1[-620,]
data1 <- data1[-345,]
mod1 <- update(mod1, data = data1)
influencePlot(mod1)
#теперь расстояние Кука
cook <- cooks.distance(mod1)
plot(cook, type = "h")
which.max(cook) #думаю, достаточно
summary(mod1)

#теперь подбираем спецификацию 
plot(mod1, which = 2)
boxCox(mod1) #ничего 

#тест Рамсея
resettest(mod1)
#not норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod1) #да
#Годтфелда-Квандта
gqtest(mod1, order.by = data1$age) #нет
#тест Уйта
white_lm(mod1) #да
#в любом случае, я использую робастные ошибки


#МОДЕЛЬ (LIKE) -----------------------------------------------------------------
mod2 <- lm(like ~ dist + age + male + tea_coffee + like_boardgames + city  + pic_num, data = data_pict_itog)
summary(mod2)
### чистка выбросов ############################
plot(mod2, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod2) #да будто все и нормально

mod22 <- stepAIC(mod2)
#проверим не сделали ли хуже
anova(mod2, mod22)
#длинная лучше
mod2 <- lm(like ~ dist + age + male + tea_coffee + like_boardgames + pic_num, data = data_pict_itog)
mod22 <- stepAIC(mod2)
anova(mod2, mod22)
mod2 <- mod22

influencePlot(mod2)
data2 <- data_pict_itog[-514,]
data2 <- data2[-362,]
data2 <- data2[-346,]
data2 <- data2[-261,]
mod_2 <- update(mod2, data = data2)
plot(mod_2, which = 2)
influencePlot(mod_2) #хватит
#теперь расстояние Кука
cook2 <- cooks.distance(mod_2)
plot(cook2, type = "h") #думаю, достаточно
summary(mod_2)

#теперь подбираем спецификацию 
plot(mod_2, which = 2)
boxCox(mod_2) #логарифм, но не будем
mod2 <- mod_2

#тест Рамсея
resettest(mod2)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod2) #yes
#Годтфелда-Квандта
gqtest(mod2, order.by = data2$age) #есть
#тест Уйта
white_lm(mod2) #нет
#в любом случае, использую робастные ошибки


#МОДЕЛЬ (GOOD) -----------------------------------------------------------------
mod3 <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + city  + pic_num, data = data_pict_itog)
summary(mod3)

### чистка выбросов ############################
plot(mod3, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod3) #да будто все и нормально

mod3 <- lm(good ~ dist + age + male + tea_coffee + like_boardgames + pic_num, data = data_pict_itog)
mod33 <- stepAIC(mod3)
summary(mod33)
#проверим не сделали ли хуже
anova(mod3, mod33)
#длинная лучше
mod3 <- mod33

influencePlot(mod3)
data3 <- data_pict_itog[-514,]
data3 <- data3[-514,]
data3 <- data3[-346,]
data3 <- data3[-268,]
mod_3 <- update(mod3, data = data3)
plot(mod_3, which = 2)
influencePlot(mod_3) #хватит
#теперь расстояние Кука
cook3 <- cooks.distance(mod_3)
plot(cook3, type = "h") #думаю, достаточно
which.max(cook3)
summary(mod_3)

#теперь подбираем спецификацию 
plot(mod_3, which = 2)
boxCox(mod_3) #мб логарифм, но не будем
mod3 <- mod_3

#тест Рамсея
resettest(mod3)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod3) #кажется, гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod3, order.by = data3$age) #есть
#тест Уйта
white_lm(mod3) #no
#использую робастные ошибки

#МОДЕЛЬ (SIMIL) ----------------------------------------------------------------
mod4 <- lm(similar ~ dist + age + male + tea_coffee + like_boardgames + pic_num, data = data_pict_itog)
summary(mod4)

### чистка выбросов ############################
plot(mod4, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod4) #да будто все и нормально

mod44 <- stepAIC(mod4)
#проверим не сделали ли хуже
anova(mod4, mod44)
#короткая лучше
mod4 <- mod44

influencePlot(mod4)
data4 <- data_pict_itog[-976,]
data4 <- data4[-975,]
data4 <- data4[-261,]
mod_4 <- update(mod4, data = data4)
plot(mod_4, which = 2)
influencePlot(mod_4) #хватит
#теперь расстояние Кука
cook4 <- cooks.distance(mod_4)
plot(cook4, type = "h")
which.max(cook4)
data4 <- data4[-755,]
mod_44 <- update(mod_4, data = data4)
cook4 <- cooks.distance(mod_44)
plot(cook4, type = "h") #думаю, достаточно
summary(mod_44)
influencePlot(mod_44)

#теперь подбираем спецификацию 
plot(mod_44, which = 2)
boxCox(mod_44) #ничего
mod4 <- mod_44

#тест Рамсея
resettest(mod4)
#все норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod4) #no
#Годтфелда-Квандта
gqtest(mod4, order.by = data4$age) #этот говорит нет
#тест Уйта
white_lm(mod4) #есть
#использую робастные ошибки


#МОДЕЛЬ (OFTEN) -----------------------------------------------------------------
mod5 <- lm(often ~ dist + age + male + tea_coffee + like_boardgames  + pic_num, data = data_pict_itog)
summary(mod5)

### чистка выбросов ############################
plot(mod5, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod5) #да будто все и нормально

mod55 <- stepAIC(mod5)
#проверим не сделали ли хуже
anova(mod5, mod55)
#короткая лучше
mod5 <- mod55

influencePlot(mod5)
data5 <- data_pict_itog[-619,]
data5 <- data5[-588,]
data5 <- data5[-261,]
data5 <- data5[-173,]
mod_5 <- update(mod5, data = data5)
plot(mod_5, which = 2)
influencePlot(mod_5) #хватит
#теперь расстояние Кука
cook5 <- cooks.distance(mod_5)
plot(cook5, type = "h")
which.max(cook5) #думаю, достаточно
summary(mod_5)

#теперь подбираем спецификацию 
plot(mod_5, which = 2)
boxCox(mod_5) #ничего 
mod5 <- mod_5

#тест Рамсея
resettest(mod5)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod5) #кажется, гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod5, order.by = data5$age) #этот говорит нет
#тест Уйта
white_lm(mod5) #есть
#использую робастные ошибки

#в одну таблицу ###############################################################
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod1, mod4, mod5, mod2, mod3,
          se=list(cse(mod1), cse(mod4), cse(mod5), cse(mod2), cse(mod3)),
          title="Линейные регрессии. Расстояние", type="text",
          df=FALSE, digits=3,out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mod1-mod5.html")





#еще поработаем с данными#######################################################
#View(data_pict_itog)
#View(data_pict)
#добавим номера картинок
data_pict$pic_num <- 1:10

#объединим
data_char <- merge(data_pict_itog, data_pict, by = "pic_num")
#View(data_char)
#добавим необходимые столбцы разницы
data_char$delta_o <- abs(data_char$d_open.x - data_char$pict_o)
data_char$delta_c <- abs(data_char$d_consci.x - data_char$pict_c)
data_char$delta_e <- abs(data_char$d_extravers.x - data_char$pict_e)
data_char$delta_a <- abs(data_char$d_agreeabl.x - data_char$pict_a)
data_char$delta_n <- abs(data_char$d_neuro.x - data_char$pict_n)
#View(data_char)
data_char$pic_num <- as.factor(data_char$pic_num)


#модели с модулями отклонения черты от картинки #####
# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod6 <- lm(prob ~ age + male + tea_coffee + like_boardgames + pic_num + delta_o
           + delta_c + delta_e + delta_a + delta_n, data = data_char)
summary(mod6)

plot(mod6, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod6) #да будто все и нормально
#crPlots(mod6)
mod62 <- update(mod6, .~. -age + log(age))

mod66 <- stepAIC(mod62)
#проверим не сделали ли хуже
anova(mod62, mod66)
#короткая лучше
mod6 <- mod66

influencePlot(mod6)

dev.off()
data6 <- data_char[-985,]
data6 <- data6[-621,]
data6 <- data6[-620,]
data6 <- data6[-345,]
mod_6 <- update(mod6, data = data6)
plot(mod_6, which = 2)
influencePlot(mod_6) #хватит
#теперь расстояние Кука
cook6 <- cooks.distance(mod_6)
plot(cook6, type = "h")
summary(mod_6)

#теперь подбираем спецификацию 
plot(mod_6, which = 2)
boxCox(mod_6) #непонятно что 
mod6 <- mod_6

#тест Рамсея
resettest(mod6)
#не ок 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod6) #кажется, гетероскедастичность есть
#тест Уйта
white_lm(mod6) #есть
#использую робастные ошибки

# МОДЕЛЬ (LIKE) ----------------------------------------------------------------
mod7 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames + pic_num + delta_o
           + delta_c + delta_e + delta_a + delta_n, data = data_char)
summary(mod7)

mod77 <- stepAIC(mod7)
#проверим не сделали ли хуже
anova(mod7, mod77)
#короткая лучше на 1% 
mod7 <- mod77

influencePlot(mod7)

data7 <- data_char[-529,]
data7 <- data7[-516,]
data7 <- data7[-514,]
mod_7 <- update(mod7, data = data7)
plot(mod_7, which = 2)
influencePlot(mod_7) #хватит
#теперь расстояние Кука
cook7 <- cooks.distance(mod_7)
plot(cook7, type = "h")
which.max(cook7)
data7 <- data7[-346,]
mod_7 <- update(mod7, data = data7)
summary(mod_7)

#теперь подбираем спецификацию 
boxCox(mod_7) #логарифм, но не булем
mod7 <- mod_7

#тест Рамсея
resettest(mod7)
#ок 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod7) #етероскедастичность есть
#тест Уйта
white_lm(mod7) #no
#использую робастные ошибки

# МОДЕЛЬ (GOOD) ----------------------------------------------------------------
mod8 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + delta_o
           + delta_c + delta_e + delta_a + delta_n, data = data_char)
summary(mod8)

plot(mod7, which = 2) #норм
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod8) #да будто все и нормально

mod82 <- update(mod8, .~. -age + log(age))
crPlots(mod82)

mod88 <- lm(good ~ log(age) + like_boardgames + pic_num + delta_o + delta_n, data = data_char)
#проверим не сделали ли хуже
anova(mod8, mod88)
#короткая лучше
mod8 <- mod88

influencePlot(mod8)

data8 <- data_char[-600,]
data8 <- data8[-514,]
data8 <- data8[-346,]
data8 <- data8[-268,]
mod_8 <- update(mod8, data = data8)
plot(mod_8, which = 2)
influencePlot(mod_8) #хватит
#теперь расстояние Кука
cook8 <- cooks.distance(mod_8)
plot(cook8, type = "h")
which.max(cook8)
mod_8 <- update(mod_8, data = data8)
plot(mod_8, which = 2)
summary(mod_8)

#теперь подбираем спецификацию 
boxCox(mod_8) #попробуем логарифм, но не будем
mod8 <- mod_8

#тест Рамсея
resettest(mod8)
#норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod8) #етероскедастичность есть
#тест Уйта
white_lm(mod8) #no
#использую робастные ошибки

#в одну таблицу ######
{cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod6, mod7, mod8,
          se=list(cse(mod6), cse(mod7), cse(mod8)),
          title="модели с модулями отклонения черты от картинки", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mod6-mod10.html")
}



#регрессиии с расстоянием чебышева #############################################

data_select <- dplyr::select(data_char, delta_o, delta_c, delta_e, delta_a, delta_n)
data_select2 <- apply(data_select, 1, max)
data_char$cheb <- data_select2

#prob --------------------------------------------------------------------------
mod1_cheb <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + cheb, data = data_char)
summary(mod1_cheb)
vif(mod1_cheb)

mod1_cheb_step <- stepAIC(mod1_cheb)
summary(mod1_cheb_step)
anova(mod1_cheb, mod1_cheb_step) #короткая лучше

influencePlot(mod1_cheb_step)

data1_cheb <- data_char[-985,]
data1_cheb <- data1_cheb[-621,]
data1_cheb <- data1_cheb[-620,]
mod1_cheb <- update(mod1_cheb_step, data = data1_cheb)
influencePlot(mod1_cheb) #хватит
#теперь расстояние Кука
cook1_cheb <- cooks.distance(mod1_cheb)
plot(cook1_cheb, type = "h") #ок
summary(mod1_cheb)

#тест Рамсея
resettest(mod1_cheb)
#не норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod1_cheb) #гетероскедастичность есть
#тест Уйта
white_lm(mod1_cheb) #yes
#использую робастные ошибки

#like --------------------------------------------------------------------------
mod2_cheb <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + cheb, data = data_char)
summary(mod2_cheb)
vif(mod2_cheb)

mod2_cheb_step <- stepAIC(mod2_cheb)
summary(mod2_cheb_step)
anova(mod2_cheb, mod2_cheb_step) #короткая лучше

influencePlot(mod2_cheb_step)

data2_cheb <- data_char[-516,]
data2_cheb <- data2_cheb[-514,]
data2_cheb <- data2_cheb[-346,]
mod2_cheb <- update(mod2_cheb_step, data = data2_cheb)
influencePlot(mod2_cheb) #хватит
#теперь расстояние Кука
cook2_cheb <- cooks.distance(mod2_cheb)
plot(cook2_cheb, type = "h") #ок
summary(mod2_cheb)

#тест Рамсея
resettest(mod2_cheb)
#норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod2_cheb) #гетероскедастичность есть
#тест Уйта
white_lm(mod2_cheb) #no
#использую робастные ошибки

#good --------------------------------------------------------------------------
mod3_cheb <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + cheb, data = data_char)
summary(mod3_cheb)
vif(mod3_cheb)

mod3_cheb_step <- stepAIC(mod3_cheb)
summary(mod1_cheb_step)
anova(mod3_cheb, mod3_cheb_step) #короткая лучше

influencePlot(mod3_cheb_step)

data3_cheb <- data_char[-600,]
data3_cheb <- data3_cheb[-514,]
data3_cheb <- data3_cheb[-346,]
mod3_cheb <- update(mod3_cheb_step, data = data3_cheb)
influencePlot(mod3_cheb) #хватит
#теперь расстояние Кука
cook3_cheb <- cooks.distance(mod3_cheb)
plot(cook3_cheb, type = "h") #ок
summary(mod3_cheb)

#тест Рамсея
resettest(mod3_cheb)
#норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod3_cheb) #гетероскедастичность есть
#тест Уйта
white_lm(mod3_cheb) #no
#использую робастные ошибки

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod1_cheb, mod2_cheb, mod3_cheb,
          se=list(cse(mod1_cheb), cse(mod2_cheb), cse(mod3_cheb)),
          title="Модели с расстоянием Чебышёва", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/расстояние Чебышева.html")


#модели с двойками и тройками координат
# регрессии с попарными расстояниям и расстояниями по тройкам ##################
View(data_char)
# создадим 10 пар --------------------------------------------------------------
data_char <- mutate(data_char, paraoc = sqrt(I(delta_o^2) + I(delta_c^2)),
                    paraoe = sqrt(I(delta_o^2) + I(delta_e^2)),
                    paraoa = sqrt(I(delta_o^2) + I(delta_a^2)),
                    paraon = sqrt(I(delta_o^2) + I(delta_n^2)),
                    parace = sqrt(I(delta_c^2) + I(delta_e^2)),
                    paraca = sqrt(I(delta_c^2) + I(delta_a^2)),
                    paracn = sqrt(I(delta_c^2) + I(delta_n^2)),
                    paraea = sqrt(I(delta_e^2) + I(delta_a^2)),
                    paraen = sqrt(I(delta_e^2) + I(delta_n^2)),
                    paraan = sqrt(I(delta_a^2) + I(delta_n^2)))

#prob --------------------------------------------------------------------------
mod1_par <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoc +
                 paraoe + paraoa + paraon + parace + paraca  
               + paracn + paraea + paraen + paraan, data = data_char)
summary(mod1_par)
vif(mod11_par)
mod11_par <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoc +
                  paraoa + paraca + paraea + paraan, data = data_char)
summary(mod11_par)

mod1_par_step <- stepAIC(mod11_par)
summary(mod1_par_step)
vif(mod1_par_step)
anova(mod11_par, mod1_par_step) #короткая лучше

influencePlot(mod1_par_step)

data1_par <- data_char[-985,]
data1_par <- data1_par[-620,]
data1_par <- data1_par[-585,]
mod1_par <- update(mod1_par_step, data = data1_par)
influencePlot(mod1_par) #хватит
#теперь расстояние Кука
cook1_par <- cooks.distance(mod1_par)
plot(cook1_par, type = "h") #ок
summary(mod1_par)

#тест Рамсея
resettest(mod1_par)
#не норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod1_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod1_par) #yes
#использую робастные ошибки

#like --------------------------------------------------------------------------
mod2_par <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoc +
                 paraoe + paraoa + paraon + parace + paraca  
               + paracn + paraea + paraen + paraan, data = data_char)
summary(mod2_par)
vif(mod21_par)
mod21_par <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoc +
                  paraoa + paraca + paraea + paraan, data = data_char)
summary(mod21_par)

mod2_par_step <- stepAIC(mod21_par)
summary(mod2_par_step)
vif(mod2_par_step)
anova(mod21_par, mod2_par_step) #короткая лучше

influencePlot(mod2_par_step)

data2_par <- data_char[-514,]
data2_par <- data2_par[-346,]
data2_par <- data2_par[-264,]
mod2_par <- update(mod2_par_step, data = data2_par)
influencePlot(mod2_par) #хватит
#теперь расстояние Кука
cook2_par <- cooks.distance(mod2_par)
plot(cook2_par, type = "h") #ок
summary(mod2_par)

#тест Рамсея
resettest(mod2_par)
# норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod2_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod2_par) #yes
#использую робастные ошибки

#good --------------------------------------------------------------------------
mod3_par <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoc +
                 paraoe + paraoa + paraon + parace + paraca  
               + paracn + paraea + paraen + paraan, data = data_char)
summary(mod3_par)
vif(mod31_par)
mod31_par <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num  + paraoc +
                  paraoa + paraca + paraea + paraan, data = data_char)
summary(mod31_par)

mod3_par_step <- stepAIC(mod31_par)
mod32_par <- lm(good ~ log(age) + like_boardgames  + pic_num + paraan, data = data_char)
summary(mod32_par)
summary(mod3_par_step)
vif(mod3_par_step)
anova(mod31_par, mod3_par_step) #короткая лучше

influencePlot(mod3_par_step)

data3_par <- data_char[-600,]
data3_par <- data3_par[-514,]
data3_par <- data3_par[-346,]
mod3_par <- update(mod3_par_step, data = data3_par)
influencePlot(mod3_par) #хватит
#теперь расстояние Кука
cook3_par <- cooks.distance(mod3_par)
plot(cook3_par, type = "h") #ок
summary(mod3_par)

#тест Рамсея
resettest(mod3_par)
#норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod3_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod3_par) #no
#использую робастные ошибки

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod1_par, mod2_par, mod3_par,
          se=list(cse(mod1_par), cse(mod2_par), cse(mod3_par)),
          title="Модели с расстоянием Чебышёва", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/пары штук.html")

# создадим 10 троек ------------------------------------------------------------
data_char <- mutate(data_char, paraoce = sqrt(I(delta_o^2) + I(delta_c^2) + I(delta_e^2)),
                    paraoea = sqrt(I(delta_o^2) + I(delta_e^2) + I(delta_a^2)),
                    paraoac = sqrt(I(delta_o^2) + I(delta_a^2) + I(delta_c^2)),
                    paraonc = sqrt(I(delta_o^2) + I(delta_n^2) + I(delta_c^2)),
                    paracea = sqrt(I(delta_c^2) + I(delta_e^2) + I(delta_a^2)),
                    paracan = sqrt(I(delta_c^2) + I(delta_a^2) + I(delta_n^2)),
                    paracne = sqrt(I(delta_c^2) + I(delta_n^2) + I(delta_e^2)),
                    paraean = sqrt(I(delta_e^2) + I(delta_a^2) + I(delta_n^2)),
                    paraeno = sqrt(I(delta_e^2) + I(delta_n^2) + I(delta_o^2)),
                    paraano = sqrt(I(delta_a^2) + I(delta_n^2) + I(delta_o^2)))

#prob --------------------------------------------------------------------------
mod13_par <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paraonc + paracea + paracan  
                + paracne + paraean + paraeno + paraano, data = data_char)
summary(mod13_par)
vif(mod33_par)
mod33_par <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paracea + paracne, data = data_char)
summary(mod33_par)

mod13_par_step <- stepAIC(mod33_par)
summary(mod13_par_step)
vif(mod13_par_step)
anova(mod33_par, mod13_par_step) #короткая лучше

influencePlot(mod13_par_step)

data13_par <- data_char[-985,]
data13_par <- data13_par[-620,]
data13_par <- data13_par[-345,]
mod13_par <- update(mod13_par_step, data = data13_par)
influencePlot(mod13_par) #хватит
#теперь расстояние Кука
cook13_par <- cooks.distance(mod13_par)
plot(cook13_par, type = "h") #ок
summary(mod13_par)

#тест Рамсея
resettest(mod13_par)
#не норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod13_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod13_par) #yes
#использую робастные ошибки

#like --------------------------------------------------------------------------
mod23_par <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paraonc + paracea + paracan  
                + paracne + paraean + paraeno + paraano, data = data_char)
summary(mod23_par)
vif(mod233_par)
mod233_par <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                   paraoea + paraoac + paracea + paracne, data = data_char)
summary(mod233_par)

mod23_par_step <- stepAIC(mod233_par)
summary(mod23_par_step)

anova(mod23_par, mod23_par_step) #короткая лучше

influencePlot(mod23_par_step)

data23_par <- data_char[-514,]
data23_par <- data23_par[-362,]
data23_par <- data23_par[-346,]
mod223_par <- update(mod23_par_step, data = data23_par)
influencePlot(mod223_par) #хватит
#теперь расстояние Кука
cook23_par <- cooks.distance(mod223_par)
plot(cook23_par, type = "h") #ок
summary(mod223_par)

#тест Рамсея
resettest(mod223_par)
# норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod223_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod223_par) #no
#использую робастные ошибки

#good --------------------------------------------------------------------------
mod43_par <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paraonc + paracea + paracan  
                + paracne + paraean + paraeno + paraano, data = data_char)
summary(mod43_par)
vif(mod334_par)
mod334_par <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num  + paraoce +
                   paraoea + paraoac + paracea + paraano, data = data_char)
summary(mod334_par)

mod43_par_step <- stepAIC(mod334_par)
summary(mod43_par_step)
anova(mod334_par, mod43_par_step) #короткая лучше

influencePlot(mod43_par_step)

data43_par <- data_char[-600,]
data43_par <- data43_par[-514,]
data43_par <- data43_par[-346,]
mod43_par <- update(mod43_par_step, data = data43_par)
influencePlot(mod43_par) #хватит
#теперь расстояние Кука
cook43_par <- cooks.distance(mod43_par)
plot(cook43_par, type = "h") #ок
summary(mod43_par)

#тест Рамсея
resettest(mod43_par)
#норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod43_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod43_par) #no
#использую робастные ошибки

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod13_par, mod223_par, mod43_par,
          se=list(cse(mod13_par), cse(mod223_par), cse(mod43_par)),
          title="Модели с расстоянием Чебышёва", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/тройки штук.html")

#а теперь повеселимся и сделаем для каждой штуки отдельно)))) ------------------
#prob --------------------------------------------------------------------------
mod1_par1 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraoc, data = data_char)
mod1_par_step1 <- stepAIC(mod1_par1)
summary(mod1_par_step1)
influencePlot(mod1_par_step1)

data1_par11 <- data_char[-985,]
data1_par11 <- data1_par11[-620,]
data1_par11 <- data1_par11[-585,]
mod1_par_step1 <- update(mod1_par_step1, data = data1_par11)
summary(mod1_par_step1)
influencePlot(mod1_par_step1) #хватит
#тест Рамсея
resettest(mod1_par_step1)
#not норм 

#остальные не значимы ----------------------------------------------------------
mod1_par2 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraoe, data = data_char)
mod1_par_step2 <- stepAIC(mod1_par2)
summary(mod1_par_step2)
influencePlot(mod1_par_step2)

mod1_par3 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraoa, data = data_char)
mod1_par_step3 <- stepAIC(mod1_par3)
summary(mod1_par_step3)

mod1_par4 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraon, data = data_char)
mod1_par_step4 <- stepAIC(mod1_par4)
summary(mod1_par_step4)

mod1_par5 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + parace, data = data_char)
mod1_par_step5 <- stepAIC(mod1_par5)
summary(mod1_par_step5)

mod1_par6 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraca, data = data_char)
mod1_par_step6 <- stepAIC(mod1_par6)
summary(mod1_par_step6)

mod1_par7 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paracn, data = data_char)
mod1_par_step7 <- stepAIC(mod1_par7)
summary(mod1_par_step7)

mod1_par8 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraea, data = data_char)
mod1_par_step8 <- stepAIC(mod1_par8)
summary(mod1_par_step8)

mod1_par9 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraen, data = data_char)
mod1_par_step9 <- stepAIC(mod1_par9)
summary(mod1_par_step9)

mod1_par10 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num +
                   paraan, data = data_char)
mod1_par_step10 <- stepAIC(mod1_par10)
summary(mod1_par_step10)


#like --------------------------------------------------------------------------
mod2_par1 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraoc, data = data_char)
mod2_par_step1 <- stepAIC(mod2_par1)
summary(mod2_par_step1)

mod2_par2 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraoe, data = data_char)
mod2_par_step2 <- stepAIC(mod2_par2)
summary(mod2_par_step2)

mod2_par3 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraoa, data = data_char)
mod2_par_step3 <- stepAIC(mod2_par3)
summary(mod2_par_step3)

mod2_par4 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraon, data = data_char)
mod2_par_step4 <- stepAIC(mod2_par4)
summary(mod2_par_step4) #!!!

mod2_par5 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + parace, data = data_char)
mod2_par_step5 <- stepAIC(mod2_par5)
summary(mod2_par_step5)

mod2_par6 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraca, data = data_char)
mod2_par_step6 <- stepAIC(mod2_par6)
summary(mod2_par_step6)

mod2_par7 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paracn, data = data_char)
mod2_par_step7 <- stepAIC(mod2_par7)
summary(mod2_par_step7) #!!!

mod2_par8 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraea, data = data_char)
mod2_par_step8 <- stepAIC(mod2_par8)
summary(mod2_par_step8)

mod2_par9 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraen, data = data_char)
mod2_par_step9 <- stepAIC(mod2_par9)
summary(mod2_par_step9) #!!!

mod2_par10 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num +
                   paraan, data = data_char)
mod2_par_step10 <- stepAIC(mod2_par10)
summary(mod2_par_step10) #!!!


#good --------------------------------------------------------------------------
mod3_par1 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraoc, data = data_char)
mod3_par_step1 <- stepAIC(mod3_par1)
summary(mod3_par_step1)

mod3_par2 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraoe, data = data_char)
mod3_par_step2 <- stepAIC(mod3_par2)
summary(mod3_par_step2)

mod3_par3 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraoa, data = data_char)
mod3_par_step3 <- stepAIC(mod3_par3)
summary(mod3_par_step3)

mod3_par4 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paraon, data = data_char)
mod3_par_step4 <- stepAIC(mod3_par4)
summary(mod3_par_step4) #!!!

mod3_par5 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + parace, data = data_char)
mod3_par_step5 <- stepAIC(mod3_par5)
summary(mod3_par_step5)

mod3_par6 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraca, data = data_char)
mod3_par_step6 <- stepAIC(mod3_par6)
summary(mod3_par_step6)

mod3_par7 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                + paracn, data = data_char)
mod3_par_step7 <- stepAIC(mod3_par7)
summary(mod3_par_step7)

mod3_par8 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraea, data = data_char)
mod3_par_step8 <- stepAIC(mod3_par8)
summary(mod3_par_step8)

mod3_par9 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                + paraen, data = data_char)
mod3_par_step9 <- stepAIC(mod3_par9)
summary(mod3_par_step9)

mod3_par10 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num +
                   paraan, data = data_char)
mod3_par_step10 <- stepAIC(mod3_par10)
summary(mod2_par_step10) #!!!

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}
m1 <- mod1_par_step1
m2 <- mod2_par_step4
m3 <- mod2_par_step7
m4 <- mod2_par_step9
m5 <- mod2_par_step10
m6 <- mod3_par_step10
stargazer(m1, m2, m3, m4, m5, m6, 
          se=list(cse(m1), cse(m2), cse(m3), cse(m4), cse(m5), cse(m6)),
          title="Модели с парами", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/с парами по одной.html")

# создадим 10 троек ------------------------------------------------------------
data_char <- mutate(data_char, paraoce = sqrt(I(delta_o^2) + I(delta_c^2) + I(delta_e^2)),
                    paraoea = sqrt(I(delta_o^2) + I(delta_e^2) + I(delta_a^2)),
                    paraoac = sqrt(I(delta_o^2) + I(delta_a^2) + I(delta_c^2)),
                    paraonc = sqrt(I(delta_o^2) + I(delta_n^2) + I(delta_c^2)),
                    paracea = sqrt(I(delta_c^2) + I(delta_e^2) + I(delta_a^2)),
                    paracan = sqrt(I(delta_c^2) + I(delta_a^2) + I(delta_n^2)),
                    paracne = sqrt(I(delta_c^2) + I(delta_n^2) + I(delta_e^2)),
                    paraean = sqrt(I(delta_e^2) + I(delta_a^2) + I(delta_n^2)),
                    paraeno = sqrt(I(delta_e^2) + I(delta_n^2) + I(delta_o^2)),
                    paraano = sqrt(I(delta_a^2) + I(delta_n^2) + I(delta_o^2)))

#prob --------------------------------------------------------------------------
mod13_par <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paraonc + paracea + paracan  
                + paracne + paraean + paraeno + paraano, data = data_char)
summary(mod13_par)
vif(mod33_par)
mod33_par <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paracea + paracne, data = data_char)
summary(mod33_par)

mod13_par_step <- stepAIC(mod33_par)
summary(mod13_par_step)
vif(mod13_par_step)
anova(mod33_par, mod13_par_step) #короткая лучше

influencePlot(mod13_par_step)

data13_par <- data_char[-985,]
data13_par <- data13_par[-620,]
data13_par <- data13_par[-345,]
mod13_par <- update(mod13_par_step, data = data13_par)
influencePlot(mod13_par) #хватит
#теперь расстояние Кука
cook13_par <- cooks.distance(mod13_par)
plot(cook13_par, type = "h") #ок
summary(mod13_par)

#тест Рамсея
resettest(mod13_par)
#не норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod13_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod13_par) #yes
#использую робастные ошибки

#like --------------------------------------------------------------------------
mod23_par <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paraonc + paracea + paracan  
                + paracne + paraean + paraeno + paraano, data = data_char)
summary(mod23_par)
vif(mod233_par)
mod233_par <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                   paraoea + paraoac + paracea + paracne, data = data_char)
summary(mod233_par)

mod23_par_step <- stepAIC(mod233_par)
summary(mod23_par_step)

anova(mod23_par, mod23_par_step) #короткая лучше

influencePlot(mod23_par_step)

data23_par <- data_char[-514,]
data23_par <- data23_par[-362,]
data23_par <- data23_par[-346,]
mod223_par <- update(mod23_par_step, data = data23_par)
influencePlot(mod223_par) #хватит
#теперь расстояние Кука
cook23_par <- cooks.distance(mod223_par)
plot(cook23_par, type = "h") #ок
summary(mod223_par)

#тест Рамсея
resettest(mod223_par)
# норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod223_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod223_par) #no
#использую робастные ошибки

#good --------------------------------------------------------------------------
mod43_par <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num + paraoce +
                  paraoea + paraoac + paraonc + paracea + paracan  
                + paracne + paraean + paraeno + paraano, data = data_char)
summary(mod43_par)
vif(mod334_par)
mod334_par <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num  + paraoce +
                   paraoea + paraoac + paracea + paraano, data = data_char)
summary(mod334_par)

mod43_par_step <- stepAIC(mod334_par)
summary(mod43_par_step)
anova(mod334_par, mod43_par_step) #короткая лучше

influencePlot(mod43_par_step)

data43_par <- data_char[-600,]
data43_par <- data43_par[-514,]
data43_par <- data43_par[-346,]
mod43_par <- update(mod43_par_step, data = data43_par)
influencePlot(mod43_par) #хватит
#теперь расстояние Кука
cook43_par <- cooks.distance(mod43_par)
plot(cook43_par, type = "h") #ок
summary(mod43_par)

#тест Рамсея
resettest(mod43_par)
#норм 

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod43_par) #гетероскедастичность есть
#тест Уйта
white_lm(mod43_par) #no
#использую робастные ошибки

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod13_par, mod223_par, mod43_par,
          se=list(cse(mod13_par), cse(mod223_par), cse(mod43_par)),
          title="Модели с расстоянием Чебышёва", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/тройки штук.html")

#а теперь повеселимся и сделаем для каждой штуки отдельно)))) ------------------
#prob --------------------------------------------------------------------------
mod13_par1 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraoce, data = data_char)
mod13_par_step1 <- stepAIC(mod13_par1)
summary(mod13_par_step1)
influencePlot(mod13_par_step1)

mod13_par2 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraoea, data = data_char)
mod13_par_step2 <- stepAIC(mod13_par2)
summary(mod13_par_step2)
influencePlot(mod13_par_step2)

mod13_par3 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraoac, data = data_char)
mod13_par_step3 <- stepAIC(mod13_par3)
summary(mod13_par_step3)

mod13_par4 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraonc, data = data_char)
mod13_par_step4 <- stepAIC(mod13_par4)
summary(mod13_par_step4)

mod13_par5 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paracea, data = data_char)
mod13_par_step5 <- stepAIC(mod13_par5)
summary(mod13_par_step5)

mod13_par6 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paracan, data = data_char)
mod13_par_step6 <- stepAIC(mod13_par6)
summary(mod13_par_step6)

mod13_par7 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paracne, data = data_char)
mod13_par_step7 <- stepAIC(mod13_par7)
summary(mod13_par_step7)

mod13_par8 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraean, data = data_char)
mod13_par_step8 <- stepAIC(mod13_par8)
summary(mod13_par_step8)

mod13_par9 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraeno, data = data_char)
mod13_par_step9 <- stepAIC(mod13_par9)
summary(mod13_par_step9)

mod13_par10 <- lm(prob ~ log(age) + male + tea_coffee + like_boardgames  + pic_num +
                    paraano, data = data_char)
mod13_par_step10 <- stepAIC(mod13_par10)
summary(mod13_par_step10)


#like --------------------------------------------------------------------------
mod23_par1 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraoce, data = data_char)
mod23_par_step1 <- stepAIC(mod23_par1)
summary(mod23_par_step1)
influencePlot(mod23_par_step1)

mod23_par2 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraoea, data = data_char)
mod23_par_step2 <- stepAIC(mod23_par2)
summary(mod23_par_step2)
influencePlot(mod23_par_step2)

mod23_par3 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraoac, data = data_char)
mod23_par_step3 <- stepAIC(mod23_par3)
summary(mod23_par_step3)

mod23_par4 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraonc, data = data_char)
mod23_par_step4 <- stepAIC(mod23_par4)
summary(mod23_par_step4) #!!!

mod23_par5 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paracea, data = data_char)
mod23_par_step5 <- stepAIC(mod23_par5)
summary(mod23_par_step5)

mod23_par6 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paracan, data = data_char)
mod23_par_step6 <- stepAIC(mod23_par6)
summary(mod23_par_step6) #!!!

mod23_par7 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paracne, data = data_char)
mod23_par_step7 <- stepAIC(mod23_par7)
summary(mod23_par_step7) #!!!

mod23_par8 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraean, data = data_char)
mod23_par_step8 <- stepAIC(mod23_par8)
summary(mod23_par_step8) #!!!

mod23_par9 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraeno, data = data_char)
mod23_par_step9 <- stepAIC(mod23_par9)
summary(mod23_par_step9) #!!!

mod23_par10 <- lm(like ~ log(age) + male + tea_coffee + like_boardgames  + pic_num +
                    paraano, data = data_char)
mod23_par_step10 <- stepAIC(mod23_par10)
summary(mod23_par_step10) #!!!


#good --------------------------------------------------------------------------
mod33_par1 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraoce, data = data_char)
mod33_par_step1 <- stepAIC(mod33_par1)
summary(mod33_par_step1)

mod33_par2 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraoea, data = data_char)
mod33_par_step2 <- stepAIC(mod33_par2)
summary(mod33_par_step2)

mod33_par3 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraoac, data = data_char)
mod33_par_step3 <- stepAIC(mod33_par3)
summary(mod33_par_step3)

mod33_par4 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paraonc, data = data_char)
mod33_par_step4 <- stepAIC(mod33_par4)
summary(mod33_par_step4)

mod33_par5 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paracea, data = data_char)
mod33_par_step5 <- stepAIC(mod33_par5)
summary(mod33_par_step5)

mod33_par6 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paracan, data = data_char)
mod33_par_step6 <- stepAIC(mod33_par6)
summary(mod33_par_step6)

mod33_par7 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num 
                 + paracne, data = data_char)
mod33_par_step7 <- stepAIC(mod33_par7)
summary(mod33_par_step7)

mod33_par8 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraean, data = data_char)
mod33_par_step8 <- stepAIC(mod33_par8)
summary(mod33_par_step8)

mod33_par9 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num
                 + paraeno, data = data_char)
mod33_par_step9 <- stepAIC(mod33_par9)
summary(mod33_par_step9)

mod33_par10 <- lm(good ~ log(age) + male + tea_coffee + like_boardgames  + pic_num +
                    paraano, data = data_char)
mod33_par_step10 <- stepAIC(mod33_par10)
summary(mod33_par_step10)

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}
m31 <- mod23_par_step4
m32 <- mod23_par_step6
m33 <- mod23_par_step7
m34 <- mod23_par_step8
m35 <- mod23_par_step9
m36 <- mod23_par_step10
stargazer(m31, m32, m33, m34, m35, m36, 
          se=list(cse(m31), cse(m32), cse(m33), cse(m34), cse(m35), cse(m36)),
          title="Модели с парами", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/с тройками по одной.html")



# модели с одной переменной ####################################################
#первая компонента из PCA ------------------------------------------------------
#построим модель
View(data_pict_itog)
dataPCA_1 <- dplyr::select(data_pict_itog, prob, good, like)
corrplot(cor(dataPCA_1), addgrid.col = TRUE, addCoef.col = TRUE)
mod_PCA_lm <- PCA(dataPCA_1)
fviz_eig(mod_PCA_lm, addlabels = TRUE) #первые две компоненты позволяют объяснить 94% изменения всех данных
#матрица нагрузок
corrplot(mod_PCA_lm$var$coord, is.corr = FALSE)
#для наглядности добавим значения
corrplot(mod_PCA_lm$var$coord, is.corr = FALSE, addgrid.col = TRUE, addCoef.col = TRUE)
#интерпретация
#первая компонента - как раз то, что нужно)
dim_att <- mod_PCA_lm$ind$coord[,1]
data_pict_itog$dim_att <- dim_att
data_pict_itog$pic_num <- as.factor(data_pict_itog$pic_num)

# модель 1 ####
mod_dim1 <- lm(dim_att ~ dist + age + male + tea_coffee + like_boardgames + pic_num, data = data_pict_itog)
summary(mod_dim1)

### чистка выбросов ############################
plot(mod_dim1, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_dim1) #да будто все и нормально

mod_dim11 <- stepAIC(mod_dim1)
#проверим не сделали ли хуже
anova(mod_dim1, mod_dim11)
#короткая лучше
summary(mod_dim11)

dev.off()
influencePlot(mod_dim11)

data_dim1 <- data_pict_itog[-514,]
data_dim1 <- data_dim1[-261,]
mod_dim1 <- update(mod_dim11, data = data_dim1)
plot(mod_dim1, which = 2)
influencePlot(mod_dim1)

#теперь расстояние Кука
cook_dim1 <- cooks.distance(mod_dim1)
plot(cook_dim1, type = "h")
which.max(cook_dim1)
summary(mod_dim1)

#тест Рамсея
resettest(mod_dim1)
#на 1% уровне значимости не отвергаем

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_dim1) #гетероскедастичность есть
#тест Уйта
white_lm(mod_dim1) #а тут говорит, что нет
#в любом случае, я использую робастные ошибки

#среднее арифметическое --------------------------------------------------------
# модель 2 ####
mean_att <- ((data_pict_itog$prob/3*5) + data_pict_itog$like + data_pict_itog$good)/3
data_pict_itog$mean_att <- mean_att
data_pict_itog$pic_num <- as.factor(data_pict_itog$pic_num)
View(data_pict_itog)

mod_mean <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + pic_num, data = data_pict_itog)
summary(mod_mean)

plot(mod_mean, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_mean) #да будто все и нормально

mod_mean1 <- stepAIC(mod_mean)
#проверим не сделали ли хуже
anova(mod_mean, mod_mean1)
#короткая лучше

influencePlot(mod_mean1)
data_mean <- data_pict_itog[-514,]
data_mean <- data_mean[-261,]
mod_mean <- update(mod_mean1, data = data_mean)
plot(mod_mean, which = 2)
influencePlot(mod_mean) #хватит
#теперь расстояние Кука
cook_mean <- cooks.distance(mod_mean)
plot(cook_mean, type = "h")
#думаю, достаточно
summary(mod_mean)

#теперь подбираем спецификацию 
plot(mod_mean, which = 2)
boxCox(mod_mean) #логарифм, но не будем

#тест Рамсея
resettest(mod_mean)
#ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_mean) #есть
#тест Уйта
white_lm(mod_mean) #нет
#в любом случае использую робастные ошибки

#в одну таблицу ######
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod_dim1, mod_mean,
          se=list(cse(mod_dim1), cse(mod_mean)),
          title="модели с объединенной зависимой переменной", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mod general.html")



#добавим переменные ############################################################
View(data_pict_itog)
View(data_char)

data_help <- data.frame(data_pict_itog$id, data_pict_itog$pic_num, 
           data_pict_itog$mean_att, data_pict_itog$dim_att)
View(data_help)
names(data_help)[1] <- "id"
names(data_help)[2] <- "pic_num"
names(data_help)[3] <- "mean_att"
names(data_help)[4] <- "dim_att"
data_char <- merge(data_char, data_help, by = c('id', "pic_num"))


#модели с расстоянием и чертами  ###############################################
# Like ------------------------------------------------------------
mod_gen_like <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                     pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                   + d_neuro.x, data = data_char)
summary(mod_gen_like)

plot(mod_gen_like, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_like2 <- stepAIC(mod_gen_like)
#проверим не сделали ли хуже
anova(mod_gen_like, mod_gen_like2)
#короткая лучше

vif(mod_gen_like)
crPlots(mod_gen_like)

mod_gen_like2 <- update(mod_gen_like, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                        + I(d_neuro.x^2) + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod_gen_like2)
crPlots(mod_gen_like2) #ок

mod_gen_like22 <- stepAIC(mod_gen_like2)
#проверим не сделали ли хуже
anova(mod_gen_like2, mod_gen_like22)
#короткая лучше
summary(mod_gen_like22)

#тест Рамсея
resettest(mod_gen_like22)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like22) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_gen_like22, order.by = data_char$age) #этот говорит нет..)
#тест Уйта
white_lm(mod_gen_like22) #есть
#использую робастные ошибки

# отдельно для каждой черты O --------------------------------------------------
mod_gen_like_o <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_open.x, data = data_char)
summary(mod_gen_like_o)

vif(mod_gen_like_o)
crPlots(mod_gen_like_o)

mod_gen_like_o2 <- update(mod_gen_like_o, .~. + I(d_open.x^2) - city - age + log(age))
summary(mod_gen_like_o2)
crPlots(mod_gen_like_o2) #ок

mod_gen_like_o22 <- stepAIC(mod_gen_like_o2)
#проверим не сделали ли хуже
anova(mod_gen_like_o2, mod_gen_like_o22)
#короткая лучше
summary(mod_gen_like_o22)

#тест Рамсея
resettest(mod_gen_like_o22)
#не норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like_o22) #есть
#тест Уйта
white_lm(mod_gen_like_o22) #тут говорит нет
#использую робастные ошибки

# отдельно для каждой черты C --------------------------------------------------
mod_gen_like_c <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_consci.x, data = data_char)
summary(mod_gen_like_c)

vif(mod_gen_like_c)
crPlots(mod_gen_like_c)

mod_gen_like_c2 <- update(mod_gen_like_c, .~. + I(d_consci.x^2) - city - age + log(age))
summary(mod_gen_like_c2)
crPlots(mod_gen_like_c2) #ок

mod_gen_like_c22 <- stepAIC(mod_gen_like_c2)
#проверим не сделали ли хуже
anova(mod_gen_like_c2, mod_gen_like_c22)
#короткая лучше
summary(mod_gen_like_c22)

#тест Рамсея
resettest(mod_gen_like_c22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like_c22) #есть
#тест Уйта
white_lm(mod_gen_like_c22) #ну как бы есть
#использую робастные ошибки

# отдельно для каждой черты E --------------------------------------------------
mod_gen_like_e <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_extravers.x, data = data_char)
summary(mod_gen_like_e)

vif(mod_gen_like_e)
crPlots(mod_gen_like_e)

mod_gen_like_e2 <- update(mod_gen_like_e, .~. + I(d_extravers.x^2) - city - age + log(age))
summary(mod_gen_like_e2)
crPlots(mod_gen_like_e2) #ок

mod_gen_like_e22 <- stepAIC(mod_gen_like_e2)
#проверим не сделали ли хуже
anova(mod_gen_like_e2, mod_gen_like_e22)
#короткая лучше
summary(mod_gen_like_e22)

#тест Рамсея
resettest(mod_gen_like_e22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like_e22) #есть
#тест Уйта
white_lm(mod_gen_like_e22) #есть
#использую робастные ошибки

# отдельно для каждой черты A --------------------------------------------------
mod_gen_like_a <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_agreeabl.x, data = data_char)
summary(mod_gen_like_a)

vif(mod_gen_like_a)
crPlots(mod_gen_like_a)

mod_gen_like_a2 <- update(mod_gen_like_a, .~. + I(d_agreeabl.x^2) - city - age + log(age))
summary(mod_gen_like_a2)
crPlots(mod_gen_like_a2) #ок

mod_gen_like_a22 <- stepAIC(mod_gen_like_a2)
#проверим не сделали ли хуже
anova(mod_gen_like_a2, mod_gen_like_a22)
#короткая лучше
summary(mod_gen_like_a22)

#тест Рамсея
resettest(mod_gen_like_a22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like_a22) #кажется, гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_like_a22) #нет
#использую робастные ошибки

# отдельно для каждой черты N --------------------------------------------------
mod_gen_like_n <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_neuro.x, data = data_char)
summary(mod_gen_like_n)

vif(mod_gen_like_n)
crPlots(mod_gen_like_n)

mod_gen_like_n2 <- update(mod_gen_like_n, .~. + I(d_neuro.x^2) - city - age + log(age))
summary(mod_gen_like_n2)
crPlots(mod_gen_like_n2) #ок

mod_gen_like_n22 <- stepAIC(mod_gen_like_n2)
#проверим не сделали ли хуже
anova(mod_gen_like_n2, mod_gen_like_n22)
#короткая лучше
summary(mod_gen_like_n22)

#тест Рамсея
resettest(mod_gen_like_n22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like_n22) #есть
#тест Уйта
white_lm(mod_gen_like_n22) #нет
#использую робастные ошибки

#в одну таблицу ----------------------------------------------------------------
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

mgl22 <- mod_gen_like22
mgl_o22 <- mod_gen_like_o22
mgl_c22 <- mod_gen_like_c22
mgl_e22 <- mod_gen_like_e22
mgl_a22 <- mod_gen_like_a22
mgl_n22 <- mod_gen_like_n22
stargazer(mgl22, mgl_o22, mgl_c22, mgl_e22, mgl_a22, mgl_n22,
          se = list(cse(mgl22), cse(mgl_o22), cse(mgl_c22), cse(mgl_e22), cse(mgl_a22), cse(mgl_n22)),
          title="Линейные модели расстояние и черты LIKE", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_like_c чертами .html")

# Good -------------------------------------------------------------------------
mod_gen_good <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + city + 
                     pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                   + d_neuro.x, data = data_char)
summary(mod_gen_good)

plot(mod_gen_good, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_good2 <- stepAIC(mod_gen_good)
#проверим не сделали ли хуже
anova(mod_gen_good, mod_gen_good2)
#длинная лучше

vif(mod_gen_good)
crPlots(mod_gen_good)

mod_gen_good2 <- update(mod_gen_good, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                        + I(d_neuro.x^2) - city + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod_gen_good2)
crPlots(mod_gen_good2) #ок

mod_gen_good22 <- stepAIC(mod_gen_good2)
#проверим не сделали ли хуже
anova(mod_gen_good2, mod_gen_good22)
#короткая лучше
summary(mod_gen_good22)

#тест Рамсея
resettest(mod_gen_good22)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good22) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_gen_good22, order.by = data_char$age) #есть
#тест Уйта
white_lm(mod_gen_good22) #есть
#использую робастные ошибки

# отдельно для каждой черты O --------------------------------------------------
mod_gen_good_o <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_open.x, data = data_char)
summary(mod_gen_good_o)

vif(mod_gen_good_o)
crPlots(mod_gen_good_o)

mod_gen_good_o2 <- update(mod_gen_good_o, .~. + I(d_open.x^2) - city - age + log(age))
summary(mod_gen_good_o2)
crPlots(mod_gen_good_o2) #ок

mod_gen_good_o22 <- stepAIC(mod_gen_good_o2)
#проверим не сделали ли хуже
anova(mod_gen_good_o2, mod_gen_good_o22)
#короткая лучше
summary(mod_gen_good_o22)

#тест Рамсея
resettest(mod_gen_good_o22)
#не норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good_o22) #есть
#тест Уйта
white_lm(mod_gen_good_o22) #есть
#использую робастные ошибки

# отдельно для каждой черты C --------------------------------------------------
mod_gen_good_c <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_consci.x, data = data_char)
summary(mod_gen_good_c)

vif(mod_gen_good_c)
crPlots(mod_gen_good_c)

mod_gen_good_c2 <- update(mod_gen_good_c, .~. + I(d_consci.x^2) - city - age + log(age))
summary(mod_gen_good_c2)
crPlots(mod_gen_good_c2) #ок

mod_gen_good_c22 <- stepAIC(mod_gen_good_c2)
#проверим не сделали ли хуже
anova(mod_gen_good_c2, mod_gen_good_c22)
#короткая лучше
summary(mod_gen_good_c22)

#тест Рамсея
resettest(mod_gen_good_c22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good_c22) #есть
#тест Уйта
white_lm(mod_gen_good_c22) #ну как бы есть
#использую робастные ошибки

# отдельно для каждой черты E --------------------------------------------------
mod_gen_good_e <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_extravers.x, data = data_char)
summary(mod_gen_good_e)

vif(mod_gen_good_e)
crPlots(mod_gen_good_e)

mod_gen_good_e2 <- update(mod_gen_good_e, .~. + I(d_extravers.x^2) - city - age + log(age))
summary(mod_gen_good_e2)
crPlots(mod_gen_good_e2) #ок

mod_gen_good_e22 <- stepAIC(mod_gen_good_e2)
#проверим не сделали ли хуже
anova(mod_gen_good_e2, mod_gen_good_e22)
#короткая лучше
summary(mod_gen_good_e22)

#тест Рамсея
resettest(mod_gen_good_e22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good_e22) #есть
#тест Уйта
white_lm(mod_gen_good_e22) #есть
#использую робастные ошибки

# отдельно для каждой черты A --------------------------------------------------
mod_gen_good_a <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_agreeabl.x, data = data_char)
summary(mod_gen_good_a)

vif(mod_gen_good_a)
crPlots(mod_gen_good_a)

mod_gen_good_a2 <- update(mod_gen_good_a, .~. + I(d_agreeabl.x^2) - city - age + log(age))
summary(mod_gen_good_a2)
crPlots(mod_gen_good_a2) #ок

mod_gen_good_a22 <- stepAIC(mod_gen_good_a2)
#проверим не сделали ли хуже
anova(mod_gen_good_a2, mod_gen_good_a22)
#короткая лучше
summary(mod_gen_good_a22)

#тест Рамсея
resettest(mod_gen_good_a22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good_a22) #кажется, гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_good_a22) #нет
#использую робастные ошибки

# отдельно для каждой черты N --------------------------------------------------
mod_gen_good_n <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + 
                       city + pic_num + d_neuro.x, data = data_char)
summary(mod_gen_good_n)

vif(mod_gen_good_n)
crPlots(mod_gen_good_n)

mod_gen_good_n2 <- update(mod_gen_good_n, .~. + I(d_neuro.x^2) - city - age + log(age))
summary(mod_gen_good_n2)
crPlots(mod_gen_good_n2) #ок

mod_gen_good_n22 <- stepAIC(mod_gen_good_n2)
#проверим не сделали ли хуже
anova(mod_gen_good_n2, mod_gen_good_n22)
#короткая лучше
summary(mod_gen_good_n22)

#тест Рамсея
resettest(mod_gen_good_n22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good_n22) #есть
#тест Уйта
white_lm(mod_gen_good_n22) #нет
#использую робастные ошибки

#в одну таблицу ----------------------------------------------------------------
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

mgg22 <- mod_gen_good22
mgg_o22 <- mod_gen_good_o22
mgg_c22 <- mod_gen_good_c22
mgg_e22 <- mod_gen_good_e22
mgg_a22 <- mod_gen_good_a22
mgg_n22 <- mod_gen_good_n22
stargazer(mgg22, mgg_o22, mgg_c22, mgg_e22, mgg_a22, mgg_n22,
          se = list(cse(mgg22), cse(mgg_o22), cse(mgg_c22), cse(mgg_e22), cse(mgg_a22), cse(mgg_n22)),
          title="Линейные модели расстояние и черты GOOD", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_good_c чертами .html")


# с одной переменной ###########################################################
# среднее значение -------------------------------------------------------------
# Mean_att -------------------------------------------------------------------------
mod_gen_mean_att <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + city + 
                         pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                       + d_neuro.x, data = data_char)
summary(mod_gen_mean_att)

plot(mod_gen_mean_att, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_mean_att2 <- stepAIC(mod_gen_mean_att)
#проверим не сделали ли хуже
anova(mod_gen_mean_att, mod_gen_mean_att2)
#длинная лучше

vif(mod_gen_mean_att)
crPlots(mod_gen_mean_att)

mod_gen_mean_att2 <- update(mod_gen_mean_att, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                            + I(d_neuro.x^2) - city + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod_gen_mean_att2)
crPlots(mod_gen_mean_att2) #ок

mod_gen_mean_att22 <- stepAIC(mod_gen_mean_att2)
#проверим не сделали ли хуже
anova(mod_gen_mean_att2, mod_gen_mean_att22)
#короткая лучше
summary(mod_gen_mean_att22)

#тест Рамсея
resettest(mod_gen_mean_att22)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att22) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_gen_mean_att22, order.by = data_char$age) #есть
#тест Уйта
white_lm(mod_gen_mean_att22) #есть
#использую робастные ошибки

# отдельно для каждой черты O --------------------------------------------------
mod_gen_mean_att_o <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + pic_num + d_open.x, data = data_char)
summary(mod_gen_mean_att_o)

vif(mod_gen_mean_att_o)
crPlots(mod_gen_mean_att_o)

mod_gen_mean_att_o2 <- update(mod_gen_mean_att_o, .~. + I(d_open.x^2) - city - age + log(age))
summary(mod_gen_mean_att_o2)
crPlots(mod_gen_mean_att_o2) #ок

mod_gen_mean_att_o22 <- stepAIC(mod_gen_mean_att_o2)
#проверим не сделали ли хуже
anova(mod_gen_mean_att_o2, mod_gen_mean_att_o22)
#короткая лучше
summary(mod_gen_mean_att_o22)

#тест Рамсея
resettest(mod_gen_mean_att_o22)
#не норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att_o22) #есть
#тест Уйта
white_lm(mod_gen_mean_att_o22) #есть
#использую робастные ошибки

# отдельно для каждой черты C --------------------------------------------------
mod_gen_mean_att_c <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + 
                           city + pic_num + d_consci.x, data = data_char)
summary(mod_gen_mean_att_c)

vif(mod_gen_mean_att_c)
crPlots(mod_gen_mean_att_c)

mod_gen_mean_att_c2 <- update(mod_gen_mean_att_c, .~. + I(d_consci.x^2) - city - age + log(age))
summary(mod_gen_mean_att_c2)
crPlots(mod_gen_mean_att_c2) #ок

mod_gen_mean_att_c22 <- stepAIC(mod_gen_mean_att_c2)
#проверим не сделали ли хуже
anova(mod_gen_mean_att_c2, mod_gen_mean_att_c22)
#короткая лучше
summary(mod_gen_mean_att_c22)

#тест Рамсея
resettest(mod_gen_mean_att_c22)
#не норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att_c22) #есть
#тест Уйта
white_lm(mod_gen_mean_att_c22) #нет
#использую робастные ошибки

# отдельно для каждой черты E --------------------------------------------------
mod_gen_mean_att_e <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + 
                           city + pic_num + d_extravers.x, data = data_char)
summary(mod_gen_mean_att_e)

vif(mod_gen_mean_att_e)
crPlots(mod_gen_mean_att_e)

mod_gen_mean_att_e2 <- update(mod_gen_mean_att_e, .~. + I(d_extravers.x^2) 
                              - city - age + log(age))
summary(mod_gen_mean_att_e2)
crPlots(mod_gen_mean_att_e2) #ок

mod_gen_mean_att_e22 <- stepAIC(mod_gen_mean_att_e2)
#проверим не сделали ли хуже
anova(mod_gen_mean_att_e2, mod_gen_mean_att_e22)
#короткая лучше
summary(mod_gen_mean_att_e22)

#тест Рамсея
resettest(mod_gen_mean_att_e22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att_e22) #есть
#тест Уйта
white_lm(mod_gen_mean_att_e22) #есть
#использую робастные ошибки

# отдельно для каждой черты A --------------------------------------------------
mod_gen_mean_att_a <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + 
                           city + pic_num + d_agreeabl.x, data = data_char)
summary(mod_gen_mean_att_a)

vif(mod_gen_mean_att_a)
crPlots(mod_gen_mean_att_a)

mod_gen_mean_att_a2 <- update(mod_gen_mean_att_a, .~. + I(d_agreeabl.x^2) 
                              - city - age + log(age))
summary(mod_gen_mean_att_a2)
crPlots(mod_gen_mean_att_a2) #ок

mod_gen_mean_att_a22 <- stepAIC(mod_gen_mean_att_a2)
#проверим не сделали ли хуже
anova(mod_gen_mean_att_a2, mod_gen_mean_att_a22)
#короткая лучше
summary(mod_gen_mean_att_a22)

#тест Рамсея
resettest(mod_gen_mean_att_a22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att_a22) #no
#тест Уйта
white_lm(mod_gen_mean_att_a22) #нет
#использую робастные ошибки

# отдельно для каждой черты N --------------------------------------------------
mod_gen_mean_att_n <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + 
                           city + pic_num + d_neuro.x, data = data_char)
summary(mod_gen_mean_att_n)

vif(mod_gen_mean_att_n)
crPlots(mod_gen_mean_att_n)

mod_gen_mean_att_n2 <- update(mod_gen_mean_att_n, .~. + I(d_neuro.x^2) 
                              - city - age + log(age))
summary(mod_gen_mean_att_n2)
crPlots(mod_gen_mean_att_n2) #ок

mod_gen_mean_att_n22 <- stepAIC(mod_gen_mean_att_n2)
#проверим не сделали ли хуже
anova(mod_gen_mean_att_n2, mod_gen_mean_att_n22)
#короткая лучше
summary(mod_gen_mean_att_n22)

#тест Рамсея
resettest(mod_gen_mean_att_n22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att_n22) #есть
#тест Уйта
white_lm(mod_gen_mean_att_n22) #нет
#использую робастные ошибки

#в одну таблицу ----------------------------------------------------------------
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

mma22 <- mod_gen_mean_att22
mma_o22 <- mod_gen_mean_att_o22
mma_c22 <- mod_gen_mean_att_c22
mma_e22 <- mod_gen_mean_att_e22
mma_a22 <- mod_gen_mean_att_a22
mma_n22 <- mod_gen_mean_att_n22
stargazer(mma22, mma_o22, mma_c22, mma_e22, mma_a22, mma_n22,
          se = list(cse(mma22), cse(mma_o22), cse(mma_c22), cse(mma_e22), cse(mma_a22), cse(mma_n22)),
          title="Линейные модели расстояние и черты MEAN_ATT", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_mean_att_c чертами .html")


# Dim_att -------------------------------------------------------------------------
mod_gen_dim_att <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + city + 
                        pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                      + d_neuro.x, data = data_char)
summary(mod_gen_dim_att)

plot(mod_gen_dim_att, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_dim_att2 <- stepAIC(mod_gen_dim_att)
#проверим не сделали ли хуже
anova(mod_gen_dim_att, mod_gen_dim_att2)
#длинная лучше

vif(mod_gen_dim_att)
crPlots(mod_gen_dim_att)

mod_gen_dim_att2 <- update(mod_gen_dim_att, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                           + I(d_neuro.x^2) - city + I(d_consci.x^2) + I(d_open.x^2) 
                           - age + log(age))
summary(mod_gen_dim_att2)
crPlots(mod_gen_dim_att2) #ок

mod_gen_dim_att22 <- stepAIC(mod_gen_dim_att2)
#проверим не сделали ли хуже
anova(mod_gen_dim_att2, mod_gen_dim_att22)
#короткая лучше
summary(mod_gen_dim_att22)

#тест Рамсея
resettest(mod_gen_dim_att22)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att22) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_gen_dim_att22, order.by = data_char$age) #есть
#тест Уйта
white_lm(mod_gen_dim_att22) #нет
#использую робастные ошибки

# отдельно для каждой черты O --------------------------------------------------
mod_gen_dim_att_o <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + 
                          city + pic_num + d_open.x, data = data_char)
summary(mod_gen_dim_att_o)

vif(mod_gen_dim_att_o)
crPlots(mod_gen_dim_att_o)

mod_gen_dim_att_o2 <- update(mod_gen_dim_att_o, .~. + I(d_open.x^2) - city 
                             - age + log(age))
summary(mod_gen_dim_att_o2)
crPlots(mod_gen_dim_att_o2) #ок

mod_gen_dim_att_o22 <- stepAIC(mod_gen_dim_att_o2)
#проверим не сделали ли хуже
anova(mod_gen_dim_att_o2, mod_gen_dim_att_o22)
#короткая лучше
summary(mod_gen_dim_att_o22)

#тест Рамсея
resettest(mod_gen_dim_att_o22)
#не норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att_o22) #есть
#тест Уйта
white_lm(mod_gen_dim_att_o22) #no
#использую робастные ошибки

# отдельно для каждой черты C --------------------------------------------------
mod_gen_dim_att_c <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + 
                          city + pic_num + d_consci.x, data = data_char)
summary(mod_gen_dim_att_c)

vif(mod_gen_dim_att_c)
crPlots(mod_gen_dim_att_c)

mod_gen_dim_att_c2 <- update(mod_gen_dim_att_c, .~. + I(d_consci.x^2) - city 
                             - age + log(age))
summary(mod_gen_dim_att_c2)
crPlots(mod_gen_dim_att_c2) #ок

mod_gen_dim_att_c22 <- stepAIC(mod_gen_dim_att_c2)
#проверим не сделали ли хуже
anova(mod_gen_dim_att_c2, mod_gen_dim_att_c22)
#короткая лучше
summary(mod_gen_dim_att_c22)

#тест Рамсея
resettest(mod_gen_dim_att_c22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att_c22) #есть
#тест Уйта
white_lm(mod_gen_dim_att_c22) #ну как бы есть
#использую робастные ошибки

# отдельно для каждой черты E --------------------------------------------------
mod_gen_dim_att_e <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + 
                          city + pic_num + d_extravers.x, data = data_char)
summary(mod_gen_dim_att_e)

vif(mod_gen_dim_att_e)
crPlots(mod_gen_dim_att_e)

mod_gen_dim_att_e2 <- update(mod_gen_dim_att_e, .~. + I(d_extravers.x^2) - city
                             - age + log(age))
summary(mod_gen_dim_att_e2)
crPlots(mod_gen_dim_att_e2) #ок

mod_gen_dim_att_e22 <- stepAIC(mod_gen_dim_att_e2)
#проверим не сделали ли хуже
anova(mod_gen_dim_att_e2, mod_gen_dim_att_e22)
#короткая лучше
summary(mod_gen_dim_att_e22)

#тест Рамсея
resettest(mod_gen_dim_att_e22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att_e22) #есть
#тест Уйта
white_lm(mod_gen_dim_att_e22) #есть
#использую робастные ошибки

# отдельно для каждой черты A --------------------------------------------------
mod_gen_dim_att_a <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + 
                          city + pic_num + d_agreeabl.x, data = data_char)
summary(mod_gen_dim_att_a)

vif(mod_gen_dim_att_a)
crPlots(mod_gen_dim_att_a)

mod_gen_dim_att_a2 <- update(mod_gen_dim_att_a, .~. + I(d_agreeabl.x^2) - city
                             - age + log(age))
summary(mod_gen_dim_att_a2)
crPlots(mod_gen_dim_att_a2) #ок

mod_gen_dim_att_a22 <- stepAIC(mod_gen_dim_att_a2)
#проверим не сделали ли хуже
anova(mod_gen_dim_att_a2, mod_gen_dim_att_a22)
#короткая лучше
summary(mod_gen_dim_att_a22)

#тест Рамсея
resettest(mod_gen_dim_att_a22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att_a22) #кажется, гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_dim_att_a22) #нет
#использую робастные ошибки

# отдельно для каждой черты N --------------------------------------------------
mod_gen_dim_att_n <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + 
                          city + pic_num + d_neuro.x, data = data_char)
summary(mod_gen_dim_att_n)

vif(mod_gen_dim_att_n)
crPlots(mod_gen_dim_att_n)

mod_gen_dim_att_n2 <- update(mod_gen_dim_att_n, .~. + I(d_neuro.x^2) - city
                             - age + log(age))
summary(mod_gen_dim_att_n2)
crPlots(mod_gen_dim_att_n2) #ок

mod_gen_dim_att_n22 <- stepAIC(mod_gen_dim_att_n2)
#проверим не сделали ли хуже
anova(mod_gen_dim_att_n2, mod_gen_dim_att_n22)
#короткая лучше
summary(mod_gen_dim_att_n22)

#тест Рамсея
resettest(mod_gen_dim_att_n22)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att_n22) #есть
#тест Уйта
white_lm(mod_gen_dim_att_n22) #нет
#использую робастные ошибки

#в одну таблицу ----------------------------------------------------------------
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

mda22 <- mod_gen_dim_att22
mda_o22 <- mod_gen_dim_att_o22
mda_c22 <- mod_gen_dim_att_c22
mda_e22 <- mod_gen_dim_att_e22
mda_a22 <- mod_gen_dim_att_a22
mda_n22 <- mod_gen_dim_att_n22
stargazer(mda22, mda_o22, mda_c22, mda_e22, mda_a22, mda_n22,
          se = list(cse(mda22), cse(mda_o22), cse(mda_c22), cse(mda_e22), cse(mda_a22), cse(mda_n22)),
          title="Линейные модели расстояние и черты DIM_ATT", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_dim_att_c чертами .html")



#в одну таблицу модели со всеми переменными ------------------------------------
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mgl22, mgg22, mma22, mda22,
          se = list(cse(mgl22), cse(mgg22), cse(mma22), cse(mda22)),
          title = "Линейные модели расстояние и черты", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods c чертами.html")




#модели с расстоянием и чертами для границы 50 лет ############################
#добавим переменные ############################################################
data_50year <- dplyr::filter(data_char, age <= 50)
View(data_50year)

#модели с расстоянием и чертами  ###############################################
# Like ------------------------------------------------------------
mod_gen_like_50 <- lm(like ~ dist + male + age + tea_coffee + like_boardgames + 
                        pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                      + d_neuro.x, data = data_50year)
summary(mod_gen_like_50)

plot(mod_gen_like_50, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_like2_50 <- stepAIC(mod_gen_like_50)
#проверим не сделали ли хуже
anova(mod_gen_like_50, mod_gen_like2_50)
#короткая лучше

vif(mod_gen_like_50)
crPlots(mod_gen_like_50)

mod_gen_like2_50 <- update(mod_gen_like_50, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                           + I(d_neuro.x^2) + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod_gen_like2_50)
crPlots(mod_gen_like2_50) #ок

mod_gen_like22_50 <- stepAIC(mod_gen_like2_50)
#проверим не сделали ли хуже
anova(mod_gen_like2_50, mod_gen_like22_50)
#короткая лучше
summary(mod_gen_like22_50)

#тест Рамсея
resettest(mod_gen_like22_50)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_like22_50) #гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_like22_50) #нет
#использую робастные ошибки

# Good -------------------------------------------------------------------------
mod_gen_good_50 <- lm(good ~ dist + male + age + tea_coffee + like_boardgames + city + 
                        pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                      + d_neuro.x, data = data_50year)
summary(mod_gen_good_50)

plot(mod_gen_good_50, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_good2_50 <- stepAIC(mod_gen_good_50)
#проверим не сделали ли хуже
anova(mod_gen_good_50, mod_gen_good2_50)
#длинная лучше

vif(mod_gen_good_50 )
crPlots(mod_gen_good_50 )

mod_gen_good2_50  <- update(mod_gen_good_50 , .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                            + I(d_neuro.x^2) - city + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod_gen_good2_50 )
crPlots(mod_gen_good2_50 ) #ок

mod_gen_good22_50  <- stepAIC(mod_gen_good2_50 )
#проверим не сделали ли хуже
anova(mod_gen_good2_50 , mod_gen_good22_50 )
#короткая лучше
summary(mod_gen_good22_50 )

#тест Рамсея
resettest(mod_gen_good22_50)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_good22_50 ) #гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_good22_50 ) #есть
#использую робастные ошибки

# с одной переменной ###########################################################
# среднее значение -------------------------------------------------------------
# Mean_att -------------------------------------------------------------------------
mod_gen_mean_att_50 <- lm(mean_att ~ dist + male + age + tea_coffee + like_boardgames + city + 
                            pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                          + d_neuro.x, data = data_50year)
summary(mod_gen_mean_att_50)

plot(mod_gen_mean_att_50, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_mean_att2_50 <- stepAIC(mod_gen_mean_att_50)
#проверим не сделали ли хуже
anova(mod_gen_mean_att_50, mod_gen_mean_att2_50)
#длинная лучше

vif(mod_gen_mean_att_50)
crPlots(mod_gen_mean_att_50)

mod_gen_mean_att2_50 <- update(mod_gen_mean_att_50, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                               + I(d_neuro.x^2) - city + I(d_consci.x^2) + I(d_open.x^2) - age + log(age))
summary(mod_gen_mean_att2_50)
crPlots(mod_gen_mean_att2_50) #ок

mod_gen_mean_att22_50 <- stepAIC(mod_gen_mean_att2_50)
#проверим не сделали ли хуже
anova(mod_gen_mean_att2_50, mod_gen_mean_att22_50)
#короткая лучше
summary(mod_gen_mean_att22_50)

#тест Рамсея
resettest(mod_gen_mean_att22_50)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_mean_att22_50) #гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_mean_att22_50) #есть
#использую робастные ошибки

# Dim_att -------------------------------------------------------------------------
mod_gen_dim_att_50 <- lm(dim_att ~ dist + male + age + tea_coffee + like_boardgames + city + 
                           pic_num + d_open.x + d_consci.x + d_extravers.x + d_agreeabl.x 
                         + d_neuro.x, data = data_50year)
summary(mod_gen_dim_att_50)

plot(mod_gen_dim_att_50, which = 2)
#для начала посмотрим hatvalues и расстояние Кука

mod_gen_dim_att2_50 <- stepAIC(mod_gen_dim_att_50)
#проверим не сделали ли хуже
anova(mod_gen_dim_att_50, mod_gen_dim_att2_50)
#длинная лучше

vif(mod_gen_dim_att_50)
crPlots(mod_gen_dim_att_50)

mod_gen_dim_att2_50 <- update(mod_gen_dim_att_50, .~. + I(d_extravers.x^2) + I(d_agreeabl.x^2)
                              + I(d_neuro.x^2) - city + I(d_consci.x^2) + I(d_open.x^2) 
                              - age + log(age))
summary(mod_gen_dim_att2_50)
crPlots(mod_gen_dim_att2_50) #ок

mod_gen_dim_att22_50 <- stepAIC(mod_gen_dim_att2_50)
#проверим не сделали ли хуже
anova(mod_gen_dim_att2_50, mod_gen_dim_att22_50)
#короткая лучше
summary(mod_gen_dim_att22_50)

#тест Рамсея
resettest(mod_gen_dim_att22_50)
#все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_gen_dim_att22_50) #гетероскедастичность есть
#тест Уйта
white_lm(mod_gen_dim_att22_50) #нет
#использую робастные ошибки

#в одну таблицу модели со всеми переменными ------------------------------------
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

mod1_50 <- mod_gen_like22_50
mod2_50 <- mod_gen_good22_50
mod3_50 <- mod_gen_mean_att22_50
mod4_50 <- mod_gen_dim_att22_50


stargazer(mod1_50, mod2_50, mod3_50, mod4_50,
          se = list(cse(mod1_50), cse(mod2_50), cse(mod3_50), cse(mod4_50)),
          title = "Линейные модели расстояние и черты", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods c чертами 50.html")





################################################################################
#Логит модели
################################################################################
# логит для расстояния граница 3------------------------------------------------
data_char %>% group_by(like) %>% summarize(count=n())
data_char %>% group_by(good) %>% summarize(count=n())
data_char %>% group_by(prob) %>% summarize(count=n())

data_char$prob_p <- ifelse(data_char$prob >= 2, 1, 0)
data_char$like_p <- ifelse(data_char$like >= 3, 1, 0)
data_char$good_p <- ifelse(data_char$good >= 3, 1, 0)
data_char$pic_num <- as.factor(data_char$pic_num)
table(data_char$prob_p)
View(data_char)

# prob ####
mod_prob_p <- glm(prob_p ~ dist + male + age + tea_coffee + like_boardgames
                  + pic_num, data = data_char, family = binomial(link = "logit"))
summary(mod_prob_p)
vif(mod_prob_p)#все хорошо

influencePlot(mod_prob_p)
data_prop_p <- data_prop_p[-481,]
mod_prob_p <- update(mod_prob_p, data = data_prop_p)
cook_log_prob <- cooks.distance(mod_prob_p)
plot(cook_log_prob, type = "h")
which.max(cook_log_prob)
summary(mod_prob_p)

mod_prob_p2 <- stepAIC(mod_prob_p)

#тест отношения правдоподобия
lrtest(mod_prob_p, mod_prob_p2) #H0: модели одинаковые - короткая лучше
#короткая лучше
mod_prob_p <- mod_prob_p2

#качество модели
hitmiss(mod_prob_p) #смогли предсказать 65,9% исходов
summary(mod_prob_p)

#посчитаем balanced acc
TPRp <- 633/966
TNRp <- 81/133
baccp <- (TPRp + TNRp)/2
baccp #смогли предсказать в 63,2% случаев

#тест Рамсея
resettest(mod_prob_p)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_prob_p) #есть
#тест Уйта
white.test(x = cbind(data_prop_p$male,data_prop_p$age,data_prop_p$tea_coffee,data_prop_p$like_boardgames), 
           y = data_prop_p$prob_p) #нет
#использую робастные ошибки


# like ####
mod_like_p <- glm(like_p ~ dist + male + age + tea_coffee + like_boardgames + pic_num,
                  data = data_char, family = binomial(link = "logit"))
summary(mod_like_p)
mod_like_p2 <- stepAIC(mod_like_p)
lrtest(mod_like_p, mod_like_p2) #короткая лучше

influencePlot(mod_like_p2)
data_llike <- data_char[-481,]
mod_like_p2 <- update(mod_like_p2, data = data_llike)
cook_log_like <- cooks.distance(mod_like_p2)
plot(cook_log_like, type = "h")
which.max(cook_log_like) #все нормально

#тест Рамсея
resettest(mod_like_p2)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_like_p2) #нет
#тест Уйта
white.test(x = cbind(data_char$mahal, data_char$age, data_char$like_boardgames, data_char$pic_num), 
           y = data_char$like_p) #нет

# good ####
mod_good_p <- glm(good_p ~ dist + male + age + tea_coffee + like_boardgames + pic_num, data = data_char, family = binomial(link = "logit"))
summary(mod_good_p2)
mod_good_p2 <- stepAIC(mod_good_p)
lrtest(mod_good_p, mod_good_p2) #короткая лучше

influencePlot(mod_good_p2)
data_lgood <- data_char[-481,]
mod_good_p2 <- update(mod_good_p2, data = data_lgood)
cook_l_good <- cooks.distance(mod_good_p2)
plot(cook_l_good, type = "h")
which.max(cook_l_good)

#тест Рамсея
resettest(mod_good_p2)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_good_p2) #нет
#тест Уйта
white.test(x = cbind(data_lgood$mahal, data_lgood$age, data_lgood$like_boardgames, data_lgood$pic_num), 
           y = data_lgood$good_p) #нет

stargazer(mod_prob_p, mod_good_p2, mod_like_p2,
          title="Логит", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/модели логит.html")


# Логит с факторами -----------------------------------------------------------
data_char$pic_num <- as.factor(data_char$pic_num)

#prob --------------------------------------------------------------------------
mod_prob_p3 <- glm(prob_p ~ male + age + tea_coffee + like_boardgames  + pic_num + delta_o
                   + delta_c + delta_e + delta_a + delta_n, data = data_char, 
                   family = binomial(link = "logit"))
summary(mod_prob_p3)

mod_prob_p32 <- stepAIC(mod_prob_p3)
summary(mod_prob_p32)
lrtest(mod_prob_p31, mod_prob_p32) #короткая лучше

dev.off()
influencePlot(mod_prob_p32)
data_prop_p3 <- data_char[-345,]
mod_prob_p32 <- update(mod_prob_p32, data = data_prop_p3)
cook_log_prob3 <- cooks.distance(mod_prob_p32)
plot(cook_log_prob3, type = "h")
which.max(cook_log_prob3)
summary(mod_prob_p32)

#тест Рамсея
resettest(mod_prob_p32)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_prob_p32) #нет
#тест Уйта
white.test(x = cbind(data_char$male, data_char$age, data_char$like_boardgames, data_char$tea_coffee), 
           y = data_char$prob_p) #нет
#использую робастные ошибки

#like --------------------------------------------------------------------------
mod_like_p3 <- glm(like_p ~ male + age + tea_coffee + like_boardgames  + pic_num + delta_o
                   + delta_c + delta_e + delta_a + delta_n, data = data_char, 
                   family = binomial(link = "logit"))
summary(mod_like_p3)

mod_like_p33 <- stepAIC(mod_like_p3)

lrtest(mod_like_p3, mod_like_p33) #короткая лучше
summary(mod_like_p33)

influencePlot(mod_like_p33)
data_like_p3 <- data_char[-756,]
mod_like_p3 <- update(mod_like_p33, data = data_like_p3)
cook_like3 <- cooks.distance(mod_like_p3)
plot(cook_like3, type = "h")
which.max(cook_log_like3)
summary(mod_like_p3)

#тест Рамсея
resettest(mod_like_p3)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_like_p3) #нет
#тест Уйта
white.test(x = cbind(data_char$pic_num, data_char$age, data_char$like_boardgames, data_char$delta_o, data_char$delta_n), 
           y = data_char$like_p) #нет

#good --------------------------------------------------------------------------
mod_good_p3 <- glm(good_p ~ age + male +  tea_coffee + like_boardgames  + pic_num + delta_o
                   + delta_c + delta_e + delta_a + delta_n, data = data_char, 
                   family = binomial(link = "logit"))
summary(mod_good_p3)
vif(mod_good_p3)

mod_good_p33 <- stepAIC(mod_good_p3)

lrtest(mod_good_p3, mod_good_p33) #короткая лучше
summary(mod_good_p33)

influencePlot(mod_good_p33)
data_good_p3 <- data_char[-346,]
mod_good_p3 <- update(mod_good_p33, data = data_good_p3)
cook_log_good3 <- cooks.distance(mod_good_p3)
plot(cook_log_good3, type = "h")
which.max(cook_log_good3)
summary(mod_good_p3)

#тест Рамсея
resettest(mod_good_p3)
#норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_good_p3) #нет
#тест Уйта
white.test(x = cbind(data_char$pic_num, data_char$age, data_char$like_boardgames, data_char$delta_n), 
           y = data_char$good_p) #да
#использую робастные ошибки

#в одну таблицу #####
stargazer(mod_prob_p32, mod_like_p3, mod_good_p3,
          title="Логит", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/модели логит факторы.html")

#Порядковые модели -------------------------------------------------------------
# порядковый логит like --------------------------------------------------------
class_like <- ifelse(data_pict_itog$like >= 4, "A", ifelse(data_pict_itog$like == 3, "B", 
                                                           ifelse(data_pict_itog$like == 2, "C","D")))
#проверим
table(class_like)

class_like <- factor(class_like, levels = c("D", "C", "B", "A"), ordered = TRUE)
data_order <- data_pict_itog
data_order$class_like <- class_like

#модель
mod_order_like <- polr(class_like ~ dist + male + age + tea_coffee + like_boardgames
                       + d_agreeabl.x + d_consci.x +d_open.x + d_neuro.x
                       + d_extravers.x, data = data_order)
mod_order_like3 <- polr(class_like ~ dist + male + age + tea_coffee + like_boardgames
                       + d_agreeabl.x + I(d_agreeabl.x^2) + d_consci.x + I(d_consci.x^2)
                       + d_open.x + I(d_open.x^2) + d_neuro.x + I(d_neuro.x^2)
                       + d_extravers.x + I(d_extravers.x^2), data = data_order)
summary(mod_order_like)
coeftest(mod_order_like)
lrtest(mod_order_like, mod_order_like3) #с квадратами лучше
mod_order_like2 <- stepAIC(mod_order_like3)
anova(mod_order_like3, mod_order_like2) #короткая лучше
coeftest(mod_order_like2)
mtable(mod_order_like2, signif.symbols = c("***"=0.01, "**"=0.05,"*"=0.1))
plot(Effect("dist", mod_order_like))

#предельные эффекты
class_like1 <- factor(class_like, levels = c("A", "B", "C", "D"))
mod_marg_like <- oglmx(class_like1 ~ dist + male + age + tea_coffee + 
                         like_boardgames + d_agreeabl.x + I(d_agreeabl.x^2) + d_consci.x + I(d_consci.x^2) + d_neuro.x +
                         I(d_neuro.x^2) + d_extravers.x + I(d_extravers.x^2), data = data_order, delta = 0)
a <- margins.oglmx(mod_marg_like) #предельные эффекты
b <- data.frame(a$A, a$B, a$C, a$D)
View(b)
write_xlsx(b, "C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/b.xlsx")
#с ростом расстояния на 1 вероятность попасть в класс B снижается на 4,4%
#с ростом расстояния на 1 вероятность попасть в класс А снижается на 25,4%
# порядковый логит good 
# порядковый логит general -----------------------------------------------------
class_mean <- ifelse(data_pict_itog$mean_att >= 4, "A", ifelse(data_pict_itog$mean_att >= 3, "B", 
                                                           ifelse(data_pict_itog$mean_att >= 2, "C", "D")))
hist(data_pict_itog$mean_att)
#проверим
table(class_mean)

class_mean <- factor(class_mean, levels = c("D", "C", "B", "A"), ordered = TRUE)
data_order_m <- data_pict_itog
data_order_m$class_mean <- class_mean

#модель
mod_order_mean <- polr(class_mean ~ dist + male + age + tea_coffee + like_boardgames 
                       + d_agreeabl.x + d_consci.x + d_open.x + d_neuro.x
                       + d_extravers.x,
                       data = data_order_m)
mod_order_mean3 <- polr(class_mean ~ dist + male + age + tea_coffee + like_boardgames 
                       + d_agreeabl.x + I(d_agreeabl.x^2) + d_consci.x + I(d_consci.x^2)
                       + d_open.x + I(d_open.x^2) + d_neuro.x + I(d_neuro.x^2)
                       + d_extravers.x + I(d_extravers.x^2),
                       data = data_order_m)
lrtest(mod_order_mean, mod_order_mean3) #лучше с квадратами!
mod_order_mean2 <- stepAIC(mod_order_mean3)
anova(mod_order_mean3, mod_order_mean2) #короткая лучше
summary(mod_order_mean2)
coeftest(mod_order_mean2)
plot(Effect("dist", mod_order_mean2))
mtable(mod_order_mean2, signif.symbols = c("***"=0.01, "**"=0.05,"*"=0.1))

#предельные эффекты
class_mean1 <- factor(class_mean, levels = c("A", "B", "C", "D"), ordered = TRUE)
mod_marg_m <- oglmx(class_mean1 ~ dist + age + tea_coffee + like_boardgames + d_agreeabl.x + 
                      I(d_agreeabl.x^2) + d_consci.x + I(d_consci.x^2) + d_extravers.x + I(d_extravers.x^2), 
                    data = data_order_m, delta = 0)
margins.oglmx(mod_marg_m) #предельные эффекты
a <- margins.oglmx(mod_marg_m) #предельные эффекты
b <- data.frame(a$A, a$B, a$C, a$D)
View(b)
write_xlsx(b, "C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/b.xlsx")


library(modelsummary)


stargazer(mod_order_like2, mod_order_good2, mod_order_mean2,
          title="модели с объединенной зависимой переменной", type="text",
          df=FALSE, digits=3,
          out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/порядковые.html")


################################################################################
## КЛАСТЕРИЗАЦИЯ
################################################################################
#k-means------------------------------------------------------------------------
View(data_pict_itog)

#отберем столбцы
data <- dplyr::select(data_pict_itog, d_open.x:d_neuro.x)
View(data)
dim(data)

#картинка
plot1 <- ggplot(data, aes(x = d_open.x, y = d_consci.x)) + geom_point(size = 2)
plot1

#сделаем кластеризацию
mod <- kmeans(data, centers = 3, nstart = 100, iter.max = 100)

mod$size#сколько точек в каждом кластере

#визуализация с кластерами
fviz_cluster(mod, data = data, geom = "text", repel = TRUE, stand = TRUE)

т#график каменистой осыпи
fviz_nbclust(data, kmeans, "wss")

#построим модель
data_scale <- scale(data)
mod_PCA_clust <- PCA(data_scale)
fviz_eig(mod_PCA_clust, addlabels = TRUE) #первые две компоненты позволяют объяснить около 62% изменения всех данных
#матрица нагрузок
corrplot(mod_PCA_clust$var$coord, is.corr = FALSE)
#для наглядности добавим значения
corrplot(mod_PCA_clust$var$coord, is.corr = FALSE, addgrid.col = TRUE, addCoef.col = TRUE)

#интерпретация
#первая компонента - добросовестные экстраверты 
#вторая - открытые
#третья - нервные добряки
#четвертая - добросовестные
#пятая - экстраверты

#интерпретация главных компонент через квадраты косинусов (квадраты коэффициентов матрицы нагрузок)
corrplot(mod_PCA_clust$var$cos2, is.corr = FALSE)

#построим наблюденияв базисе из главных компонент
fviz_pca_biplot(mod_PCA_clust, repel = TRUE, geom = "point")
fviz_pca_ind(mod_PCA_clust)

#иерархическая кластеризация ---------------------------------------------------
mod_hcpc <- HCPC(mod_PCA_clust, cluster.CA = "rows", graph = T, nb.clust = 3)
fviz_cluster(mod_hcpc, palette = "lanonc_lancet", repel = TRUE, geom = "text")

#достанем принадлежность к кластеру для каждого человека 
clust1 <- mod$cluster
data$clust1 <- clust1
table(clust1)

clust2 <- mod2$data.clust$clust
data$clust2 <- clust2
table(data$clust2)

data$clust1 <- as.numeric(data$clust1)
data$clust2 <- as.numeric(data$clust2)
cor(data$clust1, data$clust2)

# Модели с кластеризацией - как отличаются люди в разных кластерах #############
data_pitog_clust <- data_pict_itog
data_pitog_clust$clust1 <- data$clust1
data_pitog_clust$clust2 <- data$clust2
#View(data_pitog_clust)
data_pitog_clust$clust1 <- as.factor(data_pitog_clust$clust1)
data_pitog_clust$clust2 <- as.factor(data_pitog_clust$clust2)
#View(data_pitog_clust)

# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod_clust1 <- lm(prob ~ clust1 + dist + age + tea_coffee + like_boardgames + pic_num, data = data_pitog_clust)
crPlots(mod_clust1)
mod_clust11 <- update(mod_clust1, .~. - age + log(age))
crPlots(mod_clust11) #стало лучше
summary(mod_clust11)

mod_cl1 <- stepAIC(mod_clust11)
summary(mod_cl1)
anova(mod_clust11, mod_cl1) #короткая лучше

### чистка выбросов ############################
plot(mod_cl1, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl1) #да будто все и нормально

data_clust1 <- data_pitog_clust[-985,]
data_clust1 <- data_clust1[-620,]
data_clust1 <- data_clust1[-345,]
mod_cl1 <- update(mod_cl1, data = data_clust1)
influencePlot(mod_cl1)
#теперь расстояние Кука
cook_clust1 <- cooks.distance(mod_cl1)
plot(cook_clust1, type = "h")
which.max(cook_clust1)
data_clust1 <- data_clust1[-619,]
mod_cl1 <- update(mod_cl1, data = data_clust1)
summary(mod_cl1)

#теперь подбираем спецификацию 
plot(mod_cl1, which = 2)
boxCox(mod_cl1) #чего это такое 

#тест Рамсея
resettest(mod_cl1)
#не ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl1) #да
#Годтфелда-Квандта
gqtest(mod_cl1, order.by = data_clust1$age) #нет
#тест Уйта
white_lm(mod_cl1) #да
#в любом случае, я использую робастные ошибки


#МОДЕЛЬ (LIKE) -----------------------------------------------------------------
mod_clust2 <- lm(like ~ clust1 + dist + age + tea_coffee + like_boardgames  + pic_num, data = data_pitog_clust)
crPlots(mod_clust2)
mod_clust22 <- update(mod_clust2, .~. - age + log(age))
crPlots(mod_clust22)
summary(mod_clust22)

mod_cl2 <- stepAIC(mod_clust22)
summary(mod_cl2)
#проверим не сделали ли хуже
anova(mod_clust22, mod_cl2)
#короткая лучше

### чистка выбросов ############################
plot(mod_cl2, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl2)

data_clust2 <- data_pitog_clust[-362,]
data_clust2 <- data_clust2[-361,]
data_clust2 <- data_clust2[-346,]
data_clust2 <- data_clust2[-261,]
mod_cl2 <- update(mod_cl2, data = data_clust2)
plot(mod_cl2, which = 2)
influencePlot(mod_cl2) #хватит
#теперь расстояние Кука
cook_cl2 <- cooks.distance(mod_cl2)
plot(cook_cl2, type = "h")
which.max(cook_cl2)
summary(mod_cl2)

#теперь подбираем спецификацию 
boxCox(mod_cl2) #логарифм, но не будем

#тест Рамсея
resettest(mod_cl2)
#ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl2) #кажется, гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_cl2, order.by = data_clust2$age) #гетероскедастичность есть
#тест Уйта
white_lm(mod_cl2) #нет
#использую робастные ошибки


#МОДЕЛЬ (GOOD) -----------------------------------------------------------------
mod_clust3 <- lm(good ~ clust1 + dist + age + tea_coffee + like_boardgames  + pic_num, data = data_pitog_clust)
summary(mod_clust3)
crPlots(mod_clust3)
mod_clust33 <- update(mod_clust3, .~. - age + log(age))
crPlots(mod_clust33)

mod_cl3 <- stepAIC(mod_clust33)
summary(mod_cl3)
anova(mod_clust3, mod_cl3)

### чистка выбросов ############################
plot(mod3, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl3)

data_cl3 <- data_pitog_clust[-362,]
data_cl3 <- data_cl3[-361,]
data_cl3 <- data_cl3[-346,]
data_cl3 <- data_cl3[-232,]
mod_cl3 <- update(mod_cl3, data = data_cl3)
influencePlot(mod_cl3) #хватит
#теперь расстояние Кука
cook_cl3 <- cooks.distance(mod_cl3)
plot(cook_cl3, type = "h") #думаю, достаточно
summary(mod_cl3)

#теперь подбираем спецификацию 
boxCox(mod_cl3) #мб логарифм, но не будем

#тест Рамсея
resettest(mod_cl3)
#на 5% все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl3) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_cl3, order.by = data_cl3$age) #гетероскедастичность есть
#тест Уйта
white_lm(mod_cl3) #нет
#в любом случае использую робастные ошибки

#в одну таблицу #####
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod_cl1, mod_cl2, mod_cl3,
          se=list(cse(mod_cl1), cse(mod_cl2), cse(mod_cl3)),
          title="модели с переменной принадлежности к кластеру в качестве предиктора", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_clust.html")




# вторая кластеризация 
# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod_clust4 <- lm(prob ~ clust2 + dist + age + tea_coffee + like_boardgames + pic_num, data = data_pitog_clust)
crPlots(mod_clust4)
mod_clust44 <- update(mod_clust4, .~. - age + log(age))
crPlots(mod_clust44) #стало лучше
summary(mod_clust44)

mod_cl4 <- stepAIC(mod_clust44)
summary(mod_cl4)
anova(mod_clust44, mod_cl4) #короткая лучше

### чистка выбросов ############################
plot(mod_cl4, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl4) #да будто все и нормально

data_clust4 <- data_pitog_clust[-985,]
data_clust4 <- data_clust4[-620,]
data_clust4 <- data_clust4[-345,]
mod_cl4 <- update(mod_cl4, data = data_clust4)
influencePlot(mod_cl1)
#теперь расстояние Кука
cook_clust4 <- cooks.distance(mod_cl4)
plot(cook_clust4, type = "h")
which.max(cook_clust4)
data_clust4 <- data_clust4[-619,]
mod_cl4 <- update(mod_cl4, data = data_clust4)
summary(mod_cl4)

#теперь подбираем спецификацию 
plot(mod_cl4, which = 2)
boxCox(mod_cl4) #чего это такое 

#тест Рамсея
resettest(mod_cl4)
#не ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl4) #да
#Годтфелда-Квандта
gqtest(mod_cl4, order.by = data_clust4$age) #нет
#тест Уйта
white_lm(mod_cl4) #да
#в любом случае, я использую робастные ошибки


# другой уровень в качестве базового ------------------------------------------
# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod_clust12 <- lm(prob ~ relevel(clust1, ref = "2") + dist + age + tea_coffee + like_boardgames + pic_num, data = data_pitog_clust)
crPlots(mod_clust12)
mod_clust112 <- update(mod_clust12, .~. - age + log(age))
crPlots(mod_clust112) #стало лучше
summary(mod_clust112)

mod_cl12 <- stepAIC(mod_clust112)
summary(mod_cl12)
anova(mod_clust112, mod_cl12) #короткая лучше

### чистка выбросов ############################
plot(mod_cl12, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl12) #да будто все и нормально

data_clust12 <- data_pitog_clust[-985,]
data_clust12 <- data_clust12[-620,]
data_clust12 <- data_clust12[-345,]
mod_cl12 <- update(mod_cl12, data = data_clust12)
influencePlot(mod_cl12)
#теперь расстояние Кука
cook_clust12 <- cooks.distance(mod_cl12)
plot(cook_clust12, type = "h")
which.max(cook_clust12)
data_clust12 <- data_clust12[-619,]
mod_cl12 <- update(mod_cl12, data = data_clust12)
summary(mod_cl12)

#теперь подбираем спецификацию 
plot(mod_cl12, which = 2)
boxCox(mod_cl12) #чего это такое 

#тест Рамсея
resettest(mod_cl12)
#не ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl12) #да
#тест Уйта
white_lm(mod_cl12) #да
#в любом случае, я использую робастные ошибки


#МОДЕЛЬ (LIKE) -----------------------------------------------------------------
mod_clust22 <- lm(like ~ relevel(clust1, ref = "2") + dist + age + tea_coffee + like_boardgames  + pic_num, data = data_pitog_clust)
crPlots(mod_clust22)
mod_clust222 <- update(mod_clust22, .~. - age + log(age))
crPlots(mod_clust222)
summary(mod_clust222)

mod_cl22 <- stepAIC(mod_clust222)
summary(mod_cl22)
#проверим не сделали ли хуже
anova(mod_clust222, mod_cl22)
#короткая лучше

### чистка выбросов ############################
plot(mod_cl22, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl22)

data_clust22 <- data_pitog_clust[-362,]
data_clust22 <- data_clust22[-361,]
data_clust22 <- data_clust22[-346,]
data_clust22 <- data_clust22[-261,]
mod_cl22 <- update(mod_cl22, data = data_clust22)
plot(mod_cl22, which = 2)
influencePlot(mod_cl22) #хватит
#теперь расстояние Кука
cook_cl22 <- cooks.distance(mod_cl22)
plot(cook_cl22, type = "h")
which.max(cook_cl22)
summary(mod_cl22)

#теперь подбираем спецификацию 
boxCox(mod_cl22) #логарифм, но не будем

#тест Рамсея
resettest(mod_cl22)
#ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl22) #кажется, гетероскедастичность есть
#тест Уйта
white_lm(mod_cl22) #нет
#использую робастные ошибки


#МОДЕЛЬ (GOOD) -----------------------------------------------------------------
mod_clust32 <- lm(good ~ relevel(clust1, ref = "2") + dist + age + tea_coffee + like_boardgames  + pic_num, data = data_pitog_clust)
summary(mod_clust32)
crPlots(mod_clust32)
mod_clust332 <- update(mod_clust32, .~. - age + log(age))
crPlots(mod_clust332)

mod_cl32 <- stepAIC(mod_clust332)
summary(mod_cl32)
anova(mod_clust32, mod_cl32)

### чистка выбросов ############################
plot(mod32, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl32)

data_cl32 <- data_pitog_clust[-362,]
data_cl32 <- data_cl32[-361,]
data_cl32 <- data_cl32[-346,]
data_cl32 <- data_cl32[-232,]
mod_cl32 <- update(mod_cl32, data = data_cl32)
influencePlot(mod_cl32) #хватит
#теперь расстояние Кука
cook_cl32 <- cooks.distance(mod_cl32)
plot(cook_cl32, type = "h") #думаю, достаточно
summary(mod_cl32)

#теперь подбираем спецификацию 
boxCox(mod_cl32) #мб логарифм, но не будем

#тест Рамсея
resettest(mod_cl32)
#на 5% все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl32) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_cl32, order.by = data_cl3$age) #гетероскедастичность есть
#тест Уйта
white_lm(mod_cl32) #нет
#в любом случае использую робастные ошибки

#в одну таблицу #####
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod_cl12, mod_cl22, mod_cl32,
          se=list(cse(mod_cl12), cse(mod_cl22), cse(mod_cl32)),
          title="модели с переменной принадлежности к кластеру в качестве предиктора", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_clust22.html")

# вторая кластеризация 
# МОДЕЛЬ (PROB) ----------------------------------------------------------------
mod_clust4 <- lm(prob ~ clust2 + dist + age + tea_coffee + like_boardgames + pic_num, data = data_pitog_clust)
crPlots(mod_clust4)
mod_clust44 <- update(mod_clust4, .~. - age + log(age))
crPlots(mod_clust44) #стало лучше
summary(mod_clust44)

mod_cl4 <- stepAIC(mod_clust44)
summary(mod_cl4)
anova(mod_clust44, mod_cl4) #короткая лучше

### чистка выбросов ############################
plot(mod_cl4, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl4) #да будто все и нормально

data_clust4 <- data_pitog_clust[-985,]
data_clust4 <- data_clust4[-620,]
data_clust4 <- data_clust4[-345,]
mod_cl4 <- update(mod_cl4, data = data_clust4)
influencePlot(mod_cl1)
#теперь расстояние Кука
cook_clust4 <- cooks.distance(mod_cl4)
plot(cook_clust4, type = "h")
which.max(cook_clust4)
data_clust4 <- data_clust4[-619,]
mod_cl4 <- update(mod_cl4, data = data_clust4)
summary(mod_cl4)

#теперь подбираем спецификацию 
plot(mod_cl4, which = 2)
boxCox(mod_cl4) #чего это такое 

#тест Рамсея
resettest(mod_cl4)
#не ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl4) #да
#Годтфелда-Квандта
gqtest(mod_cl4, order.by = data_clust4$age) #нет
#тест Уйта
white_lm(mod_cl4) #да
#в любом случае, я использую робастные ошибки







#МОДЕЛЬ (LIKE) -----------------------------------------------------------------
mod_clust5 <- lm(like ~ clust2 + dist + age + tea_coffee + like_boardgames  + pic_num, data = data_pitog_clust)
crPlots(mod_clust5)
mod_clust55 <- update(mod_clust5, .~. - age + log(age))
crPlots(mod_clust55)
summary(mod_clust55)

mod_cl5 <- stepAIC(mod_clust55)
summary(mod_cl5)
#проверим не сделали ли хуже
anova(mod_clust55, mod_cl5)
#короткая лучше

### чистка выбросов ############################
plot(mod_cl5, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl5)

data_clust5 <- data_pitog_clust[-362,]
data_clust5 <- data_clust5[-361,]
data_clust5 <- data_clust5[-346,]
data_clust5 <- data_clust5[-261,]
mod_cl5 <- update(mod_cl5, data = data_clust5)
plot(mod_cl5, which = 2)
influencePlot(mod_cl5) #хватит
#теперь расстояние Кука
cook_cl5 <- cooks.distance(mod_cl5)
plot(cook_cl5, type = "h")
which.max(cook_cl5)
summary(mod_cl5)

#теперь подбираем спецификацию 
boxCox(mod_cl5) #логарифм, но не будем

#тест Рамсея
resettest(mod_cl5)
#ок

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl5) # есть
#Годтфелда-Квандта
gqtest(mod_cl5, order.by = data_clust5$age) #гетероскедастичность есть
#тест Уйта
white_lm(mod_cl5) #да
#использую робастные ошибки


#МОДЕЛЬ (GOOD) -----------------------------------------------------------------
mod_clust6 <- lm(good ~ clust2 + dist + age + tea_coffee + like_boardgames  + pic_num, data = data_pitog_clust)
summary(mod_clust6)
crPlots(mod_clust6)
mod_clust66 <- update(mod_clust6, .~. - age + log(age))
crPlots(mod_clust66)

mod_cl6 <- stepAIC(mod_clust66)
summary(mod_cl6)
anova(mod_clust66, mod_cl6)

### чистка выбросов ############################
plot(mod_cl6, which = 2)
#для начала посмотрим hatvalues и расстояние Кука
influencePlot(mod_cl6)

data_cl6 <- data_pitog_clust[-362,]
data_cl6 <- data_cl6[-361,]
data_cl6 <- data_cl6[-346,]
data_cl6 <- data_cl6[-232,]
mod_cl6 <- update(mod_cl6, data = data_cl6)
influencePlot(mod_cl6) #хватит
#теперь расстояние Кука
cook_cl6 <- cooks.distance(mod_cl6)
plot(cook_cl6, type = "h") #думаю, достаточно
summary(mod_cl6)

#теперь подбираем спецификацию 
boxCox(mod_cl6) #мб логарифм, но не будем

#тест Рамсея
resettest(mod_cl6)
#на 1% все норм

#проведем все тесты на гетероскедастичность
#Тест Бреуша-Пагана 
bptest(mod_cl6) #гетероскедастичность есть
#Годтфелда-Квандта
gqtest(mod_cl6, order.by = data_cl6$age) #гетероскедастичность есть
#тест Уйта
white_lm(mod_cl6) #нет
#в любом случае использую робастные ошибки

#в одну таблицу #####
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod_cl4, mod_cl5, mod_cl6,
          se=list(cse(mod_cl4), cse(mod_cl5), cse(mod_cl6)),
          title="модели с переменной принадлежности к кластеру в качестве предиктора", type="text",
          df=FALSE, digits=3, out="C:/Users/1392680/Desktop/Учеба/ДИПЛОМ/Рабочая папка/mods_clust2.html")



#графическое представление ----------------------------------------------------
# датасеты и размерность -------------------------------------------------------
data_pt1 <- data_pitog_clust %>% dplyr::filter(pic_num == 1)
data_pt2 <- data_pitog_clust %>% dplyr::filter(pic_num == 2)
data_pt3 <- data_pitog_clust %>% dplyr::filter(pic_num == 3)
data_pt4 <- data_pitog_clust %>% dplyr::filter(pic_num == 4)
data_pt5 <- data_pitog_clust %>% dplyr::filter(pic_num == 5)
data_pt6 <- data_pitog_clust %>% dplyr::filter(pic_num == 6)
data_pt7 <- data_pitog_clust %>% dplyr::filter(pic_num == 7)
data_pt8 <- data_pitog_clust %>% dplyr::filter(pic_num == 8)
data_pt9 <- data_pitog_clust %>% dplyr::filter(pic_num == 9)
data_pt10 <- data_pitog_clust %>% dplyr::filter(pic_num == 10)
dim(data_pt1)
dim(data_pt2)
dim(data_pt3)
dim(data_pt4)
dim(data_pt5)
dim(data_pt6)
dim(data_pt7)
dim(data_pt8)
dim(data_pt9)
dim(data_pt10)
View(data_pt1)

# тесты на значимость
#кластеризация 1 ---------------------------------------------------------------
data_mean1 <- data_pt1 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean2 <- data_pt2 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean3 <- data_pt3 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean4 <- data_pt4 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean5 <- data_pt5 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean6 <- data_pt6 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean7 <- data_pt7 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean8 <- data_pt8 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean9 <- data_pt9 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))
data_mean10 <- data_pt10 %>% dplyr::select(clust2, pic_num, dim_att, mean_att) %>% group_by(clust2) %>% 
  dplyr::summarise(value = mean(dim_att), sd = sd(dim_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust2))

grid.arrange((ggplot(data_mean1) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean2) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean3) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean4) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean5) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean6) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean7) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean8) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean9) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean10) + geom_bar(aes(x = clust2, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust2, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),ncol = 5)

#кластеризация 2 ---------------------------------------------------------------
# Средние с довреительным интервалом
data_mean11 <- data_pt1 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean21 <- data_pt2 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean31 <- data_pt3 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean41 <- data_pt4 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean51 <- data_pt5 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean61 <- data_pt6 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean71 <- data_pt7 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean81 <- data_pt8 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean91 <- data_pt9 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))
data_mean101 <- data_pt10 %>% dplyr::select(clust1, pic_num, dim_att, mean_att) %>% group_by(clust1) %>% 
  dplyr::summarise(value = mean(mean_att), sd = sd(mean_att)/sqrt(n())) %>% mutate(clust2 = as.factor(clust1))

grid.arrange((ggplot(data_mean11) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean21) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean31) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean41) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean51) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean61) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean71) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean81) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean91) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),
             (ggplot(data_mean101) + geom_bar(aes(x = clust1, y = value), stat="identity", fill="skyblue", alpha = 0.8) + 
                geom_errorbar(aes(x = clust1, y = value, ymin = value - 2*sd, ymax = value + 2*sd),width = 0.4, 
                              colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() + 
                labs(x = "Номер кластера", y = "Отношение к изображению")),ncol = 5)

################################################################################
# ФАКТОРНЫЙ АНАЛИЗ
################################################################################
#согласованность факторов ------------------------------------------------------
data_e <- data.frame(6-data_longquest$q1e, data_longquest$q2e, data_longquest$q3e, 
                     data_longquest$q4e, 6-data_longquest$q5e, 6-data_longquest$q6e)
data_a <- data.frame(data_longquest$q1a, 6-data_longquest$q2a, data_longquest$q3a, 
                     6-data_longquest$q4a, data_longquest$q5a, 6-data_longquest$q6a)
data_c <- data.frame(6-data_longquest$q1c, 6-data_longquest$q2c, data_longquest$q3c, 
                     data_longquest$q4c, data_longquest$q5c, 6-data_longquest$q6c)
data_n <- data.frame(data_longquest$q1n, data_longquest$q2n, 6-data_longquest$q3n, 
                     6-data_longquest$q4n, 6-data_longquest$q5n, data_longquest$q6n)
data_o <- data.frame(data_longquest$q1o, 6-data_longquest$q2o, data_longquest$q3o, 
                     6-data_longquest$q4o, data_longquest$q5o, 6-data_longquest$q6o)
data_like <- data.frame(data_pict_itog$prob, data_pict_itog$like, data_pict_itog$good, 
                        data_pict_itog$similar, data_pict_itog$often)
cronbach.alpha(data_e)#хорошо
cronbach.alpha(data_a, CI=TRUE)#приемлемо
cronbach.alpha(data_c)#хорошо
cronbach.alpha(data_n)#хорошо
cronbach.alpha(data_o, CI=TRUE)#приемлемо
cronbach.alpha(data_like) #хорошо

# факторный анализ -------------------------------------------------------------
data_FA <- merge(data_pict_itog, data_longquest, by = "id")
data_FA <- dplyr::select(data_FA, -c(phone_num:product), -c(id:p), -c(tea_coffee.x:city.x), -c(d_extravers:d_open))
View(data_FA)

data_FA <-  dplyr::select(data_FA, prob, like, good, q1e, q2e, q3e, q4e, q5e, q6e,
                          q1a, q2a, q3a, q4a, q5a, q6a,
                          q1c, q2c, q3c, q4c, q5c, q6c,
                          q1n, q2n, q3n, q4n, q5n, q6n,
                          q1o, q2o, q3o, q4o, q5o, q6o)
View(data_FA)
data_FA <- scale(data_FA)
data_FA <- data.frame(data_FA)

#коррелляционная матрица
data_for_corrplot <- dplyr::select(data_FA, prob, like, good, q1e, q2e, q3e, q4e, q5e, q6e,
                                   q1a, q2a, q3a, q4a, q5a,
                                   q1c, q2c, q3c, q4c, q5c,
                                   q1n, q2n, q3n, q4n, q5n, q6n,
                                   q1o, q2o, q3o, q4o)
corrplot(cor(data_for_corrplot))
det(cor(data_for_corrplot))

cor_p <- cor.mtest(data_for_corrplot, conf.level = 0.95)
corrplot(cor(data_for_corrplot), addgrid.col = TRUE,
         p.mat = cor_p$p, sig.level = 0.05, insig='blank')

#проведем подтверждающий факторный анализ и построим структурную регрессию
#свернем факторы в один ########################################################
mod_general <- "
Factor1 =~ q1e + q2e + q3e + q4e + q5e + q6e
Factor2 =~ q1a + q2a + q3a + q4a + q5a
Factor3 =~ q1c + q2c + q3c + q4c + q5c
Factor4 =~ q1n + q2n + q3n + q4n + q5n + q6n
Factor5 =~ q1o + q2o + q4o
Factor6 =~ prob + like + good
Factor6 ~ Factor1 + Factor2 + Factor3 + Factor4 + Factor5
"

modcfa <- cfa(mod_general, data=data_FA)
modsem <- sem(mod_general, data=data_FA)
summary(modcfa, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)
summary(modsem, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

#графически
semPaths(modcfa, "stand", "est", colFactor = 0, edge.label.cex = 0.56, title.cex = 0.5)

#рассмотрим матрицу разницы оцененной и действительной ковариационной матрицы
lavInspect(modcfa, what = "resid")

# отдельно для каждой из переменных ############################################
# prob ------------------------------------------------------------------------
mod_fa_prob <- "
Factor1 =~ q1e + q2e + q3e + q4e + q5e + q6e
Factor2 =~ q1a + q2a + q3a + q4a + q5a
Factor3 =~ q1c + q2c + q3c + q4c + q5c
Factor4 =~ q1n + q2n + q3n + q4n + q5n + q6n
Factor5 =~ q1o + q2o + q4o
prob ~ Factor1 + Factor2 + Factor3 + Factor4 + Factor5
"
modcfa_prob <- cfa(mod_fa_prob, data=data_FA)
modsem_prob <- sem(mod_fa_prob, data=data_FA)
summary(modcfa_prob, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)
summary(modsem_prob, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

#графически
semPaths(modcfa_prob, "stand", "est", colFactor = 0, edge.label.cex = 0.6, repel = TRUE)

# like ------------------------------------------------------------------------
mod_fa_like <- "
Factor1 =~ q1e + q2e + q3e + q4e + q5e + q6e
Factor2 =~ q1a + q2a + q3a + q4a + q5a
Factor3 =~ q1c + q2c + q3c + q4c + q5c
Factor4 =~ q1n + q2n + q3n + q4n + q5n + q6n
Factor5 =~ q1o + q2o + q4o
like ~ Factor1 + Factor2 + Factor3 + Factor4 + Factor5
"
modcfa_like <- cfa(mod_fa_like, data=data_FA)
modsem_like <- sem(mod_fa_like, data=data_FA)
summary(modcfa_like, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)
summary(modsem_like, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

#графически
semPaths(modcfa_like, "stand", "est", colFactor = 0, edge.label.cex = 0.6, repel = TRUE)

# good ------------------------------------------------------------------------
mod_fa_good <- "
Factor1 =~ q1e + q2e + q3e + q4e + q5e + q6e
Factor2 =~ q1a + q2a + q3a + q4a + q5a
Factor3 =~ q1c + q2c + q3c + q4c + q5c
Factor4 =~ q1n + q2n + q3n + q4n + q5n + q6n
Factor5 =~ q1o + q2o + q4o
good ~ Factor1 + Factor2 + Factor3 + Factor4 + Factor5
"
modcfa_good <- cfa(mod_fa_good, data=data_FA)
modsem_good <- sem(mod_fa_good, data=data_FA)
summary(modcfa_good, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)
summary(modsem_good, fit.measures=TRUE, standardized=TRUE, rsquare = TRUE)

#графически
semPaths(modcfa, "stand", "est", colFactor = 0, edge.label.cex = 0.6, repel = TRUE)









