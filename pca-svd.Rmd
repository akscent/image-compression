---
title: 'Сокращение размерности изображений'
output:
  html_document:
    df_print: paged
---

```{r Импорт данных, include=FALSE}
library(readr)
library(ggplot2)
library(gridExtra)
library(jpeg)
library(magick)
library(gridExtra)
library(Metrics)
library(gtable)
```

Была подготовлена рабочая директория с папкой /data/ и набором с 10 изображениями в ней. Загрузим их и подготовим к анализу.

```{r Загрузка данных}
fnames<- list.files(file.path("data"), full.names = TRUE) #список файлов в директории
image_rew <- c() # пустой вектор, который далее заполним названиями предобработанных изображений
for (x in fnames) {
  img <- image_read(x)
  img_cropped <- image_crop(img, "400x400+24+24")
  # обрезка изображений в разрешении 400 Х 400 пикселей
  file_name <- paste( x , 'photo.jpg', sep = '')
  # название файла, который будет записан в исходную папку
  image_write(img_cropped, file_name)
  # запись изображения в папку 
  # и
  # заполнение списка названиями файлов
  image_rew <- c(image_rew, file_name)
}
# ниже просто удаление ненужных побочных файлов, созданных в ходе цикла
file_name<- NULL
x<-NULL
img<-NULL
img_cropped<-NULL
```

Посмотрим на полученные изображения, используя список новых картинок.

```{r Примеры картинок}
par(mfrow = c(2, 5), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(image_rew)){
  img <- image_read(image_rew[i])
  plot(img)
}
```

Здесь используется метод главных компонент после разложения цветных картинок на р-г-б матрицы и с последующим их восстановлением в исходные изображения.

Начнём с разложения исходных изоюражений и записи новых картинок в директорию.

```{r Список PCA картинок}
image_list <- c()
image_list <- lapply(image_rew, function(x) {
  photo <- readJPEG(x) # читает фото в нужном формате
  # Создание матриц для каждого цвета
  r <- photo[,,1]
  g <- photo[,,2]
  b <- photo[,,3]
  # PCA
  r.pca <- prcomp(r, center = F)
  g.pca <- prcomp(g, center = F)
  b.pca <- prcomp(b, center = F)
  rgb.pca <- list(r.pca, g.pca, b.pca)
  # и запишем цикл для восстановления изображений с различным набором k-компонент
  for (j in seq.int(3, round(nrow(photo) - 10), length.out = 10)) {
    pca.img <- sapply(rgb.pca, function(k) {
      compressed.img <- k$x[,1:j] %*% t(k$rotation[,1:j])
    }, simplify = 'array')
    filename <- paste(x,'_','photo_', round(j,0), '_components.jpg', sep = '')
    writeJPEG(pca.img, filename)
    image_list <- c(image_list,filename)
  }
  return(image_list)
})
```

Приведём некоторые примеры полученных картинок.

```{r Пример восстановленных картинок}
par(mfrow = c(2, 5), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(image_list[[1]])){
  img <- image_read(image_list[[1]][i])
  plot(img)
}
```

Оценка качества изображения и количества потерянной информации.

Здесь мы вводим несколько метрик:

-   file_size - размер изображения в ячейках памяти (в байтах) - мера эффекта

-   MSE - среднеквадратическая ошибка - мера потерянной информации

-   PNSR - пиковое отношение сигнала к шуму - мера искажения изображения

```{r Метрики}
# создадим 2 списка: только для размера изображений и для всех остальных метрик
size_list <-c()
vars_list<-c()
for (i in 1:length(image_list)) {
  # узнаем размер ригинального файла
  file_size_orginal <- file.size(sub("_photo_.*", "", image_list[[i]][1]))
  # создание датафрейма для сравнения размера
  file_size_data <- data.frame("Number_of_k" = 'Original', 
                               "b" = file_size_orginal)
  # создание датафрейма для метрик
  df <- data.frame("Number_of_PCs", 
                   "MSE", 
                   "File_size", "PNSR")
  df<-df[-1,]
  # теперь создадим цикл для заполнения датафреймов данными изображений каждого количества компонент для каждого исходного изображения
  for (j in 1:length(image_list[[i]])) {
    file_size <- file.size(image_list[[i]][j])
    
    file_size_data<- rbind(file_size_data, data.frame("Number_of_k" = 
                                   as.numeric(stringr::str_extract
                                              (image_list[[i]][j], "\\d+")), 
                                   "b" = file_size))
    
    mse_val <- mse(readJPEG(sub("_photo_.*", "", image_list[[i]][1])), 
                   readJPEG(image_list[[i]][j]))
    
    photo_PSNR <- readJPEG(image_list[[i]][j])
    MAX <- max(photo_PSNR)
    PNSR <- 10*log10(MAX^2/mse_val)
    ##
    df <- rbind(df, data.frame("Number_of_PCs" = 
                                 as.numeric(stringr::str_extract(
                                   image_list[[i]][j], "\\d+")),
                               "MSE" = mse_val, 
                               "File_size" = file_size, "PNSR" = PNSR))
  }
  # запишем компоненты, как фактор
  file_size_data$Number_of_k <- factor(file_size_data$Number_of_k, 
                                       levels = file_size_data$Number_of_k
                                       [order(file_size_data$b)])
  # создание графиков сравнения
  ggplot(file_size_data, aes(x = Number_of_k, y = b)) + 
    geom_bar(stat = "identity") + 
    ggtitle(paste(sub(".*data/", "", sub(".jpeg.*", "", image_list[[i]][1])))) + 
    xlab("Number of k") + 
    ylab("File size (b)")
  g_name<- paste(sub(".jpeg.*", "", image_list[[i]][1]),
                 "_file_size.jpg", sep='')
  ggsave(g_name, device = "jpeg") # запись в директорию
  size_list <- c(size_list,g_name) # запись названия в список
  #
  g1=ggplot(df, aes(x = Number_of_PCs)) + 
    geom_point(aes(y = MSE), color = "purple") + 
    geom_line(aes(y = MSE), color = "purple") +
    ggtitle(ggtitle(paste(sub(".*data/", "", sub(".jpeg.*", "", image_list[[i]][1]))))) + 
    xlab(" ")
  g2=ggplot(df, aes(x = Number_of_PCs)) +
    geom_point(aes(y = PNSR), color = "pink") + 
    geom_line(aes(y = PNSR), color = "pink") + 
    xlab(" ")
  g3=ggplot(df, aes(x = Number_of_PCs)) +
    geom_point(aes(y = File_size), color = "green") + 
    geom_line(aes(y = File_size), color = "green") + 
    xlab("Number of k")
  p = grid.arrange(g1, g2, g3, nrow = 3)
  gname <- paste(sub(".jpeg.*", "", image_list[[i]][1]),
                 "_file_vars.jpg", sep='')
  ggsave(gname, plot = p, device = "jpeg") # запись в директорию
  vars_list<- c(vars_list,gname) # запись в список
}
```

Теперь визуализируем полученные метрики, чтобы понять, как справляется PCA с задачей.

```{r Визуализация метрик}
par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(size_list)){
  img <- image_read(size_list[i])
  plot(img)
}
for (i in 1:length(vars_list)){
  img <- image_read(vars_list[i])
  plot(img)
}
```

Достаточно хорошо, видно, что размер изображения, по сравнению с оригиналом, сильно сокращается для всех изображений при k\<100. MSE резко уменьшаются при количестве компонент \>50, далее выходят на плато. Мера искажения наиболее плавно изменяющаяся среди всех. тем не менее, в основном и она выдерживает большинство своей изменчивости при количестве компонент \<100. Таким образом, очевидно, что искомое количество находится между 2 и 100 компонентами.

Проведём те же шаги для k\<50 и посмотрим, как ведут себя метрики.

```{r}
#### сделаем то же самое, но с меньшими k####
image_list <- c()
image_list <- lapply(image_rew, function(x) {
  photo <- readJPEG(x)
  r <- photo[,,1]
  g <- photo[,,2]
  b <- photo[,,3]
  r.pca <- prcomp(r, center = F)
  g.pca <- prcomp(g, center = F)
  b.pca <- prcomp(b, center = F)
  rgb.pca <- list(r.pca, g.pca, b.pca)
  for (j in seq(2, 50, by = 5)) {
    pca.img <- sapply(rgb.pca, function(k) {
      compressed.img <- k$x[,1:j] %*% t(k$rotation[,1:j])
    }, simplify = 'array')
    filename <- paste(x,'_','photo_', round(j,0), '_components.jpg', sep = '')
    
    writeJPEG(pca.img, filename)
    
    image_list <- c(image_list,filename)
  }
  return(image_list)
})
par(mfrow = c(2, 5), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(image_list[[1]])){
  img <- image_read(image_list[[1]][i])
  plot(img)
}
```

Изображения, в целом, выглядят удовлетворительно. Однако журавль оказался без головы - важного видоспецифичного признака. Таким образом, данные изображения уже не сгодятся для определения вида, однако для определения морфы - птица, или семейства - журавль вполне сгодятся.

```{r}
size_list <-c()
vars_list<-c()
for (i in 1:length(image_list)) {
  # узнаем размер ригинального файла
  file_size_orginal <- file.size(sub("_photo_.*", "", image_list[[i]][1]))
  # создание датафрейма для сравнения размера
  file_size_data <- data.frame("Number_of_k" = 'Original', 
                               "b" = file_size_orginal)
  # создание датафрейма для метрик
  df <- data.frame("Number_of_PCs", 
                   "MSE", 
                   "File_size", "PNSR")
  df<-df[-1,]
  # теперь создадим цикл для заполнения датафреймов данными изображений каждого количества компонент для каждого исходного изображения
  for (j in 1:length(image_list[[i]])) {
    file_size <- file.size(image_list[[i]][j])
    
    file_size_data<- rbind(file_size_data, data.frame("Number_of_k" = 
                                   as.numeric(stringr::str_extract
                                              (image_list[[i]][j], "\\d+")), 
                                   "b" = file_size))
    
    mse_val <- mse(readJPEG(sub("_photo_.*", "", image_list[[i]][1])), 
                   readJPEG(image_list[[i]][j]))
    
    photo_PSNR <- readJPEG(image_list[[i]][j])
    MAX <- max(photo_PSNR)
    PNSR <- 10*log10(MAX^2/mse_val)
    ##
    df <- rbind(df, data.frame("Number_of_PCs" = 
                                 as.numeric(stringr::str_extract(
                                   image_list[[i]][j], "\\d+")),
                               "MSE" = mse_val, 
                               "File_size" = file_size, "PNSR" = PNSR))
  }
  # запишем компоненты, как фактор
  file_size_data$Number_of_k <- factor(file_size_data$Number_of_k, 
                                       levels = file_size_data$Number_of_k
                                       [order(file_size_data$b)])
  # создание графиков сравнения
  ggplot(file_size_data, aes(x = Number_of_k, y = b)) + 
    geom_bar(stat = "identity") + 
    ggtitle(paste(sub(".*data/", "", sub(".jpeg.*", "", image_list[[i]][1])))) + 
    xlab("Number of k") + 
    ylab("File size (b)")
  g_name<- paste(sub(".jpeg.*", "", image_list[[i]][1]),
                 "_file_size.jpg", sep='')
  ggsave(g_name, device = "jpeg") # запись в директорию
  size_list <- c(size_list,g_name) # запись названия в список
  #
  g1=ggplot(df, aes(x = Number_of_PCs)) + 
    geom_point(aes(y = MSE), color = "purple") + 
    geom_line(aes(y = MSE), color = "purple") +
    ggtitle(ggtitle(paste(sub(".*data/", "", sub(".jpeg.*", "", image_list[[i]][1]))))) + 
    xlab(" ")
  g2=ggplot(df, aes(x = Number_of_PCs)) +
    geom_point(aes(y = PNSR), color = "pink") + 
    geom_line(aes(y = PNSR), color = "pink") + 
    xlab(" ")
  g3=ggplot(df, aes(x = Number_of_PCs)) +
    geom_point(aes(y = File_size), color = "green") + 
    geom_line(aes(y = File_size), color = "green") + 
    xlab("Number of k")
  p = grid.arrange(g1, g2, g3, nrow = 3)
  gname <- paste(sub(".jpeg.*", "", image_list[[i]][1]),
                 "_file_vars.jpg", sep='')
  ggsave(gname, plot = p, device = "jpeg") # запись в директорию
  vars_list<- c(vars_list,gname) # запись в список
}
par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(size_list)){
  img <- image_read(size_list[i])
  plot(img)
}

```

Здесь можно сказать, что картина однородна для всех изображений по отдельным метрикам. Плавное увеличение до 50 компонент. Здесь нужно сказать, что выбор оптимальног околичества компонент, конечно, зависит от целей проведения анализа. Если, например, это идентификация птицы, то можно ограничиться 20-30 компонентами - оптимумом качества изображения, размера и потери информации для данной задачи.

В довершение, посмотрим на все остальные изображение

```{r Примеры}
par(mfrow = c(20, 5), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(image_list)){
  for (j in 1:length(image_list[[i]])) {
    img <- image_read(image_list[[i]][j])
    plot(img)
  }
}

```

\-\-\-\-\-\-\-\-\-\-\--


