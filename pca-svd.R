library(readr)
library(ggplot2)
library(gridExtra)
library(jpeg)
library(magick)
library(gridExtra)
library(Metrics)
library(gtable)
#######
fnames<- list.files(file.path("data"), full.names = TRUE) #список файлов в директории
image_rew <- c()
for (x in fnames) {
  img <- image_read(x)
  img_cropped <- image_crop(img, "400x400+24+24")
  file_name <- paste( x , 'photo.jpg', sep = '')
  image_write(img_cropped, file_name)
  image_rew <- c(image_rew, file_name)
}
file_name<- NULL
x<-NULL
img<-NULL
img_cropped<-NULL

par(mfrow = c(2, 5))
for (i in 1:length(image_rew)){
  img <- image_read(image_rew[i])
  plot(img)
}
####################


image_list <- c()
image_list <- lapply(image_rew, function(x) {
  photo <- readJPEG(x)
  # 
  r <- photo[,,1]
  g <- photo[,,2]
  b <- photo[,,3]
  # 
  r.pca <- prcomp(r, center = F)
  g.pca <- prcomp(g, center = F)
  b.pca <- prcomp(b, center = F)
  rgb.pca <- list(r.pca, g.pca, b.pca)
  for (j in seq.int(3, round(nrow(photo) - 10), length.out = 10)) {
    pca.img <- sapply(rgb.pca, function(k) {
      compressed.img <- k$x[,1:j] %*% t(k$rotation[,1:j])
    }, simplify = 'array')
    filename <- paste(x,'_','photo_', round(j,0), '_components.jpg', sep = '')
   
    writeJPEG(pca.img, filename)
    # 
    
    image_list <- c(image_list,filename)
  }
  return(image_list)
})

size_list <- c()
vars_list<-c()
for (i in 1:length(image_list)) {
  file_size_orginal <- file.size(sub("_photo_.*", "", image_list[[i]][1]))
  file_size_data <- data.frame("Number_of_k" = 'Original', 
                               "b" = file_size_orginal)
  df <- data.frame("Number_of_PCs", 
                   "MSE", 
                   "File_size", "PNSR")
  df<-df[-1,]
  for (j in 1:length(image_list[[i]])) {
    file_size <- file.size(image_list[[i]][j])
    
    file_size_data<- rbind(file_size_data, data.frame("Number_of_k" = 
                                   as.numeric(stringr::str_extract
                                              (image_list[[i]][j], "\\d+")), 
                                   "b" = file_size))
    
    # 
    mse_val <- mse(readJPEG(sub("_photo_.*", "", image_list[[i]][1])), 
                   readJPEG(image_list[[i]][j]))
    
    # 
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
  file_size_data$Number_of_k <- factor(file_size_data$Number_of_k, 
                                       levels = file_size_data$Number_of_k
                                       [order(file_size_data$b)])
  # 
  #
  ggplot(file_size_data, aes(x = Number_of_k, y = b)) + 
    geom_bar(stat = "identity") + 
    ggtitle(paste(sub(".*data/", "", sub(".jpeg.*", "", image_list[[i]][1])))) + 
    xlab("Number of k") + 
    ylab("File size (b)")
  g_name<- paste(sub(".jpeg.*", "", image_list[[i]][1]),
                 "_file_size.jpg", sep='')
  ggsave(g_name, device = "jpeg")
  size_list <- c(size_list,g_name)
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
  ggsave(gname, plot = p, device = "jpeg")
  vars_list<- c(vars_list,gname)
}

par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(size_list)){
  img <- image_read(size_list[i])
  plot(img)
}
for (i in 1:length(vars_list)){
  img <- image_read(vars_list[i])
  plot(img)
}


#### сделаем то же самое, но с меньшими k####
image_list <- c()
image_list <- lapply(image_rew, function(x) {
  photo <- readJPEG(x)
  # 
  r <- photo[,,1]
  g <- photo[,,2]
  b <- photo[,,3]
  # 
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
    # 
    
    image_list <- c(image_list,filename)
  }
  return(image_list)
})

size_list <- c()
vars_list<-c()
for (i in 1:length(image_list)) {
  file_size_orginal <- file.size(sub("_photo_.*", "", image_list[[i]][1]))
  file_size_data <- data.frame("Number_of_k" = 'Original', 
                               "b" = file_size_orginal)
  df <- data.frame("Number_of_PCs", 
                   "MSE", 
                   "File_size", "PNSR")
  df<-df[-1,]
  for (j in 1:length(image_list[[i]])) {
    file_size <- file.size(image_list[[i]][j])
    
    file_size_data<- rbind(file_size_data, data.frame("Number_of_k" = 
                                                        as.numeric(stringr::str_extract
                                                                   (image_list[[i]][j], "\\d+")), 
                                                      "b" = file_size))
    
    # 
    mse_val <- mse(readJPEG(sub("_photo_.*", "", image_list[[i]][1])), 
                   readJPEG(image_list[[i]][j]))
    
    # 
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
  file_size_data$Number_of_k <- factor(file_size_data$Number_of_k, 
                                       levels = file_size_data$Number_of_k
                                       [order(file_size_data$b)])
  #
  #
  ggplot(file_size_data, aes(x = Number_of_k, y = b)) + 
    geom_bar(stat = "identity") + 
    ggtitle(paste(sub(".*data/", "", sub(".jpeg.*", "", image_list[[i]][1])))) + 
    xlab("Number of k") + 
    ylab("File size (b)")
  g_name<- paste(sub(".jpeg.*", "", image_list[[i]][1]),
                 "_file_size.jpg", sep='')
  ggsave(g_name, device = "jpeg")
  size_list <- c(size_list,g_name)
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
  ggsave(gname, plot = p, device = "jpeg")
  vars_list<- c(vars_list,gname)
}
par(mfrow = c(2, 1), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:length(size_list)){
  img <- image_read(size_list[i])
  plot(img)
}
for (i in 1:length(vars_list)){
  img <- image_read(vars_list[i])
  plot(img)
}
