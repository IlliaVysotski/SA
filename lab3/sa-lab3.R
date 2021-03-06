sink("log.txt")
setwd("C:/Users/user/YandexDisk/labs/sa/lab3/")

library(imager)

jpegValueScale = 255
histogramStep = 10

img1_path <- "cat_1.jpg"
img2_path <- "cat_2.jpg"

customShow <- function(img, title){
  plot(img, main = title)
}

img_convertion <- function(image_num, img_path) {
  img_color <- load.image(img_path)
  customShow(img_color, sprintf("������� %d: �������", image_num))
  img_gray <- grayscale(img_color)
  customShow(img_gray, sprintf("������� %d: �����������", image_num))
  
  # Histogram
  img_gray_scaled <- img_gray * jpegValueScale
  histogram_res <- 
    hist(img_gray_scaled, main=sprintf("����������� ����������� ������� %d", image_num), 
         xlab = "", xlim = c(0, jpegValueScale),
         breaks = seq(0, jpegValueScale + histogramStep, by=histogramStep))
  
  list(gray = img_gray_scaled, hist = histogram_res)
}

main_calc <- function(image_num, histogram_res) {
  #hist_x <- histogram_res$breaks
  hist_y <- histogram_res$counts
  
  message("������� ", image_num, " ������� = ", mean(hist_y))
  message("������� ", image_num, " ��.��.����. = ", sd(hist_y))
  message("������� ", image_num, " ������� = ", median(hist_y))
  hist_y.t <- table(hist_y)
  message("������� ", image_num, " ���� = ", sort(unique(hist_y))[which.max(hist_y.t)])
}

conv1 <- img_convertion(1, img1_path)
main_calc(1, conv1$hist)
conv2 <- img_convertion(2, img2_path)
main_calc(2, conv2$hist)

message("���������� ���������� = ", cor(conv1$hist$counts, conv2$hist$counts))
message("���������� ����������� ����������� = ", cor(conv1$gray, conv2$gray))

chi_test <- function(dots) {
  interval <- dots$hist$breaks
  len <- length(interval)
  interval[1] <- (-Inf)
  interval[len] <- (+Inf)
  p_for_dots <- pnorm(interval, mean=mean(dots$gray), sd=sd(dots$gray))
  p_for_dots <- (p_for_dots[2:len] - p_for_dots[1:(len - 1)])
  
  chisq.test(dots$hist$counts, p = p_for_dots)
}

chi1 <- chi_test(conv1)
message("��-������� ������� 1: ", as.numeric(chi1$statistic), "; p = ", chi1$p.value)
chi2 <- chi_test(conv2)
message("��-������� ������� 2: ", as.numeric(chi2$statistic), "; p = ", chi2$p.value)
