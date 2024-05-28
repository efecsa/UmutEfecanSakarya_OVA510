#Yüksek Korelasyon Gösteren Oyuncular için Regresyon Modelinin Oluşturulması

install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# Sadece "Derek Willis" ve "Elijah Bryant" satırlarının filtrelenmesi
p5 <- basketbol_veri %>% 
  filter(Oyuncu_Adi %in% c("Derek Willis", "Elijah Bryant"))

# Çoklu regresyon modeli kurma
modelp5 <- lm(PIR ~ Oynama_Sure + Seyahat_Sure + Yas + RakipForm + Skor_Yakinlik, data = p5)

# Model özeti görüntüleme
summary(modelp5)

p5_tahmin <- data.frame(Oyuncu_Adi = p5$Oyuncu_Adi, Gercek = p5$PIR, Tahmin = predict(modelp5), rakip = p5$Rakip)

# Oyuncu Adı sütununa göre renk paleti oluşturulması
renkler <- c("Derek Willis" = "#FF0000", "Elijah Bryant" = "#eb6734")

# PNG dosyasının şeffaf arka planla yüksek çözünürlükte kaydedilmesi
png(file="C:/Users/Efe/Documents/AnadoluUni/Veri Görselleştirme/Final/RegresyonGrafigi.png", 
    width=1600, height=1000, res=150, bg="transparent")

ggplot(p5_tahmin, aes(x = Gercek, y = Tahmin, color = Oyuncu_Adi)) +
  geom_point(size = 3, aes(color = Oyuncu_Adi)) +  # Renkleri belirleme
  geom_abline(size = 1.5, slope = 1, intercept = 0, linetype = "dashed", color = "#29c2ff") +
  labs(title = "Gerçek ve Tahmin Edilen PIR Skorlarının Karşılaştırılması",
       x = "Gerçek PIR Skoru",
       y = "Tahmin Edilen PIR Skoru",
       color = "Oyuncu Adı") +  
  scale_color_manual(values = renkler) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),  # Arka planın şeffaflaştırılması
    panel.background = element_rect(fill = "transparent", color = NA),  # Panel arka planının şeffaflaştırılması
    plot.title = element_text(size = 24, color = "white", face = "bold", margin = margin(b = 20)),  
    axis.title.x = element_text(size = 14, color = "white", face = "bold", margin = margin(t = 10)),  
    axis.title.y = element_text(size = 14, color = "white", face = "bold", margin = margin(r = 10)),  
    axis.text.x = element_text(size = 12, color = "white"),  
    axis.text.y = element_text(size = 12, color = "white"),  
    legend.title = element_text(size = 14, color = "white", face = "bold"),  
    legend.position = "bottom", # Lejantı alt kısımda göster
    legend.text = element_text(color = "white")
  )

# Export grafik aracının kapatılması
dev.off()
