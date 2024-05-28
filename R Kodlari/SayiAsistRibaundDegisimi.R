#Oyuncuların Seyahat Sürelerine Göre Temel Maç İstatistiklerinin Değişimi

library(dplyr)
library(ggplot2)

# Seyahat_Sure değişkeninin gruplandırılması
grupla_seyahat_sure <- function(seyahat_sure) {
  if (seyahat_sure == 0) {
    return("0")
  } else if (seyahat_sure <= 120) {
    return("1-120")
  } else if (seyahat_sure <= 240) {
    return("121-240")
  } else {
    return("241+")
  }
}

# Seyahat_Sure'den yeni veri değişken türetilmesi
basketbol_veri <- basketbol_veri %>%
  mutate(Gruplu_Seyahat_Sure = sapply(Seyahat_Sure, grupla_seyahat_sure))

# Gruplu veri üzerinden ortalama değerlerin hesaplanması
veri_ortalama_gruplu <- basketbol_veri %>%
  group_by(Gruplu_Seyahat_Sure) %>%
  summarise(Ortalama_Sayi = mean(Sayi),
            Ortalama_Rebound = mean(Rebound),
            Ortalama_Asist = mean(Asist))

# PNG dosyasının şeffaf arka planla yüksek çözünürlükte kaydedilmesi
png(file="C:/Users/Efe/Documents/AnadoluUni/Veri Görselleştirme/Final/SeyahatSuresi_OrtStats.png", 
    width=1200, height=700, res=150, bg="transparent")

# Yığılmış çubuk grafiklerinin oluşturulması
ggplot(veri_ortalama_gruplu, aes(x = Gruplu_Seyahat_Sure)) +
  geom_bar(aes(y = Ortalama_Sayi, fill = "Sayi"), stat = "identity", alpha = 0.8, position = position_dodge(width = 0.7), width = 0.7) +
  geom_bar(aes(y = Ortalama_Rebound, fill = "Ribaund"), stat = "identity", alpha = 0.8, position = position_dodge(width = 0.7), width = 0.7) +
  geom_bar(aes(y = Ortalama_Asist, fill = "Asist"), stat = "identity", alpha = 0.8, position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(y = Ortalama_Sayi, label = round(Ortalama_Sayi, 2)), vjust = -0.5, color = "white", size = 4, position = position_dodge(width = 0.7)) +
  geom_text(aes(y = Ortalama_Rebound, label = round(Ortalama_Rebound, 2)), vjust = -0.5, color = "black", size = 4, position = position_dodge(width = 0.7)) +
  geom_text(aes(y = Ortalama_Asist, label = round(Ortalama_Asist, 2)), vjust = 1.5, color = "white", size = 4, position = position_dodge(width = 0.7)) +
  labs(title = "Seyahat Süresine Göre Ortalama Değerler",
       x = "Seyahat Süresi",
       y = "Ortalama Değer",
       fill = "Değer Türü") +
  scale_fill_manual(values = c("Sayi" = "#29ff82", "Ribaund" = "#fff829", "Asist" = "#ff2929")) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),  # Arka planın şeffaflaştırılması
    panel.background = element_rect(fill = "transparent", color = NA),  # Panel arka planının şeffaflaştırılması
    plot.title = element_text(size = 16, color = "white", face = "bold", margin = margin(b = 20)),  
    axis.title.x = element_text(size = 14, color = "white", face = "bold", margin = margin(t = 10)),  
    axis.title.y = element_text(size = 14, color = "white", face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 12, color = "white"),  
    axis.text.y = element_text(size = 12, color = "white"),  
    legend.title = element_text(size = 14, color = "white", face = "bold"),
    legend.text = element_text(size = 12, color = "white")  
  )

# Export grafik aracının kapatılması
dev.off()
