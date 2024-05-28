#Saha Bazlı Olarak Oyuncuların PIR Değişimlerinin Gösterilmesi

install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

ortalama_PIR_oyuncu_saha <- veri %>%
  group_by(Oyuncu_Adi, Saha) %>%
  summarise(Ortalama_PIR = mean(PIR, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Oyuncu_Adi = reorder(Oyuncu_Adi, Ortalama_PIR, FUN = median))

ortalama_PIR_oyuncu_saha <- ortalama_PIR_oyuncu_saha %>%
  mutate(Saha = ifelse(Saha == 0, "İç Saha", "Dış Saha"))

# PNG dosyasının şeffaf arka planla yüksek çözünürlükte kaydedilmesi
png(file="C:/Users/Efe/Documents/AnadoluUni/Veri Görselleştirme/Final/SahaBazliOyuncuPIRDegisimi.png", 
    width=1500, height=750, res=150, bg="transparent")

# Dumbbell plot'u oluştur
# ggplot ile görselleştirmeyi oluştur
# ggplot ile görselleştirmeyi oluştur
ggplot(data = ortalama_PIR_oyuncu_saha, aes(x = Ortalama_PIR, y = Oyuncu_Adi, color = Saha)) +
  geom_point(size = 5) +  # noktaları çiz
  geom_line(size = 1.5,aes(group = Oyuncu_Adi)) +
  labs(title = "Oyuncuların İç ve Dış Saha Maçlarında Ortalama PIR Değişimi", x = "Ortalama PIR", y = "Oyuncu Adı", color = "Saha") +  # başlık ve eksen etiketleri
  theme_minimal() +  # temayı belirle
  theme(legend.position = "top",  # açıklama konumunu belirle
        plot.background = element_rect(fill = "transparent", color = NA),  # Arka planın şeffaflaştırılması
        panel.background = element_rect(fill = "transparent", color = NA),  # Panel arka planının şeffaflaştırılması
        plot.title = element_text(size = 16, color = "white", face = "bold", margin = margin(b = 20)),  
        axis.title.x = element_text(size = 14, color = "white", face = "bold", margin = margin(t = 10)),  
        axis.title.y = element_text(size = 14, color = "white", face = "bold", margin = margin(r = 10)),
        axis.text.x = element_text(size = 12, color = "white"),  
        axis.text.y = element_text(size = 12, color = "white"),  
        legend.title = element_text(size = 14, color = "white", face = "bold"),
        legend.text = element_text(size = 12, color = "white"),
        panel.grid.major = element_line(color = "gray", linetype = "solid", size = 0.5),  
        panel.grid.minor = element_line(color = "gray", linetype = "dotted", size = 0.5)) +  
  scale_color_manual(values = c("#32B7EC", "#FF0000"))  # renk skalasının özelleştirilmesi

# Export grafik aracının kapatılması
dev.off()
