#OYUNCU KORELASYONLARININ GRAFİKLEŞTİRİLMESİ

# Gerekli kütüphanelerin yüklenmesi
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# Her bir oyuncu için "Seyahat_Sure" ve "PIR" arasındaki korelasyonun hesaplanması (Pearson)
korelasyonlar <- basketbol_veri %>%
  group_by(Oyuncu_Adi) %>%
  summarise(korelasyon = cor(Seyahat_Sure, PIR, use = "complete.obs")) %>%
  ungroup()

# PNG dosyasının şeffaf arka planla yüksek çözünürlükte kaydedilmesi
png(file="C:/Users/Efe/Documents/AnadoluUni/Veri Görselleştirme/Final/OyuncuBazliKorelasyon.png", 
    width=2000, height=1000, res=150, bg="transparent")

# Sütun grafiğinin oluşturulması
ggplot(korelasyonlar, aes(x = korelasyon, y = reorder(Oyuncu_Adi, korelasyon), fill = korelasyon)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(korelasyon, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5, 
            hjust = ifelse(korelasyonlar$korelasyon < 0, 1.1, -0.1), 
            color = "white") +  # Metin rengini beyaz yap
  labs(title = "Her Bir Oyuncunun Seyahat Süresi ve PIR Arasındaki Korelasyon",
       x = "Korelasyon",
       y = "Oyuncu Adı",
       fill = "Korelasyon") + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_minimal(base_size = 15) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.25)) +
  theme(
    axis.text.y = element_text(size = 12, color = "white"),  
    axis.text.x = element_text(size = 12, color = "white"),  
    axis.title.x = element_text(size = 14, color = "white", face = "bold", margin = margin(t = 10)),  
    axis.title.y = element_text(size = 14, color = "white", face = "bold", margin = margin(r = 10)),  
    plot.title = element_text(size = 24, color = "white", face = "bold", margin = margin(b = 20)),  
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "gray"),  
    legend.position = "right",  
    legend.text = element_text(color = "white"),  
    legend.title = element_text(color = "white", face = "bold")  
  )

# Grafik cihazını kapat
dev.off()