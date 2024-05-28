# MESAFEYE BAĞLI PIR GÖSTERİMİ

install.packages("readr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)

# Seyahat mesafelerinin gruplandırılması
veri <- basketbol_veri %>%
  mutate(Seyahat_Grubu = case_when(
    Seyahat_Mesafe == 0 ~ "0",
    Seyahat_Mesafe >= 1 & Seyahat_Mesafe <= 1000 ~ "1-1000",
    Seyahat_Mesafe >= 1001 & Seyahat_Mesafe <= 2000 ~ "1001-2000",
    Seyahat_Mesafe > 2000 ~ "2000+"
  ))

# Seyahat gruplarına göre ortalama PIR değerleri
ortalama_PIR <- veri %>%
  group_by(Seyahat_Grubu) %>%
  summarise(Ortalama_PIR = mean(PIR, na.rm = TRUE))

# PNG dosyasının şeffaf arka planla yüksek çözünürlükte kaydedilmesi
png(file="C:/Users/Efe/Documents/AnadoluUni/Veri Görselleştirme/Final/MesafeyeBagliPIR.png", 
    width=1200, height=700, res=150, bg="transparent")

# Nokta grafiği oluşturulması
ggplot(data = ortalama_PIR, aes(x = Seyahat_Grubu, y = Ortalama_PIR, group = 1)) +
  geom_point(size = 5, color = "yellow") +
  geom_line(size =2, color = "yellow") +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2.5)) +
  labs(title = "Seyahat Mesafelerine Göre Ortalama PIR Değerleri",
       x = "Seyahat Mesafesi Grubu",
       y = "Ortalama PIR") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),  # Arka planın şeffaf yapılması
    panel.background = element_rect(fill = "transparent", color = NA),  # Panel arka planının şeffaf yapılması
    plot.title = element_text(size = 16, color = "white", face = "bold", margin = margin(b = 10)),  
    axis.title.x = element_text(size = 14, color = "yellow", face = "bold", margin = margin(t = 10)),  
    axis.title.y = element_text(size = 14, color = "yellow", face = "bold", margin = margin(r = 10)),  
    axis.text.x = element_text(size = 12, color = "white"),  
    axis.text.y = element_text(size = 12, color = "white"),  
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "gray")   
  )

# Export grafik aracının kapatılması
dev.off()
