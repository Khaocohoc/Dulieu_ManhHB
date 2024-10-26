# Tan suat phan bo theo dia diem
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
View(data2)
data2 %>% 
  group_by(`Names`) %>%
  tally()
data2_Names_tally <- data2 %>% 
  group_by(`Names`) %>% 
  tally() %>% 
  filter(`Names` != 'NA') %>% 
  arrange(desc(n))
ggplot(data2_Names_tally,
       aes(x = reorder(`Names`, n), y = n)) +
  geom_bar(stat="identity", size = 1, fill = 'white', color = 'black', fill = "white", width = 0.45) +
  theme_bw(base_size = 20) +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Tần suất")


# Su phan bo cac cong cu manh theo lop - CAC DI CHI
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Names, y="")) +
  geom_bar(stat="identity",colour = "black", size = 1, fill = "white", white = 0.3, width = 0.5) + 
  facet_grid(~ Context) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text(size = 20)) +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("") +
  ggtitle("") +
  coord_flip()


# Loai hinh CONG CU MANH/TINH TRANG 
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Tinhtrang, y="")) +
  geom_bar(stat="identity",colour = "black",fill = "white", size = 0.8, width = 0.6) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text("")) +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("Tình trạng") +
  ggtitle("") +
  coord_flip()


# Loai hinh CONG CU MANH/MAU SAC
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Mausac, y="")) +
  geom_bar(stat="identity",colour = "black",fill = "white", size = 0.8, width = 0.6) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text("")) +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("Màu sắc vỏ cuội/đá") +
  ggtitle("") +
  coord_flip()


# Loai hinh NGUYEN LIEU - DI CHI
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
ggplot(data2, aes(x=Material, y="")) +
  geom_bar(stat="identity",colour = "black", fill = "white", size = 0.8, width = 0.6) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text("")) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("Nguyên liệu") +
  ggtitle("") +
  coord_flip()


# Loai hinh nguyen lieu theo phan bo lop van hoa
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Names, y="")) +
  geom_bar(stat="identity",colour = "black", fill = "white", size = 0.8, width = 0.6) + 
  facet_grid(~ Material_1) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text("")) +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 18, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 10, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("Lớp văn hóa") +
  ggtitle("") +
  coord_flip()


# Hinh thai MAT CANG NGANG cong cu manh cac Di chi Hoa Binh som 42-20ka
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Matcatngang, y="")) +
  geom_bar(stat="identity", colour = "black", fill = "white", size = 0.8, width = 0.6) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text("")) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("Mặt cắt ngang") +
  ggtitle("") +
  coord_flip()


# Hinh thai MAT CAT DOC cong cu manh cac Di chi Hoa Binh som 42-20ka
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Matcatdoc, y="")) +
  geom_bar(stat="identity", colour = "black", fill = "white", size = 0.8, width = 0.6) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text("")) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("Mặt cắt dọc") +
  ggtitle("") +
  coord_flip()


###----------------- TOM TAT GIA TRI TRUNG VI/TU PHAN VI---------------------###
# HANG DIEM
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data21 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Diem')
data21
View(data21)
summary(data21)
IQR(data21$Mass)
IQR(data21$Length_max)
IQR(data21$Length)
data21$Width<- as.numeric(data21$Width)
data21 <- data21 %>% drop_na(Width)
IQR(data21$Width)
IQR(data21$Thickness)
IQR(data21$Platform_width)
IQR(data21$Platform_thickness)
IQR(data21$Gocghe_ngoai)
IQR(data21$Gocghe_trong)
data21$Tyle_voda<- as.numeric(data21$Tyle_voda)
data21 <- data21 %>% drop_na(Tyle_voda)
IQR(data21$Tyle_voda)
IQR(data21$Sovetghe)
IQR(data21$Lopghe)

# THAM TAU
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data22 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Tau')
data22
View(data22)
summary(data22)
IQR(data22$Mass)
IQR(data22$Length_max)
IQR(data22$Length)
data22$Width<- as.numeric(data22$Width)
data22 <- data22 %>% drop_na(Width)
IQR(data22$Width)
IQR(data22$Thickness)
IQR(data22$Platform_width)
IQR(data22$Platform_thickness)
IQR(data22$Gocghe_ngoai)
IQR(data22$Gocghe_trong)
data22$Tyle_voda<- as.numeric(data22$Tyle_voda)
data22 <- data22 %>% drop_na(Tyle_voda)
IQR(data22$Tyle_voda)
IQR(data22$Sovetghe)
IQR(data22$Lopghe)


# THUNG LAU
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data23 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Lau')
data23
View(data23)
summary(data23)
IQR(data23$Mass)
IQR(data23$Length_max)
IQR(data23$Length)
data23$Width<- as.numeric(data23$Width)
data23 <- data23 %>% drop_na(Width)
IQR(data23$Width)
IQR(data23$Thickness)
IQR(data23$Platform_width)
IQR(data23$Platform_thickness)
IQR(data23$Gocghe_ngoai)
IQR(data23$Gocghe_trong)
data23$Tyle_voda<- as.numeric(data23$Tyle_voda)
data23 <- data23 %>% drop_na(Tyle_voda)
IQR(data23$Tyle_voda)
IQR(data23$Sovetghe)
IQR(data23$Lopghe)


###------------------------------------QUY MO--------------------------------------###
# Chi so Trung Vi va Di chi
# Di chi va Trong luong
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
names(data2)
data2$Mass <- as.numeric(data2$Mass)
data2 <- data2 %>% drop_na(Mass)
ggplot(data2 , aes(x = Names, y = Mass)) + 
  geom_boxplot(width = 0.5, size = 0.9) +
  geom_jitter(height = 0,  width = 0.05, alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5, color = 'black')) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Trọng lượng (g)") +
  ggtitle("") +
  labs(fill = "Nguyên liệu: ") +
  scale_y_log10()
# Anova: LOP VAN HOA va TRONG LUONG
attach(data2)
str(data2)
av1 = aov(Mass ~ Names)
summary(av1)
av1
TukeyHSD(av1)
tk = TukeyHSD(av1)
plot(tk)


# DI CHI VA CHIEU DAI
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
data2$Length <- as.numeric(data2$Length)
data2 <- data2 %>% drop_na(Length)
ggplot(data2 , aes(x = Names, y = Length)) + 
  geom_boxplot(width = 0.5, size = 0.7) +
  geom_jitter(height = 0,  width = 0.05, alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5, color = 'black')) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Chiều dài (mm)") +
  ggtitle("") +
  labs(fill = "Nguyên liệu: ") +
  scale_y_log10()
# Anova: LOP VAN HOA va CHIEU DAI
attach(data2)
str(data2)
av = aov(Length ~ Names)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# DI CHI VA CHIEU RONG
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
data2$Width <- as.numeric(data2$Width)
data2 <- data2 %>% drop_na(Width)
ggplot(data2 , aes(x = Names, y = Width)) + 
  geom_boxplot(width = 0.5, size = 0.9) +
  geom_jitter(height = 0,  width = 0.2, alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Chiều rộng (mm)") +
  ggtitle("") +
  labs(fill = "Nguyên liệu: ") +
  scale_y_log10()
# Anova: LOP VAN HOA va CHIEU RONG
attach(data2)
str(data2)
av = aov(Width ~ Names)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# DI CHI VA CHIEU DAY
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
data2$Thickness <- as.numeric(data2$Thickness)
data2 <- data2 %>% drop_na(Thickness)
ggplot(data2 , aes(x = Names, y = Thickness)) + 
  geom_boxplot(width = 0.5, size = 0.7) +
  geom_jitter(height = 0,  width = 0.2, alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Chiều dày (mm)") +
  ggtitle("") +
  labs(fill = "Nguyên liệu: ") +
  scale_y_log10()
# Anova: NGUYEN LIEU va CHIEU DAY
attach(data2)
str(data2)
av = aov(Thickness ~ Names)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


## HE SO TUONG QUAN - QUY MO----------------------------------------------------
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
attach(data2)
cor.test(Length, Width, method = "pearson")
cor.test(Edge_angle1, Thickness, method = "pearson")

# Mass/Max_dimension - rat OK
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
names(data2)
data2$Mass <- as.numeric(data2$Mass)
data2$Length <- as.numeric(data2$Length)
data2 <- data2 %>% drop_na(Mass)
data2 <- data2 %>% drop_na(Length)
ggscatter(data2 , x = "Mass", y = "Length",
          color = "black", shape = 1, size = 8, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence intehttp://127.0.0.1:22589/graphics/plot_zoom_png?width=2034&height=1046rval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "perason", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 10, label.y = 120, size = 6) +
  geom_point(alpha = 0.05, size = 1, fill = "black" , color = "black") +
  facet_grid(~ Names) +
  theme_bw() +
  theme(legend.text=element_text(size = 30)) +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  theme_bw(base_size = 25) +
  theme(aspect.ratio = 4/6) +
  ylab("Trọng lượng (g)") +
  xlab("Chiều dài (mm)") +
  ggtitle("")


# He so tuong quan chieu Dai va Rong - rat OK
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
names(data2)
data2$Width <- as.numeric(data2$Width)
data2$Length <- as.numeric(data2$Length)
data2 <- data2 %>% drop_na(Width)
data2 <- data2 %>% drop_na(Length)
ggscatter(data2 , x = "Width", y = "Length",
          color = "black", shape = 1, size = 6, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence intehttp://127.0.0.1:22589/graphics/plot_zoom_png?width=2034&height=1046rval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "Pearsom", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 10, label.y = 100, size = 6) +
  geom_point(alpha = 0.05, size = 8, fill = "black" , color = "black") +
  facet_grid(~ Names) +
  theme_bw() +
  theme(legend.text=element_text(size = 30)) +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 10, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  theme_bw(base_size = 25) +  theme(aspect.ratio = 4/6) +
  ggtitle("") +
  xlab("Chiều rộng (mm)") +
  ylab("Chiều dài (mm)")


# He so tuong quan chieu Dai va Rong - rat OK
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
data2$Width <- as.numeric(data2$Width)
data2$Thickness <- as.numeric(data2$Thickness)
data2 <- data2 %>% drop_na(Width)
data2 <- data2 %>% drop_na(Thickness)
ggscatter(data2 , x = "Width", y = "Thickness",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "lm", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 10, label.y = 30, size = 6) +
  geom_point(alpha = 0.05, size = 8, fill = "black" , color = "black") +
  facet_grid(~ Names) +
  theme_bw() +
  theme(legend.text=element_text(size = 30)) +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 10, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  theme_bw(base_size = 25) +
  theme(aspect.ratio = 4/6) +
  ggtitle("") +
  xlab("Chiều dày (mm)") +
  ylab("Chiều rộng (mm)")


# He so tuong quan chieu Dai và Day
library(readxl)
library(tidyverse)
library(ggforce)
library("ggpubr")
library("broom")
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
names(data2)
data2$Length <- as.numeric(data2$Length)
data2$Thickness <- as.numeric(data2$Thickness)
data2 <- data2 %>% drop_na(Width)
data2 <- data2 %>% drop_na(Thickness)
ggscatter(data2 , x = "Length", y = "Thickness",
          color = "black", shape = 21, size = 5, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "Pearson", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 10, label.y = 35, size = 6) +
  geom_point(alpha = 0.05, size = 8, fill = "black" , color = "black") +
  facet_grid(~ Names) +
  theme_bw() +
  theme(legend.text=element_text(size = 30)) +
  theme(text = element_text(size = 25),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 10, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  theme_bw(base_size = 25) +
  theme(aspect.ratio = 4/6) +
  ggtitle("") +
  xlab("Chiều dài (mm)") +
  ylab("Chiều dày (mm)")


#----------------------------- KY THUAT CHE TAC---------------------------------
#-----DIEN GHE---------------------------------
# LOAI HINH DIEN GHE
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
View(data2)
data2 %>% 
  group_by(`Dienghe`) %>%
  tally()
data2_Dienghe_tally <- data2 %>% 
  group_by(`Dienghe`) %>% 
  tally() %>% 
  filter(`Dienghe` != 'NA') %>% 
  arrange(desc(n))
ggplot(data2_Dienghe_tally,
       aes(x = reorder(`Dienghe`, n), y = n)) +
  geom_bar(stat="identity", size = 0.8, fill = 'white', color = 'black', width = 0.6) +
  theme_bw(base_size = 12) +
  theme(text = element_text(size = 30)) +
  xlab("
       Loại diện ghè") +
  ylab("Tần suất")


# KY THUAT GHE MOT/HAI MAT
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
View(data2)
data2 %>% 
  group_by(`Kythuatghe`) %>%
  tally()
data2_Kythuatghe_tally <- data2 %>% 
  group_by(`Kythuatghe`) %>% 
  tally() %>% 
  filter(`Kythuatghe` != 'NA') %>% 
  arrange(desc(n))
ggplot(data2_Kythuatghe_tally,
       aes(x = reorder(`Kythuatghe`, n), y = n)) +
  geom_bar(stat="identity", size = 0.8, fill = 'white', color = 'black', width = 0.5) +
  theme_bw(base_size = 12) +
  theme(text = element_text(size = 30)) +
  xlab("
       Ghè trực tiếp") +
  ylab("Tần suất")


# HUONG GHE CUA CONG CU MANH
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
View(data2)
data2 %>% 
  group_by(`Huongnhatghe`) %>%
  tally()
data2_Huongnhatghe_tally <- data2 %>% 
  group_by(`Huongnhatghe`) %>% 
  tally() %>% 
  filter(`Huongnhatghe` != 'NA') %>% 
  arrange(desc(n))
ggplot(data2_Huongnhatghe_tally,
       aes(x = reorder(`Huongnhatghe`, n), y = n)) +
  geom_bar(stat="identity", size = 0.8, fill = 'white', color = 'black', width = 0.5) +
  theme_bw(base_size = 20) +
  theme(text = element_text(size = 30)) +
  xlab(" 
       Hướng tu chỉnh") +
  ylab("Tần suất")


# HUONG GHE CONG CỤ MANH PHAN BO THEO DI DI CHI
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
data2 <- data2 %>% drop_na(Kythuatghe)
ggplot(data2, aes(x=Kythuatghe, y="", fill = "")) +
  geom_bar(stat="identity",colour = "black", size = 1, fill = "white", width = .4) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text(size = 25)) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("
       Kỹ thuật ghè") +
  ggtitle("")


# TU CHINH HAY NOTCH
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
data2 <- data2 %>% drop_na(Kythuatghedeo)
ggplot(data2, aes(x=Kythuatghedeo, y="", fill = "")) +
  geom_bar(stat="identity",colour = "black", size = 1, fill = "white", width = .4) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text(size = 25)) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("
       Loại tu chỉnh") +
  ggtitle("") +
  coord_flip()


# PHAN BO DIEN GHE THEO LOP/DI CHI
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
ggplot(data2, aes(x=Context, y="")) +
  geom_bar(stat="identity",colour = "black", fill = "white", size = 0.7) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text(size = 25)) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("") +
  ggtitle("") +
  coord_flip()


# VI TRI TU CHINH LUNG/DI CHI
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
data2 <- data2 %>% drop_na(Names)
data2 <- data2 %>% drop_na(Tuchinh_lung)
ggplot(data2, aes(x=Tuchinh_lung, y="")) +
  geom_bar(stat="identity",colour = "black", fill = "white", size = 0.7) + 
  facet_grid(~ Names) +
  geom_text(aes(label = "")) +
  theme_bw() +
  theme(legend.text=element_text(size = 25)) +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 1, colour = "black"),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 1, colour = "black")) +
  ylab("") +
  xlab("") +
  ggtitle("Vị trí tu chỉnh mặt lưng") +
  coord_flip()


#--------------------------------GOC LUOI --------------------------------------
# GIA TRI TRUNG BINH GOC LUOI CONG CU MANH
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
data2$Gocluoi1 <- as.numeric(data2$Gocluoi1)
data2 <- data2 %>% drop_na(Gocluoi1)
ggplot(data2 %>% filter(!is.na(Gocluoi1)), 
       aes(x = Gocluoi1, y = "")) + 
  geom_boxplot(width = 0.3, size = 0.8) +
  geom_jitter(height = 0,  width = 0.15, alpha = 0.05) +
  facet_wrap( ~ Names) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30, color = "black"),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5, color = "black"),
        axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5, color = 'black')) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  theme(aspect.ratio = 4/12) +
  xlab("Góc lưỡi (o)") +
  ylab("") +
  ggtitle("Chỉ số góc lưỡi") +
  coord_flip()

#Anova / GOC LUOI
attach(data2)
str(data2)
av = aov(Gocluoi1 ~ Names)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


#--------------------------------- SO VET GHE-----------------------------------
# SO VET GHE
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
data2$Gocluoi1 <- as.numeric(data2$Sovetghe)
data2 <- data2 %>% drop_na(Sovetghe)
data2 <- data2 %>% drop_na(Names)
ggplot(data2 %>% filter(!is.na(Sovetghe)), 
       aes(x = Sovetghe, y = "")) + 
  geom_boxplot(width = 0.3, size = 0.8) +
  geom_jitter(height = 0,  width = 0.15, alpha = 0.05) +
  facet_wrap( ~ Names) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30, color = "black"),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5, color = "black"),
        axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5, color = 'black')) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  theme(aspect.ratio = 3/4) +
  xlab("Tần suất (o)") +
  ylab("") +
  ggtitle("Số vết ghè") +
  scale_x_log10() +
  coord_flip()

#Anova / SO VET GHE
attach(data2)
str(data2)
av = aov(Sovetghe ~ Names)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# SO VET GHE/LOP GHE
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(ggpubr)
library(here)
library(ggplot2)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
names(data2)
data2$Gocluoi1 <- as.numeric(data2$Sovetghe)
data2 <- data2 %>% drop_na(Sovetghe)
data2 <- data2 %>% drop_na(Names)
ggplot(data2 %>% filter(!is.na(Sovetghe)), 
       aes(x = Lopghe, y = "")) + 
  geom_boxplot(width = 0.3, size = 0.8) +
  geom_jitter(height = 0,  width = 0.15, alpha = 0.05) +
  facet_wrap( ~ Names) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=30, color = "black"),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0.5, color = "black"),
        axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5, color = 'black')) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  theme(aspect.ratio = 3/4) +
  xlab("Tần suất (o)") +
  ylab("") +
  ggtitle("Số lớp ghè") +
  scale_x_log10() +
  coord_flip()


#Anova / SO LOP GHE
attach(data2)
str(data2)
av = aov(Lopghe ~ Names)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)

# HE SO TUONG QUAN SO NHAT GHE / GOC LUOI
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
data2 <- read_excel("Flaketools_HBearly.xlsx", sheet = 'Sheet1')
data2
str(data2)
data2$Sovetghe <- as.numeric(data2$Sovetghe)
data2 <- data2 %>% drop_na(Sovetghe)
data2 <- data2 %>% drop_na(Gocluoi1)
data2$Gocluoi1 <- as.numeric(data2$Gocluoi1)
ggscatter(data2 , x = "Gocluoi1", y = "Thickness",
          color = "black", shape = 1, size = 4, # Points color, shape and size
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
          cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
          cor.coeff.args = list(method = "", label.x = 2, label.sep = "\n")
) +
  stat_cor(method = "pearson", label.x = 0, label.y = 20, size = 8) +
  geom_point(alpha = 0.05, size = 5, fill = "black" , color = "black") +
  facet_wrap( ~ Names) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("") +
  xlab("Góc lưỡi (o)") +
  ylab("Chiều dày")


#-------------------------------- XONG -----------------------------------------