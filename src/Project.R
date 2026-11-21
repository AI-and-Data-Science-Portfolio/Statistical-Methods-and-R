# Report 1

# import data
data2 = read.csv("Data Carrard et al. 2022 MedTeach.csv", sep = ",", header = TRUE)

str(data2)

# Contingency table 1 for job status and therapy status
therapy = data2$psyt
job = data2$job

table1 = table(therapy, job)
dimnames(table1) = list("Therapy Status" = c("No therapy", "Therapy"),
                        "Job Status" = c("Unemployed", "Employed"))
print(table1)
table_1 = prop.table(table1, margin = 2)
print(round(prop.table(table1, margin = 2), 3))

# Stacked bar plot to visualise contingency table 1
par(mfrow=c(1,2), mar=c(4.1,4.1,4.1,4.1), oma=c(0,0,0, 0.1), tck = -0.05)
barplot(table1, beside = FALSE, col = c("skyblue", "orange"), legend = TRUE, las = 1,
        args.legend = list(x = 6, cex = 0.8, inset = c(0, 0), 
                           bty = "n", y.intersp = 0.7, x.intersp = 0.1),
        main = "Employment and Therapy Status",
        ylab = "No. of Students", ylim = c(0, 600), font.lab = 2)
barplot(table_1, beside = FALSE, col = c("skyblue", "orange"), legend = FALSE, las = 1,
        main = "Employment and Therapy Status",
        ylab = "Proportion of Students", ylim = c(0, 1), font.lab = 2)

# Z-test to test for difference between two proportion
prop.test(x = c(119,80), n = c(458, 229), p = NULL, alternative = c("two.sided"), 
          conf.level = 0.95, correct = TRUE)

# Contingency table 2 for year of study and therapy status
year = data2$year

table2 = table(therapy, year)
dimnames(table2) = list("Therapy Status" = c("No therapy", "Therapy"),
                        "Year" = c("One", "Two","Three", "Four", "Five", "Six"))

print(table2)
table_2 = prop.table(table2, margin = 2)
print(round(prop.table(table2, margin = 2),3))

# Balloon plot to visualise contingency table 2
#install.packages("reshape2")
#install.packages("ggplot2")
library(reshape2)
library(ggplot2)

df2 = as.data.frame(as.table(table2))

ggplot(df2, aes(y = Therapy.Status, x = Year, size = Freq)) +
  geom_point(alpha = 0.7, color = "darkred") + 
  scale_size(range = c(3, 15)) +           
  theme_minimal() + labs(title = "Year of Study and Therapy Status",
                         y = "Therapy Status", x = "Year of Study", size = "Frequency") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 12),
        axis.ticks.length = unit(0.20, "cm"),
        panel.grid.major = element_line(color = "black", linewidth = 0.1, linetype = "dashed"))

df3 = as.data.frame(as.table(table_2))

ggplot(df3, aes(y = Therapy.Status, x = Year, size = Freq)) +
  geom_point(alpha = 0.7, color = "darkred") + 
  scale_size(range = c(3, 15)) +           
  theme_minimal() + labs(title = "Year of Study and Therapy Status",
                         y = "Therapy Status", x = "Year of Study", size = "Proportion") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face = "bold", size = 12),
        legend.title = element_text(face = "bold", size = 12),
        axis.ticks.length = unit(0.20, "cm"),
        panel.grid.major = element_line(color = "black", linewidth = 0.1, linetype = "dashed"))

# Chi-squared test to test for difference between multiple proportions
chisq.test(table2)

# Contingency table 3 for gender and therapy status
gender = data2$sex

table3 = table(therapy, gender)
dimnames(table3) = list("Therapy Status" = c("No therapy", "Therapy"),
                        "Gender" = c("Male", "Female","Non-Binary"))

print(table3)
print(round(prop.table(table3, margin = 2),3))

# Mosaic plot to visualise contingency table 3
#install.packages("vcd")
library(vcd)
dev.off()
par(font = 1, font.lab = 2)
mosaicplot(t(table3), main = "Gender and Therapy Status",
           xlab = "Gender", ylab = "Therapy Status", color = c("skyblue", "orange"))

# Fisher’s exact test to test for difference between multiple proportions
fisher.test(table3)

################################################################################################
# Report 2

# import data
data3 = read.csv("Homininos_DataSet (1).csv", sep = ",", header = TRUE) 

str(data3)

# contingency table 1
table_a = addmargins(table(data3$Tecno,data3$Anatomy)) #contingency table 1
dimnames(table_a) = list("Tech Use" = c("Likely", "No", "Yes", "Total"),
                         "Anatomy" = c("Mixed", "Modern", "Old", "Very Modern", "Total"))
table_a

# correspondence analysis
#install.packages("FactoMineR")
#install.packages("FactoMineR")
library(FactoMineR)
library(factoextra)
correspondance = CA(table_a, graph = FALSE)
fviz_ca_biplot(correspondance, repel = TRUE, 
               title = "Correspondance Analysis of Anatomy and Technology Use", col.col = "blue", 
               col.row = "green", xlab = "Dimension One (98.7%)", ylab = "Dimension Two (1.3%)") + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme(panel.grid.major = element_line(color = "lightgrey", linetype = "solid"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(panel.border = element_rect(color = "black", fill = NA))

# create subset for very modern anatomy
anatomy = subset(data3, data3$Anatomy == "very modern")

# contingency table 2
table_b = addmargins(table(anatomy$Tecno_type))
dimnames(table_b) = list("Tech Type" = c("Mode 2", "Mode3", "Mode 4", "Total"))
print(table_b)

# create subsets
Mode2 = subset(anatomy, anatomy$Tecno_type == "mode 2")$Cranial_Capacity
Mode3 = subset(anatomy, anatomy$Tecno_type == "mode 3")$Cranial_Capacity
Mode4 = subset(anatomy, anatomy$Tecno_type == "mode 4")$Cranial_Capacity

df = data.frame(Mode2, Mode3, Mode4)

#centre, spread, skewness & kurtosis measures
#install.packages("moments")
library(moments)
summary(Mode2)
summary(Mode3)
summary(Mode4)

sd(Mode2)
sd(Mode3)
sd(Mode4)

kurtosis(Mode2)
kurtosis(Mode3)
kurtosis(Mode4)

skewness(Mode2)
skewness(Mode3)
skewness(Mode4)

# boxplot comparing cranial cap. by tech type
library(ggplot2)
par(font.lab = 2)
boxplot(Mode2, Mode3, Mode4, names = c("Mode 2", "Mode 3", "Mode 4"), 
        main="Cranial Cap. by Technology Type", 
        ylab="Capacity (cubic cm)", xlab="Technology type", 
        col=c("skyblue", "orange","darkred"), las=1,ylim = c(1000,1400))

#Assumption 1: check normality with histogram and Shapiro-Wilk test 
# Create bell-shaped curve
set.seed(123)
group1 = rnorm(30, mean = mean(Mode2), sd = sd(Mode2))
group2 = rnorm(30, mean = mean(Mode3), sd = sd(Mode3))
group3 = rnorm(30, mean = mean(Mode4), sd = sd(Mode4))
data_list = list(Group1 = group1, Group2 = group2, Group3 = group3)

# Create histograms and input bell-shaped curve
par(mfrow=c(1,3))
hist(Mode2, breaks = seq(from = 1200, to = 1400, by = 20), xlab = "Capacity (cubic cm)", 
     main="Mode 2", probability = TRUE, col = "skyblue",las = 1,cex.axis = 0.6, cex.lab = 0.9)
curve(dnorm(x, mean = mean(data_list$Group1), sd = sd(data_list$Group1)), 
      col = "darkred", lwd = 2,add = TRUE)
hist(Mode3, breaks = seq(from = 1000, to = 1200, by = 20), xlab = "Capacity (cubic cm)", 
     main="Mode 3", probability = TRUE, col = "skyblue",las = 1, cex.axis = 0.6, cex.lab = 0.9)
curve(dnorm(x, mean = mean(data_list$Group2), sd = sd(data_list$Group2)), 
      col = "darkred",lwd = 2,add = TRUE)
hist(Mode4, breaks = seq(from = 1100, to = 1300, by = 20), xlab = "Capacity (cubic cm)", 
     main="Mode 4", probability = TRUE, col = "skyblue",las = 1, cex.axis = 0.6, cex.lab = 0.9)
curve(dnorm(x, mean = mean(data_list$Group3), sd = sd(data_list$Group3)), 
      col = "darkred",lwd = 2,add = TRUE)

# examine missing values
is.na(Mode2)
is.na(Mode4)

subset(anatomy, anatomy$Tecno_type == "mode 2")$Time
subset(anatomy, anatomy$Tecno_type == "mode 4")$Time

subset(anatomy, anatomy$Tecno_type == "mode 2")$Current_Country
subset(anatomy, anatomy$Tecno_type == "mode 4")$Current_Country

#Assumption 1: check for homoscedasticity
model = lm(anatomy$Cranial_Capacity ~ anatomy$Tecno_type, data = anatomy)
res = residuals(model)
summary(res)

dev.off()
colors = c("skyblue", "orange", "darkred")  
level_mapping = as.numeric(as.factor(anatomy$Tecno_type))  
point_colors = colors[level_mapping] 
plot(model$fitted.values, res,
     col = point_colors, main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals",
     bty = "l", pch = 16, cex = .7,las =1, font.lab = 2, cex.axis = .7)
abline(h = 0, col = "red")
legend("topleft", legend = c("Mode 2", "Mode 3", "Mode 4"), 
       fill = c("skyblue", "orange", "darkred"), 
       horiz = TRUE, bty = "n", xpd = TRUE, , inset = c(0, -0.15), cex = .8)

# Shapiro-Wilk test
shapiro.test(Mode2)
shapiro.test(Mode4)

# Mann-Whitney U test for brain capacity differences between Mode 2 and Mode 3
wilcox.test(Mode2, Mode4)

################################################################################################
# Report 3

# import data
data = read.csv("cmu-sleep.csv", sep = ",", header = TRUE)

str(data)

# create subsets for both GPA and sleep time per race category
gpa_NUG = subset(data, data$demo_race == 1)$term_gpa
gpa_UG = subset(data, data$demo_race == 0)$term_gpa

sleep_NUG = subset(data, data$demo_race == 1)$TotalSleepTim
sleep_UG = subset(data, data$demo_race == 0)$TotalSleepTim

# centre, spread, skewness & kurtosis measures for GPA and sleep time by ethnic category
summary(gpa_NUG)
summary(gpa_UG)
sd(gpa_NUG)
sd(gpa_UG)
kurtosis(gpa_NUG)
kurtosis(gpa_UG)
skewness(gpa_NUG)
skewness(gpa_UG)

summary(sleep_NUG)
summary(sleep_UG)
sd(sleep_NUG)
sd(sleep_UG)
kurtosis(sleep_NUG)
kurtosis(sleep_UG)
skewness(sleep_NUG)
skewness(sleep_UG)

# violin plot comparing ethnic categories in terms of both GPA and sleep time
#install.packages("vioplot")
library("vioplot") 
par(mfrow=c(1,2), mar=c(3.2,3.2,3.2,3.2), oma=c(0,0,0,0))
vioplot(gpa_NUG, gpa_UG, main = "GPA by Ethnicity", ylab = "GPA", 
        las = 1, col=c("skyblue","orange"), ylim = c(0,4), cex.axis = .6, tcl = -0.2,
        line=2, xaxt='n', bty = "n")
vioplot(sleep_NUG,sleep_UG, main = "Sleep Time by Ethnicity", 
        ylab = "Sleep time (minutes)", las = 1, col=c("skyblue","orange"), ylim = c(0,650), 
        cex.axis = .6, tcl = -0.2,line=2, xaxt='n', bty = "n")
legend("bottom", legend = c("NUG", "UG"), fill = c("skyblue", "orange"), 
       horiz = TRUE, bty = "n", xpd = TRUE, , inset = c(0, -0.15), cex = .8)

# Assumption two: Q-Q plot and Shapiro-Wilk normality test
# GPA
par(mfrow=c(1,2), mar=c(4.1,4.1,4.1,4.1), oma=c(0,0,0, 0.1), tck = -0.05)
qqnorm(gpa_NUG, main = "GPA normality (NUG)", 
       ylab = "Sample quantiles", xlab = "Theoretical quantiles", 
       las = 1, col= "#8B0000", bty = "l", pch = 2, cex = .4) 
qqline(gpa_UG)
qqnorm(gpa_NUG,main = "GPA normality (UG)", 
       ylab = "Sample quantiles", xlab = "Theoretical quantiles", 
       las = 1, col= "#8B0000", bty = "l", pch = 2, cex = .4)
qqline(gpa_UG)

# sleep time
par(mfrow=c(1,2), mar=c(4.1,4.1,4.1,4.1), oma=c(0,0,0, 0.1), tck = -0.05)
qqnorm(sleep_NUG, main = "Sleep time normality (NUG)", 
       ylab = "Sample quantiles", xlab = "Theoretical quantiles", 
       las = 1, col= "#8B0000", bty = "l", pch = 2, cex = .4) 
qqline(sleep_NUG)
qqnorm(sleep_UG, main = "Sleep time normality (UG)", 
       ylab = "Sample quantiles", xlab = "Theoretical quantiles", 
       las = 1, col= "#8B0000", bty = "l", pch = 2, cex = .4)
qqline(sleep_UG)

#####
# outlier detection
 #GPA
  # NUG
   # Calculate IQR
Q1_gpa_NUG = quantile(gpa_NUG, 0.25)
Q3_gpa_NUG = quantile(gpa_NUG, 0.75)
IQR_gpa_NUG = Q3_gpa_NUG - Q1_gpa_NUG

   # Define outlier thresholds
lower_bound_gpa_NUG = Q1_gpa_NUG - 1.5 * IQR_gpa_NUG
upper_bound_gpa_NUG = Q3_gpa_NUG + 1.5 * IQR_gpa_NUG

   # Identify outliers
outliers_gpa_NUG = gpa_NUG[gpa_NUG < lower_bound_gpa_NUG | gpa_NUG > upper_bound_gpa_NUG]
print(round(outliers_gpa_NUG, 2))

  # UG
   # Calculate IQR
Q1_gpa_UG = quantile(gpa_UG, 0.25)
Q3_gpa_UG = quantile(gpa_UG, 0.75)
IQR_gpa_UG = Q3_gpa_UG - Q1_gpa_UG

   # Define outlier thresholds
lower_bound_gpa_UG = Q1_gpa_UG - 1.5 * IQR_gpa_UG
upper_bound_gpa_UG = Q3_gpa_UG + 1.5 * IQR_gpa_UG

   # Identify outliers
outliers_gpa_UG = gpa_UG[gpa_UG < lower_bound_gpa_UG | gpa_UG > upper_bound_gpa_UG]
print(round(outliers_gpa_UG, 2))

 # sleep time
  # NUG
   # Calculate IQR
Q1_NUG = quantile(sleep_NUG, 0.25)
Q3_NUG = quantile(sleep_NUG, 0.75)
IQR_NUG = Q3_NUG - Q1_NUG

   # Define outlier thresholds
lower_bound_NUG = Q1_NUG - 1.5 * IQR_NUG
upper_bound_NUG = Q3_NUG + 1.5 * IQR_NUG

   # Identify outliers
outliers_NUG = sleep_NUG[sleep_NUG < lower_bound_NUG | sleep_NUG > upper_bound_NUG]
print(round(outliers_NUG, 2))

  # UG
   # Calculate IQR
Q1_UG = quantile(sleep_UG, 0.25)
Q3_UG = quantile(sleep_UG, 0.75)
IQR_UG = Q3_UG - Q1_UG

   # Define outlier thresholds
lower_bound_UG = Q1_UG - 1.5 * IQR_UG
upper_bound_UG = Q3_UG + 1.5 * IQR_UG

   # Identify outliers
outliers_UG = sleep_UG[sleep_UG < lower_bound_UG | sleep_UG > upper_bound_UG]
print(round(outliers_UG, 2))
#####

# Anderson-Darling normality test
#install.packages("nortest")
library(nortest)
ad.test(sleep_NUG)
ad.test(sleep_UG)

# Levene's test to compare variances 
#install.packages("car")
library(car)
# GPA
leveneTest(y = c(gpa_NUG, gpa_UG), 
           group = factor(c(rep("gpa_NUG", length(gpa_NUG)), 
                            rep("gpa_UG", length(gpa_UG)))),center = mean)
# Sleep time
leveneTest(y = c(sleep_NUG, sleep_UG), 
           group = factor(c(rep("sleep_NUG", length(sleep_NUG)), 
                            rep("sleep_UG", length(sleep_UG)))),center = mean)

# Mann–Whitney U test to assess GPA differences between race categories
# GPA
wilcox.test(gpa_NUG, gpa_UG)

# Sleep time
wilcox.test(sleep_NUG, sleep_UG)

# scatterplot to assess correlation of sleep time and GPA
par(mfrow=c(1,2), mar=c(4.1,4.1,4.1,4.1), oma=c(0,0,0, 0.1), tck = -0.05)
plot(sleep_NUG, gpa_NUG, main = "NUG", ylab = "Term GPA", xlab = "Sleep time (minutes)", 
     las = 1, col= "darkred", ylim = c(0,4), xlim = c(0,650), 
     bty = "l", pch = 16, cex = .7)
model1 = lm(gpa_NUG ~ sleep_NUG)
abline(model1, col = "blue", lwd = 2)
plot(sleep_UG, gpa_UG, main = "UG", ylab = "Term GPA", xlab = "Sleep time (minutes)", 
     las = 1, col= "darkred", ylim = c(0,4), xlim = c(0,650), bty = "l", 
     pch = 16, cex = .7)
model2 = lm(gpa_UG ~ sleep_UG)
abline(model2, col = "blue", lwd = 2)

# correlation coefficient significance level
cor.test(sleep_NUG, gpa_NUG, method = "kendall")
cor.test(sleep_UG, gpa_UG, method = "kendall")