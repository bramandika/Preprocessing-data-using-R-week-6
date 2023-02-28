setwd("~/asdos aed/minggu ke 6")
df=read.csv("missing_value.csv",header=TRUE,sep=",")
df
library(gridExtra)
str(df)
sum(is.na(df))
df_clean = within(df, {
  Embarked <- factor(df$Embarked)
  Survived <- factor(df$Survived)
  Pclass <- factor(df$Pclass,
                   order=TRUE,
                   levels = c(3,2,1))
                      })
str(df_clean)  

  
  
#install.packages("Hmisc")
#install.packages("MASS)
library(Hmisc)
library(MASS)
#creating histogram
par(mfrow=c(1,2))


#probability density in histogram

hist(df_clean$Age, main = "From hist()",
     col ='cyan',
     freq = T,
     border ='red', 
     xlab="",
     breaks = 10)
truehist(df_clean$Age, main = "From truehist()",
         prob = T, 
         xlab=" ",
         nbins=10)
#histogram based on count frequencies
hist(df_clean$Age, main = "From hist()",
     col ='cyan',
     freq = T,
     border ='red', 
     xlab=" ",
     breaks = 10)
truehist(df_clean$Age, main = "From truehist()",
         prob = F, 
         xlab="Age Distribution ",
         nbins=10)


dev.off()



truehist(df_clean$Age, xlab="Age Distribution")
mu <- mean(df_clean$Age,na.rm = TRUE)
sigma <- sd(df_clean$Age,na.rm = TRUE)
x <- seq(0,80,1)
px <- dnorm(x, mean = mu, sd = sigma)

lines(density(df_clean$Age,na.rm = TRUE))
lines(x, px)

library(car)
qqPlot(df_clean$Fare)

#removing outlier with 3 sigma rules
#detecting it using scatter plot
plot(df_clean$Age,df_clean$Fare)
mean_fare=mean(df_clean$Fare,na.rm = TRUE)
sd_fare=sd(df_clean$Fare,na.rm = TRUE)
upper_bound_fare=mean_fare+3*sd_fare
lower_bound_fare=mean_fare-3*sd_fare
abline(h=upper_bound_fare,col='red')
upper_bound_fare
text(50,upper_bound_fare+20, "batas atas( h = 193.4274 )", col = "blue", adj = c(0, -.1),cex=0.8)


#detecting it using jitter plot
mean_fare=mean(df_clean$Fare,na.rm = TRUE)
sd_fare=sd(df_clean$Fare,na.rm = TRUE)
upper_bound_fare=mean_fare+3*sd_fare
lower_bound_fare=mean_fare-3*sd_fare
abline(h=upper_bound_fare,col='red')
upper_bound_fare

ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = T) +
  geom_hline(yintercept = upper_bound_fare ) +
  geom_hline(yintercept = lower_bound_fare ) 

#removing outlier
df_no_3sigma<- subset(df_clean, df_clean$Fare > lower_bound_fare & df_clean$Fare < upper_bound_fare)
mean_fare_no=mean(df_no_3sigma$Fare,na.rm = TRUE)
sd_fare_no=sd(df_no_3sigma$Fare,na.rm = TRUE)
upper_bound_fare_no=mean_fare+3*sd_fare
lower_bound_fare_no=mean_fare-3*sd_fare


plot1 = ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare ) +
  geom_hline(yintercept = lower_bound_fare ) 

plot2 = ggplot(df_no_3sigma, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare_no ) +
  geom_hline(yintercept = lower_bound_fare_no ) 
grid.arrange(plot1, plot2, ncol=2)
dev.off()
#removing outlier with hampel identifier
madm=1.4826*mad(df_clean$Fare,na.rm=TRUE)
median_fare=median(df_clean$Fare,na.rm = TRUE)
upper_bound_fare=median_fare+3*madm
lower_bound_fare=median_fare-3*madm

df_no_hampel<- subset(df_clean, df_clean$Fare > lower_bound_fare & df_clean$Fare < upper_bound_fare)
madm_no=1.4826*mad(df_no_hampel$Fare,na.rm = TRUE)
median_fare_no=median(df_no_hampel$Fare,na.rm = TRUE)
upper_bound_fare_no=median_fare+3*madm
lower_bound_fare_no=median_fare-3*madm




plot1 = ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept =upper_bound_fare ) +
  geom_hline(yintercept = median_fare,color='red') +
  geom_hline(yintercept =lower_bound_fare )

plot2 = ggplot(df_no_hampel, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_fare_no ) +
  geom_hline(yintercept = median_fare_no,color='red' ) +
  geom_hline(yintercept = lower_bound_fare_no )
grid.arrange(plot1, plot2, ncol=2)

dev.off()
#removing outlier with boxplot
Q <- quantile(df_clean$Fare, probs=c(.25,0.5,.75), na.rm = TRUE)
Q
iqr <- IQR(df_clean$Fare)
upper_bound_boxplot <-  Q[3]+1.5*iqr # Upper Range  
lower_bound_boxplot <- Q[1]-1.5*iqr # Lower Range???
median_boxplot <- Q[2]

df_boxplot_no<- subset(df_clean, df_clean$Fare > (Q[1] - 1.5*iqr) & df_clean$Fare < (Q[3]+1.5*iqr))
Q_no <- quantile(df_boxplot_no$Fare, probs=c(.25,0.5,.75), na.rm = TRUE)
iqr_no <- IQR(df_boxplot_no$Fare)
upper_bound_boxplot_no <-  Q_no[3]+1.5*iqr_no # Upper Range  
lower_bound_boxplot_no <- Q_no[1]-1.5*iqr_no # Lower Range???
median_boxplot_no <- Q_no[2]



plot1 = ggplot(df_clean, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept =upper_bound_boxplot ) +
  geom_hline(yintercept = median_boxplot,color='red') +
  geom_hline(yintercept =lower_bound_boxplot )

plot2 = ggplot(df_boxplot_no, aes(x = Parch, y = Fare, colour = Survived)) +
  geom_jitter(show.legend = FALSE,na.rm = TRUE) +
  geom_hline(yintercept = upper_bound_boxplot_no ) +
  geom_hline(yintercept = median_boxplot_no,color='red' ) +
  geom_hline(yintercept = lower_bound_boxplot_no )
grid.arrange(plot1, plot2, ncol=2)

dev.off()



#imputation missing value
df_imputation_0=df
df_imputation_median=df
df_imputation_average=df
df_imputation_0 = impute(df_imputation_0$Age,0) 
View(df_imputation_0)
hist(df$Age)
df_imputation_median$Age = impute(df_imputation_median$Age,median(df_imputation_median$Age,na.rm=TRUE)) 
df_imputation_average$Age = impute(df_imputation_average$Age,mean(df_imputation_average$Age,na.rm=TRUE))
#impute data text with random value
df$Survived=impute(df$Survived,'random')
#impute data text with mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(df$Pclass)
print(result)
df$Pclass=impute(df$Pclass,result)


