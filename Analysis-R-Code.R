# Loading the required libraries
library(MASS)
library(readxl)
library(tidyverse)
library(gridExtra)

#Importing the files
faa1 <- read_xls("faa1.xls")
faa2 <- read_xls("faa2.xls")

# Check the structure
str(faa1)
str(faa2)

#Binding rows
faa <- bind_rows(faa1,faa2)

print(paste0("The number of duplicates are ",sum(duplicated(faa[,-2]))))

# Action: split into duplicates and dedeuped data
faa_deduped <- faa[!duplicated(faa[,-2]),]
faa_duplicates <- faa[duplicated(faa[,-2]),]

# Strucute
str(faa_deduped)

#Summary
summary(faa_deduped)

# Outlier Observations lost
print(paste("The total records with abnormal values are",
sum((faa_deduped$duration < 40 & !is.na(faa_deduped$duration)) | faa_deduped$speed_ground > 140 | faa_deduped$speed_ground < 30 | faa_deduped$height < 6 |
      faa_deduped$distance > 6000)))

#Clean file
faa_clean <- faa_deduped[!((faa_deduped$duration <40 & !is.na(faa_deduped$duration)) | faa_deduped$speed_ground > 140
                           | faa_deduped$speed_ground < 30 | faa_deduped$height < 6 |
                             faa_deduped$distance > 6000),
                        ]

# Strucute
str(faa_clean)

#Summary
summary(faa_clean)

#Numeric variables
numeric_var <- sapply(faa_clean,is.numeric)

#Plots - Gathering to make the plots quicker
ggplot(data = gather(faa_clean[,numeric_var], key = "key", value="value"), aes(x = value)) + 
  geom_histogram(bins = 30, col = "cyan4", fill = "cyan4") + 
  facet_wrap(~key, scales = 'free_x')

#Correlation
faa_clean$aircraft_tp <- ifelse(faa_clean$aircraft=='airbus',1,0)
numeric_var <- sapply(faa_clean,is.numeric)

corr <- cor(faa_clean[,numeric_var],faa_clean$distance ,use="pairwise.complete.obs")
corr <- as.data.frame((corr[order(abs(corr),decreasing = T),]))

names(corr) <- c("Size of Correlation")

# Gettting the direction
corr$Direction <- ifelse(corr[,"Size of Correlation"] > 0,"Positive", "Negative")

corr[,1] <- round(corr[,1],4)

rownames(corr)[which(rownames(corr) == 'aircraft_tp')] <- "Aircraft (1 Airbus; 0 Boeing)" 
corr <- corr[-1,]

View(corr)

# Plots - Bivariate analysis
ggplot(data = gather(faa_clean[,numeric_var], key = "key", value="value", -distance), aes(x = value, y= distance)) + 
  geom_point(fill="cyan4") + 
  facet_wrap(~key, scales = "free") +
  ggtitle("Scatter Plots")


# Building the single factor models
models <- sapply(faa_clean[,!(colnames(faa_clean) %in% c("distance", "aircraft"))], function(x) { lm(faa_clean$distance ~ x)})

pvalue <- data.frame(sapply(models, function(x) {summary(x)$coefficients[,4][2] }))
coeff_dir <- data.frame(ifelse((sapply(models, function(x) {summary(x)$coefficients[,1][2] })) > 0,"Positive","Negative"))

Variable <- (rownames(pvalue))
Variable <- substr(Variable,1,nchar(Variable)-2)
Variable[7] <- "Aircraft (1 Airbus; 0 Boeing)"

x <- data.frame(Variable = Variable, Pvalue=pvalue[,1], Direction_of_coeff = coeff_dir[,1])
x <- x[order(x$Pvalue),]
x


# Statndardizing the variables
faa_standardized <- faa_clean[,!(colnames(faa_clean) %in% c("distance", "aircraft"))]

for (i in 1:(length(colnames(faa_standardized))-1)) {
  faa_standardized[[i]] = (faa_standardized[[i]] - mean(faa_standardized[[i]], na.rm = T))/sd(faa_standardized[[i]],na.rm=T)
}

# builsing models for the standarized variables
models <- sapply(faa_standardized, function(x) { lm(faa_clean$distance ~ x)})

coeff <- data.frame(sapply(models, function(x) {summary(x)$coefficients[,1][2] }))

coeff_dir <- data.frame(ifelse(sapply(models, function(x) {summary(x)$coefficients[,1][2] })>0,"Positive", "Negative"))

Variable <- (rownames(pvalue))
Variable <- substr(Variable,1,nchar(Variable)-2)
Variable[7] <- "Aircraft (1 Airbus; 0 Boeing)"

x1 <- data.frame(Variable = Variable, Coeff=round(coeff[,1],4), Direction_of_coeff = coeff_dir[,1])
x1 <- x1[order(abs(x1$Coeff), decreasing = T),]

x1

#Ranking the important variables
rank_corr <- data.frame(Variable = rownames(corr), Rank1 = row_number(-abs(corr$`Size of Correlation`)))
rank_pval <- data.frame(Variable = x$Variable, Rank2 = row_number(abs(x$Pvalue)))
rank_betas <- data.frame(Variable = x1$Variable, Rank3 = row_number(-abs(x1$Coeff)))

rank_corr %>%
  left_join(rank_pval,by = "Variable") %>%
  left_join(rank_betas,by = "Variable") %>%
  mutate(rank_avg = (Rank1+Rank2+Rank3)/3, Overall_rank = row_number(rank_avg)) %>%
  arrange(Overall_rank) %>%
  select(Variable, Overall_rank, Rank1, Rank2, Rank3) %>%
  View()

#Models
model1 <- lm(distance ~ speed_ground, data = faa_clean)
model2 <- lm(distance ~ speed_air, data = faa_clean)
model3 <- lm(distance ~ speed_ground + speed_air, data = faa_clean)

#Model summary
summary(model1)$coefficients 
summary(model2)$coefficients 
summary(model3)$coefficients 

#Correlation
corr <- cor(faa_clean$speed_ground,faa_clean$speed_air,  use="pairwise.complete.obs")

# There is correlation, we choose speed air

# Creating models based on importance
model1 <- lm(distance ~ speed_air, data=faa_clean)
model2 <- lm(distance ~ speed_air + aircraft_tp, data=faa_clean)
model3 <- lm(distance ~ speed_air + aircraft_tp + height, data=faa_clean)
model4 <- lm(distance ~ speed_air + aircraft_tp + height + pitch, data=faa_clean)
model5 <- lm(distance ~ speed_air + aircraft_tp + height + pitch + duration, data=faa_clean)
model6 <- lm(distance ~ speed_air + aircraft_tp + height + pitch + duration + no_pasg, data=faa_clean)

rsq.model <- rep(0,6)

# R-Sq of the model
rsq.model[1] <- summary(model1)$r.squared
rsq.model[2] <- summary(model2)$r.squared
rsq.model[3] <- summary(model3)$r.squared
rsq.model[4] <- summary(model4)$r.squared
rsq.model[5] <- summary(model5)$r.squared
rsq.model[6] <- summary(model6)$r.squared

ggplot(data = NULL,aes(x = 1:6, y = rsq.model)) +
  geom_line(col = "blue") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "R Squared vs. Number of variables", x = "Number of variables", y = "R Squared")

adj.rsq.model <- rep(0,6)

# Adjusted R sq of the models - Better metric to compare
adj.rsq.model[1] <- summary(model1)$adj.r.squared
adj.rsq.model[2] <- summary(model2)$adj.r.squared
adj.rsq.model[3] <- summary(model3)$adj.r.squared
adj.rsq.model[4] <- summary(model4)$adj.r.squared
adj.rsq.model[5] <- summary(model5)$adj.r.squared
adj.rsq.model[6] <- summary(model6)$adj.r.squared


# Plotting
ggplot(data = NULL,aes(x = 1:6, y = adj.rsq.model)) +
  geom_line(col = "blue") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Adjusted R Squared vs. Number of variables", x = "Number of variables", y = "Adjusted R Squared")

aic.model <- rep(0,6)

# AIC of the models
aic.model[1] <- AIC(model1)
aic.model[2] <- AIC(model2)
aic.model[3] <- AIC(model3)
aic.model[4] <- AIC(model4)
aic.model[5] <- AIC(model5)
aic.model[6] <- AIC(model6)

ggplot(data = NULL,aes(x = 1:6, y = aic.model)) +
  geom_line(col = "blue") +
  scale_x_continuous(breaks = 1:6) +
  labs(title = "AIC vs. Number of variables", x = "Number of variables", y = "AIC")

data_final <- faa_clean[,-c(1,4)]

data_final <- data_final[complete.cases(data_final),]


# Step function
nullmodel <- lm(distance ~ 1, data_final)
fullmodel <- lm(distance ~ ., data_final)

# Results of step fn - forward selection
stepAIC(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction= "forward")

# Summary of the model
summary(lm(formula = distance ~ speed_air + aircraft_tp + height + no_pasg, 
           data = data_final))$coefficients

