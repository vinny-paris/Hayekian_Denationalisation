data <- read.csv('holding.csv')
head(data)
dim(data)
admissions <- data[,c(27:36)]
head(admissions)
admissions <- apply(admissions, 1, sum)
data <- cbind(data, admissions)
data$race <- as.factor(data$race)
data$disease <- as.factor(data$disease)
data$insur <- as.factor(data$insur)
data$cancer <- as.factor(data$cancer)






ind_mod <- glm(y ~ ptid + age + sex + race +edu +insur + disease +dnr +cancer +
                 aps + scoma + weight + temp + rrate + hrt + pafi +paco2 +pH +
                 wbc + hemat + sod +pot +crea + bili +alb + card  + gastr  + seps + trauma +admissions, family = binomial, data = data)

summary(ind_mod)

restricted_mod <- glm(y ~ edu +insur + disease +dnr +cancer +
                 aps + weight  + rrate + hrt + pafi +paco2 +pH +
                 hemat + sod +pot  + card  + gastr  + seps + trauma +admissions, family = binomial, data = data)

summary(restricted_mod)

#Run at own risk
#bot <- stepAIC(restricted_mod, scope = list(lower = formula(restricted_mod), upper =~ .^2))

final <- glm(formula = y ~ edu + insur + disease + dnr + cancer + aps + 
    weight + rrate + hrt + pafi + paco2 + pH + hemat + sod + 
    pot + card + gastr + seps + trauma + admissions + cancer:pH + 
    disease:aps + disease:pafi + disease:pot + hemat:gastr + 
    disease:gastr + insur:pH + card:gastr + disease:trauma + 
    pafi:trauma + dnr:rrate + edu:hemat + pafi:paco2 + rrate:pot + 
    aps:rrate + rrate:hrt + sod:gastr + disease:seps + disease:hemat + 
    aps:hemat + hemat:trauma + pot:gastr + dnr:seps + paco2:hemat + 
    disease:sod + aps:card + pH:hemat + paco2:admissions + cancer:pot + 
    pot:admissions + dnr:weight + dnr:paco2 + hrt:gastr + weight:paco2 + 
    paco2:trauma + rrate:trauma + weight:sod + insur:weight + 
    aps:seps, family = binomial, data = data)


plot(final$residuals)
shapiro.test(final$residuals)
final$aic


samp <- sample(1:3881, 200)
dit <- data[samp,]
dit$final.values <- final$fitted.values[samp]
holding <- order(dit$final.values)
plot(1:200, dit$y[holding], ylab = '')
par(new = T)
plot(1:200, sort(dit$final.values), ylab = "")
plot(dit$y - dit$final.values, main = 'Residual Plot for the Samples Section')




table(data$y, data$disease)
table(data$y, data$cancer)
table(data$y, data$dnr)


ggplot(subset(data, weight>0), aes(weight, cancer)) + geom_jitter(aes(col = as.factor(y)))

ggplot(subset(data, rrate>0 & hrt > 0), aes(rrate, hrt)) + geom_jitter(aes(col = as.factor(y)))


ggplot(data, aes(y, index)) + geom_jitter(aes(col = as.factor(admissions))) + facet_grid(~admissions)


ggplot(subset(data, weight>0), aes(weight, temp)) + geom_jitter(aes(col = as.factor(y))) + facet_grid(~as.factor(sex)) + ggtitle('Sex')

dim(data)
samp <- sample(1:3881, 100)
dit <- data[samp,]
dit$final.values <- final$fitted.values[samp]
