setwd('/Users/vincentparis/Counts')
danish <- read.csv('danish.csv', header = T)
names(danish) <- c('alc', 'income', 'mar', 'urban', 'count')
head(danish)
danish$alc <- as.factor(danish$alc)
danish$income <- as.factor(danish$income)
danish$mar <- as.factor(danish$mar)
danish$urban <- as.factor(danish$urban)
length(danish$count)

options(contrasts=c("contr.treatment", "contr.poly"))

ind_mod <- glm(count ~ alc+income+urban+mar, family=poisson,
              data = danish, maxit=20, epsilon=.000001, x=T, trace=T)

plot(danish$count - ind_mod$fitted.values, col = danish$mar, main = 'Residuals for a Simple Main Effects Model')
goftests(danish$count, ind_mod$fitted.values, danish$count, ind_mod$df.residual)
#hardcore fail

















bot <- stepAIC(ind_mod, scope = list(lower = formula(ind_mod), upper =~ .^4))

nearex_mod <- glm(count ~ alc*income*urban*mar - alc:income:urban:mar, family=poisson,
              data = danish, maxit=20, epsilon=.000001, x=T, trace=T)


library(MASS)
final_mod <- stepAIC(nearex_mod, scope = list(upper = nearex_mod, lower = ~1))
final_mod[[length(fin)]]

final_mod <- glm(count ~ alc + income + urban + mar + alc:income + alc:urban + 
    income:urban + alc:mar + income:mar + urban:mar + alc:income:mar, family = poisson,data = danish, maxit=20, epsilon=.000001, x=T, trace=T)




#so stepAIC produces the same model going up hill as going down hill. 
#This suggests then our model is the strongest by the AIC measure.
plot(danish$count - final_mod$fitted.values, col = danish$mar, main = 'Residuals for a Final Model')
goftests(danish$count, final_mod$fitted.values, danish$count, final_mod$df.residual)







#Our model is still not good. We have strong associations acorss the board all in all
#Only things missing short of the full model is alc:income:urban, alc:mar:urban, income:mar:ubran.
plot(danish$count - nearex_mod$fitted.values, col = danish$mar, main = 'Residuals for a Near Exhuastion Model')
goftests(danish$count, nearex_mod$fitted.values, danish$count, nearex_mod$df.residual)




#Overdispersion Checking
neamod <- glm(count ~ alc*income*urban*mar - alc:income:urban:mar, family=quasipoisson,
              data = danish, maxit=20, epsilon=.000001, x=T, trace=T)

fine <- stepAIC(nearex_mod, scope = list(upper = nearex_mod, lower = ~1))
fine <- glm(formula = count ~ alc + income + urban + mar + alc:income + 
    alc:urban + income:urban + alc:mar + income:mar + urban:mar + 
    alc:income:mar, family = poisson, data = danish, x = T, maxit = 20, 
    epsilon = 1e-06, trace = T, Hessian = T)
anova(fine, test = 'Chisq')
vc <- vcov(fine)

para <- fine$coefficients
se <- summary(fine)[[12]][,2]


con <- confint(fine)
con.int <- exp(con)
holding <- data.frame(para, se, con.int)
names(holding) <- c("Parameter", 'Std. Error', "Lower CI", "Upper CI")


ggplot(danish, aes(urban, count)) + geom_point(aes(col = mar)) + facet_grid(~income) + ggtitle('Income')
ggplot(danish, aes(mar, count)) + geom_point(aes(col = alc)) + facet_grid(~income) + ggtitle('Income')




#log_it_1vs2 
data.frame(1:68, holding)
al2 <- c(2, 13,15,17,19,21, 23,25,39, 41, 57,59,61,63,65,67)
al3 <- al2+1 
log_it_1vs2 <- holding[al2,]
k <- rownames(log_it_1vs2)
rownames(log_it_1vs2) <- unlist(strsplit(k, 'alc2.'))[seq(1,31,2)]
rownames(log_it_1vs2)[1] <- 'int'
log_it_1vs2


log_it_1vs3 <- holding[al3,]
k <- rownames(log_it_1vs3)
rownames(log_it_1vs3) <- unlist(strsplit(k, 'alc3.'))[seq(1,31,2)]
log_it_1vs3 

























  