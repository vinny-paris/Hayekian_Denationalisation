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

bot <- stepAIC(restricted_mod, scope = list(lower = formula(restricted_mod), upper =~ .^2))

y ~ edu + insur + disease + dnr + cancer + aps + weight + rrate + 
    hrt + pafi + paco2 + pH + hemat + sod + pot + card + gastr + 
    seps + trauma + admissions + cancer:pH + disease:aps + disease:pafi + 
    disease:pot + hemat:gastr + disease:gastr + insur:pH + sod:card + 
    card:gastr + disease:trauma + pafi:trauma + dnr:rrate + edu:hemat + 
    pafi:paco2 + rrate:pot + aps:rrate


 ~ edu + insur + disease + dnr + cancer + aps + weight + rrate + 
    hrt + pafi + paco2 + pH + hemat + sod + pot + card + gastr + 
    seps + trauma + admissions + cancer:pH + disease:aps + disease:pafi + 
    disease:pot + hemat:gastr + disease:gastr + insur:pH + sod:card + 
    card:gastr + disease:trauma + pafi:trauma + dnr:rrate + edu:hemat + 
    pafi:paco2 + rrate:pot + aps:rrate + rrate:hrt + sod:gastr + 
    disease:seps + disease:hemat + aps:hemat
