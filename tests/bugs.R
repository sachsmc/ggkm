if(FALSE){

library(survival)
library(ggkm)

lung$SurvObj <- with(lung, Surv(time, status == 2))

ggplot(subset(lung, ph.ecog == 3), aes(time = time, status = ifelse(status == 2,1,0))) +
  geom_km(fill="transparent")

ggplot(lung[sample(1:nrow(lung), 25), ],
       aes(time = time, status = ifelse(status == 2,1,0))) +
  geom_km()

ggplot(lung, aes(time = time, status = ifelse(
  status == 2,1,0),color=as.factor(ph.ecog))) +
  geom_km() + geom_kmticks()


ggplot(lung, aes(time = time, status = status,
                 color=as.factor(ph.ecog))) +
  geom_km(se = TRUE)


+ stat_km(geom="ribbon",alpha=0.1,color="black")


sf3 <- survfit(Surv(time, ifelse(status == 2, 1, 0)) ~ as.factor(ph.ecog), data = lung)
}