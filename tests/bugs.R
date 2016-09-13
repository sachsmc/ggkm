if(FALSE){

library(survival)
library(ggkm)

  se = TRUE; trans = "identity"; firstx = 0; firsty = 1;
  type = "kaplan-meier"; error = "tsiatis"; conf.type = "log";
  conf.lower = "usual"; start.time = 0; conf.int = 0.95

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


ggplot(lung, aes(y = time, x = age, color = as.factor(ph.ecog))) + geom_point()

ggplot(lung, aes(time = time, status = ifelse(
  status == 2,1,0),col=as.factor(ph.ecog ),fill=as.factor(ph.ecog ))) +
  geom_km(fill="transparent")+ stat_kmband(geom="ribbon")+ # this to make the legend include the fill
  geom_kmticks()

sf3 <- survfit(Surv(time, ifelse(status == 2, 1, 0)) ~ as.factor(ph.ecog), data = lung)

df <- read.csv("~/../Downloads/df.csv")
p1<-  ggplot(df, aes(time=time, status = status, color = factor(sex)))
p1 +stat_km() + geom_kmband()
p1 + stat_km(trans = "cumhaz") # works no error
p1 + stat_km(trans = "cumhaz")+ scale_x_log10()# works
p1 + stat_km(trans = "cumhaz")+ geom_kmband(trans = "cumhaz") + scale_y_log10()


s1 <- dostep(x = c(1, 2, 3, 4, 5, 6 ,7, 8, 9, 10), y = rev(c(1, .8, .8, .6, .6, .5, .5, .4, .3, .2)))
s2 <- dostep(x = c(1:12), y = rev(c(1, 1, .9, .8, .8, .7, .7, .6, .6, .6, .6, .5)))

plot(s2, ylim = c(0, 1), type = 'b')
lines(s1, type = 'b')








}