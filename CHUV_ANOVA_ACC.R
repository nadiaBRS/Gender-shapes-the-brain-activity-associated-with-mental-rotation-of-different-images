library(rio)
library(plyr)
library(car)
library(dbplyr)
library(dplyr)
library(dtplyr)
library(effects)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(grid)
library(lattice)
library(magrittr)
library(multcomp)
library(PairedData)
library(plyr)
library(rio)
library(rstatix)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lme4)
library(car)
library(MuMIn)
library(multimode)
library(effsize)

##DOING ANOVA ON SCORES
 
getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/CHUV_datasets")
data_acc <- read.delim('CHUV_behavioral_filtered3.txt', header = T)
summary(data_acc)

data_acc <- data_acc %>% 
  rename("cube" = "cubes_acc",
         "body" = "bodies_acc",
         "object" = "objects_acc")

data_acc_men <- subset(data_acc, Gender == 'M')
data_acc_men %>% get_summary_stats(cube, type = "mean_sd")
data_acc_men %>% get_summary_stats(body, type = "mean_sd")
data_acc_men %>% get_summary_stats(object, type = "mean_sd")

data_acc_women <- subset(data_acc, Gender == 'F')
data_women %>% get_summary_stats(cube, type = "mean_sd")
data_women %>% get_summary_stats(body, type = "mean_sd")
data_women %>% get_summary_stats(object, type = "mean_sd")

summary(data_acc_men)
sd(data_acc_men$Age)
summary(data_acc_women)
sd(data_acc_women$Age)

data_acc <- data_acc%>% gather(key = "condition", value = "score" , "cube", "body", "object")
data_acc$score<-as.numeric(data_acc$score)
data_acc$Gender <- as.factor(data_acc$Gender)
data_acc %>% group_by(condition)
data_acc %>% get_summary_stats(score, type = "mean_sd")
data_acc$condition <- factor(data_acc$condition)
mod <- lm(score ~ Gender + condition + Gender : condition, data_acc)
anova(mod)
Anova(mod)

library(lsr)

etaSquared(mod)

# there is an effect of gender and condition, but no interaction

library(nlme)
MOD<-lme(score~condition*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_acc)
anova(MOD, test = "F")

MOD2 <- lmer(score ~ condition + Gender + condition : Gender + (1 | Sub), data_acc)
Anova(MOD2)


# there is an effect of Gender, and condition. Let's plot it.

a <- ggplot(data_acc,aes(condition, score, fill=Gender))+
  geom_boxplot() + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                      panel.grid.minor = element_blank())+
  ggtitle("Acc according to Condition and Gender")

a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

b <- ggplot(data_acc,aes(x = factor(condition, levels =c('cube', 'body', 'object')), score, fill=condition))+
  geom_boxplot(outliers= FALSE) + theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                      panel.grid.minor = element_blank())+
  ggtitle("Acc according to Condition")
b 

c <- b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

c + scale_fill_manual(values=c("#EC7063", "#42A5F5", "#4CAF50")) + ylim(0.56,1)


ggplot(data_acc,aes(condition, score, fill=condition))+
  geom_boxplot() + theme_bw() +  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                       panel.grid.minor = element_blank())


ggplot(data_acc,aes(condition, score, color=Version))+
  geom_boxplot() + theme_bw() +
  ggtitle("Scores according to Condition")

# Let's first investigate the effect of condition

g1 <- ggplot(data_acc,aes(condition, score))+
  geom_boxplot() + theme(panel.border = element_blank(),panel.background = element_blank())

g1

# Let's see what's significant

datacubes<- subset(data_acc,condition=="cube")
#View(datacubes)
databodies<- subset(data_acc,condition=="body")
#View(databodies)
dataobjects<- subset(data_acc,condition=="object")
#View(dataobjects)

t.test(datacubes$score,dataobjects$score)  # significant
cohen.d(datacubes$score,dataobjects$score)

t.test(datacubes$score,databodies$score)    # significant
cohen.d(datacubes$score,databodies$score)

t.test(dataobjects$score,databodies$score)   #  non significant
str(data)

# let's now investigate the effect of gender


g2 <- ggplot(data_acc,aes(condition, score, color=Gender))+
  geom_boxplot() + theme(panel.border = element_blank(),panel.background = element_blank())

g2


datacubesMen <- subset(datacubes, Gender =="M")
datacubesWomen <- subset(datacubes, Gender =="F")

dataobjectsMen <- subset(dataobjects, Gender =="M")
dataobjectsWomen <- subset(dataobjects, Gender =="F")

databodieslMen <- subset(databodies, Gender =="M")
databodiesWomen <- subset(databodies, Gender =="F")

library(emmeans)

# here if we do planned comparison the effect remains. If we continue and do the t.tests as well. 

emmeans(mod, list(pairwise ~ condition|Gender), adjust="tukey")
emmeans(mod, list(pairwise ~ Gender|condition), adjust="tukey")

t.test(datacubesMen$score,datacubesWomen$score)
t.test(dataobjectsMen$score,dataobjectsWomen$score)
cohen.d(dataobjectsMen$score,dataobjectsWomen$score)

t.test(databodieslMen$score,databodiesWomen$score)

# there is a gender difference in the objects condition

#Doing ANOVA with other factors, SRUDY 4 *******

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/CHUV_datasets")
data_acc <- read.delim('CHUV_study4.txt', header = T)


data_acc <- data_acc%>% gather(key = "condition", value = "score" , "cube", "body", "object")
data_acc$score<-as.numeric(data_acc$score)
data_acc$Gender <- as.factor(data_acc$Gender)
data_acc$OSIQV_style <- as.factor(data_acc$OSIQV_style)
data_acc %>% group_by(condition)
data_acc %>% get_summary_stats(score, type = "mean_sd")
data_acc$condition <- factor(data_acc$condition)

mod0 <- lm(score ~ Gender + condition + OSIQV_style + CFS_score + ST_score + ST_rt + Gender : OSIQV_style + Gender : ST_score + Gender : ST_rt, data_acc)
anova(mod0)

mod1 <- lm(score ~ Gender + condition + Spatial.scale + Object_scale + Gender : Spatial.scale, data_acc)
anova(mod1)

mod1.1 <- lm(score ~ Gender + condition + Spatial.scale + ST_score + ST_rt + CFS_score, data_acc)
anova(mod1.1)

## we add interactions 

mod1.2 <- lm(score ~ Gender + condition + Spatial.scale + ST_score + ST_rt + CFS_score + Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt, data_acc)
anova(mod1.2)

mod1.2 <- lm(score ~ Gender + condition + Spatial.scale + ST_score + ST_rt + CFS_score + Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt, data_acc)
anova(mod1.2)

mod1.3 <- lm(score ~ Gender + condition + Spatial.scale + ST_score+ Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt, data_acc)
anova(mod1.3)

mod1.4  <- lm(score ~ Gender + condition + Spatial.scale + ST_score + ST_rt + Object_scale + Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt + Object_scale:Gender, data_acc)
anova(mod1.4)

mod1.4  <- lm(score ~ Gender + condition + Spatial.scale + ST_score + ST_rt + Object_scale + Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt + Object_scale:Gender, data_acc)
anova(mod1.4)

mod1.4  <- lm(score ~ Gender + condition + Spatial.scale + ST_score + ST_rt + Object_scale + Gender : Spatial.scale + Gender : ST_score + Gender : ST_rt + Object_scale:Gender, data_acc)
anova(mod1.4)

ggplot(data=data_acc, aes(x=Spatial.scale, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_acc, aes(x=Spatial.scale, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data=data_acc, aes(x=Object_scale, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_acc, aes(x=Object_scale, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


## ST TEST 

ggplot(data=data_acc, aes(x=ST_score, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_acc, aes(x=ST_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data=data_acc, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_acc, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

MOD1<-lme(score~Spatial.scale*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_acc)
Anova(MOD1, test = "F")
anova(MOD1, test = "F")

# effect computes the fit for certains levels.
effect <- Effect(c('Spatial.scale'), MOD1, se=T, typical=mean)
as.data.frame(effect) -> effect


ggplot(effect, aes(Spatial.scale, fit)) +
  geom_point(
             size=7, shape= 18,position=position_dodge(.4)) +
  geom_errorbar(aes(ymin=lower,ymax=upper),
                size = 1, width=.1,position=position_dodge(.4))+
  theme_bw()+   theme(text = element_text(size = 15))+
  xlab("")+
  ylab("")

ggplot(data=data_acc, aes(x=Spatial.scale, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

mod2 <- lm(score ~ Gender + condition + Spatial.scale + Gender : Spatial.scale, data_acc)
anova(mod2)


mod3 <- lm(score ~ Gender + condition + OSIQV_style + Gender : OSIQV_style, data_acc)
Anova(mod3)

ggplot(data=data_acc, aes(x=Spatial.scale, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

ggplot(data,aes(OSIQV_style, score))+
  geom_boxplot() + theme_bw() +
  ggtitle("Accuracy according to OSIQV Style")

ggplot(data_acc,aes(OSIQV_style, score, color = Gender))+
  geom_boxplot() + theme_bw() +
  ggtitle("Accuracy according to OSIQV Style")


data_acc2 <- data_acc[data_acc$Sub != 18, ]

mod4 <- lm(score ~ Gender + condition + CFS_score + Gender : CFS_score, data_acc2)
anova(mod4)

ggplot(data=data_acc2, aes(x=CFS_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)



mod5 <- lm(score ~ Gender + ST_score + ST_rt + Gender : ST_score,data_acc)
anova(mod5)

ggplot(data=data_acc, aes(x=ST_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

cor(data$ST_score, data_acc$score)

mod6 <- lm(score ~ ST_rt + Gender + Gender : ST_rt,data_acc)
anova(mod6)


ggplot(data=data_acc, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_acc, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)


### Same analysis, separating for each condition. Let's subset. 

data_acc <- data_acc%>% gather(key = "condition", value = "score" , "cube", "body", "object")
data_cubes <- subset(data_acc, condition == 'cube')
data_bodies <- subset(data_acc, condition == 'body')
data_objects <- subset(data_acc, condition == 'object')


### Investigation cubes condition with spatial scale in both men and women ###

cubes_mod<-lme(score~Spatial.scale*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod, test = "F")
anova(cubes_mod, test = "F")

ggplot(data=data_cubes, aes(x=Spatial.scale, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=Spatial.scale, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

cubes_mod2<-lme(score~ST_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod2, test = "F")
anova(cubes_mod2, test = "F")

ggplot(data=data_cubes, aes(x=ST_score, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=ST_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#removing the outlier 
data_cubes2 <- data_cubes[data_cubes$Sub != 24, ]
cubes_mod2.1<-lme(score~ST_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes2)
Anova(cubes_mod2.1, test = "F")
anova(cubes_mod2.1, test = "F")

ggplot(data=data_cubes2, aes(x=ST_score, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes2, aes(x=ST_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

## women with higher (= lower scores) to the perspective taking task are worse at MR for cubes

cubes_mod3<-lme(score~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod3, test = "F")
anova(cubes_mod3, test = "F")

ggplot(data=data_cubes, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

#removing the outlier 
data_cubes3 <- data_cubes[data_cubes$Sub != 34, ]
cubes_mod3.1<-lme(score~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes3)
Anova(cubes_mod3.1, test = "F")
anova(cubes_mod3.1, test = "F")

ggplot(data=data_cubes3, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  ggtitle("Cubes accuracy according to ST_rt")

ggplot(data=data_cubes3, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  ggtitle("Cubes accuracy according to ST_rt")

datacubesMen2 <- subset(data_cubes3, Gender =="M")
datacubesWomen2 <- subset(data_cubes3, Gender =="F")

cubes_mod3.2<-lme(score~ST_rt,random=~1|Sub,method="ML",na.action = na.omit, data = datacubesMen2)
Anova(cubes_mod3.2, test = "F")

### Let's see if there is a link with the cognitive flexibility score ###

cubes_mod3<-lme(score~CFS_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_cubes)
Anova(cubes_mod3, test = "F")
anova(cubes_mod3, test = "F")

ggplot(data=data_cubes, aes(x=CFS_score, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_cubes, aes(x=CFS_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

### no effect ###


### Investigation bodies condition with spatial scale in both men and women ###

bodies_mod<-lme(score~Spatial.scale*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies)
Anova(bodies_mod, test = "F")
anova(bodies_mod, test = "F")

ggplot(data=data_bodies, aes(x=Spatial.scale, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies, aes(x=Spatial.scale, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

bodies_mod2<-lme(score~ST_score*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies)
Anova(bodies_mod2, test = "F")
anova(bodies_mod2, test = "F")

ggplot(data=data_bodies, aes(x=ST_score, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies, aes(x=ST_score, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

bodies_mod3<-lme(score~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies)
Anova(bodies_mod3, test = "F")
anova(bodies_mod3, test = "F")

ggplot(data=data_bodies, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

##remonving the outlier ##
data_bodies2 <- data_bodies[data_bodies$Sub != 34, ]
bodies_mod4<-lme(score~ST_rt*Gender,random=~1|Sub,method="ML",na.action = na.omit, data = data_bodies2)
Anova(bodies_mod4, test = "F")
anova(bodies_mod4, test = "F")

ggplot(data=data_bodies2, aes(x=ST_rt, y=score)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
ggplot(data=data_bodies2, aes(x=ST_rt, y=score, color=Gender)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x, se = FALSE)

databodiesMen2 <- subset(data_bodies2, Gender =="M")
databodiesWomen2 <- subset(data_bodies2, Gender =="F")

bodies_mod3.2<-lme(score~ST_rt,random=~1|Sub,method="ML",na.action = na.omit, data = databodiesMen2)
Anova(bodies_mod3.2, test = "F")

bodies_mod3.3<-lme(score~ST_rt,random=~1|Sub,method="ML",na.action = na.omit, data = databodiesWomen2)
Anova(bodies_mod3.3, test = "F")

