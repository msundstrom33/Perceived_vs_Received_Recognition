library(easypackages)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(coin)
library(performance)
library(ggplot2)
library(stringr)
library(lattice)
library(corrplot)
library(extrafont)
library(stargazer)
library(rstatix)
library(gridExtra)
library(effectsize)
library(ordinal)


# Import data -------------------------------------------------------------

# Load in data file for each context; adjust data types

df_lab <- read.csv("RawDataLab.csv")
df_lab[c("course", "institution","academicyear","academicmajor","gender","race")] <- lapply(df_lab[c("course", "institution","academicyear","academicmajor","gender","race")], factor)
df_lab[c("actualrecognition","perceivedrecognition")] <- lapply(df_lab[c("actualrecognition","perceivedrecognition")], as.numeric)

df_lecture<-read.csv("RawDataLecture.csv")
df_lecture[c("course", "institution","academicyear","academicmajor","gender","race")] <- lapply(df_lecture[c("course", "institution","academicyear","academicmajor","gender","race")], factor)
df_lecture[c("actualrecognition","perceivedrecognition")] <- lapply(df_lecture[c("actualrecognition","perceivedrecognition")], as.numeric)


# Preliminary analysis ----------------------------------------------------

# Student's t-test comparing men's and women's perceived recognition

t.test(perceivedrecognition ~ gender, data = df_lab[df_lab$gender=="Woman"|df_lab$gender=="Man",], var.equal=TRUE)
cohens_d(perceivedrecognition ~ gender, data = df_lab[df_lab$gender=="Woman"|df_lab$gender=="Man",])

t.test(perceivedrecognition ~ gender, data = df_lecture[df_lecture$gender=="Woman"|df_lecture$gender=="Man",], var.equal=TRUE)
cohens_d(perceivedrecognition ~ gender, data = df_lecture[df_lecture$gender=="Woman"|df_lecture$gender=="Man",])


# Mixed effects models ----------------------------------------------------

# Fit null model to determine ICC for course and institution to determine which random intercepts should be included in the model, then fit one linear mixed effects model for each context. Calculate VIFs and run model diagnostics. Also calculate standardized coefficients.

# Lab

# ICC

r2(lmer(perceivedrecognition~(1|institution), data = df_lab))$R2_conditional
r2(lmer(perceivedrecognition~(1|course), data = df_lab))$R2_conditional

# Re-format race/ethnicity data

df_lab <- df_lab %>% mutate(nat.hawaiian=ifelse(str_detect(df_lab$race, 'Hawaiian')==TRUE,"1","0"))
df_lab <- df_lab %>% mutate(am.indian=ifelse(str_detect(df_lab$race, 'American Indian')==TRUE,"1","0"))
df_lab <- df_lab %>% mutate(white=ifelse(str_detect(df_lab$race, 'White')==TRUE,"1","0"))
df_lab <- df_lab %>% mutate(black.af.american=ifelse(str_detect(df_lab$race, 'Black')==TRUE,"1","0"))
df_lab <- df_lab %>% mutate(hispanic=ifelse(str_detect(df_lab$race, 'Hispanic')==TRUE,"1","0"))
df_lab <- df_lab %>% mutate(asian=ifelse(str_detect(df_lab$race, 'Asian')==TRUE,"1","0"))
df_lab <- df_lab %>% mutate(mid.eastern=ifelse(str_detect(df_lab$race, 'Middle Eastern')==TRUE,"1","0"))

# Run models with all students

model.lab <- lmer(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american +  (1|course), data = df_lab)
summary(model.lab)
confint(model.lab)

model.lab.norm <- standardize_parameters(model.lab, ci=0.67)
model.lab.norm$uncertainty <- abs(model.lab.norm$Std_Coefficient-model.lab.norm$CI_low)
norm.coefs<-cbind(model.lab.norm$Parameter,model.lab.norm$Std_Coefficient,model.lab.norm$uncertainty)
norm.coefs

# Find VIFs and save model diagnostic plots for final model

vif(model.lab)

tiff("labresiduals.tiff", units="in", width=5, height=4, res=700)
plot(model.lab, xlab = 'Fitted values', ylab = 'Residuals')
dev.off()

tiff("labqqplot.tiff", units="in", width=5, height=4, res=700)
qqmath(model.lab)
dev.off()

df_lab$model.lab.res<- residuals(model.lab)
df_lab$abs.model.lab.res <-abs(df_lab$model.lab.res)

Levene.model.lab <- lm(model.lab.res ~ course, data=df_lab)
anova(Levene.model.lab)

tiff("labvarplot.tiff", units="in", width=5, height=4, res=700)
boxplot(df_lab$model.lab.res ~ df_lab$course,ylab="",xlab="Course")
dev.off()

# Run four model checks: (1) all predictor variables excluding students receiving 0 nominations, (2) just the three main variables excluding students receiving 0 nominations, (3) all predictor variables reducing (at random) the number of students receiving 0 nominations, (4) just the three main variables reducing (at random) the number of students receiving 0 nominations, and (5) all predictor variables excluding students receiving more than 3 nominations

df.lab.zeros<-sample_n(df_lab[df_lab$actualrecognition==0,],399) #399 students received 1 nomination, so we choose this number
df.lab.check.pre<-df_lab[df_lab$actualrecognition!=0,]
df.lab.check<-rbind(df.lab.check.pre,df.lab.zeros)

model.lab.check.1 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american, data = df.lab.check.pre)
summary(model.lab.check.1)

model.lab.check.2 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender, data = df.lab.check.pre)
summary(model.lab.check.2)

model.lab.check.3 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american, data = df.lab.check)
summary(model.lab.check.3)

model.lab.check.4 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender, data = df.lab.check)
summary(model.lab.check.4)

model.lab.check.5 <- lmer(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american +  (1|course), data = df_lab[df_lab$actualrecognition<4,])
summary(model.lab.check.5)

# Lecture

# ICC

r2(lmer(perceivedrecognition~(1|institution), data = df_lecture))$R2_conditional
r2(lmer(perceivedrecognition~(1|course), data = df_lecture))$R2_conditional

# Re-format race/ethnicity data

df_lecture <- df_lecture %>% mutate(nat.hawaiian=ifelse(str_detect(df_lecture$race, 'Hawaiian')==TRUE,"1","0"))
df_lecture <- df_lecture %>% mutate(am.indian=ifelse(str_detect(df_lecture$race, 'American Indian')==TRUE,"1","0"))
df_lecture <- df_lecture %>% mutate(white=ifelse(str_detect(df_lecture$race, 'White')==TRUE,"1","0"))
df_lecture <- df_lecture %>% mutate(black.af.american=ifelse(str_detect(df_lecture$race, 'Black')==TRUE,"1","0"))
df_lecture <- df_lecture %>% mutate(hispanic=ifelse(str_detect(df_lecture$race, 'Hispanic')==TRUE,"1","0"))
df_lecture <- df_lecture %>% mutate(asian=ifelse(str_detect(df_lecture$race, 'Asian')==TRUE,"1","0"))
df_lecture <- df_lecture %>% mutate(mid.eastern=ifelse(str_detect(df_lecture$race, 'Middle Eastern')==TRUE,"1","0"))

# Run models with all students

model.lec <- lmer(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american +  (1|course), data = df_lecture)
summary(model.lec)
confint(model.lec)

model.lec.norm <- standardize_parameters(model.lec, ci=0.67)
model.lec.norm$uncertainty <- abs(model.lec.norm$Std_Coefficient-model.lec.norm$CI_low)
lec.norm.coefs<-cbind(model.lec.norm$Parameter,model.lec.norm$Std_Coefficient,model.lec.norm$uncertainty)
lec.norm.coefs


# Find VIFs and save model diagnostic plots for final model

vif(model.lec)

tiff("lecresiduals.tiff", units="in", width=5, height=4, res=700)
plot(model.lec, xlab = 'Fitted values', ylab = 'Residuals')
dev.off()

tiff("lecqqplot.tiff", units="in", width=5, height=4, res=700)
qqmath(model.lec)
dev.off()

df_lecture$model.lec.res<- residuals(model.lec)
df_lecture$abs.model.lec.res <-abs(df_lecture$model.lec.res)

Levene.model.lec <- lm(model.lec.res ~ course, data=df_lecture)
anova(Levene.model.lec)

tiff("lecvarplot.tiff", units="in", width=5, height=4, res=700)
boxplot(df_lecture$model.lec.res ~ df_lecture$course,ylab="",xlab="Course")
dev.off()

# Run four model checks: (1) all predictor variables excluding students receiving 0 nominations, (2) just the three main variables excluding students receiving 0 nominations, (3) all predictor variables reducing (at random) the number of students receiving 0 nominations, (4) just the three main variables reducing (at random) the number of students receiving 0 nominations, and (5) all predictor variables excluding students receiving more than 3 nominations

df.lec.zeros<-sample_n(df_lecture[df_lecture$actualrecognition==0,],218) #218 students received 1 nomination, so we choose this number
df.lec.check.pre<-df_lecture[df_lecture$actualrecognition!=0,]
df.lec.check<-rbind(df.lec.check.pre,df.lec.zeros)

model.lec.check.1 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american, data = df.lec.check.pre)
summary(model.lec.check.1)

model.lec.check.2 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender, data = df.lec.check.pre)
summary(model.lec.check.2)

model.lec.check.3 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american, data = df.lec.check)
summary(model.lec.check.3)

model.lec.check.4 <- lm(perceivedrecognition~actualrecognition + gender + actualrecognition*gender, data = df.lec.check)
summary(model.lec.check.4)

model.lec.check.5 <- lmer(perceivedrecognition~actualrecognition + gender + actualrecognition*gender + academicmajor + academicyear + white + asian + hispanic + nat.hawaiian + am.indian +  mid.eastern + black.af.american +  (1|course), data = df_lecture[df_lecture$actualrecognition<4,])
summary(model.lec.check.5)

# Export all model results
class(model.lec) <- "lmerMod"
class(model.lab) <- "lmerMod"

labels = c("Received recognition", "Gender (woman)","Life sciences or biology", "Other major","Other physical sciences", "Physics","Unknown major","Fourth year","Other year","Second year","Third year","Unknown year","White","Asian","Hispanic","Native Hawaiian", "American Indian","Middle eastern", "Black","Gender*Received recognition")

stargazer(model.lab, model.lec, title="Results", align=TRUE, covariate.labels = labels)
          
# Make figures --------------------------------------------------------

# Perceived recognition by gender
df_lab <- df_lab %>% mutate(context="Lab")
df_lecture <- df_lecture %>% mutate(context="Lecture")

df.perceived.means <- rbind(df_lab[,c("perceivedrecognition","gender","context")],df_lecture[,c("perceivedrecognition","gender","context")])

df.perceived.means <- df.perceived.means %>% group_by(context,gender) %>% summarise(mean_perceived=mean(perceivedrecognition),sd_perceived=sd(perceivedrecognition),n=n()) %>% mutate(se_perceived = ifelse(is.na(sd_perceived)==FALSE,sd_perceived/sqrt(n),0)) %>% as.data.frame()

perceivedplot<-ggplot(df.perceived.means[df.perceived.means$gender=="Man"|df.perceived.means$gender=="Woman",], aes(x=context, y=mean_perceived)) +
  geom_point(aes(color=gender),size=5,shape=19,position=position_dodge(width=0.3))+ theme_classic() + ylab("Perceived Peer Recognition\n (Likert Scale Response)")+geom_errorbar(aes(ymin=mean_perceived-se_perceived, ymax=mean_perceived+se_perceived,color=gender),width=.1,size=0.6,position=position_dodge(width=0.3))+
  theme(axis.title = element_text(size=16,color="black",face="bold"),axis.text = element_text(size=16,color="black"))+theme(strip.text.x = element_text(size = 16))+theme(strip.text.y = element_text(size = 16),axis.text.x = element_text(face="bold")) + theme(legend.title = element_blank(),legend.text = element_text(size=16,color="black"),strip.background = element_rect(colour="black", size=1,fill="white"),legend.position=c(0.2,0.9))+ labs(color = "")+theme(legend.position="bottom",legend.spacing.x = unit(0.3, 'cm'))+ theme(strip.text.x = element_text(
    size = 16,face='bold'),
    strip.text.y = element_text(
      size = 16),strip.background = element_rect(
        color="gray20", fill="white", size=1, linetype="solid"),panel.border = element_rect(color = "gray20", fill = NA, size = 1))+scale_y_continuous(breaks=c(0,1,2,3,4,5,6))+
  theme(axis.ticks.x=element_blank(),legend.margin=margin(t=-20))+scale_colour_manual(values=c("#6dc6b1","#2c7fb8"))+xlab("")+ylim(2.5,3.5)+theme(text=element_text(family="DejaVu Sans"))





# Actual recognition by gender
aggregate_props <- data.frame(
  context=c("Lab","Lab","Lecture","Lecture"),
  gender=c("Man","Woman","Man","Woman"),
  prop=c(sum(df_lab$actualrecognition[df_lab$gender=="Man"])/sum(df_lab$actualrecognition),sum(df_lab$actualrecognition[df_lab$gender=="Woman"])/sum(df_lab$actualrecognition),sum(df_lecture$actualrecognition[df_lecture$gender=="Man"])/sum(df_lecture$actualrecognition),sum(df_lecture$actualrecognition[df_lecture$gender=="Woman"])/sum(df_lecture$actualrecognition)),
  conf_low=c(prop.test(sum(df_lab$actualrecognition[df_lab$gender=="Man"]),sum(df_lab$actualrecognition),correct=FALSE)$conf.int[1],prop.test(sum(df_lab$actualrecognition[df_lab$gender=="Woman"]),sum(df_lab$actualrecognition),correct=FALSE)$conf.int[1],prop.test(sum(df_lecture$actualrecognition[df_lecture$gender=="Man"]),sum(df_lecture$actualrecognition),correct=FALSE)$conf.int[1],prop.test(sum(df_lecture$actualrecognition[df_lecture$gender=="Woman"]),sum(df_lecture$actualrecognition),correct=FALSE)$conf.int[1]),
  conf_high=c(prop.test(sum(df_lab$actualrecognition[df_lab$gender=="Man"]),sum(df_lab$actualrecognition),correct=FALSE)$conf.int[2],prop.test(sum(df_lab$actualrecognition[df_lab$gender=="Woman"]),sum(df_lab$actualrecognition),correct=FALSE)$conf.int[2],prop.test(sum(df_lecture$actualrecognition[df_lecture$gender=="Man"]),sum(df_lecture$actualrecognition),correct=FALSE)$conf.int[2],prop.test(sum(df_lecture$actualrecognition[df_lecture$gender=="Woman"]),sum(df_lecture$actualrecognition),correct=FALSE)$conf.int[2]),
  expected=c(length(df_lab$gender[df_lab$gender=="Man"])/length(df_lab$gender),length(df_lab$gender[df_lab$gender=="Woman"])/length(df_lab$gender),length(df_lecture$gender[df_lecture$gender=="Man"])/length(df_lecture$gender),length(df_lecture$gender[df_lecture$gender=="Woman"])/length(df_lecture$gender))
)

actualplot<-ggplot(aggregate_props,aes(x=context, y=prop-expected,fill=gender,ymin=conf_low-expected, ymax=conf_high-expected)) +
  geom_bar(position=position_dodge(width=0.7), stat="identity",width=0.7) +
  geom_abline(intercept =0, slope = 0, color="grey20", size=1) +
  geom_errorbar(width=0.1, alpha=0.9, size=0.6,position=position_dodge(width=0.7),color="black")+
  ylab("Difference Between Observed\n and Expected Proportions of\n Received Peer Recognition") + xlab("")+theme_classic() +theme(axis.title = element_text(size=16,color='black',face="bold"),axis.text = element_text(size=16,color='black'),axis.text.x=element_text(face="bold")) +
  scale_fill_manual(values=c("#6dc6b1","#2c7fb8"))+ theme(legend.position="bottom",legend.background = element_rect(fill="white", size=0.5, linetype="solid",color="white"),legend.margin=margin(t=-20),legend.title = element_blank(),legend.text = element_text(size=16,color='black'))+ylim(-0.15,0.15)+guides(shape =FALSE)+
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+theme(text=element_text(family="DejaVu Sans"))


# Perceived recognition vs. actual recognition by gender
df.main.figure <- rbind(df_lab[,c("actualrecognition","perceivedrecognition","gender","context")],df_lecture[,c("actualrecognition","perceivedrecognition","gender","context")])

plot.main.figure <- df.main.figure %>% group_by(context,actualrecognition,gender) %>% summarise(mean_perceived=mean(perceivedrecognition),sd_perceived=sd(perceivedrecognition),n=n()) %>% mutate(se_perceived = ifelse(is.na(sd_perceived)==FALSE,sd_perceived/sqrt(n),0)) %>% as.data.frame()


plot.main.figure <- plot.main.figure[plot.main.figure$gender=="Man"|plot.main.figure$gender=="Woman",]

lineplot<-ggplot(plot.main.figure[!plot.main.figure$n==1,], aes(x=actualrecognition, y=mean_perceived,group=gender)) +
  geom_line(aes(color=gender),size=0.3,linetype='dashed')+
  geom_point(aes(color=gender),size=4,shape=19)+
  geom_point(data=plot.main.figure[plot.main.figure$n==1,],aes(x=actualrecognition,y=mean_perceived,color=gender),size=3,shape=5)+
  geom_errorbar(aes(ymin=mean_perceived-se_perceived, ymax=mean_perceived+se_perceived,color=gender),width=.1,size=0.4)+
  ylim(1,5) +facet_grid(~context,scales="free")+
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))+
  theme_classic() +
  theme(axis.title = element_text(size=16,color='black',face="bold"),axis.text = element_text(size=16,color='black')) +
  scale_colour_manual(values=c("#6dc6b1","#2c7fb8"))+
  xlab('Received Peer Recognition\n(Number of Nominations)') + ylab('Perceived Peer Recognition\n(Likert Scale Response)') + theme(legend.position="bottom",legend.background = element_rect(fill="white", size=0.5, linetype="solid",color="white"),legend.title = element_blank(),legend.text = element_text(size=16,color='black')) +
  theme(legend.title = element_blank(),legend.text = element_text(size=16,color='black'),strip.background = element_rect(colour='gray20', size=1),strip.text=element_text(size=16,color='black',face="bold")) +
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+
  theme(text=element_text(family="Helvetica"))+
  guides(color = guide_legend(
    override.aes=list(shape = 19)))+theme(text=element_text(family="DejaVu Sans"))


# Mixed effect model coefficients
df<-data.frame(coefficient=c(-0.237,0.074,-0.011,-0.302,0.108,-0.012),
               error=c(0.060,0.033,0.044,0.075,0.031,0.060),
               term=c("Gender", "Received Peer Recognition", "Gender \u00D7 Received Peer Recognition","Gender", "Received Peer Recognition", "Gender \u00D7 Received Peer Recognition"),
               context=c("Lab","Lab","Lab","Lecture","Lecture","Lecture")
)


df$term<-factor(df$term,levels=rev(c("Gender", "Received Peer Recognition", "Gender \u00D7 Received Peer Recognition")))


modelplot<-ggplot(df[df$term=="Gender"|df$term=="Received Peer Recognition"|df$term=="Gender \u00D7 Received Peer Recognition",], aes(x=term, y=coefficient,shape=context)) +geom_abline(intercept =0, slope = 0, color="grey20", size=0.6,linetype="dashed")+xlab('Semester')+ geom_point(position=position_dodge(width=0.8),size=5) + geom_errorbar(aes(ymin=coefficient-error, ymax=coefficient+error), width=.2, position=position_dodge(0.8),size=0.8) + theme_classic() +theme(axis.title = element_text(size=16,color="black",face="bold"),axis.text = element_text(size=16,color="black"))+scale_shape_manual(values = c(15,17))+theme(strip.text.x = element_text(size = 16))+theme(strip.text.y = element_text(size = 16)) + ylab('Coefficient Estimate for Perceived Peer Recognition')+xlab('Predictor Variable')+ theme(legend.title = element_blank(),legend.text = element_text(size=16,color="black"),strip.background = element_rect(colour="black", size=1,fill="white"),legend.position=c(0.2,0.9))+ labs(color = "")+ylim(-0.5,0.25)+theme(legend.position="bottom",legend.spacing.x = unit(0.3, 'cm'))+ theme(strip.text.x = element_text(
  size = 16,face='bold'),
  strip.text.y = element_text(
    size = 16),strip.background = element_rect(
      color="gray20", fill="white", size=1, linetype="solid"),panel.border = element_rect(color = "gray20", fill = NA, size = 1))+coord_flip()+theme(text=element_text(family="DejaVu Sans"))+scale_x_discrete(labels = function(x) str_wrap(x, width = 20))


# Export plots
tiff("2a.tiff", units="in", width=5, height=4, res=700)
perceivedplot
dev.off()

tiff("2b.tiff", units="in", width=5, height=4, res=700)
actualplot
dev.off()

tiff("2c.tiff", units="in", width=10, height=4, res=700)
lineplot
dev.off()

tiff("2d.tiff", units="in", width=10, height=4, res=700)
modelplot
dev.off()

# Plot distributions of actual and perceived recognition split up by course

df.distributions <- rbind(df_lab[,c("actualrecognition","perceivedrecognition","gender","context","course")],df_lecture[,c("actualrecognition","perceivedrecognition","gender","context","course")])

df.distributions$course<-factor(df.distributions$course,levels=c("1A","1B","2A","2B","3A","4A","4B","5A","6A","6B","6C","6D","7A","7B","7C","8A","8B"))

actual.split<-ggplot(df.distributions, aes(x=course, y=actualrecognition)) +xlab('Course')+ geom_boxplot(position=position_dodge(width=0.8),width=0.4,fill="gray80")+ theme_classic()  + ylab("Received Peer Recognition\n (Number of Nominations)")+ylim(0,12)+theme(axis.title = element_text(size=16,color="black",face="bold"),axis.text = element_text(size=16,color="black"))+theme(strip.text.x = element_text(size = 16))+theme(strip.text.y = element_text(size = 16)) + theme(legend.title = element_blank(),legend.text = element_text(size=16,color="black"),strip.background = element_rect(colour="black", size=1,fill="white"),legend.position=c(0.2,0.9))+ labs(color = "")+theme(legend.position="bottom",legend.spacing.x = unit(0.3, 'cm'))+ theme(strip.text.x = element_text(
  size = 16,face='bold'),
  strip.text.y = element_text(
    size = 16),strip.background = element_rect(
      color="gray20", fill="white", size=1, linetype="solid"),panel.border = element_rect(color = "gray20", fill = NA, size = 1))+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))+facet_grid(~context,scales="free")+theme(text=element_text(family="DejaVu Sans"))


tiff("actual_split.tiff", units="in", width=10, height=4, res=700)
actual.split
dev.off()

perceived.split<-ggplot(df.distributions, aes(x=course, y=perceivedrecognition)) +xlab('Course')+ geom_boxplot(position=position_dodge(width=0.8),width=0.4,fill="gray80")+ theme_classic()  + ylab("Perceived Peer Recognition\n (Likert Scale Response)")+ylim(0,12)+theme(axis.title = element_text(size=16,color="black",face="bold"),axis.text = element_text(size=16,color="black"))+theme(strip.text.x = element_text(size = 16))+theme(strip.text.y = element_text(size = 16)) + theme(legend.title = element_blank(),legend.text = element_text(size=16,color="black"),strip.background = element_rect(colour="black", size=1,fill="white"),legend.position=c(0.2,0.9))+ labs(color = "")+theme(legend.position="bottom",legend.spacing.x = unit(0.3, 'cm'))+ theme(strip.text.x = element_text(
  size = 16,face='bold'),
  strip.text.y = element_text(
    size = 16),strip.background = element_rect(
      color="gray20", fill="white", size=1, linetype="solid"),panel.border = element_rect(color = "gray20", fill = NA, size = 1))+scale_y_continuous(breaks=c(0,1,2,3,4,5,6))+facet_grid(~context,scales="free")+theme(text=element_text(family="DejaVu Sans"))

tiff("perceived_split.tiff", units="in", width=10, height=4, res=700)
perceived.split
dev.off()


# Plot aggregate distributions of actual and perceived recognition

actualplot_dist<-ggplot(df.main.figure[df.main.figure$gender=="Man"|df.main.figure$gender=="Woman",], aes(x=actualrecognition, fill=gender)) +
  geom_histogram(bins=13,aes(y=after_stat(density)),
                 alpha=1,position='identity',binwidth=1) +
  theme_classic() +
  theme(axis.title = element_text(size=16,color='black',face="bold"),axis.text = element_text(size=16,color='black')) +
  scale_fill_manual(values=c("#6dc6b1","#2c7fb8"))+
  ylab('Proportion of Students') + xlab('Received Peer Recognition\n (Number of Nominations)') + theme(legend.position="none",legend.background = element_rect(fill="white", size=0.5, linetype="solid",color="white"),legend.title = element_blank(),legend.text = element_text(size=16,color='black')) +
  facet_grid(gender~context)+ theme(legend.title = element_blank(),legend.text = element_text(size=16,color='black'),strip.background = element_rect(colour='gray20', size=1),strip.text=element_text(size=16,color='black',face="bold")) +
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+
  theme(text=element_text(family="DejaVu Sans"))

tiff("aggregate_actualrecognition.tiff", units="in", width=5, height=4, res=700)
actualplot_dist
dev.off()

perceivedplot_dist<-ggplot(df.main.figure[df.main.figure$gender=="Man"|df.main.figure$gender=="Woman",], aes(x=perceivedrecognition, fill=gender)) +
  geom_histogram(bins=13,aes(y=after_stat(density)),
                 alpha=1,position='identity',binwidth=1) +
  theme_classic() +
  theme(axis.title = element_text(size=16,color='black',face="bold"),axis.text = element_text(size=16,color='black')) +
  scale_fill_manual(values=c("#6dc6b1","#2c7fb8"))+
  ylab('Proportion of Students') + xlab('Perceived Peer Recognition\n (Likert Scale Response)') + theme(legend.position="none",legend.background = element_rect(fill="white", size=0.5, linetype="solid",color="white"),legend.title = element_blank(),legend.text = element_text(size=16,color='black')) +
  facet_grid(gender~context)+ theme(legend.title = element_blank(),legend.text = element_text(size=16,color='black'),strip.background = element_rect(colour='gray20', size=1),strip.text=element_text(size=16,color='black',face="bold")) +
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+theme(text=element_text(family="DejaVu Sans"))

tiff("aggregate_perceivedrecognition.tiff", units="in", width=5, height=4, res=700)
perceivedplot_dist
dev.off()


# Demographic breakdowns --------------------------------------------------

table(df_lecture$gender)
table(df_lecture$academicyear)
table(df_lecture$academicmajor)
table(df_lecture$white)
table(df_lecture$asian)
table(df_lecture$black.af.american)
table(df_lecture$nat.hawaiian)
table(df_lecture$hispanic)
table(df_lecture$mid.eastern)
table(df_lecture$am.indian)
table(df_lecture$race)

table(df_lab$gender)
table(df_lab$academicyear)
table(df_lab$academicmajor)
table(df_lab$white)
table(df_lab$asian)
table(df_lab$black.af.american)
table(df_lab$nat.hawaiian)
table(df_lab$hispanic)
table(df_lab$mid.eastern)
table(df_lab$am.indian)
table(df_lab$race)


# Ordinal logistic regressions --------------------------------------------

#Lab

df_lab$perceivedrecognition <- factor(df_lab$perceivedrecognition, levels = c("1","2","3","4","5"),ordered=TRUE)

ordinal.lab = clmm(perceivedrecognition~ actualrecognition + gender + actualrecognition*gender + (1|course), data = df_lab)

summary(ordinal.lab)


#Lecture

df_lecture$perceivedrecognition <- factor(df_lecture$perceivedrecognition, levels = c("1","2","3","4","5"),ordered=TRUE)

ordinal.lec = clmm(perceivedrecognition~ actualrecognition + gender + actualrecognition*gender +(1|course), data = df_lecture)

summary(ordinal.lec)

