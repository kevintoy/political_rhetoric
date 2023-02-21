raw_data<-read.csv("~/political_arguments/new_data/jian/yuqi_data/raw_data.csv")
#remove the last "sum" row
raw_data <- raw_data[-764, ] 
library(dplyr)
table(raw_data$persuadee.persuader.relationship)


#first construct various "total" variables
raw_data$historical_examples_total<-raw_data$HistoricalExamplesNegative+raw_data$HistoricalExamplesPositive
raw_data$historical_examples_outcome_total<-raw_data$HistoricalExamplesOutcomePositive+raw_data$HistoricalExamplesOutcomeNegative
raw_data$sign_total<-raw_data$SupernaturalNegative+raw_data$SupernaturalPositive
raw_data$motivation_total<-raw_data$MotivationNegative+raw_data$MotivationPositive

raw_data$historical_examples_all<-raw_data$historical_examples_outcome_total+raw_data$historical_examples_total

#barplot of the counts of different topics/themes of persuasion
library(ggplot2)
content_counts_data<-read.csv("~/political_arguments/new_data/jian/yuqi_data/content_counts.csv",fileEncoding="UTF-8-BOM")
ggplot(content_counts_data, aes(x=reorder(topic,count), y=count)) + geom_bar(stat='identity')+coord_flip()+
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 0, hjust = 0.5))+
  ylab("count")+
  xlab("topic/theme")+
  ylim(0,250)

#barplot of the counts of different strategies (total)

strategy_counts_data<-read.csv("~/political_arguments/new_data/jian/yuqi_data/strategy_counts_combined.csv",fileEncoding="UTF-8-BOM")
ggplot(strategy_counts_data, aes(x=reorder(strategy_combined,count_combined), y=count_combined)) + geom_bar(stat='identity')+coord_flip()+
  #theme(text = element_text(size=20),axis.text.x = element_text(angle = 0, hjust = 0.5),axis.text.y=element_blank(),axis.title.y=element_blank())+
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 0, hjust = 0.5),axis.title.y=element_blank())+
  ylab("count")+
  xlab("strategy (substantive)")+
  ylim(0,800)

#barplot of the counts of different tones (total)
tone_counts_data<-read.csv("~/political_arguments/new_data/jian/yuqi_data/tone_counts.csv",fileEncoding="UTF-8-BOM")
ggplot(tone_counts_data, aes(x=reorder(tone,count), y=count)) + geom_bar(stat='identity')+coord_flip()+
  #theme(text = element_text(size=20),axis.text.x = element_text(angle = 0, hjust = 0.5),axis.text.y=element_blank(),axis.title.y=element_blank())+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 0, hjust = 0.5),axis.title.y=element_blank())+
  ylab("count")+
  xlab("strategy (stylistic)")+
  ylim(0,800)

#barplot of advice taken
reordered <- within(raw_data, 
                    advice.taken <- factor(advice.taken, 
                                                levels=names(sort(table(advice.taken), 
                                                                  decreasing=TRUE))))
ggplot(reordered, aes(advice.taken)) + geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_y_continuous(labels=scales::percent)+
  xlab("persuasion outcome")+ylab("Percentage")+
  scale_x_discrete(labels=c("0" = "failure", "1" = "success",
                            "NA" = "unspecified"))

#barplot of advice correct
reordered <- within(raw_data, 
                    advice.correct <- factor(advice.correct, 
                                           levels=names(sort(table(advice.correct), 
                                                             decreasing=TRUE))))
ggplot(reordered, aes(advice.correct)) + geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_y_continuous(labels=scales::percent)+
  xlab("advice good/bad")+ylab("Percentage")+
  scale_x_discrete(labels=c("0" = "bad", "1" = "good",
                            "NA" = "unknown"))

#barplot of persuader outcome
reordered <- within(raw_data, 
                    persuader.outcome <- factor(persuader.outcome, 
                                             levels=names(sort(table(persuader.outcome), 
                                                               decreasing=TRUE))))
ggplot(reordered, aes(persuader.outcome)) + geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels=scales::percent)+
  xlab("persuader outcome")+ylab("Percentage")+
  scale_x_discrete(labels=c("unclear" = "NA"))

#barplot of persuadee-persuader relationship outcome

reordered <- within(raw_data, 
                    persuadee.persuader.relationship <- factor(persuadee.persuader.relationship, 
                                                levels=names(sort(table(persuadee.persuader.relationship), 
                                                                  decreasing=TRUE))))
ggplot(reordered, aes(persuadee.persuader.relationship)) + geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme(text = element_text(size=25),axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels=scales::percent)+
  xlab("persuadee-persuader relationship")+ylab("Percentage")

#success rate by dynasties
new_lab<-c("Pre-Han (?-202 BCE)","Western Han (202 BCE- 8 CE)","Eastern Han (25-220 CE)", "Three Kingdoms (220-280 CE)",	
           "Jin (266-420 CE)", "Southern and Northern Dynasties (420-589 CE)","Sui (581-619 CE)","Tang (618-907 CE)",	
           "Five Dynasties (907-960 CE)",	"Liao (907-1125 CE)",	"Song (960-1279 CE)",
           "Jin (1115-1234 CE)",	"Yuan (1271-1368 CE)",	"Ming (1368-1644 CE)")

ggplot(raw_data, aes(x = reorder(time_period,book_time), y = advice.taken)) + 
  geom_bar(stat = "summary", fun = "mean")+
  scale_x_discrete(labels= new_lab)+
  theme(text = element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 0.95))+
  ylab("persuasion success rate")+
  xlab("historical periods (chronological order)")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))



#calculate mean (persuasion success) by group (topic)
mean(raw_data$advice.taken[which(!is.na(raw_data$ritual))],na.rm=TRUE)
#repeat the above line to get the means of all topics, save to a csv file

persuasion_success_by_group<-read.csv("~/political_arguments/new_data/jian/yuqi_data/persuasion_success_by_topic.csv",fileEncoding="UTF-8-BOM")
ggplot(persuasion_success_by_group, aes(x=reorder(topic,-success.rate), y=success.rate)) + geom_bar(stat='identity')+
  #theme(text = element_text(size=20),axis.text.x = element_text(angle = 0, hjust = 0.5),axis.text.y=element_blank(),axis.title.y=element_blank())+
  theme(text = element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("persuasion success rate")+
  xlab("topic/theme")+
  scale_y_continuous(labels = scales::percent,limits=c(0,1))


#temporal change of the absolute count of substantive rhetorical strategies
library(data.table)
DT = as.data.table(raw_data)
num_char=DT[ ,list(sum(num_char)),by=time_period][,2]/1000

for_plot<-DT[ ,list(historical_example=sum(historical_examples_total),
          historical_examples_outcome=sum(historical_examples_outcome_total),
          analogy=sum(Analogy),
          motivation=sum(motivation_total),
          information=sum(Information),
          sign=sum(sign_total),
          tradition=sum(Tradition),
          morality=sum(Morality),
          hearsay=sum(Hearsay),
          proverb=sum(Proverb),
          remark=sum(Remark),
          authoritative_text=sum(authoritative.text)
          ), by=time_period]
#for each historical periods, divide counts by number of characters
for_plot[,2]<-for_plot[,2]/num_char
for_plot[,3]<-for_plot[,3]/num_char
for_plot[,4]<-for_plot[,4]/num_char
for_plot[,5]<-for_plot[,5]/num_char
for_plot[,6]<-for_plot[,6]/num_char
for_plot[,7]<-for_plot[,7]/num_char
for_plot[,8]<-for_plot[,8]/num_char
for_plot[,9]<-for_plot[,9]/num_char
for_plot[,10]<-for_plot[,10]/num_char
for_plot[,11]<-for_plot[,11]/num_char
for_plot[,12]<-for_plot[,12]/num_char
for_plot[,13]<-for_plot[,13]/num_char

for_plot.t<-t(for_plot) #transpose dataframe
colnames(for_plot.t) <- for_plot.t[1,]
new_data <- for_plot.t[-1, ] 

library(reshape)

long_data<-melt(new_data,id.vars="x",variable.name = "time period")

long_data<-melt(new_data,value.name="x", id="time period")
long_data$value1<-as.numeric(long_data$value)


ggplot(long_data, aes(fill=X1, y=value1, x=X2)) + 
  geom_bar(width=0.8,position=position_dodge(width=0.9), stat="identity")+
  theme(text = element_text(size=22),axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.margin = unit(c(1,1,1,2.6), "cm"))+
  coord_cartesian(clip = "off")+
  xlab("historical periods (chronological order)")+
  ylab("counts per 1000 characters")+
  labs(fill="rhetorical strategies (substantive)")+
  scale_x_discrete(labels= new_lab)+
  scale_fill_discrete(labels = c("historical example", "historical example with outcome", "analogy", "cost/benefit analysis", "information", "auspicious/inauspicious sign", "tradition",
                                 "morality","hearsay","proverb","remark","authoritative text"))



#same thing for stylistic strategies
for_plot_stylistic<-DT[ ,list(constructive_criticism=sum(StatementPositive),
                                      criticism=sum(Criticism),
                                      flattery=sum(Sycophancy),
                                      indirect_language=sum(PoliteFormula),
                                      self_deprecation=sum(Self.deprecation),
                                      threat=sum(Threat),
                                      sacrifice=sum(Sacrifice)
                              ), by=time_period]

for_plot_stylistic[,2]<-for_plot_stylistic[,2]/num_char
for_plot_stylistic[,3]<-for_plot_stylistic[,3]/num_char
for_plot_stylistic[,4]<-for_plot_stylistic[,4]/num_char
for_plot_stylistic[,5]<-for_plot_stylistic[,5]/num_char
for_plot_stylistic[,6]<-for_plot_stylistic[,6]/num_char
for_plot_stylistic[,7]<-for_plot_stylistic[,7]/num_char

for_plot_stylistic.t<-t(for_plot_stylistic) #transpose dataframe
colnames(for_plot_stylistic.t) <- for_plot_stylistic.t[1,]
new_data <- for_plot_stylistic.t[-1, ] 


long_data<-melt(new_data,id.vars="x",variable.name = "time period")
long_data$value1<-as.numeric(long_data$value)

ggplot(long_data, aes(fill=X1, y=value1, x=X2)) + 
  geom_bar(width=0.8,position=position_dodge(width=0.9), stat="identity")+
  theme(text = element_text(size=22),axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.margin = unit(c(1,1,1,2.6), "cm"))+
  coord_cartesian(clip = "off")+
  xlab("historical periods (chronological order)")+
  ylab("counts per 1000 characters")+
  labs(fill="rhetorical strategies (stylistic)")+
  scale_x_discrete(labels= new_lab)+
  ylim(0,20)+
  scale_fill_discrete(labels = c("constructive criticism", "criticism", "flattery", "indirect language", "self deprecation", "threat", "sacrifice"))

#check what substantive strategies predict persuasion success. "advice taken" is a binary variable (0/1)

suc_lm<-glm(formula = advice.taken ~ Sycophancy + Criticism + Self.deprecation+Sacrifice+Threat+PoliteFormula+sign_total+motivation_total+
               Analogy+Information+Tradition+historical_examples_total+historical_examples_outcome_total+Morality+Hearsay+Proverb+Remark+authoritative.text+
              book_time+num_char,
             family = "binomial", 
             data = raw_data)
summary(suc_lm)

library(MASS)
#using stepAIC to select optimal features
step.suc_lm<-stepAIC(suc_lm,direction='both')

suc_optimal<-glm(advice.taken ~ Sycophancy + Self.deprecation + Threat + sign_total + 
                   motivation_total + Analogy + historical_examples_outcome_total + 
                   Hearsay + Remark + book_time,
                 family = "binomial", 
                 data = raw_data)
summary(suc_optimal)

#combining "historical examples" and "historical examples with outcome" for additional analysis in SI:

suc_lm_SI<-glm(formula = advice.taken ~ Sycophancy + Criticism + Self.deprecation+Sacrifice+Threat+PoliteFormula+sign_total+motivation_total+
              Analogy+Information+Tradition+historical_examples_all+Morality+Hearsay+Proverb+Remark+authoritative.text+
              book_time+num_char,
            family = "binomial", 
            data = raw_data)
summary(suc_lm_SI)

step.suc_lm_SI<-stepAIC(suc_lm_SI,direction='both')

suc_optimal<-glm(advice.taken ~ Self.deprecation + Threat + sign_total + 
                   Analogy + historical_examples_all + 
                   Hearsay + Remark + book_time,
                 family = "binomial", 
                 data = raw_data)
summary(suc_optimal)

#examine the temporal change of rhetorical strategies
raw_data$historical_examples_total_frequency<-raw_data$historical_examples_total/raw_data$num_char*1000
raw_data$historical_examples_outcome_total_frequency<-raw_data$historical_examples_outcome_total/raw_data$num_char*1000
raw_data$analogy_frequency<-raw_data$Analogy/raw_data$num_char*1000
raw_data$sign_total_frequency<-raw_data$sign_total/raw_data$num_char*1000
raw_data$tradition_frequency<-raw_data$Tradition/raw_data$num_char*1000
raw_data$morality_frequency<-raw_data$Morality/raw_data$num_char*1000
raw_data$hearsay_frequency<-raw_data$Hearsay/raw_data$num_char*1000
raw_data$proverb_frequency<-raw_data$Proverb/raw_data$num_char*1000
raw_data$remark_frequency<-raw_data$Remark/raw_data$num_char*1000
raw_data$authoritative_text_frequency<-raw_data$authoritative.text/raw_data$num_char*1000

raw_data$indirect_language_frequency<-raw_data$StatementPositive/raw_data$num_char*1000
raw_data$criticism_frequency<-raw_data$Criticism/raw_data$num_char*1000
raw_data$flattering_frequency<-raw_data$Sycophancy/raw_data$num_char*1000
raw_data$honorific_language_frequency<-raw_data$PoliteFormula/raw_data$num_char*1000
raw_data$self_deprecation_frequency<-raw_data$Self.deprecation/raw_data$num_char*1000
raw_data$threat_frequency<-raw_data$Sacrifice/raw_data$num_char*1000

#use the code below to compute coefficients of different variable ~ book_time
lm<-lm(formula=Book_Chunqiu~book_time,data = raw_data)
summary(lm) 

#coefficient plot
coe_data<-read.csv("~/political_arguments/new_data/jian/yuqi_data/coefficient.csv",fileEncoding="UTF-8-BOM")
ggplot(coe_data, aes(x = Variable_dependent, y = Estimate)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = Variable_dependent, 
                 y = Estimate)) + 
  geom_linerange(aes(x = Variable_dependent, 
                     ymin = Estimate-SE,
                     ymax = Estimate+SE),
                 lwd = 1) +
  coord_flip()+
  theme(text = element_text(size=25))+
  ylab("coefficient")+
  xlab("dependent variable")

#coefficient plot for different authoritative texts


#pca plot, using rhetorical strategies as features
raw_data['Tradition','Hearsay']

pca_data<-select(raw_data,StatementPositive, Sycophancy, Criticism, Self.deprecation, Sacrifice, Threat, PoliteFormula,
                 Analogy, Information, Tradition,Hearsay, Proverb,authoritative.text,historical_examples_total,historical_examples_outcome_total,sign_total,motivation_total)

pca<-prcomp(pca_data,center=TRUE,scale.=TRUE)
summary(pca)
library(ggfortify)
autoplot(pca)

#time series analysis
#need to regress the frequency of usage on its persuasion success rate 
#(e.g., in a particular dynasty, out of total episodes where analogy is used, how many end up successful) in the previous dynasty?

#subset dataset with the use of some strategy
data_historical_example<-raw_data[raw_data$sign_total!=0,]
DT=as.data.table(data_historical_example)
#compute mean of persuasion success by time_period
DT[,list(mean=mean(advice.taken,na.rm=TRUE)),by=time_period]

#compute relative usage of the rhetoricla method
for_plot$historical_example
write.csv(for_plot,"~/political_arguments/new_data/jian/yuqi_data/dynastic_info.csv", row.names = TRUE)

test<-read.csv("~/political_arguments/new_data/jian/yuqi_data/dynastic_info.csv")
sign_time<-ts(test$sign)
sign_success_time<-ts(test$sign_success)
historical_example_time<-ts(test$historical_example)
historical_example_success_time<-ts(test$historical_example_success)



library(dynlm)
mod<-dynlm(historical_example_time~L(historical_example_success_time,1))
summary(mod)

#factor analysis. see https://www.uwo.ca/fhs/tc/labs/10.FactorAnalysis.pdf for instruction and interpretation
fa<-factanal(pca_data,factors=6)
fa

library(psych)
library(GPArotation)
#check whether a factor analysis is appropriate. see https://www.uwo.ca/fhs/tc/labs/10.FactorAnalysis.pdf for instruction and interpretation
KMO(pca_data)

#check scree plot (eigenvalues for different factors)
scree(pca_data)
#seems number of factors == 6 and number of principle componenets == 3
fa.out <- fa(pca_data,
              nfactors = 5,
              fm="pa",
              max.iter = 100,
              rotate = "oblimin")

fa.diagram(fa.out)
fa.out$communality

#principle component analysis
pca.out<-principal(pca_data,nfactors=5, rotate="oblimin")
fa.diagram(pca.out)

#10_25_2022, check if successful persuasion led to better outcome for the persuader

#split the data into successful persuasion and unsuccessful persuasions
suc_per<-raw_data[which(raw_data$advice.taken==1),]
unsuc_per<-raw_data[which(raw_data$advice.taken==0),]
table(suc_per$persuader.outcome)
table(unsuc_per$persuader.outcome)

#subset observations that contain the use of analogy
analogy_per<-raw_data[which(raw_data$Analogy>0),]
  #for these, check whether persuasion success decreased over time
  lm_analogy<-lm(formula=advice.taken~book_time,data = analogy_per)
  summary(lm_analogy)  

#same for signs  
sign_per<-raw_data[which(raw_data$sign_total>0),]
  #for these, check whether persuasion success decreased over time
  lm_sign<-lm(formula=advice.taken~book_time,data = sign_per)
  summary(lm_sign)  

  
#inter rater reliability analysis
library(irr)
coder_data<-read.csv("~/political_arguments/new_data/jian/yuqi_data/intercoder_reliability/data_combined.csv")
#change all NAs to zero
coder_data[is.na(coder_data)] <- 0
#change all values larger than 1 to 1
coder_data[coder_data>1] <- 1

kappam.light(coder_data[,34:36])  
