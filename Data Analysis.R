data <- read.csv("~/MEGA/Uni/Master/Thesis/Survey/Stats/Survey_data_text.csv", comment.char="#")

library(dplyr)
library(stringr)

# how many finished the survey
count(data, Finished)





# Replace Text with values for 2.3 questions
rep_matrix = cbind(c('Strongly agree','Agree','Somewhat agree','Neither agree nor disagree',
                     'Somewhat disagree',	'Disagree',	'Strongly disagree'),
                    c(1,2,3,4,5,6,7))



for (i in 1:dim(rep_matrix)[1]){
  
  
    data <- data.frame(lapply(data, function(x) {
    gsub(rep_matrix[i,1], as.numeric(rep_matrix[i,2]), x)
  }))

}



nohead = data[-c(1,2),]
lbldata <- read.csv("~/MEGA/Uni/Master/Thesis/SethParkResults/ScriptieTagger/test_exp_anno_labeled - survey_ready.csv", row.names=1)


results = matrix(nrow = 31, ncol = 13)

for (i in 2:31){

  col = paste("X",i,'_',sep='')
  col
  Q = select(data, starts_with(col))
  Q_ID = Q[1,1]
  Q_ID = sub(" - .*",'',Q_ID)
  Q = Q[-c(1,2),]
  if (length(Q)<1 )
    next
  
  #Q1
  Q1 = select(Q,contains('Q1')) %>% filter_all(all_vars(. != ''))
  Q1_n = as.integer(count(Q1))
  Q1_Non_e = as.integer(Q1 %>% filter_all(all_vars(grepl('None of the above', .))) %>% count)
  Q1_e = Q1_n - Q1_Non_e
  
  #Q2
  
  Q2 = select(Q,contains('Q2.1')) %>% filter_all(all_vars(. != ''))
  Q2_n = as.integer(count(Q2))
  Q2_Non_e = as.integer(Q2 %>% filter_all(all_vars(grepl('None of the above', .))) %>% count)
  Q2_e = Q2_n - Q2_Non_e
  
  #Q3
  
  Q3 = select(Q,contains('Q2.3')) %>% filter_all(all_vars(. != '')) 
  
  Q3_res = Q3 %>% mutate_each(funs(as.numeric)) %>% summarise_if(is.numeric, list(mean,sd), na.rm = TRUE) %>% unlist(., use.names=FALSE)

  lbl = lbldata[Q_ID,'label']
  
  
  
  results[i,] = append(c(i,Q_ID,lbl,Q1_n,Q1_e,Q2_n,Q2_e), Q3_res)

}


results = results[rowSums(is.na(results)) != ncol(results),]
results = data.frame(results,row.names = 1)
colnames(results) <- c('Q_ID','label','Q1_n','Q1','Q2_n','Q2','E4_mean','E3_mean','E2_mean','E4_stdv','E3_stdv','E2_stdv')

#1e row verwijderen want is nog pretest?

summarise(Q1, n())


is.empty(test[3][1])



# Q1 != None of the above (4) --> Cat 1 error
# Q2.1 != None of the above(17)--> Cat 2/4 error
# Q2.3_1 = Cat 4
# Q2.3_2 = Cat 3
# Q2.3_3 = Cat 2/(4)


# Collect answers per question & Answers per category


# Per Category:


cat_vec = list(matrix(nrow=0, ncol = 5),
                  matrix(nrow=0, ncol = 5),
                  matrix(nrow=0, ncol = 5),
                  matrix(nrow=0, ncol = 5))



for (i in 2:31){
  
  
  col = paste("X",i,'_',sep='')
  cat(col)

  Q = select(data, starts_with(col)) 
  if (length(Q)<2 )
    next
  Q = Q %>% filter_all(all_vars(. != ''))
  Q_ID = Q[1,1]
  Q_ID = sub(" - .*",'',Q_ID)
  Q = Q[-c(1,2),]
  
  lbl = lbldata[Q_ID,'label']
  
  if (lbl == 4)
    cat('\n',lbl,dim(Q), '\n')
  
  if (length(Q)<1 )
    next
  
  
  
  
  cat_vec[[lbl]] = rbind(cat_vec[[lbl]], as.matrix(Q))

  
}

# How often was a sentence detected as being nonsense/containing grammatical errors when it was classified as 1

#check for other cats
A1 = vector()
for (i in 1:4){
  contains_clar_error = cat_vec[[i]][,1] != 'None of the above'
  cat(i, sum(contains_clar_error)/length(contains_clar_error),'\n')
  
  length(contains_clar_error) <- 65 #max length of all cols
  A1 = cbind(A1,contains_clar_error)
}

### Highest for cat 1 as expected: 89% of the time: Significant?

A1_true = colSums(A1,na.rm=TRUE)
A1_N = colSums(!is.na(A1),na.rm=TRUE)

A1_true_summed = c(sum(A1_true[2:4]),A1_true[1])
A1_N_summed = c(sum(A1_N[1:3]),A1_N[4])



prop.test(A1_true_summed,A1_N_summed, alternative = 'l')

# With a significance of p<0.01 we can reject the null hypothesis that participants classified explanations as
# containing grammatical or semantical errors equally as often for explanations labeled clarity errors as for those that were not.

# We can therefore assume the alternative hypothesis that explanations labeled clarity errors are classified as containing grammatical
# or semantical errors more often than those that were labeled differently.

#-------

# How often was a sentence detected as containing information errors?
A2 = vector()
for (i in 1:4){
  contains_inf_error = cat_vec[[i]][,2] != 'None of the above'
  cat(i, sum(contains_inf_error)/length(contains_inf_error),'\n')
  
  length(contains_inf_error) <- 65 #max length of all cols
  A2 = cbind(A2,contains_inf_error)
}



A2_true = colSums(A2,na.rm=TRUE)
A2_N = colSums(!is.na(A2),na.rm=TRUE)

### Note that cat1 should be disregarded, since 
### sentences containing grammatical/other errors are automatically categorised as 1.
A2_true_summed = c(sum(A2_true[3:4]),A2_true[2])
A2_N_summed = c(sum(A2_N[3:4]),A2_N[2])



prop.test(A2_true_summed,A2_N_summed, alternative = 'l')
# With a significance of p<0.01 we can reject the null hypothesis that participants classified explanations as
# containing information errors equally as often for explanations labeled as containing information errors as for those that were not.

# We can therefore assume the alternative hypothesis that explanations labeled as containing information error are classified as containing information
# errors more often than those that were labeled differently.

### Note that cat1 should be disregarded, since sentences containing grammatical/other errors are automatically categorised as 1.
### Quite high for all categories, but significantly highest for 2 as expected. The high score vor 3 is unexpected though.
#----


# Did participants expect a different answer (agree < 4> disagree)
cat('Did participants expect a different answer (agree < 4> disagree)')
E4 = vector()
for (i in 1:4){
  
  vec = as.integer(cat_vec[[i]][,3])
  length(vec) <- 65 #length of the longest vector
  E4 = cbind(E4,vec )
  m = mean(vec, na.rm=TRUE)
  s = sd(vec, na.rm=TRUE)
  
  cat('\n',i,'\n\tmean:',m,'\n\tsd:',s)
}

# E4 = as.integer(cat_vec[[4]][,3])
# E42 = as.integer(cat_vec[[3]][,3])


colnames(E4) <- c('cat_1','cat_2','cat_3','cat_4')
E4 = data.frame(E4)
#plot values

library(ggplot2)
library(reshape2)

E4_plot = melt(E4)

plot_axis = rep_matrix[,1]
plot_axis[4] = 'Neither agree\nnor disagree'

ggplot(data = E4_plot) +
  geom_histogram(aes(x=value, fill = variable), position='dodge', stat ='count')+
  ggtitle('Did participants expect a different answer (agree < 4> disagree)')+
  scale_x_discrete('',breaks = 1:7,limits= 1:7, labels =plot_axis )
#-----------------

# Did participants understand how the computer arrived at the Computer Answer by looking at the Computer Explanation
cat('Did participants understand how the computer arrived at the Computer Answer by looking at the Computer Explanation?')
E3 = vector()
for (i in 1:4){
  
  vec = as.integer(cat_vec[[i]][,4])
  length(vec) <- 65 #length of the longest vector
  E3 = cbind(E3,vec )
  m = mean(vec, na.rm=TRUE)
  s = sd(vec, na.rm=TRUE)
  
  cat('\n',i,'\n\tmean:',m,'\n\tsd:',s)
}

# E3 = as.integer(cat_vec[[4]][,3])
# E42 = as.integer(cat_vec[[3]][,3])


colnames(E3) <- c('cat_1','cat_2','cat_3','cat_4')
E3 = data.frame(E3)
#plot values

library(ggplot2)
library(reshape2)

E3_plot = melt(E3)

plot_axis = rep_matrix[,1]
plot_axis[4] = 'Neither agree\nnor disagree'

ggplot(data = E3_plot) +
  geom_histogram(aes(x=value, fill = variable), position='dodge', stat ='count')+
  ggtitle('Did participants understand how the computer arrived at the Computer Answer\nby looking at the Computer Explanation')+
  scale_x_discrete('',breaks = 1:7,limits= 1:7, labels =plot_axis )
#-----------------

# Did the participants think the Computer Explanation contained errors
cat('Did the participants think the Computer Explanation contained errors?')
E2 = vector()
for (i in 1:4){
  
  vec = as.integer(cat_vec[[i]][,5])
  length(vec) <- 65 #length of the longest vector
  E2 = cbind(E2,vec )
  m = mean(vec, na.rm=TRUE)
  s = sd(vec, na.rm=TRUE)
  
  cat('\n',i,'\n\tmean:',m,'\n\tsd:',s)
}

# E2 = as.integer(cat_vec[[4]][,3])
# E42 = as.integer(cat_vec[[3]][,3])


colnames(E2) <- c('cat_1','cat_2','cat_3','cat_4')
E2 = data.frame(E2)
#plot values

library(ggplot2)
library(reshape2)

E2_plot = melt(E2)

plot_axis = rep_matrix[,1]
plot_axis[4] = 'Neither agree\nnor disagree'

ggplot(data = E2_plot) +
  geom_histogram(aes(x=value, fill = variable), position='dodge', stat ='count')+
  ggtitle('Did the participants think the Computer Explanation contained errors?')+
  scale_x_discrete('',breaks = 1:7,limits= 1:7, labels =plot_axis )



# Statistical analysis:

# Did participants expect a different answer more often for explanations labeled cat4 than for other cats.

E4_agree = colSums(E4<4, na.rm = TRUE)
E4_N = colSums(!is.na(E4),na.rm=TRUE)
E4_prop_agree = E4_agree/E4_N

#.85 for cat 4 while <.4 for the other cats.. Significant?

#Add 1,2,3 together to compare all non-4 to 4
E4_agree_summed = c(sum(E4_agree[1:3]),E4_agree[4])
E4_N_summed = c(sum(E4_N[1:3]),E4_N[4])


prop.test(E4_agree_summed,E4_N_summed, alternative = 'l')

# With a significance of p<0.001, We reject the Null Hypothesis that participants expected a different
# answer equally as often for explanations that were not labeled as containing an A-E mismatch as with those that did.

# more strongly?

##

# Did participants understtand how the computer arived at its answer using the explanation more often for explanations labeled cat3 than for other cats.

E3_agree = colSums(E3<4, na.rm = TRUE)
E3_N = colSums(!is.na(E3),na.rm=TRUE)
E3_prop_agree = E3_agree/E3_N

#~.85 for cat 3 while <.6 for the other cats.. Significant?

#Add 1,2,3 together to compare all non-4 to 4
E3_agree_summed = c(sum(E3_agree[c(1,2,4)]),E3_agree[3])
E3_N_summed = c(sum(E3_N[c(1,2,4)]),E3_N[3])

prop.test(E3_agree_summed,E3_N_summed, alternative = 'l')

# With a significance of p<0.001, We reject the Null Hypothesis that participants understand how the computer arrived
# its answe equally as often for explanations that were not labeled as containing an mistake in reasoning as with those that did.
# We can therefore assume the alternative hypothesis that participants understand how the computer arrived at its answer more often 
# for explanations labeled cat-3 than for those that are not.

# How about disagree with cat 4 

E3_disagree = colSums(E3>4, na.rm = TRUE)
E3_N = colSums(!is.na(E3),na.rm=TRUE)
E3_prop_disagree = E3_disagree/E3_N

#.85 for cat 4 while <.5 for the other cats.. Significant?

#Add 1,2,3 together to compare all non-4 to 4
E3_disagree_summed = c(sum(E3_disagree[1:3]),E3_disagree[4])
E3_N_summed = c(sum(E3_N[1:3]),E3_N[4])

prop.test(E3_disagree_summed,E3_N_summed, alternative = 'l')

# With a significance of p<0.001, We reject the Null Hypothesis that participants understand how the computer arrived
# its answer equally as often for explanations that were not labeled as containing an mistake in reasoning as with those that did.
# We can therefore assume the alternative hypothesis that participants understand how the computer arrived at its answer less often 
# for explanations labeled cat-4 than for those that are not.

# more strongly?

##




