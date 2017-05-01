#Copyright reserved 
#svenka15, nthanik, vchitto 
#BI Capstone project 
library(data.table)
library(NLP)
library(openNLP)

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

data<-fread("Reviews.csv")
dim(data)
data<-data[,c(-1,-4)]

helpful_denom_0_ind<-which(data$HelpfulnessDenominator==0)
helpful_denom_0<-data[helpful_denom_0_ind]

non_helpful_denom_0<-data[-helpful_denom_0_ind]
helpful_ratio_gt_50<-non_helpful_denom_0[non_helpful_denom_0$HelpfulnessNumerator>=0.5*non_helpful_denom_0$HelpfulnessDenominator]
######################################################################

#preprocessing

########################################################################
data<-rbind(helpful_ratio_gt_50,helpful_denom_0)
write.csv(data,"preprocessed_reviews.csv",row.names = FALSE)
data<-fread("preprocessed_reviews.csv")
head(data)
nrow(data)
train_ind <- sample(1:nrow(data), size = .7*nrow(data))
train<-data[train_ind,]
nrow(train)
test<-data[-train_ind]
nrow(test)
write.csv(train,"train.csv",row.names = FALSE)
write.csv(test,"test.csv",row.names = FALSE)
head(train)
good_bad<-train[,c(5,7)]
head(good_bad)
dim(data)
data[1,6]


#######################################################################

#Extract words

######################################################################
goodwords<-c()
count<-0
for (i in (good_bad[good_bad$Score>3]$Summary))
{
  words<-strsplit(i,'[ /",\\().!?:;-]')
  goodwords<-c(goodwords,words[[1]])
  print(count)
  count=count+1
}
goodwords
badwords<-c()
for (i in good_bad[good_bad$Score<3]$Summary)
{
  words<-strsplit(i,'[ /",\\().!?:;-]')
  badwords<-c(badwords,words[[1]])
}
modwords<-c()
modwords
for (i in good_bad[good_bad$Score==3]$Summary)
{
  print(i)
  words<-strsplit(i,'[ /",\\().!?:;-]')
  modwords<-c(modwords,words[[1]])
  
}

###############################################
#LOWER-CASE
############################################
goodwords_case<-sapply(goodwords,tolower)
modwords_case<-sapply(modwords,tolower)
badwords_case<-sapply(badwords,tolower)
##############################################
#UNIQUE
###############################################
goodwords<-unique(goodwords_case)
badwords<-unique(badwords_case)
modwords<-unique(modwords_case)
#############################################
#INTERSECT AND UNION AND DIFF
##########################################
inter_gb<-intersect(goodwords,badwords)
inter_gm<-intersect(goodwords,modwords)
inter_bm<-intersect(badwords,modwords)
u_gb_bm<-union(inter_gb,inter_gm)
u_gb_bm<-union(u_gb_bm,inter_bm)
final_goodwords<-setdiff(goodwords,u_gb_bm)
final_badwords<-setdiff(badwords,u_gb_bm)
final_modwords<-setdiff(modwords,u_gb_bm)
final_goodwords
final_modwords
final_badwords
class(final_modwords)
final_goodwords1<-data.frame()
#########################################################
#GREATER THAN 3
##########################################################
for( i in (1:length(final_goodwords)))
{
  if(nchar(final_goodwords[i])>=3)
  {
    
    final_goodwords1<-c(final_goodwords1,final_goodwords[i])
    
  }
}
final_goodwords1
final_badwords1<-data.frame()
for( i in (1:length(final_badwords)))
{
  if(nchar(final_badwords[i])>=3)
  {
    
    final_badwords1<-c(final_badwords1,final_badwords[i])
    
  }
}
#run from here
final_modwords1<-data.frame()
for( i in (1:length(final_modwords)))
{
  if(nchar(final_modwords[i])>=3)
  {
    
    final_modwords1<-c(final_modwords1,final_modwords[i])
    
  }
}
final_badwords1
final_goodwords<-final_goodwords1
final_badwords<-final_badwords1
final_modwords<-final_modwords1
##########################################################################
#3-LETTERS
#########################################################################
final_modwords1
for(i in (1:length(final_modwords))){
  final_modwords[i]<-gsub('([[:alpha:]])\\1\\1+',"\\1\\1\\1",final_modwords[i])
}

final_modwords[1]

final_badwords
for(i in (1:length(final_badwords))){
  final_badwords[i]<-gsub('([[:alpha:]])\\1\\1+',"\\1\\1\\1",final_badwords[i])
}
final_goodwords
for(i in (1:length(final_goodwords))){
  final_goodwords[i]<-gsub('([[:alpha:]])\\1\\1+',"\\1\\1\\1",final_goodwords[i])
}
####################################################################################
#PRONOUN
####################################################################################
final_modwords1<-data.frame()
final_modwords1
final_goodwords1<-data.frame()
final_badwords1<-data.frame()
tagged_str <-  tagPOS(final_goodwords)
s<-tagged_str$POStags
for(i in 1:length(final_goodwords))
{
  if(s[i]!="DT" && s[i]!="CD" && s[i]!="EX" && s[i]!="IN" && s[i]!="PDT"  && s[i]!="POS"&& s[i]!="TO" )
    final_goodwords1<-c(final_goodwords1,final_goodwords[i])
}
tagged_str <-  tagPOS(final_modwords)
s<-tagged_str$POStags
for(i in 1:length(final_modwords))
{
  if(s[i]!="DT" && s[i]!="CD" && s[i]!="EX" && s[i]!="IN" && s[i]!="PDT"  && s[i]!="POS"&& s[i]!="TO" )
    final_modwords1<-c(final_modwords1,final_modwords[i])
}
tagged_str <-  tagPOS(final_badwords)
s<-tagged_str$POStags
for(i in 1:length(final_badwords))
{
  if(s[i]!="DT" && s[i]!="CD" && s[i]!="EX" && s[i]!="IN" && s[i]!="PDT"  && s[i]!="POS"&& s[i]!="TO" )
    final_badwords1<-c(final_badwords1,final_badwords[i])
}
final_badwords1
final_badwords
final_goodwords<-final_goodwords1
final_badwords<-final_badwords1
final_modwords<-final_modwords1
length(final_goodwords)
length(final_modwords)
length(final_badwords)
data_pred<-c()
dim(test)[1]
################################################################################################
#ACCURACY
#############################################################################################
for( i in 1:dim(test)[1])
{ 
  print (i)
  #SPLIT
  two<-strsplit(test[i,7][[1]],'[ /",\\().!?:;-]')
  #LOWER
  two<-sapply(two,tolower)
  #UNIQUE
  two<-unique(two)
  
  two1<-data.frame()
  #GREATER THAN 3
  for( j in (1:length(two)))
  {
    if(nchar(two[j])>=3)
    {
      
      two1<-c(two1,two[j])
    }
  }
  #3-LETTER 
  if(length(two1)>0){
    for(k in (1:length(two1))){
      #print(two1[k])
      two1[k]<-gsub('([[:alpha:]])\\1\\1+',"\\1\\1\\1",two1[k])
      #    print(two1[k])
    }
  }
  final_badwords
  two<-two1
  
  
  #COMPARE WITH GOOD BAD
  if((length(intersect(two1,final_goodwords))%%2)>=(length(intersect(two1,final_badwords))%%2))
  {
    if((length(intersect(two1,final_goodwords))%%2)>=(length(intersect(two1,final_modwords))%%2))
      data_pred[i]="good"
    else
      data_pred[i]="mod"
    
  }
  else if((length(intersect(two1,final_goodwords))%%2)<=(length(intersect(two1,final_badwords))%%2))
  {
    if((length(intersect(two1,final_modwords))%%2)<=(length(intersect(two1,final_badwords))%%2))
      data_pred[i]="bad"
    
  }
  if(length(intersect(two1,final_badwords))%%2>0)
    data_pred[i]="bad"  
  
  
}
check<-test[,5]
check
############################################################################

#POST ACCURACY

###############################################################################
unique(data_pred)
check[check==1]<-10
check[check==2]<-10
check[check==3]<-20
check[check==5]<-30
check[check==4]<-30
check
check_pred<-data_pred
check_pred
data_pred[data_pred=="good"]<-30
data_pred[data_pred=="mod"]<-20
data_pred[data_pred=="bad"]<-10
length(data_pred)
data_pred
length(check$Score)
#ACCURACY
accuracy<-table(pred=data_pred,true=check$Score)
accuracy
print (((accuracy[1]+accuracy[5]+accuracy[9])/dim(test)[1])*100)

#WRITE TO FILE
textfile<-file("good.txt")
for(j in final_goodwords)
{
  
  cat(j, file="good.txt", append=TRUE, sep = "\n")
}
close(textfile)
textfile<-file("bad.txt")
for(j in final_badwords)
{
  cat(j, file="bad.txt", append=TRUE, sep = "\n")
}
close(textfile)
textfile<-file("mod.txt")
for(j in final_modwords)
{
  if(length(j)>=3)
    cat(j, file="mod.txt", append=TRUE, sep = "\n")
}
close(textfile)