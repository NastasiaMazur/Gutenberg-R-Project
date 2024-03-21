# NLP Project based on GutenbergR

library(gutenbergr)
library(tidytext)
library(dplyr)

my_mirror <- "http://mirrors.xmission.com/gutenberg/" # gutenberg library is stored at servers

# Troubleshooting with mirrors:
# https://github.com/ropensci/gutenbergr/issues/28  

gutenberg

df<-gutenberg_metadata # gutenberg_id, titel of the book etc.

unique(df$author)[startsWith(unique(df$author), "Maz")] # address authors whose name starts with; unique (count once)

gutenberg_works(author == "Mazzini, Giuseppe")
# ?gutenberg_works()

LitPhilEssays <- gutenberg_download(5637, mirror = my_mirror) #5637 is the id of the book; 15109 #of lines
mybook <- gutenberg_download(5637, mirror = my_mirror)

# working with texts - some ideas

words_LitPhilEssays<-unnest_tokens(LitPhilEssays,words,text) #address df (LitPhilEssays); unnest_tokens - extract/count words, n-grams; input -text
                                              #list of words accur in text

countwords<-count(words_LitPhilEssays,words,sort=TRUE) # df - which words how often


total_word_count <- sum(countwords$n) # ADDITIONAL

bigram_LitPhilEssays<-unnest_tokens(LitPhilEssays,words,text, token = "ngrams" , n=2) #count bi-grams

# test for dependence of bigram <-> phrases

countbigram<-count(bigram_LitPhilEssays,words,sort=TRUE) #count all bi-grams in book
# NA - empty row - doesn't find any bi-grams
countbigram<-countbigram[-2,] # second row (in my book also) erase

countbigram[startsWith(countbigram$words,"belonging to"),]
# how often does "belonging to" occur, looking at all bi-grams
countbigram[startsWith(countbigram$words,"belonging "),]
# how often does "belonging to" occur followed by any word - startsWith
countbigram[endsWith(countbigram$words, " to"),]
# how often does "to" occur preceded by any word - endsWith
#In contingency table does the bi-gram “belonging to” occur more often 
#than “belonging” + anything else 
#or anything + “to”?
#How is the frequency distributed?

b.t<-countbigram[startsWith(countbigram$words,"belonging to"),]$n
# bi-gram I'm testing: belonging to (b.t)
# just this number (24)
b.nott <- sum(countbigram[startsWith(countbigram$words,"belonging "),]$n) - b.t
# bi-grams that START WITH 'belonging' but do not end with 'to'
# Take all bi-grams start with 'belonging' 
# Sum their occurances (by n) -- sum n column BUT I have 'belonging to' (don't need it!)
# Subtract b.t. (24)
notb.t <- sum(countbigram[endsWith(countbigram$words," to"),]$n) - b.t
# bi-grams that END WITH 'to' but do not start with 'belonging'
# Sum up all bi-grams end with 'to'
# Sum their occurances (by n) -- sum n column BUT I have 'belonging to' (don't need it!)
# Subtract b.t. (24)
notb.nott<-sum(countbigram$n) - b.nott - notb.t - b.t
# All bi-grams that niether start with 'belonging' nor with 'to'
# Sum all bi-grams (by n) (72279)
# Subtract b.nott (0)
# Subtract notb.t (4351)
# Subtract b.t (24)


freq<-matrix(c(b.t,b.nott,notb.t,notb.nott),ncol = 2, byrow = T)


library(RColorBrewer)
pastel_colors <- brewer.pal(4, name = "Pastel2")
#row_names <- c("Belonging To", "Belonging But Not To", "Not Belonging But To", "Not Belonging Nor To")
mosaicplot(freq, col = pastel_colors, main = "Mosaic Plot of Bigram 'belonging to'")

#mosaicplot(freq)

chisq.test(freq)
#What is chi-test?     --> explain 
#What is independence? --> explain 

#bigram containing 2 independent words

countbigram<-count(bigram_LitPhilEssays,words,sort=TRUE) #count all bi-grams in book
countbigram<-countbigram[-2,] # second row (in my book also) erase

countbigram[startsWith(countbigram$words,"simply and"),]
countbigram[startsWith(countbigram$words,"simply "),]
countbigram[endsWith(countbigram$words, " and"),]


z.w<-countbigram[startsWith(countbigram$words,"simply and"),]$n
z.notw <- sum(countbigram[startsWith(countbigram$words,"simply "),]$n) - z.w
notz.w <- sum(countbigram[endsWith(countbigram$words," and"),]$n) - z.w
notz.notw<-sum(countbigram$n) - z.nott - notz.w - z.w

freq2<-matrix(c(z.w,z.notw,notz.w,notz.notw),ncol = 2, byrow = T)

library(RColorBrewer)
pastel_colors <- brewer.pal(4, name = "Pastel2")

mosaicplot(freq2, col = pastel_colors, main = "Mosaic Plot of Bigram 'simply and' ")


chisq.test(freq2)




# Entropy of 1000-word parts

#We dissect the text in 1000-word parts and 
#for each of the part we want to compute the entropy

entropy<-c() #store entropy in vector
for(i in 0:152) # loop that iterates from 0 to 152; within each iteration, it calculates the entropy of 1000-word parts 
{
  entr<-words_LitPhilEssays[(i*1000+1):(i*1000+1000),2] # take 1st 1K words +1 and for each the iteration (152) it goes further
  #For each iteration it selects a range of rows from i * 1000 + 1 to i * 1000 + 1000
  #Without + 1, the segments would start at the same index
  #i * 1000 + 1000: ensures that each segment is exactly 1000 words long
  #extracts a 1000-word segment of the text in each iteration.
  #2 - selects the second column 
  char<-unnest_tokens(entr,token,words, token="characters") #we want to get the characters
  df.char<-as.data.frame(count(char,token,sort=TRUE)) # dfchar relative frequencies
  df.char$relfreq<-df.char$n/sum(df.char$n) #we devide it over sum of all characters; dfchar relative frequencies
  df.char$ent<-df.char$relfreq*log2(df.char$relfreq) #we take relative frequencies * log2 of the relative frequency ;probability * log probability;  calculating the entropy for each character in the given text. 
  entropy<- c(entropy,- sum(df.char$ent)) #we want to do for each 1K words and add next 1K words
}

plot(entropy, col = "orange") 
title(main = "Entropy of 1000-word parts")
# Customize axis ticks and labels
axis(1, at = seq(0, 152, by = 10))

entropy

#Confidence interval (0,95%) for the entropies of the text. Using t-test
confidence_interval <- t.test(entropy)$conf.int
confidence_interval



# naive bayes 


15109/4
LitPhilEssays1<-LitPhilEssays[1:3777,]
LitPhilEssays2<-LitPhilEssays[(3777+1):(2*3777),]
LitPhilEssays3<-LitPhilEssays[(2*3777+1):(3*3777),]
LitPhilEssays4<-LitPhilEssays[(3*3777+1):15109,]

LitPhilEssays3

#we take any sentence of size ca 10 words.

# Define the sentence
target_sentence <- "Make roome for others, as others have done for you."

#extract words from parts 
words_LitPhilEssays1 <- unnest_tokens(LitPhilEssays1, token, text, token = "words")
words_LitPhilEssays2 <- unnest_tokens(LitPhilEssays2, token, text, token = "words")
words_LitPhilEssays3 <- unnest_tokens(LitPhilEssays3, token, text, token = "words")
words_LitPhilEssays4 <- unnest_tokens(LitPhilEssays4, token, text, token = "words")

# Laplace smoothing parameter
alpha <- 1

#compute relative freqs of words of target_sentence for the 4 diff parts

counted_make_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "make"),]
freq_make_1 <- 95/38351 #(=counted_make_1 / words_LitPhilEssays1)
counted_make_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "make"),]
freq_make_2 <- 38/37290
counted_make_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "make"),]
freq_make_3 <- 72/38066
counted_make_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "make"),]
freq_make_4 <- 57/39384


counted_roome_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "roome"),] 
freq_roome_1 <- 1/38351 

counted_roome_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "roome"),] 
freq_roome_2 <- 0/37290

counted_roome_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "roome"),] 
freq_roome_3 <- 0/38066

counted_roome_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "roome"),]
freq_roome_4 <- 0/39384


counted_for_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "for"),]
freq_for_1 <- 422/38351

counted_for_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "for"),] 
freq_for_2 <-396/37290

counted_for_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "for"),] 
freq_for_3 <- 618/38066

counted_for_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "for"),]
freq_for_4 <- 506/ 39384


counted_others_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "others"),]
freq_others_1 <- 52/38351

counted_others_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "others"),] 
freq_others_2 <-18/37290

counted_others_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "others"),] 
freq_others_3 <- 15/38066

counted_others_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "others"),]
freq_others_4 <- 31/ 39384


counted_as_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "as"),]
freq_as_1 <- 431/38351

counted_as_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "as"),] 
freq_as_2 <- 344/37290

counted_as_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "as"),] 
freq_as_3 <- 450/38066

counted_as_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "as"),]
freq_as_4 <- 657/ 39384


counted_others_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "others"),]
freq_others_1 <- 52/38351

counted_others_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "others"),] 
freq_others_2 <- 18/37290

counted_others_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "others"),] 
freq_others_3 <- 15/38066

counted_others_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "others"),]
freq_others_4 <- 31/ 39384


counted_have_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "have"),]
freq_have_1 <- 269/38351

counted_have_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "have"),] 
freq_have_2 <- 169/37290

counted_have_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "have"),] 
freq_have_3 <- 169/38066

counted_have_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "have"),]
freq_have_4 <- 183/39384


counted_done_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "done"),]
freq_done_1 <- 11/38351

counted_done_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "done"),] 
freq_done_2 <- 7/37290

counted_done_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "done"),] 
freq_done_3 <- 4/38066

counted_done_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "done"),]
freq_done_4 <- 27/39384


counted_for_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "for"),]
freq_for_1 <- 422/38351

counted_for_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "for"),] 
freq_for_2 <-396/37290

counted_for_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "for"),] 
freq_for_3 <- 618/38066

counted_for_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "for"),]
freq_for_4 <- 506/39384


counted_you_1 <-words_LitPhilEssays1[startsWith(words_LitPhilEssays1$token, "you"),]
freq_you_1 <- 205/38351

counted_you_2 <-words_LitPhilEssays2[startsWith(words_LitPhilEssays2$token, "you"),] 
freq_you_2 <- 54/37290

counted_you_3 <-words_LitPhilEssays3[startsWith(words_LitPhilEssays3$token, "you"),] 
freq_you_3 <- 78/38066

counted_you_4 <-words_LitPhilEssays4[startsWith(words_LitPhilEssays4$token, "you"),]
freq_you_4 <- 13/39384

#"Make roome for others, as others have done for you."

P1 <- freq_make_1*freq_roome_1*freq_for_1*freq_others_1*freq_as_1*freq_others_1*freq_have_1*freq_done_1*freq_for_1*freq_you_1*0.25
P2 <- freq_make_2*freq_roome_2*freq_for_2*freq_others_2*freq_as_2*freq_others_2*freq_have_2*freq_done_2*freq_for_2*freq_you_2*0.25
P3 <- freq_make_3*freq_roome_3*freq_for_3*freq_others_3*freq_as_3*freq_others_3*freq_have_3*freq_done_3*freq_for_3*freq_you_3*0.25
P4 <- freq_make_4*freq_roome_4*freq_for_4*freq_others_4*freq_as_4*freq_others_4*freq_have_4*freq_done_4*freq_for_4*freq_you_4*0.25
P <- c(P1, P2, P3, P4)

barplot(P,col = pastel_colors)

