library(dplyr)
library(ggplot2)
library(stringr)
rm(list=ls())

emails1 <- read.csv('ICIS2014_emails.csv', header=F)
names(emails1) <- c('User','Domain')
emails1$Year = '2014'

emails2 <- read.csv('ICIS2012_emails.csv', header=F)
names(emails2) <- c('User','Domain')
emails2$Year = '2012'

emails <- bind_rows(emails1, emails2)



#Filter out non-edu domains
d_clean <- emails %>%
  filter(!(Domain %in% c('gmail.com', '@','GMAIL.COM',
                         'gmail.co', 'googlemail.com','yahoo.com',
                         'yahoo.co.uk','yahoo.ca','yahoo.co.jp',
                         'yahoo.com.br','yahoo.com.tw','yahoo.de',
                         'yahoo.fr','yahoo.it','hotmail.ca',
                         'hotmail.com','hotmail.fr','hotmail.it'
                         )))

domaincount <- d_clean %>%
  group_by(Domain) %>%
  count(Domain) %>%
  arrange(n)

#How many emails/lab generally?
ggplot(data=domaincount, aes(domaincount$n)) + 
  geom_histogram(binwidth=1)
  
#How many babylabs are there? We sum 2 years here; so assume that a domain with just
#one email address was a collaborator or other non-babylab school (or a typo of a more common address)

domaincount %>%
  summarize(length(Domain)) 


domaincount %>%
  filter(n > 1) %>%
  summarize(length(Domain))
