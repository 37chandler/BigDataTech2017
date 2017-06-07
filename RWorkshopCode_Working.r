
# For general data manipulation
library(dplyr)
library(lubridate)
library(tidyr)

# For visualization
library(ggplot2)
library(scales)

# For cluster analysis/segmentation
library(cluster)
library(apcluster)

# For association analysis
library(arules)
library(arulesViz)

###############################################################
#             Gettting Started                                #
###############################################################


# Feel free to set the working directory via Session -> Set Working Directory,
# but if you do use the working.dir as empty string line.
working.dir <- "C:/Users/jchan/Dropbox/SpeakingGigs/20170607_Minneanalytics/"
# working.dir <- ""

input.transaction.file <- "OwnerTransactions_62.txt"
input.transaction.db <- "20170530_transaction_files.db"


# Let's read in the data from the flat file and count the 
# number of owners and check out the date range
d <- data.table::fread(paste0(working.dir,input.transaction.file),
                       na.strings=c("NULL","\\N"))
# I love data.table, but I'm not going to use it much in this 
# talk. It's way faster than any other way of reading in, though
# so we're using just fread. Note the weird missing data structure 
# here.


###############################################################
#             Manipulating Data                               #
###############################################################

# Let's just look at some data
d %>%
  sample_n(10)

# It'll be useful to have transaction date in POSIXct form
d <- d %>%
  mutate(trans_date = ymd_hms(datetime),
         item_sale = trans_type == "I" &
           total > 0 & 
           total < 100)

nrow(d)

# Number of owners
d %>% 
  distinct(card_no) %>%
  nrow

# Recs per owner. 14987 is *big*
d %>% 
  group_by(card_no) %>%
  summarize(recs = n()) %>%
  arrange(desc(recs))

# Time range within the data
d %>% 
  summarize(min(trans_date),
            max(trans_date))

# Basic verbs of dplyr
d %>%
  filter(grepl("bacon",tolower(description))) %>%
  sample_n(10)

d %>% 
  filter(card_no == 18171) %>%
  head

d %>%
  arrange(trans_date) %>%
  head

d %>%
  filter(20 < total,total < 100,trans_type=="I") %>%
  arrange(desc(total)) %>%
  head

d %>% 
  arrange(trans_date,card_no) %>% 
  select(trans_date,total,card_no) %>%
  head

# A nice example of piping. 
d %>%
  group_by(card_no) %>%
  filter(item_sale) %>%
  summarize(sales = sum(total)) %>%
  sample_n(10)




###############################################################
#             Working with Databases                          #
###############################################################

# Now same metrics for DB data source

d.db <- src_sqlite(paste0(working.dir,input.transaction.db))
trans.tbl <- tbl(d.db,"transactions")

trans.tbl %>% 
  filter(trans_type == "I",
         0 < total,
         total < 100) %>%
  select(card_no,total,description) %>%
  arrange(desc(total)) %>%
  head


tbl(d.db,  
    sql("SELECT COUNT(*) FROM transactions"))

tbl(d.db,  
    sql("SELECT COUNT(DISTINCT card_no) FROM transactions"))


###############################################################
#             Segmentation                                    #
###############################################################

# Let's build a summary table with total spend
# number of visits and produce spend

cl.d.1 <- d %>% 
  filter(item_sale) %>%
  group_by(card_no) %>% 
  summarize(total_spend = sum(total),
            num_visits = length(unique(date(trans_date)))) %>% 
  ungroup

# We'll make a holder table for the produce spend. Which department?
d %>%
  filter(grepl("avocado",tolower(description))) %>%
  sample_n(10) # looks like 2

holder <- d %>% 
  filter(item_sale,
         department==2) %>%
  group_by(card_no) %>% 
  summarize(produce_spend = sum(total)) %>% 
  ungroup

cl.d.1 <- merge(cl.d.1,
                holder,
                by="card_no",
                all.x=T)

# NA's are zeros in produce
cl.d.1$produce_spend[is.na(cl.d.1$produce_spend)] <- 0

cl.1 <- pam(cl.d.1[,2:4],
            k=5)

cl.1$medoids

cl.d.1[cl.1$clustering==5,]
cl.d.1[cl.1$clustering==3,]


ap.cl <- apcluster(s=negDistMat(r=2),
                   x=cl.d.1[cl.d.1$total_spend < 10^5,2:4],
                   details=TRUE)

ap.cl

plot(x=ap.cl,
     y=cl.d.1[cl.d.1$total_spend < 10^5,2:4])


# A better distance matrix. 

# Let's try to build percentages by department for 
# the top departments.
d %>% 
  filter(item_sale) %>% 
  group_by(department) %>% 
  summarize(sales=sum(total)) %>% 
  arrange(desc(sales))

top.depts <- c(1,2,4,8,10,3,13,11)

d2 <- d %>% 
  filter(item_sale,
         department %in% top.depts)

d2 <- d2 %>% 
  group_by(card_no,department) %>% 
  summarize(sales=sum(total)) %>% 
  group_by(department) %>% 
  mutate(sales_rank = rank(sales)) %>% 
  ungroup

# a fun trick from tidyr to get the data in the right shape
d2 <- spread(data=d2[,-3],key=department,value=sales_rank,fill=0)

# Now play around with the clustering algorithms and see
# if you get more interesting results. 



###############################################################
#             Association Analysis                            #
###############################################################

# Association modeling needs it's own special data
# format. Let's limit to the top 100 items.
item.sales <- d %>%
  filter(item_sale) %>% 
  mutate(description=tolower(description)) %>%
  group_by(description) %>% 
  summarize(sales=sum(total)) %>%
  arrange(desc(sales)) %>% 
  head(200)

# Let's build the transactions from the db
d3 <- trans.tbl %>%
  select(card_no,description) %>%
  collect(n=Inf) %>%
  mutate(description = tolower(description)) %>%
  filter(description %in% item.sales$description) %>% 
  unique # don't need to count multiples.


# The Apriori algorithm wants a matrix with a row 
# being an owner and a column being each item. 
owners <- unique(d3$card_no)
items <- unique(d3$description)

d3.mat <- matrix(0,nrow=length(owners),
                 ncol=length(items))
rownames(d3.mat) <- owners
colnames(d3.mat) <- items

# Loops in R are slow, typically
for (i in 1:nrow(d3)){
  this.owner <- d3$card_no[i]
  this.item <- d3$description[i]
  
  d3.mat[owners==this.owner,
         this.item==items] <- 1
}

d3.mat[1:10,1:20]


# Finally, convert to "transactions"
trans <- as(d3.mat,"transactions")

# arules has some nice visualizations
itemFrequencyPlot(trans,topN=20,type="absolute")


# Our data is now ready for the apriori algorithm
rules <- apriori(trans,
                 parameter=list(sup = 0.2, 
                                conf = 0.5,
                                target="rules",
                                minlen=3,
                                maxlen=4)) 
rules

rules<-sort(rules, decreasing=TRUE,by="confidence")

inspect(rules[1:10])

rules<-sort(rules, decreasing=TRUE,by="lift")

inspect(rules[1:10])

