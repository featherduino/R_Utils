library(readxl)
lead_hero_dummy<-read_excel("2023-07-04-11-52-AM-leads.xlsx")
View(lead_hero_dummy)
lead_hero_may<-read_excel("2023-07-04-13-30-PM-leads.xlsx")
View(lead_hero_may)

lead_june<-read_excel("June.xlsx")
View(lead_june)
####List of KPIs######

summary(lead_june)
#########################Ads Data##########################
AdSpend<-read.csv("Leads-By-Campaign-region-date.csv",stringsAsFactors = FALSE)
AdSpend$Link.clicks[which(is.na(AdSpend$Link.clicks))]<-0
AdSpend$Clicks..all.[which(is.na(AdSpend$Clicks..all.))]<-0
AdSpend$Leads[which(is.na(AdSpend$Meta.leads))]<-0
Adspend_grouping_by_month <- AdSpend %>% group_by(Ad.set.name,Ad.name,Region) %>% summarise(sum(Reach),sum(Impressions),sum(Frequency),sum(Amount.spent..INR.),sum(Link.clicks),sum(Clicks..all.),sum(Meta.leads))
View(Adspend_grouping_by_month)

###############EAAEfuci7yyABAMS7BpB1F9TgRPhhgUosMsV431T8AcPNlOmHXi9C7cHDEDHXZAsfU0xmweS6lc6lQH3bc3jsJYzHypFt1NgHXm0tWyJCuZC4ZBkpMEZA038A8tYiCWzPAcGUGyEQlbAtb0ltkfh70oHHHPMcIh3piGR8tj7KslDkQqGELGGZB
library(RMySQL)
cn<-dbConnect(RMySQL::MySQL(), dbname = "lead_hero",username = 'root',
          password = 'new_password')

table_names<-RMySQL::dbListTables(cn) %>% as.data.frame()

tables <- dbGetQuery(cn, "SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE='BASE TABLE'")

result_df <- data.frame(Table = character(), Row_Count = integer(), stringsAsFactors = FALSE)

Table_names<-function(){
  for (table_name in tables$TABLE_NAME) {
    query <- paste("SELECT COUNT(*) as row_count FROM", table_name)
    result <- dbGetQuery(cn, query)
    result_df <- rbind(result_df, data.frame(Table = table_name, Row_Count = result$row_count))
  }
}




tables

table_lead<-RMySQL::dbReadTable(cn,"tbl_lead")
View(tables_metadat)

table_lead_overview<-RMySQL::dbReadTable(cn,"tbl_lead_overview")

View(table_lead_overview)
zapier<-dbReadTable(cn,"tbl_zapier_log")
colnames(table_lead)

table_lead_clean<-table_lead[,c("lead_id","lead_email","lead_phone","lead_contacted_time","stage_value","stage_id","group_id","lead_unactioned","screening_points","screening_by","screening_from_lead_pool","screening_summary","source_name","funnel_name","funnel_id","segment_name","segment_color","project_id","sitevisit_date","sitevisit_on","sitevisit_date_first","add_time","add_by","purchase_date","purchase_marked_by","reason_name","re_enquiries","re_enquiries_status","re_enquiries_date")]
table_lead_clean$year<-substr(table_lead_clean$add_time,1,4)
table_lead_clean<-filter(table_lead_clean,table_lead_clean$year=='2023')

head(table_lead_clean)


table_lead_overview$year<-substr(table_lead_overview$add_time,1,4)
table_lead_overview_clean<-filter(table_lead_overview,table_lead_overview$year=='2023')

table_lead_overview_clean_DB <-table_lead_overview_clean[,c("lead_id","log_type","log_text","add_by","add_time")]
nrow(table_lead_overview_clean_DB)
sheet_write(ss="https://docs.google.com/spreadsheets/d/1Rv0MUUl9WlkQwrucJ-IO5NomT-k4wFB7wLgZaYWlJUw/edit#gid=1470088784",table_lead_overview_clean_DB,sheet = "Sheet4")

################################################
num_rows<-lapply(table_names_fresh,function(table_name){
 tryCatch(nrow(dbReadTable(cn,table_name)))
})

num_rows<-unlist(list.flatten(num_rows))
tables_metadat<-cbind(table_names_fresh,num_rows)
#########Leads contacted#############
leads_contacted<-table_lead_clean[which(table_lead$lead_contacted==1),]

View(leads_contacted)

#########Leads not contacted/Lost#############
leads_not_contacted<-table_lead_clean[which(table_lead$lead_contacted==0),]
length(which(zapier$log_timestamp==''))

##########################
master_attribution<-inner_join(table_lead,zapier,by=c('lead_id'='id'))
View(master_attribution %>% group_by(enquiry_tag,funnel_name,segment_name,reason_name) %>% summarise(n()))
length(intersect(substr(zapier$log_timestamp,0,10),AdSpend$Day))

##########Zapier with Adspend Data###########
library(sqldf)
zapier$year <- substr(zapier$log_timestamp,0,4)
zapier_this_year<-filter(zapier, year == '2023')
zapier_this_year$day<-substr(zapier_this_year$log_timestamp,0,10)

test_1<-left_join(AdSpend,zapier_this_year,by=c('Day'='day'),multiple = "all")
##Drop Cols
 test_1 <- subset(test_1, select = -c(Purchases,Cost.per.purchase,Purchases.conversion.value,Account.name))
 test_1 <- subset(test_1, select = -c(Clicks..all.,CPC..cost.per.link.click.,Link.clicks,Account.name))
 
#######
 
library(googlesheets4) 
 ss="https://docs.google.com/spreadsheets/d/1jDo_ikUevaeHCORnO4tA7TKO_CbRjVwQO_uOkRwCtuQ/edit#gid=1846600269"
 googlesheets4::sheet_write(test_1,ss,sheet = 1)
 
###############################################################
table_lead_clean$year<-substr(table_lead_clean$add_time,1,4)
table_lead_clean_2023<-filter(table_lead_clean,table_lead_clean$year=='2023')
nrow(table_lead_clean_2023)
#################Trace of leads Contacted##########
trace_contacted<-merge(table_lead_overview_clean,table_lead_clean_2023,by="lead_id",all.x = TRUE)
trace_contacted$log_type[which(trace_contacted$log_type=='INFORMATION')] <- "INFO"
trace_contacted<-trace_contacted[,c("lead_id","log_type","add_by.x","add_time.x","lead_email","lead_phone","lead_contacted_time",
                                    "funnel_name","segment_name","source_name","stage_value","screening_points","year.y","purchase_date","purchase_")]


nrow(table_lead_overview)

nrow(trace_contacted)

View(trace_contacted %>% group_by(lead_id,lead_email) %>% summarise(n()) %>% arrange(desc(`n()`)))
X<-trace_contacted %>% group_by(lead_id,trace_contacted$add_time.x) %>% summarise(time_difference=difftime(max(add_time.x),min(add_time.x))) %>% arrange(desc(time_difference))


#################Trace of leads Sankey##########


transition_counts <- trace_contacted %>%
  group_by(add_time.x_day_month,log_type) %>%
  summarise(transition_count = n()) %>%
  pivot_wider(names_from = log_type, values_from = transition_count, values_fill = 0)

trace_contacted$add_by.x
transition_counts <- transition_counts %>%
  select(-c("EMAIL-PROFILE", "SOURCE", "RECEIVED EMAIL", "STAGE"))

# Calculate column sums
column_sums <- colSums(transition_counts[, -1])

# Sort column sums in descending order
sorted_column_sums <- sort(column_sums, decreasing = TRUE)

# Display the sorted column sums
print(sorted_column_sums)

View(zapier)


