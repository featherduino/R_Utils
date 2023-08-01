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

#######################

library(stringr)
filtered_data <- subset(table_lead_overview, log_type=='QUERY-FORM')
urls <- str_match_all(log_text, "(https?://\\S+)(?: \\[ (\\d+) Sec \\])?")

extract_urls <- function(text) {
  url_pattern <- "(https?://\\S+)(?: \\[ (\\d+) Sec \\])?"
  return(str_extract_all(text, url_pattern)[[1]])
}
data$urls <- sapply(filtered_data$log_body, extract_urls)


data_1 <- data.frame(
  url = unlist(data$urls),
  time_spent = as.numeric(unlist(lapply(urls, function(x) x[, 2])))
)

merged_column <- lapply(data$urls, function(urls_with_time) {
  paste(urls_with_time, collapse = " ")
})

l1<-as.vector(unlist(merged_column))
c1<-cbind(filtered_data,l1)

# New name for the specific column
new_column_name <- "urls_withtime_duration"

# Changing the name of a specific column
names(c1)[names(c1) == "l1"] <- new_column_name
View(c1)


####intersect(c1$lead_id,table_site_visits$lead_id))
table_contacts_email_lead_id<-table_contacts[,c("contact_value","lead_id")]

site_visits_source<-merge(c1,table_site_visits,by.x = 'lead_id',by.y = "lead_id")
site_visits_source_email<-merge(site_visits_source,table_contacts_email_lead_id,by.x = "lead_id")


site_visits_source_email_clean <- site_visits_source_email[, c(
  "lead_id", "log_type", "log_text", "log_time", "add_time.x",
  "urls_withtime_duration", "sitevisit_id", "sitevisit_summary", "sitevisit_date",
  "sitevisit_project_name", "sitevisit_budget_start", "sitevisit_budget_end",
  "sitevisit_client_profile", "sitevisit_client_comments", "add_time.y", "contact_value"
)]

library(googlesheets4) 
ss="https://docs.google.com/spreadsheets/d/1Rv0MUUl9WlkQwrucJ-IO5NomT-k4wFB7wLgZaYWlJUw/edit#gid=895037483"
sheet_write(site_visits_source_email_clean,ss,sheet = "Sheet5")

X<-site_visits_source_email_clean %>% group_by(lead_id,urls_withtime_duration) %>% summarise(n())
X$urls_clean_2<- str_replace_all(X$urls_clean, "<.*?>|'>|Name:|\\[.*\\]", "")





