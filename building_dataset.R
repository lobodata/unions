#Necessary packages
packages <- c("tidyverse", "rvest","pdftools","stringi","data.table","rtweet","httr")
#Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {install.packages(packages[!installed_packages])}

#Load packages
invisible(lapply(packages, library, character.only = TRUE))

#Create some functions

#First one which reads in pdf page, and produces a dataframe (with a row for each line)
page_reader <- function(pdf, page){
  
  output <- pdf[page] #Pick out page number 3
  output <- strsplit(output, "\n") #Split into rows based on where there are line breaks
  output <- output[[1]] %>% as.data.frame() %>%rename(data = 1)
  
}

#Second, one which filters that dataset (by row) and pulls out the relevant number.
line_extract <- function(dataframe, text,type,reject){
  
  output <- dataframe %>% 
    mutate(data = gsub("Oiher","Other",data)) %>%
    filter(grepl(text,data)) %>%
    filter(!grepl(reject,data)) %>%
    mutate(data = gsub(",","",data),
           data = gsub(text," ",as.character(data))) %>%
    str_squish()
  
  if(type == "number"){output <- output %>% as.numeric(data)}
  
  return(output)
}

#Third, one which finds which page a certain text is contained on

page_finder <- function(dataframe,text_identifier) {
  
details <- stri_detect_fixed(dataframe, text_identifier) %>% data.frame() %>%
  mutate(id = row_number()) %>% rename(list = 1) %>%
  filter(list == T) %>%
  pull(id) %>% as.numeric()

}


#Main link for list of trade unions
tu_gov_link <- "https://www.gov.uk/government/publications/public-list-of-active-trade-unions-official-list-and-schedule/trade-unions-the-current-list-and-schedule"

#Create list of all the urls to "annual returns" data for each TU
annual_returns_list <- read_html(tu_gov_link) %>% #Read in gov url
  html_nodes("table") %>% #Identify table objects
  .[[1]] %>% 
  html_nodes(xpath=paste0("//a[text()='Annual returns']")) %>% #Find all nodes featuring a url with label of annual returns 
  html_attr("href") %>%
  as.data.frame %>%
  rename(links = 1) %>%
  filter(links != "") %>%
  mutate(union = gsub("https://www.gov.uk/government/publications/","",links),
         union = gsub("-annual-returns","",union))

#Now roll through each of those links, getting the underlying URLS for the annual accounts

links <- data.frame()

for (i in annual_returns_list$links) {
  
  print(i)
  
  union_name <- annual_returns_list %>% filter(links == i) %>% pull(union)
  
  temp_headings <- read_html(i) %>%  html_nodes('h3') %>% html_text() %>% as.data.frame() %>% rename(link = 1) %>%
    filter(!grepl("GOV.UK",link)) %>%
    filter(!grepl("Search",link)) 
  
  for (j in temp_headings$link) {
    
    temp_links <- read_html(i) %>% html_nodes(xpath=paste0("//a[text()='", j, "']")) %>% 
      html_attr("href") %>% 
      as.data.frame() %>%
      mutate(union_name = union_name)
    
    links <- bind_rows(temp_links,links)
    
  }
  
}

#Limit this to just 2020 and 2021 returns
recent_links <- links %>%
  as.data.frame() %>%
  rename(link = 1) %>%
  filter(grepl("2020", link) |
           grepl("2021",link))
  
#Now read in each of those pdf urls, pulling the data we are interested in

union_dataset <- data.frame()

eng <- tesseract(language = "eng", options = list(tessedit_pageseg_mode = 4)) #I tried 1,3,4 and 12 on a difficult pdf. 4 was by far the best (as it is essentially designe dfor tables: https://pyimagesearch.com/2021/11/15/tesseract-page-segmentation-modes-psms-explained-how-to-improve-your-ocr-accuracy/ )

for (i in recent_links$link) {
  
  print(i)
  
  union_name <- recent_links %>% filter(link == i) %>% pull(union_name)
  
  report <- pdf_text(i)
  
  if(sum(nchar(report)) < 30000) {
  print("Reading file from scratch")
  pngfile <- pdftools::pdf_convert(i, dpi = 600)
  report <- tesseract::ocr(pngfile, engine = eng)
  }
  
  report_collapsed <- strsplit(report, "\n")
  
  #Find page numbers
  pg_details <- page_finder(report_collapsed, "Head or Main Office address")
  pg_membership <- page_finder(report_collapsed, "Return of Members")
  
  if(length(pg_membership) == 0) {
    pg_membership <- page_finder(report_collapsed, "Return of members")
  }
  
  
  pg_income <- page_finder(report_collapsed, "From Members: Contributions and Subscriptions")
  pg_bs <- page_finder(report_collapsed, "Net Assets (Total Assets less Total Liabilities)") %>% as.data.frame() %>% rename(cold = 1)
  
  if(length(pg_bs) == 0) {
    pg_bs <- page_finder(report_collapsed, "NET ASSETS (Total Assets less Total Liabilities)") %>% as.data.frame() %>% rename(cold = 1)
  }
  
  pg_bs_remove <- page_finder(report_collapsed, "Only Incorporate Bodies")
  pg_bs <- pg_bs %>% filter(! cold %in% pg_bs_remove) %>%pull(cold)
  
  
  #Check that we have a readable report
  check = pg_details * pg_membership * pg_income * pg_bs
  
  if(length(check) == 1) {
  #First get return details
  page_1 <- page_reader(report, pg_details)
  name <- line_extract(page_1, "Name of Trade Union:", type = "text",reject = "NO REJECT")
  date <- line_extract(page_1, "Year ended:", type = "text",reject = "NO REJECT")
  
  
  #First, get the membership numbers
  page_3 <- page_reader(report,pg_membership)
  membership_numbers <- data.frame(item = c("members"),
                                  value = c(line_extract(page_3, "Number of members at end of year contributing to the General Fund", type = "number",reject = "NO REJECT"))) %>%

    mutate(field = "membership")
  
  #Next get total annual income from members
  
  page_6 <- page_reader(report,pg_income)
  
  income_statement <- data.frame(
    item = c("income_members_contributions",
             "income_members_other",
             "income_investment",
             "income_other",
             "expenditure_benefits",
             "expenditure_admin",
             "expenditure_federation",
             "expenditure_tax",
             "net_income"
    ),
    value = c(line_extract(page_6, "From Members: Contributions and Subscriptions", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Total other income from members", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Investment income \\(as at page 12\\)", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Total of other income \\(as at page 4\\)", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Benefits to members \\(as at page 5\\)", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Administrative expenses \\(as at page 10\\)", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Total expenditure Federation and other bodies", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Taxation", type = "number",reject = "NO REJECT"),
              line_extract(page_6, "Surplus \\(deficit\\) for year", type = "number",reject = "NO REJECT")
    ))  %>%
    mutate(field = "income_statement")
  
  page_28 <- page_reader(report,pg_bs)
  
  balance_sheet <- data.frame(
    item = c("fixed_assets",
             "investment_assets",
             "other_assets",
             "total_assets",
             "total_liabilities_1",
             "total_liabilities_2",
             "net_assets"
    ),
    value = c(line_extract(page_28, "Fixed Assets", type = "number",reject = "NO REJECT"),
              line_extract(page_28, "Investment Assets", type = "number",reject = "NO REJECT"),
              line_extract(page_28, "Other Assets", type = "number",reject = "NO REJECT"),
              line_extract(page_28, "Total Assets", type = "number",reject = "Net Assets"),
              line_extract(page_28, "Liabilities                                                          Total Liabilities", type = "number",reject = "NO REJECT"),                                                         
              line_extract(page_28, "Liabilities Total Liabilities", type = "number",reject = "NO REJECT"),
              line_extract(page_28, "Net Assets \\(Total Assets less Total Liabilities\\)", type = "number",reject = "NO REJECT")
    )) %>%
    mutate(field = "balance_sheet")
  
carve <- balance_sheet %>% filter(grepl("total_liabilities",item)) %>% arrange(value) %>% slice(1) %>% mutate(item = "total_liabilities")
balance_sheet <- balance_sheet %>% filter(!grepl("total_liabilities",item)) %>% bind_rows(.,carve)

output_dataframe <- bind_rows(income_statement,balance_sheet,membership_numbers) %>%
                    mutate(union = name,
                           date = date,
                           id = union_name)

union_dataset <- bind_rows(union_dataset,output_dataframe)

  }
}

save(union_dataset, file = "union_dataset.RData")

#Now clean up dates, so that we can get the latest for each union.
clean <- union_dataset %>%
         mutate(clean_date = gsub("131","31",date),
                clean_date = gsub("1st","1",clean_date),
                clean_date = gsub("th","",clean_date),
                clean_date = gsub("nd","",clean_date),
                clean_date = gsub("130","30",clean_date),
                clean_date = gsub("\\(","",clean_date),
                clean_date = gsub("\\|","",clean_date),
                clean_date = gsub("\\{","",clean_date),
                clean_date = gsub("Friday","",clean_date),
                clean_date = gsub("Tuesday","",clean_date),
                clean_date = gsub("Monday","",clean_date),
                clean_date = gsub("Wednesday","",clean_date),
                clean_date = gsub("1905","2020",clean_date), #One union randomly report 1905 instead 2020.
                clean_date = gsub("\\:"," ",clean_date),
                clean_date = gsub('"8208 0) eS eR as',"",clean_date),
                clean_date = gsub("\\°","",clean_date),
                clean_date = str_squish(clean_date),

                final_date = as.Date(clean_date, format = "%d %B %Y"),
                final_date = fifelse(is.na(final_date),as.Date(clean_date, format = "%d.%b.%y"),final_date)) %>%
                
                select(-c(clean_date,date))
                
                
largest <- clean %>% group_by(union) %>% filter(field == "membership") %>% arrange(desc(final_date)) %>% slice(1)
wealthiest <- clean %>% group_by(union) %>% filter(item == "net_assets") %>% arrange(desc(final_date)) %>% slice(1)
expensive <- clean %>% filter(item == "income_members_contributions" | item == "members") %>% group_by(union,item)  %>% 
                       arrange(desc(final_date)) %>% slice(1) %>%ungroup() %>% select(-c(item)) %>%
                       spread(field,value) %>%
                       mutate(fee = income_statement / membership)
cash_flow <- clean %>% group_by(union) %>% filter(item == "net_income") %>% arrange(desc(final_date)) %>% slice(1)

#Twitter followers.


t <- read_html(tu_gov_link) %>% #Read in gov url
  html_nodes("table") %>% #Identify table objects
  .[[1]] %>%
  html_nodes(xpath=paste0("//a[@rel]")) %>% #Find all nodes featuring an external url 
  html_attr("href") %>%
  as.list()

# create token named "twitter_token"
appname <- "union_follower"
key <- "NLI15hBIC2uNoIXOV1z45pEbu"
secret <- "gavyxCMgdaJ7DCzHqz2iMhiVyNaq25hu5yY9Qhpa2kanOjwTNw"
access_token <- "1200141270796111872-vr36dl47lj2cRgVq7XoEtAxoW8krMd"
access_secret <- "PTg2eXDUCTv0IPROYabw372oQUpmbhkdOagn0IoP3uGXl"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


follower_count <- data.frame()

for ( i in t) {
  
print(i)

tryCatch(
  
twitter <-
  
  GET(i, timeout(10)) %>%
  
  read_html(i) %>%
  
  html_nodes('a') %>% 
  
  html_attr('href') %>%
  
  as.data.frame() %>%
  
  rename(links = 1) %>%
  
  filter(grepl("twitter.com",links)) %>%
  
  mutate(links = gsub("https://www.twitter.com/","",links),
         links = gsub("http://www.twitter.com/","",links),
         links = gsub("http://twitter.com/","",links),
         links = gsub("https://twitter.com/","",links),
         links = gsub("//www.twitter.com/","",links),
         
         links = gsub('\\/$', '', links),
         links = gsub('\\#\\!\\/','',links)) %>%
  
  filter(links != "theRCN?ref_src=twsrc%5Etfw") %>%
  
  filter(links != "share?url=https://www.artistsunion.scot/&text=Scottish+Artists+Union") %>%
  
  filter(links != "ucu?ref_src=twsrc%5Etfw") %>%
  
  filter(links != "BASW_UK?ref_src=twsrc%5Etfw") %>%

  slice(1) %>%
  
  pull(links),

error = function(e) { twitter <<- "Timed_out!" } )

print(twitter)

if(length(twitter) == 0) {

followers <- 0

} else if (twitter == "Timed_out!") {
  
  followers <- 0
  
} else if ( i == "http://www.naht.org.uk") {
  
  followers <- 0
  
} else {
  followers <- lookup_users(twitter) %>% 
    select(followers_count) %>%
    pull()

}

follower_df <- data.frame(union = c(i), followers = c(followers), handle = c(twitter))

follower_count <- bind_rows(follower_count,follower_df)

}

follower_removals <- c( #These are the actual companies, not the unions.
  "http://www.RSPB.org.uk",
  "http://www.thepfa.com",
  "http://www.bma.org.uk",
 "http://www.balfourbeatty.com"
)

follower_count <- follower_count %>% filter(! union %in% follower_removals)
save(follower_count, file = "follower_count.Rdata")


#Pay rise position
djt <- get_timeline("unitetheunion", n = 3200)



