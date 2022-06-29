#Necessary packages
packages <- c("tidyverse", "rvest")
#Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {install.packages(packages[!installed_packages])}

#Load packages
invisible(lapply(packages, library, character.only = TRUE))

#Create some functions
page_reader <- function(pdf, page){
  
  output <- pdf[page] #Pick out page number 3
  output <- strsplit(output, "\n") #Split into rows based on where there are line breaks
  output <- output[[1]] %>% as.data.frame() %>%rename(data = 1)
  
}


line_extract <- function(dataframe, text){
  
  output <- dataframe %>% 
    filter(grepl(text,data)) %>%
    mutate(data = gsub(",","",data),
           data = gsub(text," ",data)) %>%
    str_squish() %>%
    as.numeric(data)
}


tu_gov_link <- "https://www.gov.uk/government/publications/public-list-of-active-trade-unions-official-list-and-schedule/trade-unions-the-current-list-and-schedule"

annual_returns_list <- read_html(tu_gov_link) %>% #Read in gov url
  html_nodes("table") %>% #Identify table objects
  .[[1]]  %>% #Take first table
  html_nodes(xpath=paste0("//a[text()='Annual returns']")) %>% #Find all nodes featuring a url with label of annual returns 
  html_attr("href")

i <- "https://www.gov.uk/government/publications/accord-annual-returns"

links <- data.frame()

for (i in annual_returns_list) {
  
  temp_headings <- read_html(i) %>%  html_nodes('h3') %>% html_text() %>% as.data.frame() %>% rename(link = 1) %>%
    filter(!grepl("GOV.UK",link)) %>%
    filter(!grepl("Search",link)) 
  
  for (j in temp_headings$link) {
    
    temp_links <- read_html(i) %>% html_nodes(xpath=paste0("//a[text()='", j, "']")) %>% 
      html_attr("href") %>% 
      as.data.frame()
    
    links <- bind_rows(temp_links,links)
    
  }
  
  
  report <- pdf_text("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/995825/588T_2020.pdf")
  
  #First, get the membership numbers
  row_labels <- data.frame(sex = c("male","female","other","total"))
  page_3 <- page_reader(report,3)
  
  membership_numbers <- page_3 %>% #Pick out first item in list (which is the page 3 as thats the only one in the sample)
    slice(8:14) %>% #Select only rows the table appears on
    mutate(total = str_sub(data,start = -10), #Select only the final column, as this is where totals appear.
           total = gsub(",","",total),
           total = as.numeric(total)) %>%
    na.omit() %>% 
    select(total) %>%
    bind_cols(row_labels,.)
  
  #Next get total annual income from members
  
  page_6 <- page_reader(report,6)
  
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
    value = c(line_extract(page_6, "From Members: Contributions and Subscriptions"),
              line_extract(page_6, "Total other income from members"),
              line_extract(page_6, "Investment income \\(as at page 12\\)"),
              line_extract(page_6, "Total of other income \\(as at page 4\\)"),
              line_extract(page_6, "Benefits to members \\(as at page 5\\)"),
              line_extract(page_6, "Administrative expenses \\(as at page 10\\)"),
              line_extract(page_6, "Total expenditure Federation and other bodies"),
              line_extract(page_6, "Taxation"),
              line_extract(page_6, "Surplus \\(deficit\\) for year")
    ))  
  
  page_28 <- page_reader(report,28)
  
  balance_sheet <- data.frame(
    item = c("fixed_assets",
             "investment_assets",
             "other_assets",
             "total_liabilities",
             "net_assets"
    ),
    value = c(line_extract(page_28, "Fixed Assets"),
              line_extract(page_28, "Investment Assets"),
              line_extract(page_28, "Other Assets"),
              line_extract(page_28, "Liabilities                                                          Total Liabilities"),
              line_extract(page_28, "Net Assets \\(Total Assets less Total Liabilities\\)")
    ))