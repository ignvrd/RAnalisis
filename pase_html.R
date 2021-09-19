
install.packages("rvest")

library(rvest)

web <- read_html("http://jiraprod.munimadrid.es/jira/issues/?filter=10101") %>% strsplit(split = "a")


html_nodes(web,"/jira/s/33ba9e8f54325bcef90d1209cc496596-CDN/-ouefzq/713005/622e909d521623ebc8b40950d83395a1/d302fde02b80c462b2798dc5fcafad3e/_/download/contextbatch/css/jira.global.look-and-feel,-_super/batch.css")
           
           
