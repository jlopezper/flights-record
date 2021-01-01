library(DBI)
library(RMySQL)

con <- dbConnect(DBI::dbDriver("MySQL"),
                 host = '178.62.218.45',
                 dbname = Sys.getenv('MYSQL_DATABASE'),
                 port = 3306,
                 user = Sys.getenv('MYSQL_USER'),
                 password = Sys.getenv('MYSQL_PASSWORD'))

