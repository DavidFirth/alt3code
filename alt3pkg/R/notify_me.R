pushbullet <- function(head, body){
    pushbullet_key <- Sys.getenv("PUSHBULLET_APIKEY")
    system(paste("curl -s -u ",
                 pushbullet_key,
                 ": https://api.pushbullet.com/v2/pushes -d type=note -d title='",
                 head,
                 "' -d body='",
                 body,
                 "' > /dev/null",
                 sep = ""))
    }

slack <- function(head, body){
    slack_url <- Sys.getenv("SLACK_URL")
    system(paste("curl -X POST -H 'Content-type: application/json' --data '{\"text\":\"*",
                 head, "*\n", body, "\"}' ", slack_url, sep = ""))
    }
