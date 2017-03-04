library(tidyverse)
library(mailR)
library(knitr)
library(rmarkdown)

knit2html("reg_mail.Rmd",options="")

rmarkdown::render(input = "reg.Rmd",
                  output_format = "html_fragment",
                  output_file = "reg.html",
                  encoding = "UTF-8")

send.mail(
  from = c(zDmin[10]),
  to = "wldmrgml@gmail.com",
  replyTo = c(zDmin[11]),
  subject = paste("Реєстрація нового користувача:", uName, sep = " "),
  body = "reg.html",
  html = TRUE,
  #inline = TRUE,
  encoding = "utf-8",
  smtp = list(
    host.name = "smtp.gmail.com",
    port = 465,
    user.name = zDmin[12],
    passwd = zDmin[5],
    ssl = TRUE
  ),
  authenticate = TRUE,
  send = TRUE
)

default_options <- options()

readLines("reg.Rmd")
