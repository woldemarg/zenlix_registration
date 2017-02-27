library(tidyverse)
library(RMySQL)
library(RSelenium)
library(mailR)


#===basic settings===#
zDmin <- readLines("../etpp.txt")

#host <- zDmin[6] #с работы
host <- zDmin[7] #из дома


#===выборка новых анкетных записей===#
new <- read_csv(url(zDmin[8])) %>%
  filter(is.na(status))


#===функции RMySQL===#
conSELECT <- function(q, FUN = dbGetQuery) {
  zCon <-
    dbConnect(
      RMySQL::MySQL(),
      host = host,
      dbname = zDmin[9],
      user = zDmin[3],
      password = zDmin[4]
    )
  val <- FUN(zCon, q)
  dbDisconnect(zCon)#закрытие соединения (!)
  return(val)
}
conINSERT <- function(q, FUN = dbExecute) {
  zCon <-
    dbConnect(
      RMySQL::MySQL(),
      host = host,
      dbname = zDmin[9],
      user = zDmin[3],
      password = zDmin[4]
    )
  FUN(zCon, "SET NAMES 'utf8'") #изменение кодировки
  FUN(zCon, q)
  dbDisconnect(zCon)
}


#===вход в админ-панель===#
#открытие сессии
rD <-
  rsDriver(
    port = 4444L,
    browser = "chrome",
    #or "latest"
    version = "3.1.0",
    #or "latest"
    chromever = "2.27",
    #or "latest"
    geckover = "0.14.0",
    #or "latest"
    iedrver = NULL,
    #or "latest"
    phantomver = NULL,
    #or "2.1.1"
    verbose = FALSE,
    check = FALSE #загрузка обновлений драйверов
  )

remDr <- rD[["client"]]

# remDr <-
#   remoteDriver(remoteServerAddr = "localhost",
#                port = 4444,
#                browserName = "firefox")
# remDr$open()

remDr$navigate("http://online.e-tpp.org")
remDr$setImplicitWaitTimeout(milliseconds = 5000)
remDr$findElement(using = "css", "input[name = 'login']")$sendKeysToElement(list(zDmin[1]))
remDr$findElement(using = "css", "input[name = 'password']")$sendKeysToElement(list(zDmin[2]))
remDr$findElement(using = "class", "btn-block")$clickElement()


#===цикл обработки записей===#
for (i in 1:nrow(new)) {
  uName <- paste(new$l_name[i], new$f_name[i], sep = " ")
  uPass <-
    paste(sample(c(1:9, LETTERS), 6, replace = TRUE), collapse = "")
  uAdr <-
    paste(new$adr_str[i], new$adr_city[i], new$adr_index[i], sep = ", ")
  uLogin <- tolower(new$mail_p[i])

  uID <- numeric()
  counter <- 1#счетчик попыток регистрации

  #===цикл создания нового пользователя===#
  while (length(uID) == 0 & counter <= 2) {
    counter <- counter + 1

    #создание пользователя
    remDr$navigate("http://online.e-tpp.org/users?create")
    Sys.sleep(3)
    remDr$findElement(using = "css", "input[name = 'login_user']")$sendKeysToElement(list(uLogin))
    remDr$findElement(using = "css", "input[id = 'exampleInputPassword1']")$sendKeysToElement(list(uPass))
    remDr$findElement(using = "css", "input[id = 'fio_user']")$sendKeysToElement(list(uName))
    remDr$findElement(using = "class", "btn-success")$clickElement()
    Sys.sleep(3)

    #откл. предупреждения
    oldw <- getOption("warn")
    options(warn = -1)

    #id нового пользователя
    uID <-
      conSELECT(paste("SELECT id FROM users  WHERE login='", uLogin, "';", sep =  ""))

    #вкл. предупреждения
    options(warn = oldw)

    #заполнение дополнительных полей,
    #если пользователь создан успешно
    if (length(uID) != 0) {
      if (!is.na(new$company[i])) {
        #название предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','1','",
            new$company[i],
            "','Company');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$edrpou[i])) {
        #ЕДРПОУ предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','2','",
            new$edrpou[i],
            "','EDRPOU');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$tel_comp[i])) {
        #телефон предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','3','",
            new$tel_comp[i],
            "','Tel.');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$mail_comp[i])) {
        #почта предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','4','",
            new$mail_comp[i],
            "','E-mail');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$web[i])) {
        #сайт предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','5','",
            new$web[i],
            "','Web');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$activity[i])) {
        #направление деятельности
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','6','",
            new$activity[i],
            "','Activity');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$dep[i])) {
        #подразделение
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','8','",
            new$dep[i],
            "','Department');",
            sep = ""
          )
        conINSERT(SQL)
      }

      if (!is.na(new$pos[i])) {
        #должность
        SQL <-
          paste("UPDATE users SET posada='",
                new$pos[i],
                "' WHERE id=",
                uID,
                ";",
                sep = "")
        conINSERT(SQL)
      }

      #обязательные поля
      SQLmail <-
        paste("UPDATE users SET email='",
              #почта нового пользователя
              new$mail_p[i],
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      conINSERT(SQLmail)

      SQLtel <-
        paste("UPDATE users SET tel='",
              #телефон нового пользователя
              new$tel[i],
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      conINSERT(SQLtel)

      SQLadr <-
        paste("UPDATE users SET adr='", #адрес нового пользователя
              uAdr,
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      conINSERT(SQLadr)


      #===рассылка уведомления===#
      send.mail(
        from = c(zDmin[10]),
        to = new$mail_p[i],
        bcc = "admin@e-tpp.org",
        replyTo = c(zDmin[11]),
        subject = paste("Реєстрація нового користувача:", uName, sep = " "),
        body = paste(
          "Доброго дня!\n\nДякуємо за реєстрацію в системі обробки електронних заявок online.e-tpp.org.\n\nДля входу в систему використовуйте:\nлогін:",
          uLogin,
          "\nпароль:",
          uPass,
          "\n\nОформити заявку на послуги управління експертиз Запорізької ТПП, а також змінити Ваші особові дані, пароль і налаштування електронного кабінету Ви зможете після входу в систему за адресою - http://e-tpp.org/podaty-zayavku/ \n\n\n--\nЗ повагою,\nкоманда експертів ЗТПП\nт. (061) 213-43-25\nф. (061) 213-50-27\nм. (050) 488-03-83\nнаш сайт: e-tpp.org",
          sep = " "
        ),
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

      cat(paste("Registration of", uName, "has been successful\n", sep = " "))
    }
  }

  #сообщение об ошибке после двух неудачных попыток регистрации
  if (length(uID) == 0) {
    cat(paste("Registration of", uName, "has failed\n", sep = " "))
  }
}

#окончание сессии (!)
remDr$close()
rD[["server"]]$stop()
