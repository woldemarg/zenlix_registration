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
connectRead <- function(t, FUN = dbReadTable) {
  zCon <-
    dbConnect(
      RMySQL::MySQL(),
      host = host,
      dbname = zDmin[9],
      user = zDmin[3],
      password = zDmin[4]
    )
  val <- FUN(zCon, t)
  dbDisconnect(zCon)#обязательное закрытие соединения
  return(val)
}
connectInsert <- function(q, FUN = dbExecute) {
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
rD <- rsDriver()#открытие сессии
remDr <- rD[["client"]]
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

  uID <- numeric()
  counter <- 1#счетчик попыток регистрации

  #===цикл создания нового пользователя===#
  while (length(uID) == 0 & counter <= 2) {
    counter <- counter + 1

    #создание пользователя
    remDr$navigate("http://online.e-tpp.org/users?create")
    Sys.sleep(3)
    remDr$findElement(using = "css", "input[name = 'login_user']")$sendKeysToElement(list(new$mail_p[i]))
    remDr$findElement(using = "css", "input[id = 'exampleInputPassword1']")$sendKeysToElement(list(uPass))
    remDr$findElement(using = "css", "input[id = 'fio_user']")$sendKeysToElement(list(uName))
    remDr$findElement(using = "class", "btn-success")$clickElement()
    Sys.sleep(3)

    #id нового пользователя
    uID <- connectRead("users") %>%
      filter(login == new$mail_p[i]) %>% .$id

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
            "','Company name');",
            sep = ""
          )
        connectInsert(SQL)
      }

      if (!is.na(new$edrpou[i])) {
        #ЕДРПОУ предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','2','",
            new$edrpou[i],
            "','Company EDRPOU');",
            sep = ""
          )
        connectInsert(SQL)
      }

      if (!is.na(new$tel_comp[i])) {
        #телефон предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','3','",
            new$tel_comp[i],
            "','Company tel');",
            sep = ""
          )
        connectInsert(SQL)
      }

      if (!is.na(new$mail_comp[i])) {
        #почта предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','4','",
            new$mail_comp[i],
            "','Company mail');",
            sep = ""
          )
        connectInsert(SQL)
      }

      if (!is.na(new$web[i])) {
        #сайт предприятия
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','5','",
            new$web[i],
            "','Company Web');",
            sep = ""
          )
        connectInsert(SQL)
      }

      if (!is.na(new$activity[i])) {
        #направление деятельности
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','6','",
            new$activity[i],
            "','Company activity');",
            sep = ""
          )
        connectInsert(SQL)
      }

      if (!is.na(new$dep[i])) {
        #подразделение
        SQL <-
          paste(
            "INSERT INTO user_data (user_id,field_id,field_val,field_name) VALUES ('",
            uID,
            "','8','",
            new$dep[i],
            "','Company dep');",
            sep = ""
          )
        connectInsert(SQL)
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
        connectInsert(SQL)
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
      connectInsert(SQLmail)

      SQLtel <-
        paste("UPDATE users SET tel='",
              #телефон нового пользователя
              new$tel[i],
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      connectInsert(SQLtel)

      SQLadr <-
        paste("UPDATE users SET adr='", #адрес нового пользователя
              uAdr,
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      connectInsert(SQLadr)


      #===рассылка уведомления===#
      send.mail(
        from = c(zDmin[10]),
        to = new$mail_p[i],
        bcc = "admin@e-tpp.org",
        replyTo = c(zDmin[11]),
        subject = paste("Реєстрація нового користувача:", uName, sep = " "),
        body = paste(
          "Доброго дня!\n\nДякуємо за реєстрацію в системі обробки електронних заявок online.e-tpp.org.\n\nДля входу в систему використовуйте:\nлогін:",
          new$mail_p[i],
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

#окончание сессии
remDr$close()
rD[["server"]]$stop()
