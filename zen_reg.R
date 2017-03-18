library(DBI)       #для работы RMySQL
library(RMySQL)    #работа с БД phpmyadmin
library(RSelenium) #навигация в Интернет
library(mailR)     #рассылка почты

#===basic settings===#
setwd("D:/OneDrive/myRepos/zenlix_registration") #рабочая директория

args <-
  commandArgs(trailingOnly = TRUE) #чтение аргументов из bat-файла

zDmin <- readLines("../etpp.txt") #скрытые параметры

if (args[1] == "atHome") {
  host <- zDmin[7]
} else if (args[1] == "atWork") {
  host <- zDmin[6]
}

#===функции RMySQL===#
conSELECT <- function(q, FUN = dbGetQuery) {
  #создание соединения каждый раз при обращении к БД
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

#===выборка новых анкетных записей===#

#список логинов активных пользователей (status = 1)
regLogs <- conSELECT("SELECT login FROM users WHERE status=1")

#список пользователей из google forms с проверенными анкетами
allForms <-
  read.csv(url(zDmin[8]),
           encoding = "UTF-8",
           stringsAsFactors = FALSE)

okForms <- subset(allForms, status == "ok")

#список новых пользователей для регистрации
new <- okForms[!tolower(okForms$mail_p) %in% regLogs$login,]

#проверка наличия данных для внесения в БД
if (nrow(new) != 0) {
  #===вход в админ-панель===#

  #проверка статуса сервера и остановка, если был запущен в фоне
  oldw <- getOption("warn")
  options(warn = -1) #warns off

  isSeleniumRunning <-
    tryCatch(
      readLines("http://localhost:4444/wd/hub/status"),
      error = function(e) {
        #no error message here
      }
    )

  if (!inherits(isSeleniumRunning, "try-error")) {
    system2("curl.exe", args = "-s http://localhost:4444/extra/LifecycleServlet?action=shutdown") #-s for silent mode
  }

  options(warn = oldw) #warns on

  #ручной старт сервера с параметрами по умолчанию (firefox, порт 4444)
  shell("run_selenium_server.bat")
  Sys.sleep(20) #ожидание запуска сервера - 20 сек. (для рабочего ПК)
  remDr <- remoteDriver()

  #открытие браузера и навигация
  remDr$open(silent = TRUE) #без вывода сообщений в консоль
  remDr$navigate("http://online.e-tpp.org")
  remDr$setImplicitWaitTimeout(milliseconds = 5000)
  remDr$findElement(using = "css", "input[name = 'login']")$sendKeysToElement(list(zDmin[1]))
  remDr$findElement(using = "css", "input[name = 'password']")$sendKeysToElement(list(zDmin[2]))
  remDr$findElement(using = "class", "btn-block")$clickElement()


  #===цикл обработки новых записей===#
  for (i in 1:nrow(new)) {
    uName <- paste(new$l_name[i], new$f_name[i], sep = " ")

    #генерация пароля без цифры "0" и буквы "O"
    uPass <-
      paste(sample(c(1:9, LETTERS[-15]), 6, replace = TRUE), collapse = "")

    uAdr <-
      paste(new$adr_str[i], new$adr_city[i], new$adr_index[i], sep = ", ")

    uLogin <- tolower(new$mail_p[i])

    #===создания нового пользователя===#
    remDr$navigate("http://online.e-tpp.org/users?create")
    Sys.sleep(3)
    remDr$findElement(using = "css", "input[name = 'login_user']")$sendKeysToElement(list(uLogin))
    remDr$findElement(using = "css", "input[id = 'exampleInputPassword1']")$sendKeysToElement(list(uPass))
    remDr$findElement(using = "css", "input[id = 'fio_user']")$sendKeysToElement(list(uName))
    remDr$findElement(using = "class", "btn-success")$clickElement()
    Sys.sleep(3)

    oldw <- getOption("warn")
    options(warn = -1) #warns off

    #id нового пользователя
    uID <-
      conSELECT(paste("SELECT id FROM users  WHERE login='", uLogin, "';", sep =  ""))

    options(warn = oldw) #warns on

    #заполнение доп. полей таблицы user_data
    if (length(uID) != 0) {
      #название предприятия
      if (!is.na(new$company[i])) {
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

      #ЕДРПОУ предприятия
      if (!is.na(new$edrpou[i])) {
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

      #телефон предприятия
      if (!is.na(new$tel_comp[i])) {
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

      #сайт предприятия
      if (!is.na(new$web[i])) {
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

      #направление деятельности
      if (!is.na(new$activity[i])) {
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

      #отдел
      if (!is.na(new$dep[i])) {
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

      #должность
      if (!is.na(new$pos[i])) {
        SQL <-
          paste("UPDATE users SET posada='",
                new$pos[i],
                "' WHERE id=",
                uID,
                ";",
                sep = "")
        conINSERT(SQL)
      }

      #заполнение полей таблицы users
      #почта
      SQLmail <-
        paste("UPDATE users SET email='",
              new$mail_p[i],
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      conINSERT(SQLmail)

      #телефон
      SQLtel <-
        paste("UPDATE users SET tel='",
              new$tel[i],
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      conINSERT(SQLtel)

      #адрес
      SQLadr <-
        paste("UPDATE users SET adr='",
              uAdr,
              "' WHERE id=",
              uID,
              ";",
              sep = "")
      conINSERT(SQLadr)

      #===рассылка уведомления===#
      #письмо по абзацам
      zLetter <-
        read.csv("zLetter.csv",
                 encoding = "UTF-8",
                 stringsAsFactors = FALSE)

      #отправка
      send.mail(
        from = c(zDmin[10]),
        to = new$mail_p[i],
        bcc = "admin@e-tpp.org",
        replyTo = c(zDmin[11]),
        subject = paste(zLetter$p0, uName, sep = ""),
        body = paste(
          zLetter$p1,
          "\n\n",
          zLetter$p2,
          "\n\n",
          zLetter$p3,
          "\n",
          zLetter$p4,
          uLogin,
          "\n",
          zLetter$p5,
          uPass,
          "\n\n",
          zLetter$p6,
          "\n\n",
          zLetter$p7,
          "\n---\n",
          zLetter$p8,
          "\n",
          zLetter$p9,
          "\n",
          zLetter$p10,
          "\n",
          zLetter$p11,
          sep = ""
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

  #окончание сессии и ручная остановка сервера(!)
  remDr$close()
  system2("curl.exe", args = "-s http://localhost:4444/extra/LifecycleServlet?action=shutdown") #-s for silent mode


} else {
  cat("You've got no users to register")
}
