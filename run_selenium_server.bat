@echo off
START "" /D "d:\OneDrive\myRepos\zenlix_registration\" java -jar selenium-server-standalone-3.0.1.jar -role node -servlet org.openqa.grid.web.servlet.LifecycleServlet -registerCycle 0 -port 4444
REM why start server manually? - https://github.com/woldemarg/zenlix_registration/issues
REM how to start server from batch? - https://forums.techguy.org/threads/solved-creating-a-batch-file-to-run-selenium-rc.975344/