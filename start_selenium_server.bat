@echo off
START "" /D "d:\OneDrive\myRepos\zenlix_registration\" java -jar selenium-server-standalone-3.0.1.jar -role node -servlet org.openqa.grid.web.servlet.LifecycleServlet -registerCycle 0 -port 4444
REM why manually start server - https://github.com/woldemarg/zenlix_registration/issues