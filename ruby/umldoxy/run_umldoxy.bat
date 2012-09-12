@ECHO off
REM Tool settings
SET PLANTUML_JAR=plantuml.jar
SET RUN_UMLDOXY_RB=run_umldoxy.rb


ruby "%RUN_UMLDOXY_RB%" "%PLANTUML_JAR%" %1

if %ERRORLEVEL% neq 0 (
  PAUSE
)

