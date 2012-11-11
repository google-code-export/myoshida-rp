@ECHO off
::
:: PlantUML Execute
:: 

:: SET GRAPHVIZ_DOT=d:\Programme\Dev\graphviz\bin\dot.exe

SET CONFIG_FILE="%~dp0\config.txt"
SET PLANTUML_JAR="%~dp0\plantuml.jar"

java -jar %PLANTUML_JAR% -config %CONFIG_FILE% %*
