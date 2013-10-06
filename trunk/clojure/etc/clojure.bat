@ECHO off
set CLOJURE_JAR=""
for %%i in ("%~dp0clojure-*.jar") do (
    if %CLOJURE_JAR% leq %%i (
        set CLOJURE_JAR=%%i
    )
)

if %CLOJURE_JAR% == "" (
    echo "Not found. : clojure-x.x.x.jar"
) else (
    java -jar "%CLOJURE_JAR%" %*
)
