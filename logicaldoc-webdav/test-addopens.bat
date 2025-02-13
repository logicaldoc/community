rem java -XX:+IgnoreUnrecognizedVMOptions --add-opens=java.activation/*=ALL-UNNAMED --add-opens=java.transaction/*=ALL-UNNAMED --add-opens=java.xml.ws.annotation/*=ALL-UNNAMED --add-opens=java.xml.ws/*=ALL-UNNAMED --add-opens=java.xml.bind/*=ALL-UNNAMED --add-opens=java.corba/*=ALL-UNNAMED -jar logicaldoc-installer.jar

rem SET ANT_OPTS="--add-modules java.util"
rem SET ANT_OPTS="--add-opens=java.util"
SET ANT_OPTS=

SET JDK_JAVA_OPTIONS=--add-opens java.base/java.lang=ALL-UNNAMED --add-opens=java.base/java.util=ALL-UNNAMED --add-opens java.base/java.lang.reflect=ALL-UNNAMED --add-opens java.base/java.text=ALL-UNNAMED --add-opens java.desktop/java.awt.font=ALL-UNNAMED
mvn test