# Build this Sources

You need JDK 11, Maven 3.6.3, Ant 1.9.5 to build this sources

1. Open a command shell to the folder where you unzipped the archive

1. go into folder: `build/poms`
   launch the command:
``` 
mvn install
```

1. go into folder: `community/logicaldoc/`
   launch the command: 
```
mvn -Dmaven.test.skip=true install
```
   
   on the subfolder: `community/logicaldoc/logicaldoc-web/target`
   you should find the .war archive containing the web application
   

For more instructions regarding maven installation or better workspace organization
you can read the LogicalDOC Developer's Guide
https://wiki.logicaldoc.com/wiki/Developer_Guide