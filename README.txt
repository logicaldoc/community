            __                _            ______  ____  ______
           / /   ____  ____ _(_)________ _/ / __ \/ __ \/ ____/
          / /   / __ \/ __ `/ / ___/ __ `/ / / / / / / / /     
         / /___/ /_/ / /_/ / / /__/ /_/ / / /_/ / /_/ / /___   
        /_____/\____/\__, /_/\___/\__,_/_/_____/\____/\____/   
                     /____/                 


                      http://www.logicaldoc.com                     

                    LogicalDOC Community Edition


You need JDK 1.7, Maven 3.0.3, Ant 1.7 to build this sources

0) Open a command shell to the folder where you unzipped the archive

1) go into folder: build/poms
   launch the command: mvn install

2) go into folder: community/logicaldoc/
   launch the command: mvn -Dmaven.test.skip=true install
   
   on the subfolder: community/logicaldoc/logicaldoc-web/target
   you should find the .war archive containing the web application
   

For more instructions regarding maven installation or better workspace organization
you can read the LogicalDOC Developer's Manual
http://www.logicaldoc.com/documents/logicaldoc-devmanual.pdf