# Open Source Document Management Software
LogicalDOC Community Edition is an [Open Source Document Management Software](https://www.logicaldoc.com/download-logicaldoc-community). If you are looking for a free cost then "Community" is your best option. LogicalDOC [document management system (DMS)](https://www.logicaldoc.com/solutions/document-management) allows businesses to control the production, storage, management and distribution of electronic documents, yielding greater effectiveness and the ability to reuse information and to control the flow of the documents.

LogicalDOC integrates all essential documents management, collaboration and an advanced search functionality into one easy to use solution. The system also includes administration tools to define the roles of various users, access control, user quota, level of document security, detailed logs of activity and automations setup.

LogicalDOC builds a highly valuable repository of corporate information assets to facilitate [knowledge management](https://www.logicaldoc.com/solutions/knowledge-management) and improve business decision making, boosting workgroups and enterprise productivity through shared practices, greater, better customer relations, faster sales cycles, improved product time-to-market, and better-informed decision making.

With LogicalDOC Community Edition you can:
 * Collect information from any digital source.
 * Collaborate with colleagues on documents and projects.
 * Empower organizations to capitalize on accumulated knowledge by locating documents, experts, and information sources.
 * Embedded workflow engine to take control of your business case.
 * Automate task.
 
## Installing binaries
You can install LogicalDOC binaries from [SourceForge](https://sourceforge.net/projects/logicaldoc/):
 * **logicaldoc-community-installer-${Version}.zip**: The LogicalDOC installer assistant. Usage information at [using the installer](https://docs.logicaldoc.com/en/installation).
 * **logicaldoc-${Version}-tomcat-bundle.zip**: A bundle with Tomcat and the latest LogicalDOC web application ready to use.
 * **logicaldoc-webapp-${Version}.war**: Just the Java web application packaged in .war format (Web Application Archive)

## Installation wizard videos
### Linux
This video shows step by step the installation process of LogicalDOC Enterprise version in Linux.
(Note: the process is exactly the same of the open-source edition)

[![Enterprise version installation in Linux](https://img.youtube.com/vi/Al2Pi5e1wi0/0.jpg)](https://youtu.be/Al2Pi5e1wi0 "Enterprise version installation in Linux")

### Windows
This video shows step by step the installation process of LogicalDOC Enterprise version in Windows.

[![Enterprise installation process in Windows](https://img.youtube.com/vi/mmshkU6_glQ/0.jpg)](https://youtu.be/mmshkU6_glQ "Enterprise installation process in Windows")

## Building from Source
```sh
$ git clone [git-repo-url] logicaldoc-community
$ cd logicaldoc-community
$ cd build/poms
$ mvn clean install
$ cd ../../community/logicaldoc
$ mvn clean package
```

See also the file README.txt

## Documentation
 * [LogicalDOC Knowledge Center](https://docs.logicaldoc.com)
 * [Hardware and Software Requirements](https://www.logicaldoc.com/resources/system-requirements)
 * [Installation](https://docs.logicaldoc.com/en/installation)
 * [Install the Application on Ubuntu](https://docs.logicaldoc.com/en/installation/install-on-ubuntu/install-logicaldoc-ubuntu)
 * [LogicalDOC CE Migration Guide](https://wiki.logicaldoc.com/wiki/LogicalDOC_CE_Migration_Guide)
 * [Administrator's Guide](https://docs.logicaldoc.com/en/system/general-informations)
 * [User guide](https://docs.logicaldoc.com/en/entering-in-the-system)
 * [Developer's Guide](https://wiki.logicaldoc.com/wiki/Developer_Guide)
 * [Developers Manual](https://www.logicaldoc.com/documents/logicaldoc-devmanual.pdf)
 * [Web Services API](https://docs.logicaldoc.com/en/web-services-api)

## Reporting issues
LogicalDOC Community Edition is supported by developers and technical enthusiasts via [the forum](http://forums.logicaldoc.com) of the user community. If you want to raise an issue, please follow the below recommendations:
 * Before you post a question, please search the question to see if someone has already reported it / asked for it.
 * If the question does not already exist, create a new post. 
 * Please provide as much detailed information as possible with the issue report. We need to know the version of LogicalDOC, Operating System, browser and whatever you think might help us to understand the problem or question.

## License
[LogicalDOC Community Edition](https://www.logicaldoc.com/download-logicaldoc-community) is made available to the Open Source community under the [GNU Lesser General Public License v3.0](https://www.gnu.org/licenses/lgpl-3.0.en.html).
The LogicalDOC source code is available for the entire community, which is free to use, modify and redistribute under the premises of such license.

