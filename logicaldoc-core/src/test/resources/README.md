# LogicalDOC Enterprise Edition  
Official Docker Image for LogicalDOC Enterprise Edition - https://www.logicaldoc.com

This **Dockerfile** is a [published build](https://hub.docker.com/r/logicaldoc/logicaldoc) on [Docker Hub](https://hub.docker.com/).

**Note:** This image requires to be connected to an external database

## What is LogicalDOC ?
LogicalDOC is a flexible and highly performant Document Management System platform
By leveraging on the best-of-breed Java frameworks, it creates a flexible and powerful document management platform, which thanks to the most advanced presentation technology (Google GWT), is able to meet the needs of usability and more demanding management.
LogicalDOC is both document management and collaboration system. The software is loaded with many functions and allows organizing, index, retrieving, controlling and distributing important business documents securely and safely for any organization and individual.

![LogicalDOC](https://www.logicaldoc.com/images/assets/LogicalDocWhiteH02-167.png)

* **Manuals**: https://docs.logicaldoc.com
* **Twitter**: https://twitter.com/logicaldoc
* **Blog**: https://www.logicaldoc.com/blog

## Your UserNo
You have to pass your activation code(the UserNo) when you launch this image.
If you need an activation code, you can get one delivered to your email by filling-out the form at https://www.logicaldoc.com/try

## Start a LogicalDOC instance linked to a MySQL container
1. Run the MySQL container
```Shell
docker run -d --name=logicaldoc-db -e MYSQL_ROOT_PASSWORD=mypassword -e MYSQL_DATABASE=logicaldoc -e MYSQL_USER=ldoc -e MYSQL_PASSWORD=changeme mysql:8.0 --default-authentication-plugin=mysql_native_password
```

2. Run the LogicalDOC container
```Shell
docker run -d -p 8080:8080 -p 8022:22 -e LDOC_USERNO=<your userno> --link logicaldoc-db logicaldoc/logicaldoc
```

This image includes EXPOSE 8080 (the logicaldoc port). The default LogicalDOC configuration is applied.

Then, access it via `http://localhost:8080` or `http://host-ip:8080` in a browser. Default User and Password are **admin** / **admin**.

Notes:
In the most recent versions of MySQL it is necessary to enable native authentication, otherwise LogicalDOC will not be able to connect. 
To do this, simply add the default-authentication-plugin command line argument to the container launch string
e.g.:
```Shell
docker run -d --name=logicaldoc-db -e MYSQL_ROOT_PASSWORD=mypassword -e MYSQL_DATABASE=logicaldoc -e MYSQL_USER=ldoc -e MYSQL_PASSWORD=changeme mysql:8.0.23 --default-authentication-plugin=mysql_native_password 
```
or with the latest MySQL 8 image
```Shell
docker run -d --name=logicaldoc-db -e MYSQL_ROOT_PASSWORD=mypassword -e MYSQL_DATABASE=logicaldoc -e MYSQL_USER=ldoc -e MYSQL_PASSWORD=changeme mysql:latest --default-authentication-plugin=mysql_native_password
```


## Start a LogicalDOC with some settings
```Shell
docker run -d -p 8080:8080 -p 8022:22 -e LDOC_USERNO=<your userno> -e LDOC_MEMORY=4000 --link logicaldoc-db logicaldoc/logicaldoc
```
This will run the same image as above but with 4000 MB memory allocated to LogicalDOC.

Then, access it via `http://localhost:8080` or `http://host-ip:8080` in a browser.

If you'd like to use an external database instead of a linked `mysql-ld` container, specify the hostname with `DB_HOST` and port with `DB_PORT` along with database name `DB_NAME`, the password in `DB_PASSWORD` and the username in `DB_USER` (if it is something other than `ldoc`):

```console
$ docker run -d -p 8080:8080 -p 8022:22 -e DB_HOST=10.1.2.3 -e DB_PORT=3306 -e DB_USER=... -e DB_PASSWORD=... logicaldoc/logicaldoc
```

## Persistence of configuration and documents
Start as a daemon with attached volumes to persist the configuration and the documents
```console
$ docker run -d --name logicaldoc --restart=always -p 8080:8080 -v /path/conf:/LogicalDOC/conf -v /path/repository:/LogicalDOC/repository --link logicaldoc-db logicaldoc/logicaldoc
```

All document files will be stored in the host path specified by ``/path/conf``, the configuration files insead will be stored in ``/path/repository``


## Persistence of the whole LogicalDOC's deployment
Start as a daemon with attached volume to persist all the deployment
```console
$ docker run -d --name logicaldoc --restart=always -p 8080:8080 -v /path/conf:/LogicalDOC/conf -v /path/repository:/LogicalDOC/repository -v /mount-LogicalDOC:/path  --link logicaldoc-db logicaldoc/logicaldoc
```

The deployment(that also includes the conf and repository volumes) gets persisted in the host path specified by ``/path``


## Environment Variables
The LogicalDOC image uses environment variables that allow to obtain a more specific setup.

* **LDOC_USERNO**: your own license activation code ([`click here to get a fee trial code`](https://www.logicaldoc.com/try))
* **LDOC_MEMORY**: memory allocated for LogicalDOC expressed in MB (default is 2000)
* **DB_ENGINE**: the database type, possible vaues are: mysql (default), mssql, oracle, postgres, mariadb
* **DB_HOST**: the database server host (default is 'logicaldoc-db')
* **DB_PORT**: the database communication port (default is 3306)
* **DB_NAME**: the database name (default is 'logicaldoc')
* **DB_INSTANCE**: some databases require the instance specification
* **DB_USER**: the database username (default is 'ldoc')
* **DB_PASSWORD**: the database password (default is 'changeme')
* **DB_MANUALURL**: must be true when using DB_URL (default is 'false')
* **DB_URL**: the jdbc url to connect to the database (remember to set DB_MANUALURL to 'true')
* **SSH_USER**: the username to connect via SSH (default is 'logicaldoc')
* **SSH_PASSWORD**: the password to connect via SSH (default is 'changeme')



## Stopping and starting the container
Assuming that you have assigned the "logicaldoc" alias to the container

To stop the container use:

```console
$ docker stop logicaldoc
```

To start the container again:

```console
$ docker start logicaldoc
```

## Configuration
(You must have enabled data persistence with volume assignment)

To edit the settings file, check the physical location of the ``logicaldoc-conf`` volume using:

```console
$ docker volume inspect logicaldoc-conf
```

Which should produce an output similar to this one:

```console
    [
        {
            "Name": "logicaldoc-conf",
            "Driver": "local",
            "Mountpoint": "/var/lib/docker/volumes/logicaldoc-conf/_data",
            "Labels": null,
            "Scope": "local"
        }
    ]
```
In this case the physical location of the ``logicaldoc-conf`` volume is ``/var/lib/docker/volumes/logicaldoc-conf/_data``.

## Performing backups

To backup the existing data, check the physical location of the ``logicaldoc-conf`` and ``logicaldoc-repo`` volumes using:

```console
$ docker volume inspect logicaldoc-conf
```

Which should produce an output similar to this one:

```console
    [
        {
            "Name": "logicaldoc-conf",
            "Driver": "local",
            "Mountpoint": "/var/lib/docker/volumes/logicaldoc-conf/_data",
            "Labels": null,
            "Scope": "local"
        }
    ]
```

```console
$ sudo tar -zcvf backup.tar.gz /var/lib/docker/volumes/logicaldoc-conf/_data /var/lib/docker/volumes/logicaldoc-repo/_data
$ sudo chown `whoami` backup.tar.gz
```

If an external PostgreSQL or MySQL database or database containers, these too need to be backed up using their respective procedures.


## Restoring from a backup

Uncompress the backup archive in the original docker volume using:

```console
$ sudo tar -xvzf backup.tar.gz -C /
```

## Building the image

Clone the repository with:

```console
$ git clone https://github.com/logicaldoc/docker.git
```

Change to the directory of the cloned repository:

```console
$ cd docker
```

Execute Docker's build command:

```console
$ docker build -t logicaldoc/logicaldoc .
```

Or using an apt cacher to speed up the build:

```console
$ docker build -t logicaldoc/logicaldoc --build-arg APT_PROXY=172.18.0.1:3142 .
```

Replace the IP address `172.18.0.1` with the IP address of the Docker host used from which these commands are running.

## Using Docker compose

To deploy a complete production stack using the included Docker compose file execute:

```console
$ docker-compose -f docker-compose.yml up -d
```

This Docker compose file will provision 2 containers:

- MySQL as the database
- LogicalDOC using the above service container

To stop the stack use:

```console
$ docker-compose -f docker-compose.yml stop
```

The stack will also create three volumes to store the data of each container. These are:

- ldoc_conf - The LogicalDOC configuration container, normally called `logicaldoc-conf` when not using Docker compose.
- ldoc_repository - The LogicalDOC DMS data container for documents and search indexes, normally called `logicaldoc-repo` when not using Docker compose.
- ldoc_db - The database volume, in this case MySQL.

### Stopping and starting with Docker compose 

To stop the services use:

```console
$ docker-compose -f docker-compose.yml stop
```

To start the services again:

```console
$ docker-compose -f docker-compose.yml start
```

To remove the stopped containers:

```console
$ docker-compose -f docker-compose.yml rm -v
```

Destroys the containers and all the created volumes:

```console
$ docker-compose -f docker-compose.yml down -v
```

### Docker compose examples
Some docker-compose examples are available in the repository of this container on GitHub https://github.com/logicaldoc/docker

## ... via [`docker stack deploy`](https://docs.docker.com/engine/reference/commandline/stack_deploy/) or [`docker-compose`](https://github.com/docker/compose)

Example `stack.yml` for `logicaldoc`:

```yaml
version: "3.1"

services:

  logicaldoc:
    depends_on:
      - logicaldoc-db
    command: ["./wait-for-it.sh", "logicaldoc-db:3306", "-t", "30", "--", "/LogicalDOC/logicaldoc.sh", "run"]
    image: logicaldoc/logicaldoc
    ports:
      - 8080:8080
      - 8022:22
    environment:
      - LDOC_MEMORY=2000

  mysql-ld:
    image: mysql:8.0.28
    command: --default-authentication-plugin=mysql_native_password
    environment:
      - MYSQL_ROOT_PASSWORD=example
      - MYSQL_DATABASE=logicaldoc
      - MYSQL_USER=ldoc
      - MYSQL_PASSWORD=changeme

```

[![Try in PWD](https://github.com/play-with-docker/stacks/raw/cff22438cb4195ace27f9b15784bbb497047afa7/assets/images/button.png)](http://play-with-docker.com?stack=https://raw.githubusercontent.com/logicaldoc/docker/master/stack.yml)

Run `docker stack deploy -c stack.yml logicaldoc` , wait for it to initialize completely, and visit `http://swarm-ip:8080`, `http://localhost:8080`, or `http://host-ip:8080` (as appropriate).

## Shell Access

For debugging and maintenance purposes you may want access the containers shell. If you are using Docker version `1.3.0` or higher you can access a running containers shell by starting `bash` using `docker exec`:

```bash
docker exec -it logicaldoc bash
```
