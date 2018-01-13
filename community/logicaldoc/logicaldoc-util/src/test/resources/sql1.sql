CREATE TABLE co_menus (
co_menuid INTEGER NOT NULL,
co_menutext VARCHAR(100),
co_menuparent INTEGER,
co_menusort INTEGER,
co_menuicon VARCHAR(255),
co_menupath VARCHAR(255),
co_menutype INTEGER,
co_menuhier INTEGER,
co_menuref VARCHAR(255),
co_menusize INTEGER,
PRIMARY KEY(co_menuid)
);

INSERT INTO co_menus VALUES(1,'menu.home',0,1,'home.gif','',0,0,NULL,0);
INSERT INTO co_menus VALUES(2,'menu.admin',1,1,'administration.gif','ROOT',1,1,NULL,0);
INSERT INTO co_menus VALUES(4,'menu.personal',1,3,'personal.gif','ROOT',1,1,NULL,0);
INSERT INTO co_menus VALUES(5,'menu.documents',1,4,'documents.gif','ROOT',1,1,'document/browse',0);
INSERT INTO co_menus VALUES(6,'menu.user',2,1,'user.gif','ROOT/2',1,2,'admin/users',0);
INSERT INTO co_menus VALUES(7,'menu.group',2,2,'group.gif','ROOT/2',1,2,'admin/groups',0);
INSERT INTO co_menus VALUES(8,'menu.logging',2,3,'logging.gif','ROOT/2',1,2,'admin/logs',0);
INSERT INTO co_menus VALUES(13,'menu.messages',4,1,'message.gif','ROOT/4',1,2,'communication/messages',0);
INSERT INTO co_menus VALUES(16,'menu.changepassword',4,3,'password.gif','ROOT/4',1,2,'settings/password',0);
INSERT INTO co_menus VALUES(17,'directory',2,5,'open.gif','ROOT/2',1,2,'admin/folders',0);
INSERT INTO co_menus VALUES(19,'menu.editme',4,4,'user.gif','ROOT/4',1,2,'settings/personalData',0);
INSERT INTO co_menus VALUES(20,'menu.emails',2,4,'mail.gif','ROOT/2',1,2,NULL,0);
INSERT INTO co_menus VALUES(23,'smtp',20,3,'editmail.gif','ROOT/2/20',1,3,'admin/smtp',0);
INSERT INTO co_menus VALUES(24,'logicaldoc-email.accounts',20,4,'user.gif','ROOT/2/20',1,3,'admin/accounts',0);
INSERT INTO co_menus VALUES(25,'menu.searchengine',2,6,'search.gif','ROOT/2',1,2,'admin/searchEngine',0);
INSERT INTO co_menus VALUES(26,'db.keywords',1,5,'keywords.gif','ROOT',1,1,'search/keywords',0);

CREATE TABLE co_groups (
co_groupname VARCHAR(30) NOT NULL,
co_groupdesc VARCHAR(255),
PRIMARY KEY(co_groupname)
);

INSERT INTO co_groups VALUES('admin','Group of admins');
INSERT INTO co_groups VALUES('author','Group of authors');
INSERT INTO co_groups VALUES('guest','Group of guest');

CREATE TABLE co_users (
co_username VARCHAR(30) NOT NULL,
co_password VARCHAR(255),
co_name VARCHAR(30),
co_firstname VARCHAR(30),
co_street VARCHAR(100),
co_postalcode VARCHAR(10),
co_city VARCHAR(30),
co_country VARCHAR(30),
co_language VARCHAR(10),
co_email VARCHAR(255),
co_telephone VARCHAR(30),
PRIMARY KEY(co_username)
);

CREATE TABLE co_systemmessage (
co_messageid INTEGER NOT NULL,
co_author VARCHAR(100),
co_recipient VARCHAR(100),
co_messagetext VARCHAR(2000),
co_subject VARCHAR(255) NOT NULL,
co_sentdate VARCHAR(20) NOT NULL,
co_datescope INTEGER,
co_prio INTEGER,
co_confirmation INTEGER,
co_red INTEGER NOT NULL,
PRIMARY KEY(co_messageid)
);

INSERT INTO co_users (co_username,co_password,co_name,co_firstname,co_street,co_postalcode,co_city,co_country,co_language,co_email,co_telephone)
VALUES ('admin','d033e22ae348aeb566fc214aec3585c4da997','Admin','Admin','','','','','en','admin@admin.net','');

CREATE TABLE co_document (
co_docid INTEGER NOT NULL,
co_docname VARCHAR(255),
co_docversion VARCHAR(10),
co_docdate VARCHAR(20),
co_docpublisher VARCHAR(30),
co_docstatus INTEGER,
co_menuid INTEGER,
co_doctype VARCHAR(10),
co_checkoutuser VARCHAR(30),
co_source VARCHAR(255),
co_sourceauthor VARCHAR(255),
co_sourcedate VARCHAR(20),
co_sourcetype VARCHAR(255),
co_coverage VARCHAR(255),
co_language VARCHAR(10),
PRIMARY KEY(co_docid),
FOREIGN KEY(co_menuid) REFERENCES co_menus(co_menuid)
);

CREATE TABLE co_account (
co_accountid int NOT NULL,
co_username varchar(30),
co_mailaddress varchar(255),
co_provider varchar(255),
co_host varchar(255),
co_port varchar(10),
co_accountuser varchar(255),
co_accountpassword varchar(255),
co_targetfolder integer,
co_allowedtypes varchar(255),
co_language varchar(10),
co_deletefrommailbox integer,
co_enabled integer,
PRIMARY KEY(co_accountid),
FOREIGN KEY(co_username) REFERENCES co_users(co_username),
FOREIGN KEY(co_targetfolder) REFERENCES co_menus(co_menuid)
);

CREATE TABLE co_email (
co_messageid INTEGER NOT NULL,
co_accountid INTEGER NOT NULL,
co_emailid varchar(255) NOT NULL,
co_messageText varchar(255),
co_author varchar(255),
co_subject varchar(255),
co_sentdate varchar(20),
co_red integer,
co_authoraddress varchar(255),
co_username varchar(30),
co_folder varchar(30),
PRIMARY KEY(co_messageid),
FOREIGN KEY(co_username) REFERENCES co_users(co_username),
FOREIGN KEY(co_accountid) REFERENCES co_account(co_accountid)
);

CREATE TABLE co_terms (
co_menuid INTEGER NOT NULL,
co_stem VARCHAR(50) NOT NULL,
co_value DOUBLE PRECISION,
co_wordcount INTEGER,
co_word VARCHAR(70),
FOREIGN KEY(co_menuid) REFERENCES co_menus(co_menuid)
);

CREATE TABLE co_versions (
co_docid INTEGER NOT NULL,
co_version VARCHAR(10) NOT NULL,
co_versionuser VARCHAR(30),
co_versiondate VARCHAR(20),
co_versioncomment VARCHAR(255),
FOREIGN KEY(co_docid) REFERENCES co_document(co_docid)
);

CREATE TABLE co_attachment (
co_messageid integer NOT NULL,
co_partid integer NOT NULL,
co_filename varchar(255),
co_icon varchar(255),
co_mimetype varchar(255),
PRIMARY KEY  (co_partid,co_messageid),
FOREIGN KEY(co_messageid) REFERENCES co_email(co_messageid)
);

CREATE TABLE co_searchdocument (co_luceneid INTEGER NOT NULL,co_menuid INTEGER NOT NULL,co_index VARCHAR(10),PRIMARY KEY (co_luceneid),FOREIGN KEY(co_menuid) REFERENCES co_menus(co_menuid));

CREATE TABLE co_menugroup (
co_menuid INTEGER NOT NULL,
co_groupname VARCHAR(30) NOT NULL,
co_writeenable INTEGER,
FOREIGN KEY(co_menuid) REFERENCES co_menus(co_menuid),
FOREIGN KEY(co_groupname) REFERENCES co_groups(co_groupname)
);

INSERT INTO co_menugroup VALUES(1,'admin',0);
INSERT INTO co_menugroup VALUES(2,'admin',0);
INSERT INTO co_menugroup VALUES(4,'admin',0);
INSERT INTO co_menugroup VALUES(5,'admin',1);
INSERT INTO co_menugroup VALUES(6,'admin',0);
INSERT INTO co_menugroup VALUES(7,'admin',0);
INSERT INTO co_menugroup VALUES(8,'admin',0);
INSERT INTO co_menugroup VALUES(13,'admin',0);
INSERT INTO co_menugroup VALUES(16,'admin',0);
INSERT INTO co_menugroup VALUES(17,'admin',0);
INSERT INTO co_menugroup VALUES(19,'admin',0);
INSERT INTO co_menugroup VALUES(20,'admin',0);
INSERT INTO co_menugroup VALUES(23,'admin',0);
INSERT INTO co_menugroup VALUES(24,'admin',0);
INSERT INTO co_menugroup VALUES(25,'admin',0);
INSERT INTO co_menugroup VALUES(26,'admin',1);
INSERT INTO co_menugroup VALUES(1,'author',0);
INSERT INTO co_menugroup VALUES(4,'author',0);
INSERT INTO co_menugroup VALUES(5,'author',1);
INSERT INTO co_menugroup VALUES(13,'author',0);
INSERT INTO co_menugroup VALUES(16,'author',0);
INSERT INTO co_menugroup VALUES(19,'author',0);
INSERT INTO co_menugroup VALUES(20,'author',0);
INSERT INTO co_menugroup VALUES(23,'author',0);
INSERT INTO co_menugroup VALUES(24,'author',0);
INSERT INTO co_menugroup VALUES(26,'author',1);
INSERT INTO co_menugroup VALUES(1,'guest',0);
INSERT INTO co_menugroup VALUES(4,'guest',0);
INSERT INTO co_menugroup VALUES(5,'guest',0);
INSERT INTO co_menugroup VALUES(13,'guest',0);
INSERT INTO co_menugroup VALUES(16,'guest',0);
INSERT INTO co_menugroup VALUES(19,'guest',0);
INSERT INTO co_menugroup VALUES(26,'guest',0);

CREATE TABLE co_keywords (
co_docid INTEGER NOT NULL,
co_keyword VARCHAR(20) NOT NULL,
FOREIGN KEY(co_docid) REFERENCES co_document(co_docid)
);

CREATE TABLE co_recipient (
co_messageid INTEGER NOT NULL,
co_address VARCHAR(255) NOT NULL,
co_name VARCHAR(255),
PRIMARY KEY(co_messageid,co_address),
FOREIGN KEY(co_messageid) REFERENCES co_email(co_messageid)
);


CREATE TABLE co_usergroup (
co_username VARCHAR(30)  NOT NULL,
co_groupname VARCHAR(30) NOT NULL,
FOREIGN KEY(co_username) REFERENCES co_users(co_username),
FOREIGN KEY(co_groupname) REFERENCES co_groups(co_groupname)
);

INSERT INTO co_usergroup (co_username,co_groupname) 
VALUES ('admin','admin');

CREATE TABLE co_article (co_articleid INTEGER NOT NULL,co_docid INTEGER,co_subject VARCHAR(255),co_message VARCHAR(2000),co_articledate VARCHAR(20),co_username VARCHAR(30),PRIMARY KEY(co_articleid),FOREIGN KEY(co_docid) REFERENCES co_document(co_docid));
CREATE TABLE co_history (co_historyid INTEGER NOT NULL,co_docid INTEGER NOT NULL,co_date VARCHAR(20),co_username VARCHAR(30),co_event VARCHAR(255),PRIMARY KEY(co_historyid),FOREIGN KEY(co_docid) REFERENCES co_document(co_docid),FOREIGN KEY(co_username) REFERENCES co_users(co_username));
CREATE TABLE co_ticket (co_ticketid VARCHAR(255) NOT NULL,co_menuid INTEGER,co_username VARCHAR(30),PRIMARY KEY (co_ticketid),FOREIGN KEY (co_menuid) REFERENCES co_menus(co_menuid),FOREIGN KEY(co_username) REFERENCES co_users(co_username));
