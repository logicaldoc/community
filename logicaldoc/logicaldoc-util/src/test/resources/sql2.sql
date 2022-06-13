INSERT INTO co_users (co_username,co_password,co_name,co_firstname,co_street,co_postalcode,co_city,co_country,co_language,co_email,co_telephone)
VALUES ('boss','d033e22ae348aeb566fc214aec3585c4da997','Meschieri','Marco','','','','','it','m.meschieri@logicalobjects.it','');

INSERT INTO co_users (co_username,co_password,co_name,co_firstname,co_street,co_postalcode,co_city,co_country,co_language,co_email,co_telephone)
VALUES ('sebastian','d033e22ae348aeb566fc214aec3585c4da997','Sebastian','Stein','','','','','de','seb_stein@gmx.de','');

INSERT INTO co_users (co_username,co_password,co_name,co_firstname,co_street,co_postalcode,co_city,co_country,co_language,co_email,co_telephone)
VALUES ('author','d033e22ae348aeb566fc214aec3585c4da997','Author','Author','','','','','de','author@acme.com','');

INSERT INTO co_users (co_username,co_password,co_name,co_firstname,co_street,co_postalcode,co_city,co_country,co_language,co_email,co_telephone)
VALUES ('test','d033e22ae348aeb566fc214aec3585c4da997','test','Test','','','','','de','test@acme.com','');


INSERT INTO co_menus VALUES(99,'menu.admin',1,1,'administration.gif','ROOT',5,1,NULL,0);
INSERT INTO co_menus VALUES(100,'menu.admin',1,1,'administration.gif','ROOT',3,1,NULL,0);
INSERT INTO co_menus VALUES(101,'text',100,1,'administration.gif','ROOT',3,1,NULL,0);
INSERT INTO co_menus VALUES(102,'menu.admin',101,1,'administration.gif','ROOT',5,1,NULL,0);
INSERT INTO co_menus VALUES(103,'menu.admin',101,1,'administration.gif','ROOT',5,1,NULL,0);

INSERT INTO co_menugroup VALUES(100,'admin',1);
INSERT INTO co_menugroup VALUES(100,'guest',1);
INSERT INTO co_menugroup VALUES(103,'admin',1);
INSERT INTO co_menugroup VALUES(99,'admin',0);


INSERT INTO co_searchdocument (co_luceneid,co_menuid,co_index) 
VALUES (12345,4,'testIndex');
INSERT INTO co_searchdocument (co_luceneid,co_menuid,co_index) 
VALUES (12346,99,'testIndex');

INSERT INTO co_usergroup (co_username,co_groupname) 
VALUES ('sebastian','admin');
INSERT INTO co_usergroup (co_username,co_groupname) 
VALUES ('sebastian','author');
INSERT INTO co_usergroup (co_username,co_groupname) 
VALUES ('admin','admin');
INSERT INTO co_usergroup (co_username,co_groupname) 
VALUES ('author','author');
INSERT INTO co_usergroup (co_username,co_groupname) 
VALUES ('test','guest');

INSERT INTO co_groups VALUES('testGroup','Group for tests');

INSERT INTO co_ticket VALUES('1',1,'admin');
INSERT INTO co_ticket VALUES('2',2,'sebastian');
INSERT INTO co_ticket VALUES('3',1,'sebastian');

INSERT INTO co_document 
VALUES(1,'testDocname','testDocVer','2006-12-19','myself', 1, 103, 'PDF','sebastian', 'source', 'sourceauthor', 'sourcedate','sourcetype','coverage','en');
INSERT INTO co_document 
VALUES(2,'testDocname2','testDocVer','2006-12-20','myself', 1, 8, 'PDF','sebastian', 'source', 'sourceauthor', 'sourcedate','sourcetype','coverage','en');


INSERT INTO co_versions 
VALUES(1,'testVers01','testUser','2006-12-19','testComment');
INSERT INTO co_versions 
VALUES(1,'testVers02','testUser','2006-12-20','testComment');

INSERT INTO co_keywords VALUES(1,'abc');
INSERT INTO co_keywords VALUES(1,'def');
INSERT INTO co_keywords VALUES(1,'ghi');

INSERT INTO co_history VALUES(1,1,'2006-12-20','author','data test 01');
INSERT INTO co_history VALUES(2,1,'2006-12-20','author','data test 02');

INSERT INTO co_terms values (1,'a',2.5,50,'test');
INSERT INTO co_terms values (1,'b',0.7,12,'test2');
INSERT INTO co_terms values (2,'a',0.3,12,'test3');
INSERT INTO co_terms values (4,'a',0.14,20,'test4');
INSERT INTO co_terms values (103,'a',0.14,20,'test5');

INSERT INTO co_article values(1,1,'subject','message','12233434','admin');
INSERT INTO co_article values(2,1,'subject2','message2','12233435','admin');
INSERT INTO co_article values(3,1,'subject3','message3','12233436','sebastian');

INSERT INTO co_account values(1,'admin','author@logicaldoc.sf.net','Aruba','pcalle','port','author@logicaldoc.sf.net','authorPSWD',1,'pdf,doc','it',1,1);
INSERT INTO co_account values(2,'author','admin@logicaldoc.sf.net','Aruba','pcalle','port','admin@logicaldoc.sf.net','adminPSWD',2,'doc,txt','en',0,1);

INSERT INTO co_email values(17,1,'id1','messageText','Morven Macauley','Re: maintenanc','12/14/2006 04:49 AM',1,'sprou@l2r9f8.varberg.net','admin','Junk');
INSERT INTO co_email values(18,1,'id2','messageText','Nels Keough','Re: overgil','12/18/2006 10:23 PM',1,'woodsoy@obacom.com','author','Junk');
INSERT INTO co_email values(19,2,'id1','messageText','Nels Keough','Re: overgil','12/18/2006 10:23 PM',1,'woodsoy@obacom.com','author','Junk');

INSERT INTO co_attachment values(17,54,'holiday06_1.gif','gif.png','image/gif');
INSERT INTO co_attachment values(18,1,'hibernate.log','log.png','application/octet-stream');

INSERT INTO co_systemmessage values(1,'admin','sebastian','message text1','subject1','1111999999999999999',5,3,1,0);
INSERT INTO co_systemmessage values(2,'admin','sebastian','message text2','subject2','11119999',5,3,1,1);
INSERT INTO co_systemmessage values(3,'sebastian','admin','message text3','subject3','1111999999999999999',5,3,1,1);


--Shut down hsql
shutdown;