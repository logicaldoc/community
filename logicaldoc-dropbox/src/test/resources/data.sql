insert into ld_user
		   (ld_id,ld_lastmodified,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_enforcewrktime,ld_evalform)
values     (2,'2008-11-20 00:00:00',0,1,'boss','d033e22ae348aeb566fc214aec3585c4da997','Meschieri','Marco','','','','','it','m.meschieri@logicalobjects.it','',0,null,0,0,-1,0,0,0);

insert into ld_user
		   (ld_id,ld_lastmodified,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_enforcewrktime,ld_evalform)
values     (3,'2008-11-20 00:00:00',0,1,'sebastian','d033e22ae348aeb566fc214aec3585c4da997','Sebastian','Stein','','','','','de','seb_stein@gmx.de','',0,null,0,0,-1,0,0,0);


insert into ld_document
		   (ld_id,ld_lastmodified,ld_deleted,ld_customid,ld_version,ld_creation,ld_date,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_folderid,ld_templateid,ld_immutable,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded, LD_PUBLISHED,ld_links,ld_docattrs)
values     (1,'2008-11-20 00:00:00',0,null,'testDocVer','2006-12-19 00:00:00','2006-12-19 00:00:00','myself',1,1,'PDF',3,'en','pippo',1356,1,5,null,0,0,'creator',1,0,0,1,0,0);

insert into ld_document
		   (ld_id,ld_lastmodified,ld_deleted,ld_customid,ld_version,ld_creation,ld_date,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_folderid,ld_templateid,ld_immutable,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded, LD_PUBLISHED,ld_links,ld_docattrs)
values     (2,'2008-11-20 00:00:00',0,null,'testDocVer','2006-12-19 00:00:00','2006-12-19 00:00:00','myself',1,1,'PDF',3,'en','pluto',122345,1,5,null,0,0,'creator',1,0,0,1,0,0);



insert into ld_event
		(ld_id, ld_lastmodified, ld_deleted, ld_title, ld_startdate, ld_expirationdate, ld_description, 
		 ld_parentid, ld_creatorid, ld_creator, ld_reminded, ld_frequency, ld_remindtime, ld_remindunit, ld_reminddate, ld_status, ld_compdate)
values  (1,'2013-01-14 00:00:00',0,'event1','2013-01-15 01:00:00','2013-01-15 02:00:00','body1',null,1,'marco',0,10,15,'minute', '2013-01-15 00:45:00',0,'2014-01-15 00:00:00');
insert into ld_userevent(ld_eventid, ld_userid)  values (1,1);
insert into ld_userevent(ld_eventid, ld_userid)  values (1,2);
insert into ld_docevent(ld_eventid, ld_docid)  values (1,2);

insert into ld_event
		(ld_id, ld_lastmodified, ld_deleted, ld_title, ld_startdate, ld_expirationdate, ld_description, 
		 ld_parentid, ld_creatorid, ld_creator, ld_reminded, ld_frequency, ld_remindtime, ld_remindunit, ld_reminddate, ld_status, ld_compdate)
values  (2,'2013-01-14 00:00:00',0,'event2','2013-01-25 01:00:00','2013-01-25 02:00:00','body2',1,1,'marco',0,0,15,'minute', '2013-01-15 00:45:00',0,'2014-01-15 00:00:00');
insert into ld_event
		(ld_id, ld_lastmodified, ld_deleted, ld_title, ld_startdate, ld_expirationdate, ld_description, 
		 ld_parentid, ld_creatorid, ld_creator, ld_reminded, ld_frequency, ld_remindtime, ld_remindunit, ld_reminddate, ld_status, ld_compdate)
values  (3,'2013-01-14 00:00:00',1,'event3','2013-01-25 01:00:00','2013-01-25 02:00:00','body3',null,1,'marco',0,0,15,'minute', '2013-01-15 00:45:00',0,'2014-01-15 00:00:00');