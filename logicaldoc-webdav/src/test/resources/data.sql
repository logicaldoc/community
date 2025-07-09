insert into ld_attributeset
			(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_description, ld_readonly, ld_type, ld_tenantid, ld_recordversion)
values (-1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'default','default',1,0,1,1);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,0,'source', 'Source', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,1,'sourceAuthor', 'Author', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,2,'sourceId', 'Original ID', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,3,'sourceType', 'Type', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,4,'object', 'Object', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,5,'coverage', 'Coverage', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,0,6,'recipient', 'Recipient', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,3,7,'sourceDate', 'Date', 0, -1, 0, 0, 0);
insert into ld_attributeset_ext(ld_attsetid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
values (-1,0,3,7,'multi', 'Multi', 0, -1, 0, 1, 0);

insert into ld_template
			(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_description, ld_readonly, ld_type, ld_tenantid, ld_recordversion)
values (-1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'default','default',0,0,1,1);

insert into ld_template_ext(ld_templateid, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly)
select -1, ld_mandatory, ld_type, ld_position, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_readonly from ld_attributeset_ext where ld_setid=-1;

insert into ld_user
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_tenantid,ld_recordversion,ld_enforcewrktime,ld_evalform)
values     (2,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'boss','d033e22ae348aeb566fc214aec3585c4da997','Meschieri','Marco','','','','','it','m.meschieri@logicalobjects.it','',0,null,0,0,-1,0,1,1,0,0);
insert into ld_group
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_tenantid,ld_name,ld_type,ld_recordversion)
values     (-2,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'_user_2',1,1);
insert into ld_usergroup
values (-2,2);

insert into ld_user
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_tenantid,ld_recordversion,ld_enforcewrktime,ld_evalform)
values     (3,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'sebastian','d033e22ae348aeb566fc214aec3585c4da997','Sebastian','Stein','','','','','de','seb_stein@gmx.de','',0,null,0,0,0,0,1,1,0,0);
insert into ld_group
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_tenantid,ld_name,ld_type,ld_recordversion)
values     (-3,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'_user_3',1,1);
insert into ld_usergroup
values (-3,3);

insert into ld_user
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_tenantid,ld_recordversion,ld_enforcewrktime,ld_evalform)
values     (4,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'author','d033e22ae348aeb566fc214aec3585c4da997','Author','Author','','','','','de','author@acme.com','',0,null,0,0,-1,0,1,1,0,0);
insert into ld_group
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_tenantid,ld_name,ld_type,ld_recordversion)
values     (-4,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'_user_4',1,1);
insert into ld_usergroup
values (-4,4);

insert into ld_user
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_tenantid,ld_recordversion,ld_enforcewrktime,ld_evalform)
values     (5,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,0,'test','d033e22ae348aeb566fc214aec3585c4da997','test','Test','','','','','de','test@acme.com','',0,null,0,0,-1,0,1,1,0,0);
insert into ld_group
           (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_tenantid,ld_name,ld_type,ld_recordversion)
values     (-5,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,'_user_5',1,1);
insert into ld_usergroup
values (-5,5);


insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (6,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'folder6',4,0,0,1,1,1,0);
insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (7,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'folder7',4,0,0,1,1,1,0);
insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (8,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1,'folder8',7,0,0,1,1,1,0);
insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (1200,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'test',4,0,0,1,1,1,0);
insert into ld_folder_store(ld_folderid, ld_nodeid, ld_storeid) values (1200, '', 2);

insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (1201,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'ABC',1200,1,0,1,1,1,0);
insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (1202,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'xyz',1201,1,0,1,1,1,0);
insert into ld_folder (ld_id,ld_lastmodified, ld_creation,ld_deleted,ld_name,ld_parentid,ld_type,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden)
values (1204,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1,'deleted',1201,1,0,1,1,1,0);

insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (6,2,1,1,1,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,1,0,0,0,1,1,1);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (6,3,1,1,1,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,1,0,0,0,1,1,1);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (6,-3,1,1,1,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,1,0,0,0,1,1,1);

insert into ld_document
           (ld_id,ld_folderid,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (1,4,'2008-10-22 00:00:00',0,0,'a','1.0','2006-12-19 00:00:00','2006-12-19 00:00:00','myself',1,0,'PDF',3,'en','one.pdf',1356,1,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_document
           (ld_id,ld_folderid,ld_docref,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (2,6,1,'2008-10-22 00:00:00',0,0,'b','1.0','2006-12-19 00:00:00','2006-12-19 00:00:00','myself',1,0,'PDF',3,'en','pippo',1356,0,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_document
           (ld_id,ld_folderid,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (3,4,'2008-10-22 00:00:00',0,0,'c','1.1','2006-12-19 00:00:00','2006-12-19 00:00:00','myself',1,0,'zip',3,'en','test.zip',1356,1,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_document
           (ld_id,ld_folderid,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (4,4,'2008-10-22 00:00:00',0,0,'d','1.0','2006-12-19 00:00:00','2006-12-19 00:00:00','myself',1,0,'PDF',3,'en','pippo',1356,1,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_document
           (ld_id,ld_folderid,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (5,4,'2022-10-21 00:00:00',0,0,'e','1.0','2022-10-21 00:00:00','2022-10-21 00:00:00','myself',1,0,'eml',3,'en','five.pdf',19003,1,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_document
           (ld_id,ld_folderid,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (6,4,'2022-10-22 00:00:00',0,0,'f','1.0','2022-10-22 00:00:00','2022-10-22 00:00:00','myself',1,0,'msg',3,'en','Hurry up! Only a few hours for the Prime Day VGA promos !!!.msg',159744,1,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_document
           (ld_id,ld_folderid,ld_lastmodified,ld_deleted,ld_immutable,ld_customid,ld_version,ld_fileversion,ld_date,ld_creation,ld_publisher,ld_publisherid,ld_status,ld_type,ld_lockuserid,ld_language,ld_filename,ld_filesize,ld_indexed,ld_signed,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (7,4,'2022-10-22 00:00:00',0,0,'g','1.0','1.0','2022-10-22 00:00:00','2022-10-22 00:00:00','myself',1,0,'eml',3,'en','New error indexing documents.eml',159682,1,0,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_ticket
           (ld_id,ld_lastmodified, ld_deleted,ld_ticketid,ld_docid,ld_userid,ld_type,ld_creation,ld_expired,ld_count,ld_tenantid,ld_recordversion,ld_enabled,ld_views)
values     (1,'2008-10-22 00:00:00',0,'1',1,1,0,'2011-01-01 00:00:00','2011-01-02 00:00:00',0,1,1,1,0);

insert into ld_ticket
           (ld_id,ld_lastmodified, ld_deleted,ld_ticketid,ld_docid,ld_userid,ld_type,ld_creation,ld_expired,ld_count,ld_tenantid,ld_recordversion,ld_enabled,ld_views)
values     (2,'2008-10-22 00:00:00',0,'2',2,3,0,'2011-01-01 00:00:00','2011-01-02 00:00:00',0,1,1,1,0);

insert into ld_ticket
           (ld_id,ld_lastmodified, ld_deleted,ld_ticketid,ld_docid,ld_userid,ld_type,ld_creation,ld_expired,ld_count,ld_tenantid,ld_recordversion,ld_enabled,ld_views)
values     (3,'2008-12-22 00:00:00',0,'3',1,3,0,'2011-01-01 00:00:00','2011-01-02 00:00:00',0,1,1,1,0);

insert into ld_version(ld_id, ld_documentid, ld_version, ld_fileversion, ld_templateid, ld_username, ld_userid, ld_versiondate, ld_comment, ld_lastmodified, ld_deleted, ld_immutable, ld_creation, ld_publisherid, ld_indexed, ld_signed, ld_status, ld_filesize, ld_folderid,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (1,1,'testVer01','fileVer01',5,'testUser',1,'2006-12-19 00:00:00','testComment','2009-02-09 00:00:00',0,0,'2009-02-09 00:00:00',1,0,0,0,0,5,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_version(ld_id, ld_documentid, ld_version, ld_fileversion, ld_templateid, ld_username, ld_userid, ld_versiondate, ld_comment, ld_lastmodified, ld_deleted, ld_immutable, ld_creation, ld_publisherid, ld_indexed, ld_signed, ld_status, ld_filesize, ld_folderid,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (2,1,'testVer02','fileVer02',5,'testUser',1,'2006-12-20 00:00:00','testComment','2009-02-09 00:00:00',0,0,'2009-02-09 00:00:00',1,0,0,0,0,5,'',1,0,0,1,1,1,5,0,0,0,0,1,0);

insert into ld_version(ld_id, ld_documentid, ld_version, ld_fileversion, ld_filename, ld_templateid, ld_username, ld_userid, ld_versiondate, ld_comment, ld_lastmodified, ld_deleted, ld_immutable, ld_creation, ld_publisherid, ld_indexed, ld_signed, ld_status, ld_filesize, ld_folderid,ld_creator,ld_creatorid,ld_exportstatus,ld_barcoded,ld_published,ld_tenantid,ld_recordversion,ld_pages,ld_stamped,ld_nature,ld_links,ld_ocrd,ld_previewpages,ld_docattrs)
values     (3,7,'1.0','1.0','test.zip',5,'testUser',1,'2006-12-20 00:00:00','testComment','2009-02-09 00:00:00',0,0,'2009-02-09 00:00:00',1,0,0,0,0,5,'',1,0,0,1,1,1,5,0,0,0,0,1,0);


INSERT INTO ld_version_ext (ld_versionid, ld_mandatory, ld_type, ld_position, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_parent, ld_stringvalues, ld_dependson, ld_readonly) 
VALUES (1, 0, 0, 0, 'Karma is a God', NULL, NULL, NULL, 'attr1', NULL, 0, NULL, 0, 0, NULL, NULL, NULL, 0);

INSERT INTO ld_version_ext (ld_versionid, ld_mandatory, ld_type, ld_position, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_name, ld_label, ld_editor, ld_setid, ld_hidden, ld_multiple, ld_parent, ld_stringvalues, ld_dependson, ld_readonly) 
VALUES (2, 0, 0, 0, 'Karma is a cat purring in my lap', NULL, NULL, NULL, 'attr1', NULL, 0, NULL, 0, 0, NULL, NULL, NULL, 0);


insert into ld_tag(ld_docid, ld_tenantid, ld_tag)
values     (1, 1,'abc');

insert into ld_tag(ld_docid, ld_tenantid, ld_tag)
values     (1, 1, 'def');

insert into ld_tag(ld_docid, ld_tenantid, ld_tag)
values     (1, 1,'ghi');

insert into ld_tag(ld_docid, ld_tenantid, ld_tag)
values     (2, 1,'ask');

insert into ld_tag(ld_docid, ld_tenantid, ld_tag)
values     (3, 1,'zzz');

insert into ld_history 
				(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_docid, ld_folderid, ld_userid, ld_date, ld_username, ld_event, ld_comment, ld_version, ld_notified, ld_new,ld_tenantid,ld_recordversion)
values     (-1,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,1,5,1,'2006-12-20 00:00:00','author','data test 01','reason test 01','1.0',0,1,1,1);

insert into ld_history 
			    (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_docid, ld_folderid, ld_userid, ld_date, ld_username, ld_event, ld_comment, ld_version, ld_notified, ld_new,ld_tenantid,ld_recordversion)
values     (-2,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,2,5,1,'2006-12-25 00:00:00','author','data test 01','reason test 02','1.0',0,1,1,1);

insert into ld_history 
			   (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_docid, ld_folderid, ld_userid, ld_date, ld_username, ld_event, ld_comment, ld_version, ld_notified,ld_tenantid,ld_recordversion)
values     (-3,'2008-10-22 00:00:00','2008-10-22 00:00:00',0,null,5,3,'2006-12-27 00:00:00','sebastian','data test 03','reason test 03','1.0',1,1,1);