-- datetime -> timestamp
-- mediumtext -> varchar(100000)
-- double -> float

create table ld_bookmark (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                          ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint not null, 
                          ld_docid bigint not null, ld_title varchar(255) not null, ld_description varchar(4000),
                          ld_position int not null, ld_filetype varchar(40), ld_type int not null, primary key (ld_id));
create table ld_document (ld_id bigint not null, ld_lastmodified timestamp not null, ld_recordversion bigint not null, 
						  ld_deleted int not null, ld_tenantid bigint not null, ld_immutable int not null,
                          ld_customid varchar(200), ld_version varchar(10), ld_fileversion varchar(10), ld_revision varchar(255), ld_date timestamp, 
                          ld_creation timestamp not null, ld_publisher varchar(255), ld_publisherid bigint not null, ld_creator varchar(255),
                          ld_creatorid bigint not null, ld_status int, ld_type varchar(255), ld_lockuserid bigint, ld_lockuser varchar(255),
                          ld_language varchar(10), ld_filename varchar(255), ld_password varchar(255),
                          ld_filesize bigint, ld_indexed int not null, ld_barcoded int not null, ld_signed int not null, ld_stamped int not null, 
                          ld_digest varchar(255), ld_folderid bigint, ld_templateid bigint, ld_exportstatus int not null, 
                          ld_exportid bigint, ld_exportname varchar(255), ld_exportversion varchar(10), ld_docref bigint, ld_docreftype varchar(255),
                          ld_deleteuserid bigint, ld_deleteuser varchar(255), ld_rating int, ld_comment varchar(1000), ld_lastnote varchar(4000), 
                          ld_workflowstatus varchar(1000), ld_workflowstatusdisp varchar(1000), 
                          ld_published int not null, ld_startpublishing timestamp, ld_stoppublishing timestamp null, ld_transactionid varchar(255), 
                          ld_extresid varchar(255), ld_tgs varchar(1000), ld_pages int not null, ld_previewpages int not null, ld_nature int not null,
                          ld_formid bigint, ld_links int not null, ld_docattrs int not null, ld_ocrtemplateid bigint, ld_ocrd int not null, 
                          ld_barcodetemplateid bigint, ld_color varchar(255), primary key (ld_id));
create table ld_document_ext (ld_docid bigint not null, ld_mandatory int not null, ld_type int not null, 
                              ld_editor int not null, ld_position int not null, ld_stringvalue varchar(4000), ld_stringvalues varchar(4000), 
                              ld_intvalue bigint, ld_doublevalue float, ld_datevalue timestamp null, 
                              ld_name varchar(255) not null, ld_label varchar(255), ld_setid bigint,
                              ld_hidden int not null, ld_readonly int not null, ld_multiple int not null, ld_parent varchar(255),
                              ld_dependson varchar(255), ld_validation varchar(1), ld_initialization varchar(1), primary key (ld_docid, ld_name));
create table ld_document_acl (ld_docid bigint not null, ld_groupid bigint not null, ld_read int not null, ld_write int not null, 
                             ld_security int not null, ld_immutable int not null, ld_delete int not null, ld_customid int not null, 
                             ld_rename int not null, ld_sign int not null, ld_preview int not null, ld_archive int not null, 
                             ld_workflow int not null, ld_download int not null, ld_calendar int not null, ld_subscription int not null, 
                             ld_print int not null, ld_password int not null, ld_move int not null, ld_email int not null, 
                             ld_automation int not null, ld_readingreq int not null, ld_revision int not null, primary key (ld_docid, ld_groupid));                              
create table ld_generic (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null, 
                         ld_deleted int not null, ld_tenantid bigint not null, ld_type varchar(255) not null, 
                         ld_subtype varchar(255) not null, ld_qualifier bigint null, ld_string1 varchar(4000), 
                         ld_string2 varchar(4000), ld_string3 varchar(4000), ld_string4 varchar(4000),
                         ld_string5 varchar(1000), ld_string6 varchar(1000), ld_string7 varchar(1000), ld_string8 varchar(1000),
                         ld_integer1 bigint null, ld_integer2 bigint null, ld_integer3 bigint null, 
                         ld_double1 float, ld_double2 float, ld_text1 varchar(100000),
                         ld_date1 timestamp null, ld_date2 timestamp null, primary key (ld_id));
create table ld_generic_ext (ld_genid bigint not null, ld_mandatory int not null, ld_type int not null, 
                             ld_editor bigint not null, ld_position int not null, ld_stringvalue varchar(4000), ld_stringvalues varchar(4000), 
                             ld_intvalue bigint, ld_doublevalue float, ld_datevalue timestamp null, 
                             ld_name varchar(255) not null, ld_label varchar(255), ld_setid bigint, 
                             ld_hidden int not null, ld_readonly int not null, ld_multiple int not null, ld_parent varchar(255), 
                             ld_dependson varchar(255), ld_validation varchar(1), ld_initialization varchar(1), primary key (ld_genid, ld_name));
create table ld_group (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null, 
                       ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255) not null, 
                       ld_description varchar(255), ld_type int not null, ld_source varchar(255), primary key (ld_id));
create table ld_history (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                         ld_deleted int not null, ld_tenantid bigint not null, ld_docid bigint, ld_keylabel varchar(255), 
                         ld_folderid bigint, ld_userid bigint, ld_date timestamp, ld_username varchar(255), ld_event varchar(255), 
                         ld_comment varchar(4000), ld_reason varchar(4000), ld_version varchar(255), ld_fileversion varchar(10), ld_path varchar(4000), 
                         ld_pathold varchar(4000), ld_notified int not null, ld_sessionid varchar(255), ld_new int, ld_filename varchar(255),
                         ld_filenameold varchar(255), ld_userlogin varchar(255), ld_ip varchar(255), 
                         ld_geolocation varchar(255), ld_device varchar(255), ld_filesize bigint, ld_color varchar(255), primary key (ld_id));
create table ld_tag (ld_docid bigint, ld_tenantid bigint not null, ld_tag varchar(255));
create table ld_foldertag (ld_folderid bigint, ld_tenantid bigint not null, ld_tag varchar(255));
create table ld_link (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,  
                      ld_deleted int not null, ld_tenantid bigint not null, 
                      ld_type varchar(255) not null, ld_docid1 bigint, 
                      ld_docid2 bigint, primary key (ld_id));
create table ld_menu (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null, 
                      ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255), 
                      ld_parentid bigint not null, ld_icon varchar(255), 
                      ld_type int not null, ld_description varchar(4000), ld_position int not null, 
                      ld_enabled int not null, ld_routineid bigint, ld_automation varchar(4000), primary key (ld_id));
create table ld_menu_acl (ld_menuid bigint not null, ld_groupid bigint not null, ld_read int not null, ld_write int not null, primary key (ld_menuid, ld_groupid));
create table ld_recipient (ld_messageid bigint not null, ld_name varchar(255) not null, ld_address varchar(255) not null, ld_mode varchar(255) not null, ld_type int not null, ld_read int not null);
create table ld_systemmessage (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                               ld_deleted int not null, ld_tenantid bigint not null, 
                               ld_author varchar(255), ld_messagetext varchar(4000), ld_subject varchar(1000), 
                               ld_sentdate timestamp not null, ld_datescope int, ld_prio int, ld_confirmation int, 
                               ld_lastnotified timestamp, ld_status int not null, ld_trials int,
                               ld_type int not null, ld_html int not null, primary key (ld_id));
create table ld_template (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                          ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255) not null, ld_label varchar(255), 
                          ld_description varchar(2000), ld_readonly int not null, ld_type int not null, ld_validation varchar(4000),
                          primary key (ld_id));                         
create table ld_template_ext (ld_templateid bigint not null, ld_mandatory int not null, ld_type int not null, 
                              ld_editor int not null, ld_position int not null, ld_stringvalue varchar(4000), ld_stringvalues varchar(4000), 
                              ld_intvalue bigint, ld_doublevalue float, ld_datevalue timestamp null, ld_validation varchar(4000),
                              ld_initialization varchar(4000), ld_name varchar(255) not null, ld_label varchar(255), ld_setid bigint,
                              ld_hidden int not null, ld_readonly int not null, ld_multiple int not null, ld_parent varchar(255), 
                              ld_dependson varchar(255), primary key (ld_templateid, ld_name));
create table ld_template_acl (ld_templateid bigint not null, ld_groupid bigint not null, ld_read int not null, ld_write int not null, 
                               primary key (ld_templateid, ld_groupid));                              
create table ld_attributeset (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                              ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255) not null, ld_label varchar(255), 
                              ld_description varchar(2000), ld_readonly int not null, ld_type int not null, 
                              primary key (ld_id));
create table ld_attributeset_ext (ld_attsetid bigint not null, ld_mandatory int not null, ld_type int not null, 
                                  ld_editor int not null, ld_position int not null, ld_stringvalue varchar(4000), ld_stringvalues varchar(4000), 
                                  ld_intvalue bigint, ld_doublevalue float, ld_datevalue timestamp null, ld_validation varchar(4000),
                                  ld_initialization varchar(4000), ld_name varchar(255) not null, ld_label varchar(255), ld_setid bigint, 
                                  ld_hidden int not null, ld_readonly int not null, ld_multiple int not null, ld_parent varchar(255), 
                                  ld_dependson varchar(255), primary key (ld_attsetid, ld_name));
create table ld_ticket (ld_id bigint not null, ld_lastmodified timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_ticketid varchar(255) not null, 
                        ld_docid bigint not null, ld_userid bigint not null, ld_type int not null, 
                        ld_creation timestamp not null, ld_expired timestamp, ld_count int not null, ld_suffix varchar(255),
                        ld_enabled int not null, ld_maxcount int, ld_maxviews int, ld_views int not null,
                        primary key (ld_id));
create table ld_user (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                      ld_deleted int not null, ld_tenantid bigint not null, ld_enabled int not null, 
                      ld_username varchar(255) not null, ld_password varchar(255), ld_passwordmd4 varchar(255), ld_name varchar(255), ld_firstname varchar(255), 
                      ld_street varchar(255), ld_postalcode varchar(255), ld_city varchar(255), ld_country varchar(255), 
                      ld_state varchar(255), ld_language varchar(10), ld_email varchar(255), ld_emailsignature varchar(1000), ld_telephone varchar(255), 
                      ld_telephone2 varchar(255), ld_type int not null, ld_passwordchanged timestamp, ld_passwordexpires int not null,
                      ld_source int not null, ld_quota bigint not null, ld_welcomescreen int null, ld_ipwhitelist varchar(1000), 
                      ld_ipblacklist varchar(1000), ld_passwordexpired int not null, ld_defworkspace bigint,
                      ld_email2 varchar(255), ld_emailsignature2 varchar(1000), ld_avatar varchar(4000), ld_expire timestamp,
                      ld_certexpire timestamp, ld_certdn varchar(1000), ld_enforcewrktime int not null, ld_lastlogin timestamp,
                      ld_secondfactor varchar(255), ld_key varchar(255), ld_lastenabled timestamp, ld_maxinactivity int,
                      ld_docsgrid varchar(4000), ld_hitsgrid varchar(4000), ld_evalform int not null, ld_legals int not null,
                      ld_dateformat varchar(255), ld_dateformatshort varchar(255), ld_dateformatlong varchar(255),
                      ld_searchpref varchar(255), ld_timezone varchar(255), ld_company varchar(255), 
                      ld_department varchar(255), ld_organizationalunit varchar(255), ld_building varchar(255), primary key (ld_id));
create table ld_workingtime (ld_userid bigint not null, ld_dayofweek int not null, ld_hourstart int not null, ld_minutestart int not null, 
                             ld_hourend int not null, ld_minuteend int not null, ld_label varchar(255), ld_description varchar(1000), 
                             primary key(ld_userid, ld_dayofweek, ld_hourstart, ld_minutestart));
create table ld_user_history (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                              ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint, 
                              ld_date timestamp, ld_username varchar(255), ld_event varchar(255), ld_keylabel varchar(255), 
                              ld_comment varchar(4000), ld_notified int not null, ld_sessionid varchar(255), 
                              ld_filename varchar(255), ld_userlogin varchar(255), ld_ip varchar(255),
                              ld_author varchar(255), ld_geolocation varchar(255), ld_device varchar(255), 
                              ld_filesize bigint, ld_folderid bigint, ld_docid bigint, ld_path varchar(4000), primary key (ld_id));
create table ld_usergroup (ld_groupid bigint not null, ld_userid bigint not null, primary key (ld_groupid, ld_userid));
create table ld_version (ld_id bigint not null, ld_lastmodified timestamp not null, ld_recordversion bigint not null,
                         ld_deleted int not null, ld_tenantid bigint not null, ld_immutable int not null, ld_customid varchar(200),
                         ld_version varchar(10), ld_fileversion varchar(10), ld_revision varchar(255), ld_date timestamp, ld_creation timestamp, ld_publisher varchar(255),
                         ld_publisherid bigint not null,  ld_creator varchar(255), ld_creatorid bigint not null, ld_status int, ld_type varchar(255),
                         ld_lockuserid bigint, ld_lockuser varchar(255), ld_lastnote varchar(4000),
                         ld_language varchar(10), ld_filename varchar(255), ld_password varchar(255),
                         ld_filesize bigint, ld_indexed int not null, ld_barcoded int not null, ld_signed int not null, ld_stamped int not null,
                         ld_digest varchar(255), ld_folderid bigint, ld_foldername varchar(1000), ld_templateid bigint, 
                         ld_templatename varchar(1000), ld_tgs varchar(1000), ld_username varchar(255), ld_userid bigint, ld_versiondate timestamp, 
                         ld_comment varchar(1000), ld_event varchar(255), ld_documentid bigint not null, ld_exportstatus int not null, 
                         ld_exportid bigint, ld_exportname varchar(255), ld_exportversion varchar(10), ld_deleteuserid bigint, 
                         ld_workflowstatus varchar(1000), ld_workflowstatusdisp varchar(1000), ld_published int not null, 
                         ld_startpublishing timestamp, ld_stoppublishing timestamp null, 
                         ld_transactionid varchar(255), ld_extresid varchar(255), ld_pages int not null, ld_previewpages int not null, ld_nature int not null,
                         ld_formid bigint, ld_links int not null, ld_docattrs int not null, ld_ocrtemplateid bigint, ld_ocrd int not null, 
                         ld_barcodetemplateid bigint, ld_color varchar(255), primary key (ld_id));
create table ld_version_ext (ld_versionid bigint not null, ld_mandatory int not null, ld_type int not null, ld_editor int not null, 
                             ld_position int not null, ld_stringvalue varchar(4000), ld_stringvalues varchar(4000), 
                             ld_intvalue bigint, ld_doublevalue float, ld_datevalue timestamp null, ld_name varchar(255) not null, 
                             ld_label varchar(255), ld_setid bigint, ld_hidden int not null, ld_readonly int not null, ld_multiple int not null, 
                             ld_parent varchar(255), ld_dependson varchar(255), ld_validation varchar(1), ld_initialization varchar(1), primary key (ld_versionid, ld_name));
create table ld_folder (ld_id bigint not null, ld_lastmodified timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255), 
                        ld_parentid bigint not null, ld_securityref bigint, ld_description varchar(4000), 
                        ld_type int not null, ld_creation timestamp, ld_creator varchar(255), ld_creatorid bigint, 
                        ld_templateid bigint, ld_templocked int not null, ld_deleteuserid bigint, ld_deleteuser varchar(255), ld_position int not null,
                        ld_quotadocs bigint, ld_quotasize bigint, ld_hidden int not null, ld_foldref bigint, 
                        ld_level int, ld_maxversions int, ld_color varchar(255), ld_tgs varchar(1000),
                        ld_qthreshold int, ld_qrecipients varchar(1000), ld_path varchar(255), ld_grid varchar(4000), 
                        ld_ocrtemplateid bigint, ld_barcodetemplateid bigint, ld_tile varchar(10000), primary key (ld_id));
create table ld_folder_ext (ld_folderid bigint not null, ld_mandatory int not null, ld_type int not null, ld_editor int not null,
                            ld_position int not null, ld_stringvalue varchar(4000), ld_stringvalues varchar(4000), 
                            ld_intvalue bigint, ld_doublevalue float, ld_datevalue timestamp null, 
                            ld_name varchar(255) not null, ld_label varchar(255), ld_setid bigint, 
                            ld_hidden int not null, ld_readonly int not null, ld_multiple int not null, ld_parent varchar(255), 
                            ld_dependson varchar(255), ld_validation varchar(1), ld_initialization varchar(1), primary key (ld_folderid, ld_name));
create table ld_folder_store (ld_folderid bigint not null, ld_nodeid varchar(255) not null, ld_storeid int not null, primary key (ld_folderid, ld_nodeid));                          
create table ld_folder_history (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                                ld_deleted int not null, ld_tenantid bigint not null, ld_docid bigint, ld_keylabel varchar(255),
                                ld_folderid bigint, ld_userid bigint, ld_date timestamp, ld_username varchar(255), 
                                ld_event varchar(255), ld_comment varchar(4000), ld_reason varchar(4000), ld_version varchar(255), ld_fileversion varchar(10),  
                                ld_path varchar(4000), ld_pathold varchar(4000), ld_notified int not null, ld_sessionid varchar(255),
                                ld_filename varchar(255), ld_filenameold varchar(255), ld_userlogin varchar(255),
                                ld_ip varchar(255), ld_geolocation varchar(255), ld_device varchar(255),  
                                ld_filesize bigint, ld_color varchar(255), primary key (ld_id));
create table ld_folder_acl (ld_folderid bigint not null, ld_groupid bigint not null, ld_read int not null, ld_write int not null, 
                             ld_add int not null, ld_security int not null, ld_immutable int not null, ld_delete int not null, 
                             ld_rename int not null, ld_import int not null, ld_export int not null, ld_sign int not null, 
                             ld_archive int not null, ld_workflow int not null, ld_download int not null, ld_calendar int not null,
                             ld_subscription int not null, ld_print int not null, ld_password int not null, ld_move int not null,  
                             ld_email int not null, ld_automation int not null, ld_store int not null, ld_readingreq int not null,
                             ld_preview int not null, ld_customid int not null, ld_revision int not null, primary key (ld_folderid, ld_groupid));
create table ld_rating (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_docid bigint not null, 
                        ld_userid bigint not null, ld_vote int not null, ld_username varchar(255), primary key (ld_id));                       
create table ld_note (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                      ld_deleted int not null, ld_tenantid bigint not null, ld_docid bigint not null, 
                      ld_username varchar(255), ld_userid bigint, ld_date timestamp, ld_filename varchar(255), 
                      ld_message varchar(4000), ld_page int not null, ld_fileversion varchar(10),
                      ld_opacity int not null, ld_color varchar(255), ld_left float not null,
 					  ld_top float not null, ld_width float not null, ld_height float not null, 
 					  ld_type varchar(255), ld_recipient varchar(255), ld_recipientemail varchar(255),
 					  ld_shape varchar(255), ld_linecolor varchar(255), ld_lineopacity int not null, ld_linewidth int not null, ld_rotation float not null,
 					  primary key (ld_id));
create table ld_messagetemplate (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                                 ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255) not null, ld_language varchar(10) not null,
                                 ld_description varchar(1000), ld_body varchar(4000), ld_type varchar(255),
                                 ld_subject varchar(1000), primary key (ld_id));
create table ld_contact (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null, 
                         ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint null,
                         ld_firstname varchar(255), ld_lastname varchar(255), ld_email varchar(512),
                         ld_company varchar(255), ld_address varchar(512), ld_phone varchar(255),
                         ld_mobile varchar(255), primary key (ld_id));                                 
create table ld_tenant (ld_id bigint not null, ld_lastmodified timestamp not null, ld_recordversion bigint not null, 
                        ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255),
                        ld_displayname varchar(4000), ld_enabled int not null, ld_expire timestamp null, 
                        ld_street varchar(255), ld_postalcode varchar(255),
                        ld_city varchar(255), ld_country varchar(255), ld_state varchar(255),
                        ld_email varchar(255), ld_telephone varchar(255),
                        ld_maxusers int, ld_maxsessions int, ld_maxrepodocs bigint,
                        ld_maxreposize bigint, ld_type int not null, ld_creation timestamp,
                        ld_qthreshold int, ld_qrecipients varchar(1000), ld_maxguests int, primary key (ld_id));   
create table ld_sequence (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                          ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255) not null,
                          ld_objectid bigint not null, ld_lastreset timestamp null, ld_value bigint not null,
                          primary key (ld_id));
create table ld_extoption (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                          ld_deleted int not null, ld_tenantid bigint not null, ld_setid bigint not null,
                          ld_attribute varchar(255) not null, ld_value varchar(255) not null, ld_category varchar(255), 
                          ld_label varchar(1000), ld_position int not null,
                          primary key (ld_id));
create table ld_temp (ld_int bigint, ld_int1 bigint, ld_date timestamp, ld_string varchar(4000));
create table ld_uniquetag(ld_tag varchar(255), ld_tenantid bigint, ld_count bigint, primary key (ld_tag, ld_tenantid));
create table ld_update (ld_update varchar(255), ld_date timestamp, ld_version varchar(255));
create table ld_patch (ld_patch varchar(255), ld_date timestamp, ld_version varchar(255), ld_name varchar(255), ld_rating int not null, ld_size bigint not null, ld_description varchar(4000));
create table ld_legal (ld_name varchar(255), ld_category varchar(255), ld_title varchar(255), ld_date timestamp, ld_sort int, ld_content varchar(100000), primary key (ld_name));
create table ld_legal_confirmation(ld_legal varchar(255), ld_date timestamp, ld_username varchar(255), ld_user varchar(255), primary key(ld_legal, ld_username));
create table ld_session(ld_id bigint not null, ld_lastmodified timestamp not null, ld_recordversion bigint not null,
                          ld_deleted int not null, ld_tenantid bigint not null, ld_sid varchar(255) not null,
                          ld_username varchar(255), ld_key varchar(255), ld_node varchar(255), ld_tenantname varchar(255),
                          ld_creation timestamp null, ld_finished timestamp null, ld_lastrenew timestamp null, ld_status int not null,
                          ld_clientid varchar(255), ld_clientaddr varchar(255), ld_clienthost varchar(255), ld_keylabel varchar(255), primary key (ld_id));
create table ld_dashlet(ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_name varchar(255), ld_title varchar(255), 
                        ld_type varchar(255), ld_query varchar(4000), ld_content varchar(4000), ld_max int, ld_columns varchar(1000),
                        ld_unique int not null, primary key (ld_id));                           
create table ld_device (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint, 
                        ld_deviceid varchar(255) not null, ld_username varchar(255),
                        ld_browser varchar(255), ld_browserversion varchar(255), ld_operativesystem varchar(255),
                        ld_type varchar(255), ld_ip varchar(255), ld_trusted int, ld_lastlogin timestamp, ld_label varchar(255), primary key (ld_id));
create table ld_password_history (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                                  ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint not null, 
                                  ld_password varchar(255), ld_date timestamp, primary key (ld_id));                    
create table ld_search (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint not null, 
                        ld_name varchar(255), ld_description varchar(1000), ld_options varchar(4000),
                        ld_date timestamp, ld_type int not null, primary key (ld_id));
create table ld_apikey (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                        ld_deleted int not null, ld_tenantid bigint not null, ld_userid bigint not null, ld_lastused timestamp, ld_key varchar(255) not null,
                        ld_name varchar(255) not null, ld_label varchar(255), primary key (ld_id));
                          
-- Create the sequences for ID generation
create sequence ld_bookmark_SEQ start with 100 INCREMENT BY 50;
create sequence ld_document_SEQ start with 100 INCREMENT BY 50;
create sequence ld_generic_SEQ start with 100 INCREMENT BY 50;
create sequence ld_group_SEQ start with 100 INCREMENT BY 50;
create sequence ld_history_SEQ start with 100 INCREMENT BY 50;
create sequence ld_link_SEQ start with 100 INCREMENT BY 50;
create sequence ld_menu_SEQ start with 100 INCREMENT BY 50;
create sequence ld_systemmessage_SEQ start with 100 INCREMENT BY 50;
create sequence ld_template_SEQ start with 100 INCREMENT BY 50;                             
create sequence ld_attributeset_SEQ start with 100 INCREMENT BY 50;
create sequence ld_ticket_SEQ start with 100 INCREMENT BY 50;
create sequence ld_user_SEQ start with 100 INCREMENT BY 50;
create sequence ld_user_history_SEQ start with 100 INCREMENT BY 50;
create sequence ld_version_SEQ start with 100 INCREMENT BY 50;
create sequence ld_folder_SEQ start with 100 INCREMENT BY 50;                         
create sequence ld_folder_history_SEQ start with 100 INCREMENT BY 50;
create sequence ld_rating_SEQ start with 100 INCREMENT BY 50;                      
create sequence ld_note_SEQ start with 100 INCREMENT BY 50;
create sequence ld_messagetemplate_SEQ start with 100 INCREMENT BY 50;
create sequence ld_contact_SEQ start with 100 INCREMENT BY 50;                                 
create sequence ld_tenant_SEQ start with 100 INCREMENT BY 50;  
create sequence ld_sequence_SEQ start with 100 INCREMENT BY 50;
create sequence ld_extoption_SEQ start with 100 INCREMENT BY 50;
create sequence ld_session_SEQ start with 100 INCREMENT BY 50;
create sequence ld_dashlet_SEQ start with 100 INCREMENT BY 50;                          
create sequence ld_device_SEQ start with 100 INCREMENT BY 50;
create sequence ld_password_history_SEQ start with 100 INCREMENT BY 50;                   
create sequence ld_search_SEQ start with 100 INCREMENT BY 50;
create sequence ld_apikey_SEQ start with 100 INCREMENT BY 50;


alter table ld_document add constraint FK75ED9C0276C86307 foreign key (ld_templateid) references ld_template(ld_id);
alter table ld_document add constraint FK75ED9C027C565C60 foreign key (ld_folderid) references ld_folder(ld_id);
alter table ld_document_ext add constraint FK4E0884647C693DFD foreign key (ld_docid) references ld_document(ld_id);
alter table ld_document_acl add constraint FK_DOCACL_GROUP foreign key (ld_groupid) references ld_group(ld_id) on delete cascade;
alter table ld_document_acl add constraint FK_DOCACL_DOCUMENT foreign key (ld_docid) references ld_document(ld_id) on delete cascade;
alter table ld_generic_ext add constraint FK913AF772CF8376C7 foreign key (ld_genid) references ld_generic(ld_id);
alter table ld_tag add constraint FK55BBDA227C693DFD foreign key (ld_docid) references ld_document(ld_id);
alter table ld_link add constraint FK1330661CADD6217 foreign key (ld_docid2) references ld_document(ld_id);
alter table ld_link add constraint FK1330661CADD6216 foreign key (ld_docid1) references ld_document(ld_id);
alter table ld_usergroup add constraint FK2435438DB8B12CA9 foreign key (ld_userid) references ld_user(ld_id) on delete cascade;
alter table ld_usergroup add constraint FK2435438D76F11EA1 foreign key (ld_groupid) references ld_group(ld_id) on delete cascade;
alter table ld_version add constraint FK9B3BD9118A053CE foreign key (ld_documentid) references ld_document(ld_id);
alter table ld_version_ext add constraint FK78C3A1F3B90495EE foreign key (ld_versionid) references ld_version(ld_id);
alter table ld_template_ext add constraint FK6BABB84376C86307 foreign key (ld_templateid) references ld_template(ld_id);
alter table ld_template_acl add constraint FK_TEMPLACL_GROUP foreign key (ld_groupid) references ld_group(ld_id) on delete cascade;
alter table ld_template_acl add constraint FK_TEMPLACL_TEMPL foreign key (ld_templateid) references ld_template(ld_id) on delete cascade;
alter table ld_recipient add constraint FK406A04126621DEBE foreign key (ld_messageid) references ld_systemmessage(ld_id);

alter table ld_attributeset_ext add constraint FK_ATT_ATTSET foreign key (ld_attsetid) references ld_attributeset(ld_id);
alter table ld_ticket add constraint FK_TICKET_USER foreign key (ld_userid) references ld_user(ld_id) on delete cascade;
alter table ld_menu add constraint FK_MENU_PARENT foreign key (ld_parentid) references ld_menu(ld_id);
alter table ld_menu_acl add constraint FK_MENUACL_GROUP foreign key (ld_groupid) references ld_group(ld_id) on delete cascade;
alter table ld_menu_acl add constraint FK_MENUACL_MENU foreign key (ld_menuid) references ld_menu(ld_id) on delete cascade;
alter table ld_folder add constraint FK_FOLDER_PARENT foreign key (ld_parentid) references ld_folder(ld_id);
alter table ld_folder add constraint FK_FOLDER_TEMPLATE foreign key (ld_templateid) references ld_template(ld_id);
alter table ld_folder_ext add constraint FK_FOLDEREXT_FOLDER foreign key (ld_folderid) references ld_folder(ld_id);
alter table ld_folder_acl add constraint FK_FOLDACL_FOLDER foreign key (ld_folderid) references ld_folder(ld_id) on delete cascade;
alter table ld_folder_acl add constraint FK_FOLDACL_GROUP foreign key (ld_groupid) references ld_group(ld_id) on delete cascade;
alter table ld_folder_store add constraint FK_FOLDER_STORE foreign key (ld_folderid) references ld_folder(ld_id) on delete cascade;
alter table ld_foldertag add constraint FK_TAG_FOLDER foreign key (ld_folderid) references ld_folder(ld_id);
alter table ld_workingtime add constraint FK_WRKTIME_USER foreign key (ld_userid) references ld_user(ld_id) on delete cascade;
alter table ld_apikey add constraint FK_APIKEY_USER foreign key (ld_userid) references ld_user(ld_id) on delete cascade;

create unique index AK_DOCUMENT on ld_document (ld_customid, ld_tenantid);
create unique index AK_USER on ld_user (ld_username);
create unique index AK_GROUP on ld_group (ld_name, ld_tenantid);  
create unique index AK_TICKET on ld_ticket (ld_ticketid);
create unique index AK_LINK on ld_link (ld_docid1, ld_docid2, ld_type);
create unique index AK_TEMPLATE on ld_template (ld_name, ld_tenantid);
create unique index AK_GENERIC on ld_generic (ld_type, ld_subtype, ld_qualifier,ld_tenantid);
create unique index AK_VERSION on ld_version (ld_documentid, ld_version);
create unique index AK_RATING on ld_rating (ld_docid, ld_userid);
create unique index AK_MSGTEMPL on ld_messagetemplate (ld_name, ld_language, ld_tenantid);
create unique index AK_TENANT on ld_tenant (ld_name);
create unique index AK_SEQUENCE on ld_sequence (ld_name, ld_objectid, ld_tenantid);
create unique index AK_EXTOPTION on ld_extoption (ld_setid, ld_attribute, ld_value);
create unique index AK_ATTRIBUTESET on ld_attributeset (ld_name, ld_tenantid);
create unique index AK_SESSION on ld_session (ld_sid);
create unique index AK_DASHLET on ld_dashlet (ld_name, ld_tenantid);
create unique index AK_DEVICE on ld_device (ld_deviceid);
create unique index AK_SEARCH on ld_search (ld_userid, ld_name);
create unique index AK_APIKEY on ld_apikey (ld_key);
create unique index AK_APIKEY2 on ld_apikey (ld_name, ld_userid);



--Prepare some indexes
create index LD_DOC_LUID on ld_document (ld_lockuserid);
create index LD_DOC_FID on ld_document (ld_folderid);
create index LD_DOC_RID on ld_document (ld_docref);
create index LD_DOC_STATUS on ld_document (ld_status);
create index LD_HIST_DOCID on ld_history (ld_docid);
create index LD_HIST_UID on ld_history (ld_userid);
create index LD_HIST_NOT on ld_history (ld_notified);
create index LD_HIST_EVENT on ld_history (ld_event);
create index LD_FHIST_FID on ld_folder_history (ld_folderid);
create index LD_FHIST_NOT on ld_folder_history (ld_notified);
create index LD_UHIST_UID on ld_user_history (ld_userid);
create index LD_TAG_TAG on ld_tag (ld_tag);
create index LD_FTAG_TAG on ld_foldertag (ld_tag);
create index LD_EXT_NAME on ld_document_ext (ld_name);
create index LD_FLD_NAME on ld_folder (ld_name);
create index LD_FLD_PATH on ld_folder (ld_path);
create index LD_FLD_FOLDREF on ld_folder (ld_foldref);
create index LD_RCP_MID_NAME on ld_recipient (ld_messageid, ld_name);
create index LD_DEV_USERID on ld_device (ld_userid);
create index LD_PHIST_USERID on ld_password_history (ld_userid);
create index LD_APIKEY_USERID on ld_apikey (ld_userid);


insert into ld_tenant(ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_displayname,ld_type,ld_enabled,ld_expire,ld_recordversion)
values     (1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'default','Default',0,1,null,1);


insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'/',1,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (2,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'administration',1,'menu.png',1,1,1,20,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (9,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'security',2,'menu.png',1,1,1,20,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (8,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'impex',2,'menu.png',1,1,1,40,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (25,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'documentmetadata',2,'menu.png',1,1,1,30,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (7,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'settings',2,'menu.png',1,1,1,60,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (5,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'frontend',1,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1500,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'documents',5,'menu.png',1,1,1,50,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1600,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'history',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1601,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'sessions',1600,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1602,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'trash',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1603,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'versions',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1605,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'aliases',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1606,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'calendar',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1607,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'signature',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1608,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'capture',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1609,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'preview',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1610,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'rating',1500,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1510,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'search',5,'menu.png',1,1,1,60,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1511,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'sharesearch',1510,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1520,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'dashboard',5,'menu.png',1,1,1,40,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1525,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'messages',1520,'menu.png',1,1,1,40,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1526,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'calendar',1520,'menu.png',1,1,1,40,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (80,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'system',2,'system.png',1,1,1,10,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (90,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'reports',2,'reports.png',1,1,1,50,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (99,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'proxy',7,'proxy.png',1,1,1,6,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (100,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'parameters',7,'parameters.png',1,1,1,6,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (101,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'searchandindexing',7,'searchandindexing.png',1,1,1,6,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (102,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'guisettings',7,'guisettings.png',1,1,1,6,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (103,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'outgoingemail',7,'outgoingemail.png',1,1,1,6,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (105,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'repositories',7,'repositories.png',1,1,1,6,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (106,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'auditing',7,'settings.png',1,1,1,6,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (14,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'scheduledtasks',80,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-2,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'lastchanges',90,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-3,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'lockeddocs',90,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-5,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'deleteddocs',90,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-9,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'destroydocs',-5,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-6,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'deletedfolders',90,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-7,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'tickets',90,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (-8,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'apicalls',90,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (3,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'protocols',7,'menu.png',1,1,1,1,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (110,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'mainmenu',5,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (16,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'tools',110,'menu.png',1,1,1,20,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (40,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'account',110,'menu.png',1,1,1,10,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1530,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'contacts',40,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (1535,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'interfacedensity',110,'menu.png',1,1,1,5,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (70,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'general',80,'menu.png',1,1,1,20,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (71,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'sessions',70,'menu.png',1,1,1,20,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (72,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'logs',70,'menu.png',1,1,1,20,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (73,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'runlevel',70,'menu.png',1,1,1,20,1);

insert into ld_menu 
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_recordversion,ld_tenantid,ld_position,ld_enabled)
values     (200,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'textcontent',16,'text.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (10,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'index',1500,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (11,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'userinterface',1500,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (12,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'dropspot',1500,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (41,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'security',40,'menu.png',1,1,1,1,1);

insert into ld_menu
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values     (42,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'apikeys',41,'menu.png',1,1,1,1,1);



insert into ld_group
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_description,ld_type,ld_recordversion,ld_source)
values     (1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'admin','Group of admins',0,1,'local');

insert into ld_group
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_description,ld_type,ld_recordversion,ld_source)
values     (2,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'author','Group of authors',0,1,'local');

insert into ld_group
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_description,ld_type,ld_recordversion,ld_source)
values     (3,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'guest','Group of guests',0,1,'local');

insert into ld_group
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_description,ld_type,ld_recordversion,ld_source)
values     (4,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'poweruser','Group of power users',0,1,'local');

insert into ld_group
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_description,ld_type,ld_recordversion,ld_source)
values     (-10000,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'publisher','Group of publishers',0,1,'local');

insert into ld_user
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_enabled,ld_username,ld_password,ld_passwordmd4,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_telephone2,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_welcomescreen,ld_passwordexpired,ld_tenantid,ld_recordversion,ld_enforcewrktime,ld_maxinactivity,ld_evalform,ld_legals)
values     (1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'admin','8C6976E5B5410415BDE908BD4DEE15DFB167A9C873FC4BB8A81F6F2AB448A918','U8FeEPvxYRhKNCBsLa0K+1rD1tTtR6yctJIwxje2QMwEOlEQx9HuiA==','Admin','Admin','','','','','en','admin@admin.net','','',0,null,0,0,-1,1520,0,1,1,0,-1,1,1);
insert into ld_group
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_tenantid,ld_name,ld_type,ld_recordversion,ld_source)
values     (-1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'_user_1',1,1,'local');
insert into ld_usergroup
values (1,1);
insert into ld_usergroup
values (-1,1);

insert into ld_generic (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_type, ld_subtype, ld_qualifier, ld_string1, ld_integer1, ld_integer2, ld_integer3,ld_tenantid,ld_recordversion)
values (-50, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'usersetting', 'dashlet-1', 1, 0, 1, 0, 0, 1, 1);
insert into ld_generic (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_type, ld_subtype, ld_qualifier, ld_string1, ld_integer1, ld_integer2, ld_integer3,ld_tenantid,ld_recordversion)
values (-51, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'usersetting', 'dashlet-3', 1, 0, 3, 0, 1, 1, 1);
insert into ld_generic (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_type, ld_subtype, ld_qualifier, ld_string1, ld_integer1, ld_integer2, ld_integer3,ld_tenantid,ld_recordversion)
values (-52, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 0, 'usersetting', 'dashlet-6', 1, 0, 6, 1, 0, 1, 1);

insert into ld_user
           (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_enabled,ld_username,ld_password,ld_name,ld_firstname,ld_street,ld_postalcode,ld_city,ld_country,ld_language,ld_email,ld_telephone,ld_type,ld_passwordchanged,ld_passwordexpires,ld_source,ld_quota,ld_passwordexpired,ld_tenantid,ld_recordversion,ld_enforcewrktime,ld_evalform,ld_legals)
values     (-1010,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,'_system','','User','System','','','','','en','system@acme.com','',1,null,0,0,-1,0,1,1,0,1,0);
insert into ld_group(ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_description,ld_type,ld_tenantid,ld_recordversion,ld_source)
values     (-1010,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'_user_-1010','',1,1,1,'local');
insert into ld_usergroup
values (-1010,-1010);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (2,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (14,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (25,4,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (5,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (5,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (5,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (5,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1500,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1500,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1500,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1500,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1510,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1510,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1510,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1510,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1520,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1520,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1520,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1520,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1525,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1525,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1525,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1525,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1526,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1526,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1526,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1526,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1530,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1530,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1530,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1530,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1535,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1535,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1535,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1535,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (200,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (200,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (200,4,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1602,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1602,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1602,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1602,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1603,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1603,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1603,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1603,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1606,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1606,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1606,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1606,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1607,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1607,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1607,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1607,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1609,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1609,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1609,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (1609,-10000,1,0);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 10,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 11,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 12,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 16,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 40,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 41,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 42,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) select 1610,ld_id,1,0 from ld_group where ld_type=0 and not ld_name='admin';

insert into ld_folder (ld_id,ld_lastmodified,ld_deleted,ld_name,ld_parentid,ld_type,ld_creation,ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden,ld_path)
values (5,CURRENT_TIMESTAMP,0,'/',5,1,CURRENT_TIMESTAMP,0,1,1,1,0,'/');
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (5,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (5,3,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (5,4,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (5,-10000,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1);
insert into ld_folder_history (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_docid, ld_folderid, ld_userid, ld_date, ld_username, ld_event, ld_comment, ld_version, ld_notified, ld_path, ld_tenantid, ld_recordversion)
values (1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,null,5,1,CURRENT_TIMESTAMP,'admin','event.folder.created',null,'1.0',1,'/',1,1);

insert into ld_folder (ld_id,ld_lastmodified,ld_deleted,ld_name,ld_parentid,ld_type,ld_creation, ld_templocked,ld_tenantid,ld_recordversion,ld_position,ld_hidden,ld_path)
values (4,CURRENT_TIMESTAMP,0,'Default',5,1,CURRENT_TIMESTAMP,0,1,1,1,0,'/4');
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (4,2,1,1,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (4,3,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,0,1,0,0);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write , ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (4,4,1,1,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1);
insert into ld_folder_acl(ld_folderid, ld_groupid, ld_read, ld_write, ld_add, ld_security, ld_immutable, ld_delete, ld_rename, ld_import, ld_export, ld_sign, ld_archive, ld_workflow, ld_download, ld_calendar, ld_subscription, ld_print, ld_password, ld_move, ld_email, ld_automation, ld_store, ld_readingreq, ld_preview, ld_customid, ld_revision)
values (4,-10000,1,1,1,0,0,1,1,0,0,0,0,0,1,1,0,1,0,1,1,0,0,1,1,1,0);
insert into ld_folder_history (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_docid, ld_folderid, ld_userid, ld_date, ld_username, ld_event, ld_comment, ld_version, ld_notified, ld_path, ld_tenantid, ld_recordversion)
values (2,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,null,4,1,CURRENT_TIMESTAMP,'admin','event.folder.created',null,'1.0',1,'/Default',1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'task.report','system','en', '$product - $task',
'$task<br/>
$I18N.get(''startedon''): <b>$DateTool.format($started, true)</b><br/>
$I18N.get(''finishedon''): <b>$DateTool.format($ended, true)</b><br/>
<hr/>
#if( $error )
$I18N.get(''error''): <b>$error</b>
<hr />
#end
$report',1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(2, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'psw.rec1','system','en', '$product - $I18N.get(''emailnotifyaccountobject'')', 
'$I18N.format(''emailnotifyaccount'', $user.fullName)<br/>
$I18N.get(''username''): <b>$user.username</b><br/>
$I18N.get(''password''): <b>$password</b><br/>
$I18N.get(''clickhere''): <a href="$url">$url</a><br/><br/>
$I18N.get(''askedtochangepswdatlogin'')',1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(3, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'psw.rec2','system','en', '$product - $I18N.get(''passwordrequest'')',
'$product - $I18N.get(''passwordrequest'')<br/><br/>
$I18N.get(''clickhere''): <a href="$url">$url</a>',1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(4, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'newdoc','system','en', '$product - $I18N.get(''newdocscreated'')',
'$I18N.get(''newdocscreated'')<br/>
<hr/>
$message
<hr/>
$I18N.get(''user''): <b>$creator.fullName</b> $UserTool.getAvatarImg($creator.id, 16) <br/>
$I18N.get(''date''): <b>$DateTool.format($CURRENT_DATE, true)</b><br/>
<hr/>
<b>$I18N.get(''documents'')</b>:
#foreach( $doc in $documents )
  <br/>$doc.fileName | <a href="$DocTool.downloadUrl($doc)">$I18N.get(''download'')</a> | <a href="$DocTool.displayUrl($doc)">$I18N.get(''display'')</a>
#end',1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(5, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'empty','user','en', null,null,1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(7, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'newdevice','system','en', '$I18N.get(''newloginto'') $product $I18N.get(''ffrom'') $device.browser $I18N.get(''on'') $device.operativeSystem',
'$I18N.get(''wenoticednewlogin'') <b>$user.username</b> $I18N.get(''fromnewdevice'')<br/>
$I18N.get(''device''): $device.browser $I18N.get(''on'') $device.operativeSystem<br/>
$I18N.get(''dateandtime''): $DateTool.formatDate($event.date)<br/>
$I18N.get(''ipaddress''): $client.address<br/>
#if($client.geolocation)
$I18N.get(''ipgeolocation''): $client.geolocation<br/>
#end
<br/>
<b>$I18N.get(''ifthiswasyou'')</b><br/>$I18N.get(''youcanignore'')
<br/><br/>
<b>$I18N.get(''ifthiswasnotyou'')</b><br/>$I18N.get(''stepstoprotect'')<ul>
<li>$I18N.get(''changeyourpasswd'')</li>
<li>$I18N.get(''reporttosysadmin'')</li></ul>',1,1);

insert into ld_messagetemplate (ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_name, ld_type, ld_language, ld_subject, ld_body,ld_tenantid,ld_recordversion)
values(8, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP,0,'checkin','system','en', '$product - $I18N.get(''newdocversion'')',
'$I18N.get(''newdocversion'')<br/>
<hr/>
$message
<hr/>
$I18N.get(''user''): <b>$user.fullName</b> $UserTool.getAvatarImg($user.id, 16) <br/>
$I18N.get(''date''): <b>$DateTool.format($CURRENT_DATE, true)</b><br/>
<hr/>
<b>$I18N.get(''document'')</b>:<br/>
$document.fileName | <a href="$DocTool.downloadUrl($document)">$I18N.get(''download'')</a> | <a href="$DocTool.displayUrl($document)">$I18N.get(''display'')</a><br/>
#if($tile)<img src="$tile" />#end',1,1);


insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'checkout','event.checkedoutdocs','from Document where lockUserId=$user.id and status=1 order by date desc',null,'document', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(2,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'checkin','event.checkedindocs','from DocumentHistory where userId=$user.id and event=''event.checkedin'' order by date desc',null,'docevent', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(3,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'locked','event.lockeddocs','from Document where lockUserId=$user.id and status=2 order by date desc',null,'document', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(4,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'download','event.downloadeddocs','from DocumentHistory where userId=$user.id and event=''event.downloaded'' order by date desc',null,'docevent', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(5,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'change','event.changeddocs','from DocumentHistory where userId=$user.id and event=''event.changed'' order by date desc',null,'docevent', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(6,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'notes','lastnotes','from DocumentNote where userId=$user.id order by date desc',null,'note', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(7,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'tagcloud','tagcloud',null,null,'content', 10, 0);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(8,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'lastaccessed','lastaccesseddocuments', 
'from DocumentHistory where userId=$user.id and event in(''event.stored'',''event.changed'',''event.downloaded'',''event.checkedin'',''event.checkedout'',
''event.renamed'',''event.moved'',''event.locked'',''event.unlocked'',''event.workflowstatus'',''event.viewed'',''event.restored'',''event.exported'',
''event.exportpdf'',''event.password.protected'',''event.password.unprotected'',''event.form.submitted'',''event.form.edited'',''event.copyed'') 
and date >= ''$DateTool.formatSQL($DateTool.addDays($CURRENT_DATE, -10))'' 
order by date desc',null,'docevent', 10, 1);
insert into ld_dashlet	(ld_id, ld_lastmodified, ld_creation, ld_deleted, ld_recordversion, ld_tenantid, ld_name, ld_title, ld_query, ld_content, ld_type, ld_max, ld_unique)
values(9,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,1,1,'bookmark','bookmarks','',null,'bookmark', 10, 0);