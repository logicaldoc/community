insert into ld_menu (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values (201,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'onlyoffice',16,'document.png',1,1,1,1,1);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (201,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (201,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (201,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (201,-10000,1,0);
