insert into ld_menu (ld_id,ld_lastmodified,ld_creation,ld_deleted,ld_name,ld_parentid,ld_icon,ld_type,ld_tenantid,ld_recordversion,ld_position,ld_enabled)
values (-2070,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,0,'dropbox',16,'document.png',1,1,1,1,1);

insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (-2070,2,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (-2070,3,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (-2070,4,1,0);
insert into ld_menu_acl(ld_menuid, ld_groupid, ld_read, ld_write) values (-2070,-10000,1,0);
