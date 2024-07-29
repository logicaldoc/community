-- datetime -> timestamp
-- mediumtext -> varchar(100000)

create table ld_webservicecall (ld_id bigint not null, ld_lastmodified timestamp not null, ld_creation timestamp not null, ld_recordversion bigint not null,
                                ld_deleted int not null, ld_tenantid bigint not null, ld_keylabel varchar(255),
                                ld_userid bigint, ld_date timestamp, ld_username varchar(255), ld_event varchar(255), 
                                ld_comment varchar(4000), ld_path varchar(4000), ld_sessionid varchar(255),
                                ld_userlogin varchar(255), ld_ip varchar(255), ld_geolocation varchar(255), 
                                ld_device varchar(255), ld_protocol varchar(255), primary key (ld_id));
                                
insert into hibernate_sequences(sequence_name, next_val) values ('ld_webservicecall', 100);