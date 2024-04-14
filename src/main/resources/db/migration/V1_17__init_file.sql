create table if not exists file_entity
(
    id                   bigint       not null
        primary key,
    created_by           bigint       null,
    created_on           datetime(6)  not null,
    deleted              bit          not null,
    updated_by           bigint       null,
    updated_on           datetime(6)  null,
    version              smallint     null,
    bucket_name          varchar(255) null,
    business_type        smallint     not null,
    content_type         varchar(255) null,
    digest               varchar(255) null,
    file                 mediumblob   null,
    file_category        smallint     not null,
    media_type           varchar(255) null,
    name                 varchar(255) null,
    object_key           varchar(255) null,
    object_name          varchar(255) null,
    object_value         text         null,
    original_name        varchar(255) null,
    overview             varchar(255) null,
    size                 bigint       null,
    storage_service_type smallint     not null,
    url                  varchar(255) null,
    view_count           int          null,
    user_id              bigint       null,
    constraint UK_au3b3dng56cgbtly8ntjjhusi
        unique (digest),
    constraint UK_j6i8vbhluf996oa1xj5ck1nkh
        unique (object_key),
    constraint FK4tffb6acfi5hjgr5oms7njdpu
        foreign key (user_id) references user_entity (id),
    check (`business_type` between 0 and 8),
    check (`file_category` between 0 and 2),
    check (`storage_service_type` between 0 and 1)
);

create table if not exists file_entity_seq
(
    next_val bigint null
);

INSERT INTO file_entity_seq (next_val)
VALUES (1);

SET @permission_max_id = (SELECT max(id) FROM permission_entity);
SET @permission_next_val = (SELECT next_val FROM permission_entity_seq);
SET @permission_id_1 = (@permission_max_id + 1);
SET @permission_id_2 = (@permission_max_id + 2);
SET @permission_id_3 = (@permission_max_id + 3);
SET @permission_id_4 = (@permission_max_id + 4);
SET @permission_id_5 = (@permission_max_id + 5);
SET @permission_id_6 = (@permission_max_id + 6);
SET @permission_id_7 = (@permission_max_id + 7);

INSERT INTO permission_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, case_insensitive, method, name, overview, sort, type, matcher_id)
VALUES (@permission_id_1, null, now(), false, null, now(), 0, '[files] POST files/images', false, 2, '/files/images', null, 0, 0, null),
       (@permission_id_2, null, now(), false, null, now(), 0, '[files] GET files/images/users/{userId}', false, 0, '/files/images/users/{userId}', null, 0, 0, null),
       (@permission_id_3, null, now(), false, null, now(), 0, '[files] GET files/images/{id}/{name}', false, 0, '/files/images/{id}/{name}', null, 0, 0, null),
       (@permission_id_4, null, now(), false, null, now(), 0, '[files] GET files', false, 0, '/files', null, 0, 0, null),
       (@permission_id_5, null, now(), false, null, now(), 0, '[files] GET files/{id}', false, 0, '/files/{id}', null, 0, 0, null),
       (@permission_id_6, null, now(), false, null, now(), 0, '[files] DELETE files/{id}', false, 5, '/files/{id}', null, 0, 0, null),
       (@permission_id_7, null, now(), false, null, now(), 0, '[files] DELETE files/images/{id}', false, 5, '/files/images/{id}', null, 0, 0, null);

INSERT INTO role_entity_permissions (roles_id, permissions_id)
VALUES (1, @permission_id_4),
       (1, @permission_id_5),
       (1, @permission_id_6);

UPDATE permission_entity_seq t SET t.next_val = @permission_max_id + 50 + 7 WHERE next_val = @permission_next_val;

SET @menu_max_id = (SELECT max(id) FROM menu_entity);
SET @menu_next_val = (SELECT next_val FROM menu_entity_seq);
SET @menu_id_1 = (@menu_next_val + 50 + 1);

INSERT INTO menu_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, link, name, sort)
VALUES (@menu_id_1, null, now(), false, null, now(), 0, '/admin/files', 'File Mgmt', 8);

INSERT INTO menu_entity_roles (menus_id, roles_id)
VALUES (@menu_id_1, 1);

UPDATE menu_entity_seq t SET t.next_val = @menu_max_id + 100 + 1 WHERE next_val = @menu_next_val;

SET @action_max_id = (SELECT max(id) FROM action_entity);
SET @action_next_val = (SELECT next_val FROM action_entity_seq);
SET @action_id_1 = (@action_max_id + 1);

INSERT INTO action_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, name, sort, menu_id, submenu_id)
VALUES (@action_id_1, null, now(), false, null, now(), 0, 'Files Delete', 'Files#Delete', 0, @menu_id_1, null);

INSERT INTO action_entity_roles (actions_id, roles_id)
VALUES (@action_id_1, 1);

UPDATE action_entity_seq t SET t.next_val = @action_max_id + 50 + 1 WHERE next_val = @action_next_val;
