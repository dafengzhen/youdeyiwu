alter table post_entity add styles varchar(255) null;

alter table post_entity add class_names varchar(255) null;

SET @permission_max_id = (SELECT max(id) FROM permission_entity);
SET @permission_next_val = (SELECT next_val FROM permission_entity_seq);
SET @permission_id_1 = (@permission_max_id + 1);

INSERT INTO permission_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, case_insensitive, method, name, overview, sort, type, matcher_id)
VALUES (@permission_id_1, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/styles', false, 3, '/posts/{id}/styles', null, 0, 0, null);

INSERT INTO role_entity_permissions (roles_id, permissions_id)
VALUES (1, @permission_id_1);

UPDATE permission_entity_seq t SET t.next_val = @permission_max_id + 50 + 1 WHERE next_val = @permission_next_val;

SET @action_max_id = (SELECT max(id) FROM action_entity);
SET @action_next_val = (SELECT next_val FROM action_entity_seq);
SET @action_id_1 = (@action_max_id + 1);

INSERT INTO action_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, name, sort, menu_id, submenu_id)
VALUES (@action_id_1, null, now(), false, null, now(), 0, 'Post Update Styles', 'Posts#Update Styles', 0, 6, null);

INSERT INTO action_entity_roles (actions_id, roles_id)
VALUES (@action_id_1, 1);

UPDATE action_entity_seq t SET t.next_val = @action_max_id + 50 + 1 WHERE next_val = @action_next_val;
