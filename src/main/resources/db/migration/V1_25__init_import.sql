SET @permission_max_id = (SELECT max(id) FROM permission_entity);
SET @permission_next_val = (SELECT next_val FROM permission_entity_seq);
SET @permission_id_1 = (@permission_max_id + 1);
SET @permission_id_2 = (@permission_max_id + 2);
SET @permission_id_3 = (@permission_max_id + 3);
SET @permission_id_4 = (@permission_max_id + 4);
SET @permission_id_5 = (@permission_max_id + 5);
SET @permission_id_6 = (@permission_max_id + 6);

INSERT INTO permission_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, case_insensitive, method, name, overview, sort, type, matcher_id)
VALUES (@permission_id_1, null, now(), false, null, now(), 0, '[imports] POST imports/users', false, 2, '/imports/users', null, 0, 0, null),
       (@permission_id_2, null, now(), false, null, now(), 0, '[imports] POST imports/sections', false, 2, '/imports/sections', null, 0, 0, null),
       (@permission_id_3, null, now(), false, null, now(), 0, '[imports] POST imports/section-groups', false, 2, '/imports/section-groups', null, 0, 0, null),
       (@permission_id_4, null, now(), false, null, now(), 0, '[imports] POST imports/tags', false, 2, '/imports/tags', null, 0, 0, null),
       (@permission_id_5, null, now(), false, null, now(), 0, '[imports] POST imports/tag-groups', false, 2, '/imports/tag-groups', null, 0, 0, null),
       (@permission_id_6, null, now(), false, null, now(), 0, '[imports] POST imports/posts', false, 2, '/imports/posts', null, 0, 0, null);

INSERT INTO role_entity_permissions (roles_id, permissions_id)
VALUES (1, @permission_id_1),
       (1, @permission_id_2),
       (1, @permission_id_3),
       (1, @permission_id_4),
       (1, @permission_id_5),
       (1, @permission_id_6);

UPDATE permission_entity_seq t SET t.next_val = @permission_max_id + 50 + 6 WHERE next_val = @permission_next_val;
