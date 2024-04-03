alter table user_entity add temporary_storage text null;

SET @permission_max_id = (SELECT max(id) FROM permission_entity);
SET @permission_next_val = (SELECT next_val FROM permission_entity_seq);
SET @permission_id_1 = (@permission_max_id + 1);
SET @permission_id_2 = (@permission_max_id + 2);

INSERT INTO permission_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, case_insensitive, method, name, overview, sort, type, matcher_id)
VALUES (@permission_id_1, null, now(), false, null, now(), 0, '[users] GET users/temporary-storage', false, 0, '/users/temporary-storage', null, 0, 0, null),
       (@permission_id_2, null, now(), false, null, now(), 0, '[users] PUT users/temporary-storage', false, 3, '/users/temporary-storage', null, 0, 0, null);

UPDATE permission_entity_seq t SET t.next_val = @permission_max_id + 50 + 2 WHERE next_val = @permission_next_val;
