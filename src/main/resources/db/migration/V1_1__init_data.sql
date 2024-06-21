SET @role_next_val = (SELECT next_val FROM role_entity_seq);

INSERT INTO role_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, display, name, overview, sort)
VALUES (1, null, now(), false, null, now(), 0, true, 'Admin', null, 0);

UPDATE role_entity_seq t SET t.next_val = 1 + 1 WHERE next_val = @role_next_val;

SET @permission_next_val = (SELECT next_val FROM permission_entity_seq);

insert into permission_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, case_insensitive, method, name, overview, sort, type, matcher_id)
values  (1, null, now(), false, null, now(), 0, '[users] POST users/register', false, 2, '/users/register', null, 0, 0, null),
        (2, null, now(), false, null, now(), 0, '[users] POST users/login', false, 2, '/users/login', null, 0, 0, null),
        (3, null, now(), false, null, now(), 0, '[users] POST users/logout', false, 2, '/users/logout', null, 0, 0, null),
        (4, null, now(), false, null, now(), 0, '[users] POST users/{id}/add-roles', false, 2, '/users/{id}/add-roles', null, 0, 0, null),
        (5, null, now(), false, null, now(), 0, '[users] POST users/{id}/remove-roles', false, 2, '/users/{id}/remove-roles', null, 0, 0, null),
        (6, null, now(), false, null, now(), 0, '[users] PUT users/{id}/roles', false, 3, '/users/{id}/roles', null, 0, 0, null),
        (7, null, now(), false, null, now(), 0, '[users] PUT users/{id}/profile', false, 3, '/users/{id}/profile', null, 0, 0, null),
        (8, null, now(), false, null, now(), 0, '[users] PUT users/{id}/username', false, 3, '/users/{id}/username', null, 0, 0, null),
        (9, null, now(), false, null, now(), 0, '[users] PUT users/{id}/password', false, 3, '/users/{id}/password', null, 0, 0, null),
        (10, null, now(), false, null, now(), 0, '[users] PUT users/{id}/states', false, 3, '/users/{id}/states', null, 0, 0, null),
        (11, null, now(), false, null, now(), 0, '[users] GET users/{id}/roles-permissions', false, 0, '/users/{id}/roles-permissions', null, 0, 0, null),
        (12, null, now(), false, null, now(), 0, '[users] GET users/menus', false, 0, '/users/menus', null, 0, 0, null),
        (13, null, now(), false, null, now(), 0, '[users] GET users/login-info', false, 0, '/users/login-info', null, 0, 0, null),
        (14, null, now(), false, null, now(), 0, '[users] GET users/count-by-date', false, 0, '/users/count-by-date', null, 0, 0, null),
        (15, null, now(), false, null, now(), 0, '[users] GET users/select-all', false, 0, '/users/select-all', null, 0, 0, null),
        (16, null, now(), false, null, now(), 0, '[users] GET users/{id}/details', false, 0, '/users/{id}/details', null, 0, 0, null),
        (17, null, now(), false, null, now(), 0, '[users] GET users/{id}', false, 0, '/users/{id:\d+}', null, 0, 0, null),
        (18, null, now(), false, null, now(), 0, '[users] DELETE users/{id}', false, 5, '/users/{id:\d+}', null, 0, 0, null),
        (19, null, now(), false, null, now(), 0, '[users] GET users', false, 0, '/users', null, 0, 0, null),
        (20, null, now(), false, null, now(), 0, '[roles] POST roles', false, 2, '/roles', null, 0, 0, null),
        (21, null, now(), false, null, now(), 0, '[roles] GET roles', false, 0, '/roles', null, 0, 0, null),
        (22, null, now(), false, null, now(), 0, '[roles] POST roles/{id}/add-permissions', false, 2, '/roles/{id}/add-permissions', null, 0, 0, null),
        (23, null, now(), false, null, now(), 0, '[roles] POST roles/{id}/remove-permissions', false, 2, '/roles/{id}/remove-permissions', null, 0, 0, null),
        (24, null, now(), false, null, now(), 0, '[roles] PUT roles/{id}/permissions', false, 3, '/roles/{id}/permissions', null, 0, 0, null),
        (25, null, now(), false, null, now(), 0, '[roles] GET roles/{id}/permissions', false, 0, '/roles/{id}/permissions', null, 0, 0, null),
        (26, null, now(), false, null, now(), 0, '[roles] PUT roles/{id}', false, 3, '/roles/{id:\d+}', null, 0, 0, null),
        (27, null, now(), false, null, now(), 0, '[roles] GET roles/{id}', false, 0, '/roles/{id:\d+}', null, 0, 0, null),
        (28, null, now(), false, null, now(), 0, '[roles] DELETE roles/{id}', false, 5, '/roles/{id:\d+}', null, 0, 0, null),
        (29, null, now(), false, null, now(), 0, '[permissions] POST permissions', false, 2, '/permissions', null, 0, 0, null),
        (30, null, now(), false, null, now(), 0, '[permissions] GET permissions', false, 0, '/permissions', null, 0, 0, null),
        (31, null, now(), false, null, now(), 0, '[permissions] PUT permissions/{id}/roles', false, 3, '/permissions/{id}/roles', null, 0, 0, null),
        (32, null, now(), false, null, now(), 0, '[permissions] PUT permissions/{id}', false, 3, '/permissions/{id:\d+}', null, 0, 0, null),
        (33, null, now(), false, null, now(), 0, '[permissions] GET permissions/{id}', false, 0, '/permissions/{id:\d+}', null, 0, 0, null),
        (34, null, now(), false, null, now(), 0, '[permissions] DELETE permissions/{id}', false, 5, '/permissions/{id:\d+}', null, 0, 0, null),
        (35, null, now(), false, null, now(), 0, '[menus] POST menus', false, 2, '/menus', null, 0, 0, null),
        (36, null, now(), false, null, now(), 0, '[menus] GET menus', false, 0, '/menus', null, 0, 0, null),
        (37, null, now(), false, null, now(), 0, '[menus] PUT menus/{id}/roles', false, 3, '/menus/{id}/roles', null, 0, 0, null),
        (38, null, now(), false, null, now(), 0, '[menus] PUT menus/{id}', false, 3, '/menus/{id:\d+}', null, 0, 0, null),
        (39, null, now(), false, null, now(), 0, '[menus] GET menus/{id}', false, 0, '/menus/{id:\d+}', null, 0, 0, null),
        (40, null, now(), false, null, now(), 0, '[menus] DELETE menus/{id}', false, 5, '/menus/{id:\d+}', null, 0, 0, null),
        (41, null, now(), false, null, now(), 0, '[submenus] POST submenus', false, 2, '/submenus', null, 0, 0, null),
        (42, null, now(), false, null, now(), 0, '[submenus] GET submenus', false, 0, '/submenus', null, 0, 0, null),
        (43, null, now(), false, null, now(), 0, '[submenus] PUT submenus/{id}/roles', false, 3, '/submenus/{id}/roles', null, 0, 0, null),
        (44, null, now(), false, null, now(), 0, '[submenus] PUT submenus/{id}', false, 3, '/submenus/{id:\d+}', null, 0, 0, null),
        (45, null, now(), false, null, now(), 0, '[submenus] GET submenus/{id}', false, 0, '/submenus/{id:\d+}', null, 0, 0, null),
        (46, null, now(), false, null, now(), 0, '[submenus] DELETE submenus/{id}', false, 5, '/submenus/{id:\d+}', null, 0, 0, null),
        (47, null, now(), false, null, now(), 0, '[actions] POST actions', false, 2, '/actions', null, 0, 0, null),
        (48, null, now(), false, null, now(), 0, '[actions] GET actions', false, 0, '/actions', null, 0, 0, null),
        (49, null, now(), false, null, now(), 0, '[actions] PUT actions/{id}/roles', false, 3, '/actions/{id}/roles', null, 0, 0, null),
        (50, null, now(), false, null, now(), 0, '[actions] PUT actions/{id}', false, 3, '/actions/{id:\d+}', null, 0, 0, null),
        (51, null, now(), false, null, now(), 0, '[actions] GET actions/{id}', false, 0, '/actions/{id:\d+}', null, 0, 0, null),
        (52, null, now(), false, null, now(), 0, '[actions] DELETE actions/{id}', false, 5, '/actions/{id:\d+}', null, 0, 0, null),
        (53, null, now(), false, null, now(), 0, '[points] POST points/rules', false, 2, '/points/rules', null, 0, 0, null),
        (54, null, now(), false, null, now(), 0, '[points] GET points/rules', false, 0, '/points/rules', null, 0, 0, null),
        (55, null, now(), false, null, now(), 0, '[points] POST points/permission-rules', false, 2, '/points/permission-rules', null, 0, 0, null),
        (56, null, now(), false, null, now(), 0, '[points] GET points/permission-rules', false, 0, '/points/permission-rules', null, 0, 0, null),
        (57, null, now(), false, null, now(), 0, '[points] GET points/histories', false, 0, '/points/histories', null, 0, 0, null),
        (58, null, now(), false, null, now(), 0, '[messages] POST messages/global-messages', false, 2, '/messages/global-messages', null, 0, 0, null),
        (59, null, now(), false, null, now(), 0, '[messages] GET messages/global-messages', false, 0, '/messages/global-messages', null, 0, 0, null),
        (60, null, now(), false, null, now(), 0, '[messages] POST messages', false, 2, '/messages', null, 0, 0, null),
        (61, null, now(), false, null, now(), 0, '[messages] GET messages', false, 0, '/messages', null, 0, 0, null),
        (62, null, now(), false, null, now(), 0, '[messages] PUT messages/global-messages/{id}/state', false, 3, '/messages/global-messages/{id}/state', null, 0, 0, null),
        (63, null, now(), false, null, now(), 0, '[messages] PUT messages/{id}/state', false, 3, '/messages/{id}/state', null, 0, 0, null),
        (64, null, now(), false, null, now(), 0, '[messages] GET messages/global-messages/{id}', false, 0, '/messages/global-messages/{id:\d+}', null, 0, 0, null),
        (65, null, now(), false, null, now(), 0, '[messages] GET messages/{id}', false, 0, '/messages/{id:\d+}', null, 0, 0, null),
        (66, null, now(), false, null, now(), 0, '[messages] DELETE messages/{id}', false, 5, '/messages/{id:\d+}', null, 0, 0, null),
        (67, null, now(), false, null, now(), 0, '[configs] GET configs/jwt/generate-random-secret', false, 0, '/configs/jwt/generate-random-secret', null, 0, 0, null),
        (68, null, now(), false, null, now(), 0, '[configs] GET configs/jwt', false, 0, '/configs/jwt', null, 0, 0, null),
        (69, null, now(), false, null, now(), 0, '[configs] PUT configs/jwt', false, 3, '/configs/jwt', null, 0, 0, null),
        (70, null, now(), false, null, now(), 0, '[configs] PUT configs/root', false, 3, '/configs/root', null, 0, 0, null),
        (71, null, now(), false, null, now(), 0, '[configs] GET configs/point', false, 0, '/configs/point', null, 0, 0, null),
        (72, null, now(), false, null, now(), 0, '[configs] PUT configs/point', false, 3, '/configs/point', null, 0, 0, null),
        (73, null, now(), false, null, now(), 0, '[sections] POST sections', false, 2, '/sections', null, 0, 0, null),
        (74, null, now(), false, null, now(), 0, '[sections] GET sections', false, 0, '/sections', null, 0, 0, null),
        (75, null, now(), false, null, now(), 0, '[sections] POST sections/{id}/upload-cover', false, 2, '/sections/{id}/upload-cover', null, 0, 0, null),
        (76, null, now(), false, null, now(), 0, '[sections] PUT sections/{id}/states', false, 3, '/sections/{id}/states', null, 0, 0, null),
        (77, null, now(), false, null, now(), 0, '[sections] PUT sections/{id}/admins', false, 3, '/sections/{id}/admins', null, 0, 0, null),
        (78, null, now(), false, null, now(), 0, '[sections] PUT sections/{id}/tags', false, 3, '/sections/{id}/tags', null, 0, 0, null),
        (79, null, now(), false, null, now(), 0, '[sections] PUT sections/{id}/tag-groups', false, 3, '/sections/{id}/tag-groups', null, 0, 0, null),
        (80, null, now(), false, null, now(), 0, '[sections] PUT sections/{id}', false, 3, '/sections/{id:\d+}', null, 0, 0, null),
        (81, null, now(), false, null, now(), 0, '[sections] GET sections/{id}', false, 0, '/sections/{id:\d+}', null, 0, 0, null),
        (82, null, now(), false, null, now(), 0, '[sections] DELETE sections/{id}', false, 5, '/sections/{id:\d+}', null, 0, 0, null),
        (83, null, now(), false, null, now(), 0, '[sections] GET sections/select-all', false, 0, '/sections/select-all', null, 0, 0, null),
        (84, null, now(), false, null, now(), 0, '[sections] GET sections/{id}/details', false, 0, '/sections/{id}/details', null, 0, 0, null),
        (85, null, now(), false, null, now(), 0, '[sections] GET sections/{id}/cover', false, 0, '/sections/{id}/cover', null, 0, 0, null),
        (86, null, now(), false, null, now(), 0, '[tags] POST tags', false, 2, '/tags', null, 0, 0, null),
        (87, null, now(), false, null, now(), 0, '[tags] GET tags', false, 0, '/tags', null, 0, 0, null),
        (88, null, now(), false, null, now(), 0, '[tags] PUT tags/{id}', false, 3, '/tags/{id:\d+}', null, 0, 0, null),
        (89, null, now(), false, null, now(), 0, '[tags] GET tags/{id}', false, 0, '/tags/{id:\d+}', null, 0, 0, null),
        (90, null, now(), false, null, now(), 0, '[tags] DELETE tags/{id}', false, 5, '/tags/{id:\d+}', null, 0, 0, null),
        (91, null, now(), false, null, now(), 0, '[tags] GET tags/select-all', false, 0, '/tags/select-all', null, 0, 0, null),
        (92, null, now(), false, null, now(), 0, '[posts] POST posts', false, 2, '/posts', null, 0, 0, null),
        (93, null, now(), false, null, now(), 0, '[posts] GET posts', false, 0, '/posts', null, 0, 0, null),
        (94, null, now(), false, null, now(), 0, '[posts] POST posts/{id}/view-page', false, 2, '/posts/{id}/view-page', null, 0, 0, null),
        (95, null, now(), false, null, now(), 0, '[posts] POST posts/{id}/upload-cover', false, 2, '/posts/{id}/upload-cover', null, 0, 0, null),
        (96, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/like', false, 3, '/posts/{id}/like', null, 0, 0, null),
        (97, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/favorite', false, 3, '/posts/{id}/favorite', null, 0, 0, null),
        (98, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/section', false, 3, '/posts/{id}/section', null, 0, 0, null),
        (99, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/states', false, 3, '/posts/{id}/states', null, 0, 0, null),
        (100, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/tags', false, 3, '/posts/{id}/tags', null, 0, 0, null),
        (101, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}', false, 3, '/posts/{id:\d+}', null, 0, 0, null),
        (102, null, now(), false, null, now(), 0, '[posts] GET posts/{id}', false, 0, '/posts/{id:\d+}', null, 0, 0, null),
        (103, null, now(), false, null, now(), 0, '[posts] DELETE posts/{id}', false, 5, '/posts/{id:\d+}', null, 0, 0, null),
        (104, null, now(), false, null, now(), 0, '[posts] GET posts/random', false, 0, '/posts/random', null, 0, 0, null),
        (105, null, now(), false, null, now(), 0, '[posts] GET posts/select-all', false, 0, '/posts/select-all', null, 0, 0, null),
        (106, null, now(), false, null, now(), 0, '[posts] GET posts/{id}/comment-reply', false, 0, '/posts/{id}/comment-reply', null, 0, 0, null),
        (107, null, now(), false, null, now(), 0, '[posts] GET posts/{id}/details', false, 0, '/posts/{id}/details', null, 0, 0, null),
        (108, null, now(), false, null, now(), 0, '[posts] GET posts/{id}/cover', false, 0, '/posts/{id}/cover', null, 0, 0, null),
        (109, null, now(), false, null, now(), 0, '[comments] POST comments', false, 2, '/comments', null, 0, 0, null),
        (110, null, now(), false, null, now(), 0, '[comments] PUT comments/{id}/state', false, 3, '/comments/{id}/state', null, 0, 0, null),
        (111, null, now(), false, null, now(), 0, '[comments] GET comments/{id}', false, 0, '/comments/{id:\d+}', null, 0, 0, null),
        (112, null, now(), false, null, now(), 0, '[replies] POST replies', false, 2, '/replies', null, 0, 0, null),
        (113, null, now(), false, null, now(), 0, '[replies] PUT replies/{id}/state', false, 3, '/replies/{id}/state', null, 0, 0, null),
        (114, null, now(), false, null, now(), 0, '[replies] GET replies/{id}', false, 0, '/replies/{id:\d+}', null, 0, 0, null),
        (115, null, now(), false, null, now(), 0, '[section-groups] POST section-groups', false, 2, '/section-groups', null, 0, 0, null),
        (116, null, now(), false, null, now(), 0, '[section-groups] GET section-groups', false, 0, '/section-groups', null, 0, 0, null),
        (117, null, now(), false, null, now(), 0, '[section-groups] PUT section-groups/{id}/sections', false, 3, '/section-groups/{id}/sections', null, 0, 0, null),
        (118, null, now(), false, null, now(), 0, '[section-groups] PUT section-groups/{id}', false, 3, '/section-groups/{id:\d+}', null, 0, 0, null),
        (119, null, now(), false, null, now(), 0, '[section-groups] GET section-groups/{id}', false, 0, '/section-groups/{id:\d+}', null, 0, 0, null),
        (120, null, now(), false, null, now(), 0, '[section-groups] DELETE section-groups/{id}', false, 5, '/section-groups/{id:\d+}', null, 0, 0, null),
        (121, null, now(), false, null, now(), 0, '[section-groups] GET section-groups/select-all', false, 0, '/section-groups/select-all', null, 0, 0, null),
        (122, null, now(), false, null, now(), 0, '[tag-groups] GET tag-groups', false, 2, '/tag-groups', null, 0, 0, null),
        (123, null, now(), false, null, now(), 0, '[tag-groups] GET tag-groups', false, 0, '/tag-groups', null, 0, 0, null),
        (124, null, now(), false, null, now(), 0, '[tag-groups] PUT tag-groups/{id}/tags', false, 3, '/tag-groups/{id}/tags', null, 0, 0, null),
        (125, null, now(), false, null, now(), 0, '[tag-groups] PUT tag-groups/{id}', false, 3, '/tag-groups/{id:\d+}', null, 0, 0, null),
        (126, null, now(), false, null, now(), 0, '[tag-groups] GET tag-groups/{id}', false, 0, '/tag-groups/{id:\d+}', null, 0, 0, null),
        (127, null, now(), false, null, now(), 0, '[tag-groups] DELETE tag-groups/{id}', false, 5, '/tag-groups/{id:\d+}', null, 0, 0, null),
        (128, null, now(), false, null, now(), 0, '[posts] POST posts/review-queues/receive', false, 2, '/posts/review-queues/receive', null, 0, 0, null),
        (129, null, now(), false, null, now(), 0, '[posts] POST posts/review-queues/{id}/return', false, 2, '/posts/review-queues/{id}/return', null, 0, 0, null),
        (130, null, now(), false, null, now(), 0, '[posts] POST posts/review-queues/{id}/approved', false, 2, '/posts/review-queues/{id}/approved', null, 0, 0, null),
        (131, null, now(), false, null, now(), 0, '[posts] POST posts/review-queues/{id}/not-approved', false, 2, '/posts/review-queues/{id}/not-approved', null, 0, 0, null),
        (132, null, now(), false, null, now(), 0, '[posts] GET posts/review-queues/{id}', false, 0, '/posts/review-queues/{id:\d+}', null, 0, 0, null),
        (133, null, now(), false, null, now(), 0, '[posts] GET posts/review-queues', false, 0, '/posts/review-queues', null, 0, 0, null),
        (134, null, now(), false, null, now(), 0, '[comments] PUT comments/{id}/like', false, 3, '/comments/{id}/like', null, 0, 0, null),
        (135, null, now(), false, null, now(), 0, '[replies] PUT replies/{id}/like', false, 3, '/replies/{id}/like', null, 0, 0, null),
        (136, null, now(), false, null, now(), 0, '[configs] GET configs/post/create-guide', false, 0, '/configs/post/create-guide', null, 0, 0, null),
        (137, null, now(), false, null, now(), 0, '[configs] PUT configs/post/create-guide', false, 3, '/configs/post/create-guide', null, 0, 0, null),
        (138, null, now(), false, null, now(), 0, '[sections] PUT sections/{id}/create-post-guide', false, 3, '/sections/{id}/create-post-guide', null, 0, 0, null),
        (139, null, now(), false, null, now(), 0, '[users] GET users/temporary-storage', false, 0, '/users/temporary-storage', null, 0, 0, null),
        (140, null, now(), false, null, now(), 0, '[users] PUT users/temporary-storage', false, 3, '/users/temporary-storage', null, 0, 0, null),
        (141, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/disable-comment-reply', false, 3, '/posts/{id}/disable-comment-reply', null, 0, 0, null),
        (142, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/users/{userId}/disable-comment-reply', false, 3, '/posts/{id}/users/{userId}/disable-comment-reply', null, 0, 0, null),
        (143, null, now(), false, null, now(), 0, '[posts] GET posts/{id}/user-relationship', false, 0, '/posts/{id}/user-relationship', null, 0, 0, null),
        (144, null, now(), false, null, now(), 0, '[users] PUT users/{id}/disable-comment-reply', false, 3, '/users/{id}/disable-comment-reply', null, 0, 0, null),
        (145, null, now(), false, null, now(), 0, '[posts] GET posts/{id}/users/{userId}/user-relationship', false, 0, '/posts/{id}/users/{userId}/user-relationship', null, 0, 0, null),
        (146, null, now(), false, null, now(), 0, '[files] POST files/images', false, 2, '/files/images', null, 0, 0, null),
        (147, null, now(), false, null, now(), 0, '[files] GET files/images/users/{userId}', false, 0, '/files/images/users/{userId}', null, 0, 0, null),
        (148, null, now(), false, null, now(), 0, '[files] GET files/images/{id}/{name}', false, 0, '/files/images/{id}/{name}', null, 0, 0, null),
        (149, null, now(), false, null, now(), 0, '[files] GET files', false, 0, '/files', null, 0, 0, null),
        (150, null, now(), false, null, now(), 0, '[files] GET files/{id}', false, 0, '/files/{id:\d+}', null, 0, 0, null),
        (151, null, now(), false, null, now(), 0, '[files] DELETE files/{id}', false, 5, '/files/{id:\d+}', null, 0, 0, null),
        (152, null, now(), false, null, now(), 0, '[files] DELETE files/images/{id}', false, 5, '/files/images/{id:\d+}', null, 0, 0, null),
        (153, null, now(), false, null, now(), 0, '[points] GET points/histories/users/{userId}', false, 0, '/points/histories/users/{userId}', null, 0, 0, null),
        (154, null, now(), false, null, now(), 0, '[configs] GET configs/root/disable-registration', false, 0, '/configs/root/disable-registration', null, 0, 0, null),
        (155, null, now(), false, null, now(), 0, '[configs] PUT configs/root/secret', false, 3, '/configs/root/secret', null, 0, 0, null),
        (156, null, now(), false, null, now(), 0, '[configs] GET configs/root', false, 0, '/configs/root', null, 0, 0, null),
        (157, null, now(), false, null, now(), 0, '[tag-groups] GET tag-groups/select-all', false, 0, '/tag-groups/select-all', null, 0, 0, null),
        (158, null, now(), false, null, now(), 0, '[posts] PUT posts/{id}/styles', false, 3, '/posts/{id}/styles', null, 0, 0, null),
        (159, null, now(), false, null, now(), 0, '[imports] POST imports/users', false, 2, '/imports/users', null, 0, 0, null),
        (160, null, now(), false, null, now(), 0, '[imports] POST imports/sections', false, 2, '/imports/sections', null, 0, 0, null),
        (161, null, now(), false, null, now(), 0, '[imports] POST imports/section-groups', false, 2, '/imports/section-groups', null, 0, 0, null),
        (162, null, now(), false, null, now(), 0, '[imports] POST imports/tags', false, 2, '/imports/tags', null, 0, 0, null),
        (163, null, now(), false, null, now(), 0, '[imports] POST imports/tag-groups', false, 2, '/imports/tag-groups', null, 0, 0, null),
        (164, null, now(), false, null, now(), 0, '[imports] POST imports/posts', false, 2, '/imports/posts', null, 0, 0, null);

UPDATE permission_entity_seq t SET t.next_val = 164 + 1 WHERE next_val = @permission_next_val;

insert into role_entity_permissions (roles_id, permissions_id)
values  (1, 4),
        (1, 5),
        (1, 6),
        (1, 8),
        (1, 10),
        (1, 11),
        (1, 17),
        (1, 18),
        (1, 19),
        (1, 20),
        (1, 21),
        (1, 22),
        (1, 23),
        (1, 24),
        (1, 25),
        (1, 26),
        (1, 27),
        (1, 28),
        (1, 29),
        (1, 30),
        (1, 31),
        (1, 32),
        (1, 33),
        (1, 34),
        (1, 35),
        (1, 36),
        (1, 37),
        (1, 38),
        (1, 39),
        (1, 40),
        (1, 41),
        (1, 42),
        (1, 43),
        (1, 44),
        (1, 45),
        (1, 46),
        (1, 47),
        (1, 48),
        (1, 49),
        (1, 50),
        (1, 51),
        (1, 52),
        (1, 53),
        (1, 54),
        (1, 55),
        (1, 56),
        (1, 57),
        (1, 58),
        (1, 59),
        (1, 60),
        (1, 64),
        (1, 65),
        (1, 67),
        (1, 68),
        (1, 69),
        (1, 70),
        (1, 71),
        (1, 72),
        (1, 73),
        (1, 74),
        (1, 75),
        (1, 76),
        (1, 77),
        (1, 78),
        (1, 79),
        (1, 80),
        (1, 81),
        (1, 82),
        (1, 86),
        (1, 87),
        (1, 88),
        (1, 89),
        (1, 90),
        (1, 93),
        (1, 98),
        (1, 99),
        (1, 100),
        (1, 102),
        (1, 103),
        (1, 110),
        (1, 111),
        (1, 113),
        (1, 114),
        (1, 115),
        (1, 116),
        (1, 117),
        (1, 118),
        (1, 119),
        (1, 120),
        (1, 122),
        (1, 123),
        (1, 124),
        (1, 125),
        (1, 126),
        (1, 127),
        (1, 128),
        (1, 129),
        (1, 130),
        (1, 131),
        (1, 132),
        (1, 133),
        (1, 137),
        (1, 138),
        (1, 141),
        (1, 142),
        (1, 143),
        (1, 144),
        (1, 145),
        (1, 149),
        (1, 150),
        (1, 151),
        (1, 158),
        (1, 159),
        (1, 160),
        (1, 161),
        (1, 162),
        (1, 163),
        (1, 164);

SET @menu_next_val = (SELECT next_val FROM menu_entity_seq);

insert into menu_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, link, name, sort)
values  (1, null, now(), false, null, now(), 0, '/admin', 'Dashboard', 18),
        (2, null, now(), false, null, now(), 0, '/admin/sections', 'Section Mgmt', 17),
        (3, null, now(), false, null, now(), 0, '/admin/section-groups', 'Section Group Mgmt', 16),
        (4, null, now(), false, null, now(), 0, '/admin/tags', 'Tag Mgmt', 15),
        (5, null, now(), false, null, now(), 0, '/admin/tag-groups', 'Tag Group Mgmt', 14),
        (6, null, now(), false, null, now(), 0, '/admin/posts', 'Post Mgmt', 13),
        (7, null, now(), false, null, now(), 0, '/admin/posts/review-queues', 'Post Review Mgmt', 12),
        (8, null, now(), false, null, now(), 0, '/admin/comments', 'Comment Mgmt', 11),
        (9, null, now(), false, null, now(), 0, '/admin/messages', 'Message Mgmt', 10),
        (10, null, now(), false, null, now(), 0, '/admin/points/rules', 'Point Rule Mgmt', 9),
        (11, null, now(), false, null, now(), 0, '/admin/points/permission-rules', 'Point Perm Mgmt', 8),
        (12, null, now(), false, null, now(), 0, '/admin/users', 'User Mgmt', 7),
        (13, null, now(), false, null, now(), 0, '/admin/roles', 'Role Mgmt', 6),
        (14, null, now(), false, null, now(), 0, '/admin/permissions', 'Permission Mgmt', 5),
        (15, null, now(), false, null, now(), 0, '/admin/menus', 'Menu Mgmt', 4),
        (16, null, now(), false, null, now(), 0, '/admin/submenus', 'Submenu Mgmt', 3),
        (17, null, now(), false, null, now(), 0, '/admin/actions', 'Action Mgmt', 2),
        (18, null, now(), false, null, now(), 0, '/admin/configs', 'Config Mgmt', 1),
        (19, null, now(), false, null, now(), 0, '/admin/files', 'File Mgmt', 8);

UPDATE menu_entity_seq t SET t.next_val = 19 + 1 WHERE next_val = @menu_next_val;

insert into menu_entity_roles (menus_id, roles_id)
values  (1, 1),
        (2, 1),
        (3, 1),
        (4, 1),
        (5, 1),
        (6, 1),
        (7, 1),
        (8, 1),
        (9, 1),
        (10, 1),
        (11, 1),
        (12, 1),
        (13, 1),
        (14, 1),
        (15, 1),
        (16, 1),
        (17, 1),
        (18, 1),
        (19, 1);

SET @submenu_next_val = (SELECT next_val FROM submenu_entity_seq);

insert into submenu_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, link, name, sort, menu_id)
values  (1, null, now(), false, null, now(), 0, '/admin/sections', 'Dashboard', 2, 2),
        (2, null, now(), false, null, now(), 0, '/admin/sections?type=add', 'Create Section', 1, 2),
        (3, null, now(), false, null, now(), 0, '/admin/section-groups', 'Dashboard', 2, 3),
        (4, null, now(), false, null, now(), 0, '/admin/section-groups?type=add', 'Create Section Group', 1, 3),
        (5, null, now(), false, null, now(), 0, '/admin/tags', 'Dashboard', 2, 4),
        (6, null, now(), false, null, now(), 0, '/admin/tags?type=add', 'Create Tag', 1, 4),
        (7, null, now(), false, null, now(), 0, '/admin/tag-groups', 'Dashboard', 2, 5),
        (8, null, now(), false, null, now(), 0, '/admin/tag-groups?type=add', 'Create Tag Group', 1, 5),
        (9, null, now(), false, null, now(), 0, '/admin/messages', 'Dashboard', 2, 9),
        (10, null, now(), false, null, now(), 0, '/admin/messages?type=add', 'Create Message', 1, 9),
        (11, null, now(), false, null, now(), 0, '/admin/users', 'Dashboard', 2, 12),
        (12, null, now(), false, null, now(), 0, '/register', 'Create User', 1, 12),
        (13, null, now(), false, null, now(), 0, '/admin/roles', 'Dashboard', 2, 13),
        (14, null, now(), false, null, now(), 0, '/admin/roles?type=add', 'Create Role', 1, 13),
        (15, null, now(), false, null, now(), 0, '/admin/permissions', 'Dashboard', 2, 14),
        (16, null, now(), false, null, now(), 0, '/admin/permissions?type=add', 'Create Permission', 1, 14),
        (17, null, now(), false, null, now(), 0, '/admin/menus', 'Dashboard', 2, 15),
        (18, null, now(), false, null, now(), 0, '/admin/menus?type=add', 'Create Menu', 1, 15),
        (19, null, now(), false, null, now(), 0, '/admin/submenus', 'Dashboard', 2, 16),
        (20, null, now(), false, null, now(), 0, '/admin/submenus?type=add', 'Create Submenu', 1, 16),
        (21, null, now(), false, null, now(), 0, '/admin/actions', 'Dashboard', 2, 17),
        (22, null, now(), false, null, now(), 0, '/admin/actions?type=add', 'Create Action', 1, 17);

UPDATE submenu_entity_seq t SET t.next_val = 22 + 1 WHERE next_val = @submenu_next_val;

insert into submenu_entity_roles (submenus_id, roles_id)
values  (1, 1),
        (2, 1),
        (3, 1),
        (4, 1),
        (5, 1),
        (6, 1),
        (7, 1),
        (8, 1),
        (9, 1),
        (10, 1),
        (11, 1),
        (12, 1),
        (13, 1),
        (14, 1),
        (15, 1),
        (16, 1),
        (17, 1),
        (18, 1),
        (19, 1),
        (20, 1),
        (21, 1),
        (22, 1);

SET @action_next_val = (SELECT next_val FROM action_entity_seq);

insert into action_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, name, sort, menu_id, submenu_id)
values  (1, null, now(), false, null, now(), 0, 'Section Create', 'Sections#Create', 0, 2, null),
        (2, null, now(), false, null, now(), 0, 'Section Update', 'Sections#Update', 0, 2, null),
        (3, null, now(), false, null, now(), 0, 'Section Update States', 'Sections#Update States', 0, 2, null),
        (4, null, now(), false, null, now(), 0, 'Section Update Admins', 'Sections#Update Admins', 0, 2, null),
        (5, null, now(), false, null, now(), 0, 'Section Update Tags', 'Sections#Update Tags', 0, 2, null),
        (6, null, now(), false, null, now(), 0, 'Section Update Tag Groups', 'Sections#Update Tag Groups', 0, 2, null),
        (7, null, now(), false, null, now(), 0, 'Section Groups Create', 'Section Groups#Create', 0, 3, null),
        (8, null, now(), false, null, now(), 0, 'Section Groups Update', 'Section Groups#Update', 0, 3, null),
        (9, null, now(), false, null, now(), 0, 'Section Groups Update Sections', 'Section Groups#Update Sections', 0, 3, null),
        (10, null, now(), false, null, now(), 0, 'Section Groups Delete', 'Section Groups#Delete', 0, 3, null),
        (11, null, now(), false, null, now(), 0, 'Tags Create', 'Tags#Create', 0, 4, null),
        (12, null, now(), false, null, now(), 0, 'Tags Update', 'Tags#Update', 0, 4, null),
        (13, null, now(), false, null, now(), 0, 'Tags Delete', 'Tags#Delete', 0, 4, null),
        (14, null, now(), false, null, now(), 0, 'Tag Groups Create', 'Tag Groups#Create', 0, 5, null),
        (15, null, now(), false, null, now(), 0, 'Tag Groups Update', 'Tag Groups#Update', 0, 5, null),
        (16, null, now(), false, null, now(), 0, 'Tag Groups Update Tags', 'Tag Groups#Update Tags', 0, 5, null),
        (17, null, now(), false, null, now(), 0, 'Tag Groups Delete', 'Tag Groups#Delete', 0, 5, null),
        (18, null, now(), false, null, now(), 0, 'Post Update States', 'Posts#Update States', 0, 6, null),
        (19, null, now(), false, null, now(), 0, 'Post Update Tags', 'Posts#Update Tags', 0, 6, null),
        (20, null, now(), false, null, now(), 0, 'Post Update Section', 'Posts#Update Section', 0, 6, null),
        (21, null, now(), false, null, now(), 0, 'Post Review Receive', 'Post Review Queues#Receive', 0, 7, null),
        (22, null, now(), false, null, now(), 0, 'Post Review Return', 'Post Review Queues#Return', 0, 7, null),
        (23, null, now(), false, null, now(), 0, 'Post Review Approved', 'Post Review Queues#Approved', 0, 7, null),
        (24, null, now(), false, null, now(), 0, 'Post Review NotApproved', 'Post Review Queues#NotApproved', 0, 7, null),
        (25, null, now(), false, null, now(), 0, 'Message Create', 'Messages#Create', 0, 9, null),
        (26, null, now(), false, null, now(), 0, 'Point Rule Update', 'Point Rules#Update', 0, 10, null),
        (27, null, now(), false, null, now(), 0, 'Point Permission Update', 'Point Permissions#Update', 0, 11, null),
        (28, null, now(), false, null, now(), 0, 'Users Update States', 'Users#Update States', 1, 12, null),
        (29, null, now(), false, null, now(), 0, 'Users Update Roles', 'Users#Update Roles', 1, 12, null),
        (30, null, now(), false, null, now(), 0, 'Users Delete', 'Users#Delete', 1, 12, null),
        (31, null, now(), false, null, now(), 0, 'Roles Create', 'Roles#Create', 1, 13, null),
        (32, null, now(), false, null, now(), 0, 'Roles Update', 'Roles#Update', 1, 13, null),
        (33, null, now(), false, null, now(), 0, 'Roles Update Permissions', 'Roles#Update Permissions', 1, 13, null),
        (34, null, now(), false, null, now(), 0, 'Roles Delete', 'Roles#Delete', 1, 13, null),
        (35, null, now(), false, null, now(), 0, 'Permissions Create', 'Permissions#Create', 1, 14, null),
        (36, null, now(), false, null, now(), 0, 'Permissions Update', 'Permissions#Update', 1, 14, null),
        (37, null, now(), false, null, now(), 0, 'Permissions Update Roles', 'Permissions#Update Roles', 1, 14, null),
        (38, null, now(), false, null, now(), 0, 'Permissions Delete', 'Permissions#Delete', 1, 14, null),
        (39, null, now(), false, null, now(), 0, 'Menus Create', 'Menus#Create', 1, 15, null),
        (40, null, now(), false, null, now(), 0, 'Menus Update', 'Menus#Update', 1, 15, null),
        (41, null, now(), false, null, now(), 0, 'Menus Update Roles', 'Menus#Update Roles', 1, 15, null),
        (42, null, now(), false, null, now(), 0, 'Menus Delete', 'Menus#Delete', 1, 15, null),
        (43, null, now(), false, null, now(), 0, 'Submenus Create', 'Submenus#Create', 1, 16, null),
        (44, null, now(), false, null, now(), 0, 'Submenus Update', 'Submenus#Update', 1, 16, null),
        (45, null, now(), false, null, now(), 0, 'Submenus Update Roles', 'Submenus#Update Roles', 1, 16, null),
        (46, null, now(), false, null, now(), 0, 'Submenus Delete', 'Submenus#Delete', 1, 16, null),
        (47, null, now(), false, null, now(), 0, 'Actions Create', 'Actions#Create', 1, 17, null),
        (48, null, now(), false, null, now(), 0, 'Actions Update', 'Actions#Update', 1, 17, null),
        (49, null, now(), false, null, now(), 0, 'Actions Update Roles', 'Actions#Update Roles', 1, 17, null),
        (50, null, now(), false, null, now(), 0, 'Actions Delete', 'Actions#Delete', 1, 17, null),
        (51, null, now(), false, null, now(), 0, 'JwtConfigs Update', 'JwtConfigs#Update', 1, 18, null),
        (52, null, now(), false, null, now(), 0, 'PointConfigs Update', 'PointConfigs#Update', 1, 18, null),
        (53, null, now(), false, null, now(), 0, 'Post Delete', 'Posts#Delete', 1, 6, null),
        (54, null, now(), false, null, now(), 0, 'Section Update', 'Sections#Delete', 1, 2, null),
        (55, null, now(), false, null, now(), 0, 'Section Groups Delete', 'Section Groups#Delete', 1, 3, null),
        (56, null, now(), false, null, now(), 0, 'Comments Update State', 'Comments#Update State', 1, 8, null),
        (57, null, now(), false, null, now(), 0, 'PostConfigs Update Create Guide', 'PostConfigs#Update Create Guide', 1, 18, null),
        (58, null, now(), false, null, now(), 0, 'Section Update Create Post Guide', 'Sections#Update Create Post Guide', 1, 2, null),
        (59, null, now(), false, null, now(), 0, 'Posts Disable Comment Reply', 'Posts#Disable Comment Reply', 1, 6, null),
        (60, null, now(), false, null, now(), 0, 'Users Disable Comment Reply', 'Users#Disable Comment Reply', 1, 12, null),
        (61, null, now(), false, null, now(), 0, 'Posts Update User Relationship', 'Posts#Update User Relationship', 1, 6, null),
        (62, null, now(), false, null, now(), 0, 'Files Delete', 'Files#Delete', 1, 19, null),
        (63, null, now(), false, null, now(), 0, 'RootConfigs Update', 'RootConfigs#Update', 1, 18, null),
        (64, null, now(), false, null, now(), 0, 'Post Update Styles', 'Posts#Update Styles', 1, 6, null);

UPDATE action_entity_seq t SET t.next_val = 64 + 1 WHERE next_val = @action_next_val;

insert into action_entity_roles (actions_id, roles_id)
values  (1, 1),
        (2, 1),
        (3, 1),
        (4, 1),
        (5, 1),
        (6, 1),
        (7, 1),
        (8, 1),
        (9, 1),
        (10, 1),
        (11, 1),
        (12, 1),
        (13, 1),
        (14, 1),
        (15, 1),
        (16, 1),
        (17, 1),
        (18, 1),
        (19, 1),
        (20, 1),
        (21, 1),
        (22, 1),
        (23, 1),
        (24, 1),
        (25, 1),
        (26, 1),
        (27, 1),
        (28, 1),
        (29, 1),
        (30, 1),
        (31, 1),
        (32, 1),
        (33, 1),
        (34, 1),
        (35, 1),
        (36, 1),
        (37, 1),
        (38, 1),
        (39, 1),
        (40, 1),
        (41, 1),
        (42, 1),
        (43, 1),
        (44, 1),
        (45, 1),
        (46, 1),
        (47, 1),
        (48, 1),
        (49, 1),
        (50, 1),
        (51, 1),
        (52, 1),
        (53, 1),
        (54, 1),
        (55, 1),
        (56, 1),
        (57, 1),
        (58, 1),
        (59, 1),
        (60, 1),
        (61, 1),
        (62, 1),
        (63, 1),
        (64, 1);