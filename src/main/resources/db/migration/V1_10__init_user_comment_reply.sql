create table if not exists comment_user_entity
(
    created_by bigint      null,
    created_on datetime(6) not null,
    deleted    bit         not null,
    updated_by bigint      null,
    updated_on datetime(6) null,
    version    smallint    null,
    liked      bit         null,
    comment_id bigint      not null,
    user_id    bigint      not null,
    primary key (comment_id, user_id),
    constraint FK43ht7oqk3cwvf7ea583lsquq9
        foreign key (comment_id) references comment_entity (id),
    constraint FKqvbgr86lyoa72stbe2suf6v47
        foreign key (user_id) references user_entity (id)
);

create table if not exists quote_reply_user_entity
(
    created_by     bigint      null,
    created_on     datetime(6) not null,
    deleted        bit         not null,
    updated_by     bigint      null,
    updated_on     datetime(6) null,
    version        smallint    null,
    liked          bit         null,
    quote_reply_id bigint      not null,
    user_id        bigint      not null,
    primary key (quote_reply_id, user_id),
    constraint FKjnneix57pwdvnjuksxsadyxyb
        foreign key (quote_reply_id) references quote_reply_entity (id),
    constraint FKt4q3pdhwrk7nwct9gpq2koxq2
        foreign key (user_id) references user_entity (id)
);

SET @permission_max_id = (SELECT max(id) FROM permission_entity);
SET @permission_next_val = (SELECT next_val FROM permission_entity_seq);

INSERT INTO permission_entity (id, created_by, created_on, deleted, updated_by, updated_on, version, alias, case_insensitive, method, name, overview, sort, type, matcher_id)
VALUES (@permission_max_id + 1, null, now(), false, null, now(), 0, '[comments] PUT comments/{id}/like', false, 3, '/comments/{id}/like', null, 0, 0, null),
       (@permission_max_id + 2, null, now(), false, null, now(), 0, '[replies] PUT replies/{id}/like', false, 3, '/replies/{id}/like', null, 0, 0, null);

UPDATE permission_entity_seq t SET t.next_val = @permission_max_id + 50 + 2 WHERE next_val = @permission_next_val;
