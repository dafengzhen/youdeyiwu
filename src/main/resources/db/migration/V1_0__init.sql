create table if not exists action_entity_seq
(
    next_val bigint null
);

INSERT INTO action_entity_seq (next_val)
VALUES (1);

create table if not exists comment_entity_seq
(
    next_val bigint null
);

INSERT INTO comment_entity_seq (next_val)
VALUES (1);

create table if not exists config_entity
(
    id          bigint                                not null
        primary key,
    created_by  bigint                                null,
    created_on  datetime(6)                           not null,
    deleted     bit                                   not null,
    updated_by  bigint                                null,
    updated_on  datetime(6)                           null,
    version     smallint                              null,
    description varchar(255)                          null,
    name        varchar(255)                          not null,
    type        enum ('ROOT', 'JWT', 'POINT', 'POST') not null,
    value       text                                  not null,
    constraint UKcuv54x6fdxtcid1r8ory6l8ec
        unique (type, name)
);

create table if not exists config_entity_seq
(
    next_val bigint null
);

INSERT INTO config_entity_seq (next_val)
VALUES (1);

create table if not exists file_entity_seq
(
    next_val bigint null
);

INSERT INTO file_entity_seq (next_val)
VALUES (1);

create table if not exists global_message_entity_seq
(
    next_val bigint null
);

INSERT INTO global_message_entity_seq (next_val)
VALUES (1);

create table if not exists menu_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    link       varchar(255) not null,
    name       varchar(255) not null,
    sort       int          not null
);

create table if not exists menu_entity_seq
(
    next_val bigint null
);

INSERT INTO menu_entity_seq (next_val)
VALUES (1);

create table if not exists message_entity_seq
(
    next_val bigint null
);

INSERT INTO message_entity_seq (next_val)
VALUES (1);

create table if not exists permission_entity
(
    id               bigint       not null
        primary key,
    created_by       bigint       null,
    created_on       datetime(6)  not null,
    deleted          bit          not null,
    updated_by       bigint       null,
    updated_on       datetime(6)  null,
    version          smallint     null,
    alias            varchar(255) null,
    case_insensitive bit          not null,
    method           smallint     not null,
    name             varchar(255) not null,
    overview         varchar(255) null,
    sort             int          not null,
    type             smallint     not null,
    matcher_id       bigint       null,
    constraint FK4ks9hkfvc8nbjbt29af0at9rm
        foreign key (matcher_id) references permission_entity (id),
    check (`method` between 0 and 7),
    check (`type` between 0 and 1)
);

create table if not exists permission_entity_seq
(
    next_val bigint null
);

INSERT INTO permission_entity_seq (next_val)
VALUES (1);

create table if not exists point_entity
(
    id         bigint      not null
        primary key,
    created_by bigint      null,
    created_on datetime(6) not null,
    deleted    bit         not null,
    updated_by bigint      null,
    updated_on datetime(6) null,
    version    smallint    null,
    max_points int         null,
    min_points int         null,
    points     int         null,
    user_id    bigint      null,
    constraint UK_fgdr5sn8ytuc70o823y5ciux2
        unique (user_id)
);

create table if not exists point_entity_seq
(
    next_val bigint null
);

INSERT INTO point_entity_seq (next_val)
VALUES (1);

create table if not exists point_history_entity_seq
(
    next_val bigint null
);

INSERT INTO point_history_entity_seq (next_val)
VALUES (1);

create table if not exists point_permission_rule_entity
(
    id                   bigint                                                                                                                                                    not null
        primary key,
    created_by           bigint                                                                                                                                                    null,
    created_on           datetime(6)                                                                                                                                               not null,
    deleted              bit                                                                                                                                                       not null,
    updated_by           bigint                                                                                                                                                    null,
    updated_on           datetime(6)                                                                                                                                               null,
    version              smallint                                                                                                                                                  null,
    enable               bit                                                                                                                                                       null,
    operation_cost       int                                                                                                                                                       null,
    permission_rule_name enum ('CREATE_POST', 'CREATE_COMMENT', 'CREATE_REPLY', 'UPDATE_POST', 'ADD_POST_TAG', 'ADD_POST_CONTENT_LINK', 'ADD_POST_COVER_LINK', 'ADD_POST_SECTION') null,
    required_points      int                                                                                                                                                       null,
    constraint UK_hs5lxfgbbioweo9yadjvc71cq
        unique (permission_rule_name)
);

create table if not exists point_permission_rule_entity_seq
(
    next_val bigint null
);

INSERT INTO point_permission_rule_entity_seq (next_val)
VALUES (1);

create table if not exists point_rule_entity
(
    id                      bigint                                                                                                                                                                                                                                                       not null
        primary key,
    created_by              bigint                                                                                                                                                                                                                                                       null,
    created_on              datetime(6)                                                                                                                                                                                                                                                  not null,
    deleted                 bit                                                                                                                                                                                                                                                          not null,
    updated_by              bigint                                                                                                                                                                                                                                                       null,
    updated_on              datetime(6)                                                                                                                                                                                                                                                  null,
    version                 smallint                                                                                                                                                                                                                                                     null,
    enable                  bit                                                                                                                                                                                                                                                          null,
    initiator_reward_points int                                                                                                                                                                                                                                                          null,
    receiver_reward_points  int                                                                                                                                                                                                                                                          null,
    rule_name               enum ('LIKE_POST', 'LIKE_COMMENT', 'LIKE_REPLY', 'COMMENT_POST', 'REPLY_POST', 'FOLLOW_POST', 'FAVORITE_POST', 'DISLIKE_POST', 'DISLIKE_COMMENT', 'DISLIKE_REPLY', 'POST_APPROVED', 'POST_NOT_APPROVED', 'POST_PENDING_REVIEW', 'VISIT_POST', 'CREATE_POST') null,
    constraint UK_5do4d4csxlsnk9coer6vkvdyl
        unique (rule_name)
);

create table if not exists point_rule_entity_seq
(
    next_val bigint null
);

INSERT INTO point_rule_entity_seq (next_val)
VALUES (1);

create table if not exists post_badge_entity_seq
(
    next_val bigint null
);

INSERT INTO post_badge_entity_seq (next_val)
VALUES (1);

create table if not exists post_entity_seq
(
    next_val bigint null
);

INSERT INTO post_entity_seq (next_val)
VALUES (1);

create table if not exists post_favorite_entity_seq
(
    next_val bigint null
);

INSERT INTO post_favorite_entity_seq (next_val)
VALUES (1);

create table if not exists post_history_entity_seq
(
    next_val bigint null
);

INSERT INTO post_history_entity_seq (next_val)
VALUES (1);

create table if not exists post_image_entity_seq
(
    next_val bigint null
);

INSERT INTO post_image_entity_seq (next_val)
VALUES (1);

create table if not exists post_review_history_entity_seq
(
    next_val bigint null
);

INSERT INTO post_review_history_entity_seq (next_val)
VALUES (1);

create table if not exists post_review_queue_entity_seq
(
    next_val bigint null
);

INSERT INTO post_review_queue_entity_seq (next_val)
VALUES (1);

create table if not exists quote_reply_entity_seq
(
    next_val bigint null
);

INSERT INTO quote_reply_entity_seq (next_val)
VALUES (1);

create table if not exists role_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    display    bit          not null,
    name       varchar(255) not null,
    overview   varchar(255) null,
    sort       int          not null
);

create table if not exists menu_entity_roles
(
    menus_id bigint not null,
    roles_id bigint not null,
    primary key (menus_id, roles_id),
    constraint FKa8wwk4pl13rim5p700phpro05
        foreign key (menus_id) references menu_entity (id),
    constraint FKd2ct4b13soh6m50uv5ebd6w7w
        foreign key (roles_id) references role_entity (id)
);

create table if not exists role_entity_permissions
(
    roles_id       bigint not null,
    permissions_id bigint not null,
    primary key (roles_id, permissions_id),
    constraint FK28w79lmksgkp5t210aixqysxg
        foreign key (permissions_id) references permission_entity (id),
    constraint FK38nst7aknrjjbs0w99ub91f35
        foreign key (roles_id) references role_entity (id)
);

create table if not exists role_entity_seq
(
    next_val bigint null
);

INSERT INTO role_entity_seq (next_val)
VALUES (1);

create table if not exists section_entity
(
    id                bigint       not null
        primary key,
    created_by        bigint       null,
    created_on        datetime(6)  not null,
    deleted           bit          not null,
    updated_by        bigint       null,
    updated_on        datetime(6)  null,
    version           smallint     null,
    access_key        varchar(255) null,
    access_points     int          null,
    content           text         null,
    cover             varchar(255) null,
    cover_image       mediumblob   null,
    cover_image_type  smallint     not null,
    create_post_guide text         null,
    name              varchar(255) not null,
    overview          varchar(255) null,
    sort              int          not null,
    check (`cover_image_type` between 0 and 1)
);

create table if not exists section_entity_seq
(
    next_val bigint null
);

INSERT INTO section_entity_seq (next_val)
VALUES (1);

create table if not exists section_entity_states
(
    section_entity_id bigint  not null,
    states            tinyint null,
    constraint FKmo2jd2hl8ejotqsvf1oieutxs
        foreign key (section_entity_id) references section_entity (id),
    check (`states` between 0 and 5)
);

create table if not exists section_group_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    name       varchar(255) not null,
    sort       int          not null
);

create table if not exists section_group_entity_sections
(
    section_groups_id bigint not null,
    sections_id       bigint not null,
    primary key (section_groups_id, sections_id),
    constraint FK3qnw72q5mifvcdajtgn98k6im
        foreign key (sections_id) references section_entity (id),
    constraint FKm37wsao4dgan1cg18sbarwd7k
        foreign key (section_groups_id) references section_group_entity (id)
);

create table if not exists section_group_entity_seq
(
    next_val bigint null
);

INSERT INTO section_group_entity_seq (next_val)
VALUES (1);

create table if not exists submenu_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    link       varchar(255) not null,
    name       varchar(255) not null,
    sort       int          not null,
    menu_id    bigint       null,
    constraint FKa31rpj1kp804a82i5ramn9xst
        foreign key (menu_id) references menu_entity (id)
);

create table if not exists action_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    alias      varchar(255) null,
    name       varchar(255) not null,
    sort       int          not null,
    menu_id    bigint       null,
    submenu_id bigint       null,
    constraint FK1ebgngak0984tgntr54pd26qg
        foreign key (submenu_id) references submenu_entity (id),
    constraint FK6x66faap9dixvrvu9nwy4uink
        foreign key (menu_id) references menu_entity (id)
);

create table if not exists action_entity_roles
(
    actions_id bigint not null,
    roles_id   bigint not null,
    primary key (actions_id, roles_id),
    constraint FKf4d9rq3mi7yj6pycli5rro2k0
        foreign key (roles_id) references role_entity (id),
    constraint FKjfb8cfwifhccn3k6ge4reyrsu
        foreign key (actions_id) references action_entity (id)
);

create table if not exists submenu_entity_roles
(
    submenus_id bigint not null,
    roles_id    bigint not null,
    primary key (submenus_id, roles_id),
    constraint FK67wlj1kg7xaq3q0q7i6kbohx7
        foreign key (roles_id) references role_entity (id),
    constraint FKrypekcm2f3q070ibc53hsslaw
        foreign key (submenus_id) references submenu_entity (id)
);

create table if not exists submenu_entity_seq
(
    next_val bigint null
);

INSERT INTO submenu_entity_seq (next_val)
VALUES (1);

create table if not exists tag_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    name       varchar(255) not null,
    sort       int          not null,
    constraint UK_3hgn1x4ryaml5ndh5lt7t3n2
        unique (name)
);

create table if not exists section_entity_tags
(
    sections_id bigint not null,
    tags_id     bigint not null,
    primary key (sections_id, tags_id),
    constraint FK8b7v06i0qej10q7i973i98p1h
        foreign key (sections_id) references section_entity (id),
    constraint FKdl0d5yxe2nmhwobt7h46m1maw
        foreign key (tags_id) references tag_entity (id)
);

create table if not exists tag_entity_seq
(
    next_val bigint null
);

INSERT INTO tag_entity_seq (next_val)
VALUES (1);

create table if not exists tag_group_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    name       varchar(255) not null,
    sort       int          not null
);

create table if not exists section_entity_tag_groups
(
    sections_id   bigint not null,
    tag_groups_id bigint not null,
    primary key (sections_id, tag_groups_id),
    constraint FKn021duvptr887e8x90of67v2i
        foreign key (sections_id) references section_entity (id),
    constraint FKpsggr3y9wpsgbsxardn8jm7al
        foreign key (tag_groups_id) references tag_group_entity (id)
);

create table if not exists tag_group_entity_seq
(
    next_val bigint null
);

INSERT INTO tag_group_entity_seq (next_val)
VALUES (1);

create table if not exists tag_group_entity_tags
(
    tag_groups_id bigint not null,
    tags_id       bigint not null,
    primary key (tag_groups_id, tags_id),
    constraint FK44la2rwxuf0xefppdgoxepx06
        foreign key (tag_groups_id) references tag_group_entity (id),
    constraint FKp2u96w3mhd0mcc1135om5del8
        foreign key (tags_id) references tag_entity (id)
);

create table if not exists user_entity
(
    id                      bigint       not null
        primary key,
    created_by              bigint       null,
    created_on              datetime(6)  not null,
    deleted                 bit          not null,
    updated_by              bigint       null,
    updated_on              datetime(6)  null,
    version                 smallint     null,
    account_non_expired     bit          not null,
    account_non_locked      bit          not null,
    alias                   varchar(255) null,
    avatar                  varchar(255) null,
    comment_disable_reason  varchar(255) null,
    credentials_non_expired bit          not null,
    disable_comments        bit          null,
    disable_replies         bit          null,
    email                   varchar(255) null,
    enabled                 bit          not null,
    last_login_time         datetime(6)  not null,
    no_posting_allowed      bit          null,
    no_posting_reason       varchar(255) null,
    one_sentence            varchar(255) null,
    password                varchar(255) null,
    reply_disable_reason    varchar(255) null,
    root                    bit          null,
    temporary_storage       text         null,
    token                   varchar(255) null,
    username                varchar(255) null,
    point_id                bigint       null,
    constraint UK_2jsk4eakd0rmvybo409wgwxuw
        unique (username),
    constraint UK_4xad1enskw4j1t2866f7sodrx
        unique (email),
    constraint UK_dn94k3s93vqlu1ufqus4t6y13
        unique (token),
    constraint UK_h68ijywxr8fb0aecy1w3tmoqb
        unique (point_id),
    constraint FKotxi17nb0gbck27xv06rdcvyb
        foreign key (point_id) references point_entity (id)
);

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

create table if not exists global_message_entity
(
    id            bigint       not null
        primary key,
    created_by    bigint       null,
    created_on    datetime(6)  not null,
    deleted       bit          not null,
    updated_by    bigint       null,
    updated_on    datetime(6)  null,
    version       smallint     null,
    content       json         null,
    link          varchar(255) null,
    links         json         null,
    message_range smallint     not null,
    message_type  smallint     not null,
    name          varchar(255) not null,
    overview      varchar(512) not null,
    sort          int          not null,
    sender_id     bigint       null,
    constraint UK_pt1jenvifwkmyypi8m8856td5
        unique (sender_id),
    constraint FKm68afdpt9e6ycxdmvgjq9nln6
        foreign key (sender_id) references user_entity (id),
    check (`message_range` between 0 and 1),
    check (`message_type` between 0 and 18)
);

create table if not exists global_message_user_entity
(
    created_by        bigint      null,
    created_on        datetime(6) not null,
    deleted           bit         not null,
    updated_by        bigint      null,
    updated_on        datetime(6) null,
    version           smallint    null,
    state             smallint    not null,
    global_message_id bigint      not null,
    user_id           bigint      not null,
    primary key (global_message_id, user_id),
    constraint FK2k75kgso1d45sm28yv35oop6f
        foreign key (global_message_id) references global_message_entity (id),
    constraint FKihfxiaiq17vyldcyotrn65i1t
        foreign key (user_id) references user_entity (id),
    check (`state` between 0 and 1)
);

create table if not exists message_entity
(
    id            bigint       not null
        primary key,
    created_by    bigint       null,
    created_on    datetime(6)  not null,
    deleted       bit          not null,
    updated_by    bigint       null,
    updated_on    datetime(6)  null,
    version       smallint     null,
    content       json         null,
    link          varchar(255) null,
    links         json         null,
    message_range smallint     not null,
    message_type  smallint     not null,
    name          varchar(255) not null,
    overview      varchar(512) not null,
    state         smallint     not null,
    receiver_id   bigint       null,
    sender_id     bigint       null,
    constraint FKchngvnhlot2wncosjrnmd1qjp
        foreign key (sender_id) references user_entity (id),
    constraint FKf1eboma4d9p0wlj48qwpd2ban
        foreign key (receiver_id) references user_entity (id),
    check (`message_range` between 0 and 1),
    check (`message_type` between 0 and 18),
    check (`state` between 0 and 1)
);

alter table point_entity
    add constraint FKpognvqglbn467dcgrvfsybyex
        foreign key (user_id) references user_entity (id);

create table if not exists point_history_entity
(
    id                   bigint                                                                                                                                                                                                                                                       not null
        primary key,
    created_by           bigint                                                                                                                                                                                                                                                       null,
    created_on           datetime(6)                                                                                                                                                                                                                                                  not null,
    deleted              bit                                                                                                                                                                                                                                                          not null,
    updated_by           bigint                                                                                                                                                                                                                                                       null,
    updated_on           datetime(6)                                                                                                                                                                                                                                                  null,
    version              smallint                                                                                                                                                                                                                                                     null,
    max_points           int                                                                                                                                                                                                                                                          null,
    min_points           int                                                                                                                                                                                                                                                          null,
    permission_rule_name enum ('CREATE_POST', 'CREATE_COMMENT', 'CREATE_REPLY', 'UPDATE_POST', 'ADD_POST_TAG', 'ADD_POST_CONTENT_LINK', 'ADD_POST_COVER_LINK', 'ADD_POST_SECTION')                                                                                                    null,
    point_value          int                                                                                                                                                                                                                                                          null,
    points               int                                                                                                                                                                                                                                                          null,
    reason               varchar(255)                                                                                                                                                                                                                                                 null,
    rule_name            enum ('LIKE_POST', 'LIKE_COMMENT', 'LIKE_REPLY', 'COMMENT_POST', 'REPLY_POST', 'FOLLOW_POST', 'FAVORITE_POST', 'DISLIKE_POST', 'DISLIKE_COMMENT', 'DISLIKE_REPLY', 'POST_APPROVED', 'POST_NOT_APPROVED', 'POST_PENDING_REVIEW', 'VISIT_POST', 'CREATE_POST') null,
    sign                 enum ('POSITIVE', 'NEGATIVE', 'ZERO')                                                                                                                                                                                                                        null,
    source               varchar(255)                                                                                                                                                                                                                                                 null,
    source_link          varchar(255)                                                                                                                                                                                                                                                 null,
    user_id              bigint                                                                                                                                                                                                                                                       null,
    constraint FKksk0vbx82ws1j6vrhn6j4y3wb
        foreign key (user_id) references user_entity (id)
);

create table if not exists post_entity
(
    id                   bigint       not null
        primary key,
    created_by           bigint       null,
    created_on           datetime(6)  not null,
    deleted              bit          not null,
    updated_by           bigint       null,
    updated_on           datetime(6)  null,
    version              smallint     null,
    access_key           varchar(255) null,
    class_names          varchar(255) null,
    comments_count       bigint       not null,
    content              text         null,
    content_link         varchar(255) null,
    cover                varchar(255) null,
    cover_image          mediumblob   null,
    cover_image_type     smallint     not null,
    disable_comments     bit          null,
    disable_replies      bit          null,
    favorites_count      bigint       not null,
    followers_count      bigint       not null,
    initial_score        bigint       not null,
    likes_count          bigint       not null,
    name                 varchar(255) not null,
    overview             varchar(255) null,
    page_views           bigint       not null,
    replies_count        bigint       not null,
    review_state         smallint     not null,
    sort_state           smallint     not null,
    styles               varchar(255) null,
    post_review_queue_id bigint       null,
    section_id           bigint       null,
    user_id              bigint       null,
    constraint UK_kc7ibbd39111bkurue4tppjsj
        unique (post_review_queue_id),
    constraint FK2e9ivvlpgr8x6wd2qxvlefvub
        foreign key (section_id) references section_entity (id),
    constraint FK2jmp42lmrw2f3ljd16f1re3c8
        foreign key (user_id) references user_entity (id),
    check (`cover_image_type` between 0 and 1),
    check (`review_state` between 0 and 2),
    check (`sort_state` between 0 and 3)
);

create table if not exists comment_entity
(
    id                bigint       not null
        primary key,
    created_by        bigint       null,
    created_on        datetime(6)  not null,
    deleted           bit          not null,
    updated_by        bigint       null,
    updated_on        datetime(6)  null,
    version           smallint     null,
    content           varchar(255) not null,
    likes_count       bigint       null,
    review_state      smallint     not null,
    unique_identifier varchar(255) null,
    post_id           bigint       null,
    user_id           bigint       null,
    constraint FK5q5av5arkm3of9b5n493p992p
        foreign key (post_id) references post_entity (id),
    constraint FK7u6osru73338guaca8ukops8l
        foreign key (user_id) references user_entity (id),
    check (`review_state` between 0 and 2)
);

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

create table if not exists post_badge_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    classes    varchar(255) null,
    name       varchar(255) not null,
    sort       int          not null,
    styles     varchar(255) null,
    post_id    bigint       null,
    constraint FK34jm5kbrkkl6q3wa36o5u0rmt
        foreign key (post_id) references post_entity (id)
);

create table if not exists post_entity_allows
(
    post_entity_id bigint not null,
    allows_id      bigint not null,
    primary key (post_entity_id, allows_id),
    constraint FK2clas6qso2gstahky81barvnd
        foreign key (allows_id) references user_entity (id),
    constraint FKq6n3tuvco6olnvxv7gn1k5taq
        foreign key (post_entity_id) references post_entity (id)
);

create table if not exists post_entity_blocks
(
    post_entity_id bigint not null,
    blocks_id      bigint not null,
    primary key (post_entity_id, blocks_id),
    constraint FKhvo9kl08dpnop0l1ccjp4vbnd
        foreign key (blocks_id) references user_entity (id),
    constraint FKkbu3bea3hoy7o3phai6r3nx4h
        foreign key (post_entity_id) references post_entity (id)
);

create table if not exists post_entity_states
(
    post_entity_id bigint  not null,
    states         tinyint null,
    constraint FKhk200yvna3bh9gkjnlpqvl2yj
        foreign key (post_entity_id) references post_entity (id),
    check (`states` between 0 and 5)
);

create table if not exists post_entity_tags
(
    posts_id bigint not null,
    tags_id  bigint not null,
    primary key (posts_id, tags_id),
    constraint FKmcn4gpskbh1v98223jjutwr6g
        foreign key (posts_id) references post_entity (id),
    constraint FKp0o3aqs76n1279phskbg5bgty
        foreign key (tags_id) references tag_entity (id)
);

create table if not exists post_favorite_entity
(
    id           bigint       not null
        primary key,
    created_by   bigint       null,
    created_on   datetime(6)  not null,
    deleted      bit          not null,
    updated_by   bigint       null,
    updated_on   datetime(6)  null,
    version      smallint     null,
    content      text         null,
    content_link varchar(255) null,
    name         varchar(255) not null,
    overview     varchar(255) null,
    post_id      bigint       null,
    user_id      bigint       null,
    constraint UK_pn91hw7x2pb4pfuks68g04m3i
        unique (post_id),
    constraint FK8xpmgt9es0635uycto71vs3p5
        foreign key (post_id) references post_entity (id),
    constraint FKjm8m6tss6sybggikryth2qeny
        foreign key (user_id) references user_entity (id)
);

create table if not exists post_history_entity
(
    id                     bigint       not null
        primary key,
    created_by             bigint       null,
    created_on             datetime(6)  not null,
    deleted                bit          not null,
    updated_by             bigint       null,
    updated_on             datetime(6)  null,
    version                smallint     null,
    comment_disable_reason varchar(255) null,
    disable_comments       bit          null,
    disable_replies        bit          null,
    reply_disable_reason   varchar(255) null,
    post_id                bigint       null,
    constraint FKqjm8b49lnyt2dkhpftk13hq5y
        foreign key (post_id) references post_entity (id)
);

create table if not exists post_image_entity
(
    id         bigint       not null
        primary key,
    created_by bigint       null,
    created_on datetime(6)  not null,
    deleted    bit          not null,
    updated_by bigint       null,
    updated_on datetime(6)  null,
    version    smallint     null,
    image      mediumblob   null,
    image_type smallint     not null,
    sort       int          not null,
    url        varchar(255) not null,
    post_id    bigint       null,
    constraint FK7x84i77w0pu88xutpuiu1u801
        foreign key (post_id) references post_entity (id),
    check (`image_type` between 0 and 1)
);

create table if not exists post_review_history_entity
(
    id                        bigint       not null
        primary key,
    created_by                bigint       null,
    created_on                datetime(6)  not null,
    deleted                   bit          not null,
    updated_by                bigint       null,
    updated_on                datetime(6)  null,
    version                   smallint     null,
    latest_review_result_time date         null,
    review_reason             varchar(255) null,
    review_state              smallint     not null,
    post_id                   bigint       null,
    reviewer_id               bigint       null,
    constraint FK4q0iimsun4lk5kkaw3bn5chti
        foreign key (post_id) references post_entity (id),
    constraint FKe7ak4w53axiwmkagrtx9fvogh
        foreign key (reviewer_id) references user_entity (id),
    check (`review_state` between 0 and 2)
);

create table if not exists post_review_queue_entity
(
    id                        bigint      not null
        primary key,
    created_by                bigint      null,
    created_on                datetime(6) not null,
    deleted                   bit         not null,
    updated_by                bigint      null,
    updated_on                datetime(6) null,
    version                   smallint    null,
    latest_review_result_time date        null,
    received                  bit         null,
    post_id                   bigint      null,
    receiver_id               bigint      null,
    constraint UK_r19orfn87k4jo00673bo8vbcf
        unique (post_id),
    constraint FKh2dnlbgv8gapf297i2o802et6
        foreign key (receiver_id) references user_entity (id),
    constraint FKlgcpqwakmoatjlql3bpxcmaoe
        foreign key (post_id) references post_entity (id)
);

alter table post_entity
    add constraint FKopdbggrpv46rq9ef247xwweos
        foreign key (post_review_queue_id) references post_review_queue_entity (id);

create table if not exists post_user_entity
(
    created_by             bigint       null,
    created_on             datetime(6)  not null,
    deleted                bit          not null,
    updated_by             bigint       null,
    updated_on             datetime(6)  null,
    version                smallint     null,
    comment_disable_reason varchar(255) null,
    disable_comments       bit          null,
    disable_replies        bit          null,
    favorited              bit          null,
    followed               bit          null,
    liked                  bit          null,
    reply_disable_reason   varchar(255) null,
    post_id                bigint       not null,
    user_id                bigint       not null,
    primary key (post_id, user_id),
    constraint FK3t98bw4lrjgnby8l0an7fjs4r
        foreign key (post_id) references post_entity (id),
    constraint FKqatx932e4rfg2essm3pf5c20o
        foreign key (user_id) references user_entity (id)
);

create table if not exists quote_reply_entity
(
    id                bigint       not null
        primary key,
    created_by        bigint       null,
    created_on        datetime(6)  not null,
    deleted           bit          not null,
    updated_by        bigint       null,
    updated_on        datetime(6)  null,
    version           smallint     null,
    content           varchar(255) not null,
    likes_count       bigint       null,
    review_state      smallint     not null,
    unique_identifier varchar(255) null,
    comment_id        bigint       null,
    post_id           bigint       null,
    quote_reply_id    bigint       null,
    user_id           bigint       null,
    constraint FKc1f8k34dp659y9sv9gnvhl01x
        foreign key (comment_id) references comment_entity (id),
    constraint FKeovnjq8qxbk8q7j3bs8jryf5s
        foreign key (user_id) references user_entity (id),
    constraint FKkj3qjbne6xpaaxkpii78h8pup
        foreign key (post_id) references post_entity (id),
    constraint FKq7q1x1bygvfj3pba4jlub436h
        foreign key (quote_reply_id) references quote_reply_entity (id),
    check (`review_state` between 0 and 2)
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

create table if not exists section_entity_admins
(
    sections_id bigint not null,
    admins_id   bigint not null,
    primary key (sections_id, admins_id),
    constraint FK6kf0atjcei00xmm3bxx1hpo8s
        foreign key (sections_id) references section_entity (id),
    constraint FKadxio4oxaakcksyk6eplyg8ei
        foreign key (admins_id) references user_entity (id)
);

create table if not exists section_entity_allows
(
    section_allows_id bigint not null,
    allows_id         bigint not null,
    primary key (section_allows_id, allows_id),
    constraint FK1crgsnfbeyuah0pt31pv1vrj
        foreign key (section_allows_id) references section_entity (id),
    constraint FKk8vd217q5vljpjxvm3gd7es32
        foreign key (allows_id) references user_entity (id)
);

create table if not exists section_entity_blocks
(
    section_blocks_id bigint not null,
    blocks_id         bigint not null,
    primary key (section_blocks_id, blocks_id),
    constraint FK4gbqogag230vpo2bpto59ou7d
        foreign key (section_blocks_id) references section_entity (id),
    constraint FKk3ml2lbcehawbqlq6242wrwnq
        foreign key (blocks_id) references user_entity (id)
);

create table if not exists user_entity_roles
(
    users_id bigint not null,
    roles_id bigint not null,
    primary key (users_id, roles_id),
    constraint FKjrrr5bv2349ah0u4fm0g84v8
        foreign key (users_id) references user_entity (id),
    constraint FKr70hb6wpq5vq5ennenkk12nqk
        foreign key (roles_id) references role_entity (id)
);

create table if not exists user_entity_seq
(
    next_val bigint null
);

INSERT INTO user_entity_seq (next_val)
VALUES (1);
