create table if not exists post_entity_tags_new
(
    posts_id bigint not null,
    tags_id  bigint not null,
    primary key (posts_id, tags_id),
    constraint FKmcn4gpskbh1v98223jjutwr6g
        foreign key (posts_id) references post_entity (id),
    constraint FKp0o3aqs76n1279phskbg5bgty_new
        foreign key (tags_id) references tag_entity (id)
);

insert into post_entity_tags_new (posts_id, tags_id)
select post_entity_id, tags_id
from post_entity_tags;

drop table if exists post_entity_tags;

alter table post_entity_tags_new rename to post_entity_tags;

alter table post_entity_tags
    drop foreign key FKp0o3aqs76n1279phskbg5bgty_new,
    add constraint FKp0o3aqs76n1279phskbg5bgty
        foreign key (tags_id) references tag_entity (id);
