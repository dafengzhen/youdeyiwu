// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Post _$PostFromJson(Map<String, dynamic> json) => Post(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      disableComments: json['disableComments'] as bool,
      disableReplies: json['disableReplies'] as bool,
      states: (json['states'] as List<dynamic>)
          .map((e) => $enumDecode(_$PostStateEnumEnumMap, e))
          .toSet(),
      reviewState:
          $enumDecode(_$PostReviewStateEnumEnumMap, json['reviewState']),
      sortState: $enumDecode(_$PostSortStateEnumEnumMap, json['sortState']),
      pageViews: (json['pageViews'] as num).toInt(),
      commentsCount: (json['commentsCount'] as num).toInt(),
      repliesCount: (json['repliesCount'] as num).toInt(),
      likesCount: (json['likesCount'] as num).toInt(),
      followersCount: (json['followersCount'] as num).toInt(),
      favoritesCount: (json['favoritesCount'] as num).toInt(),
      tags: (json['tags'] as List<dynamic>)
          .map((e) => Tag.fromJson(e as Map<String, dynamic>))
          .toSet(),
      cover: json['cover'] as String?,
      coverImage: (json['coverImage'] as List<dynamic>?)
          ?.map((e) => (e as num).toInt())
          .toList(),
      coverImageType:
          $enumDecodeNullable(_$FileTypeEnumEnumMap, json['coverImageType']),
      overview: json['overview'] as String?,
      content: json['content'] as String?,
      contentLink: json['contentLink'] as String?,
      accessKey: json['accessKey'] as String?,
      styles: json['styles'] as String?,
      classNames: json['classNames'] as String?,
      section: json['section'] == null
          ? null
          : Section.fromJson(json['section'] as Map<String, dynamic>),
      user: json['user'] == null
          ? null
          : User.fromJson(json['user'] as Map<String, dynamic>),
      liked: json['liked'] as bool?,
      followed: json['followed'] as bool?,
      favorited: json['favorited'] as bool?,
      comments: json['comments'] == null
          ? null
          : Page<CommentReply>.fromJson(
              json['comments'] as Map<String, dynamic>,
              (value) => CommentReply.fromJson(value as Map<String, dynamic>)),
      postReviewQueue: json['postReviewQueue'] == null
          ? null
          : PostReviewQueue.fromJson(
              json['postReviewQueue'] as Map<String, dynamic>),
      badges: (json['badges'] as List<dynamic>?)
          ?.map((e) => PostBadge.fromJson(e as Map<String, dynamic>))
          .toSet(),
      images: (json['images'] as List<dynamic>?)
          ?.map((e) => PostImage.fromJson(e as Map<String, dynamic>))
          .toSet(),
      allows: (json['allows'] as List<dynamic>?)
          ?.map((e) => User.fromJson(e as Map<String, dynamic>))
          .toSet(),
      blocks: (json['blocks'] as List<dynamic>?)
          ?.map((e) => User.fromJson(e as Map<String, dynamic>))
          .toSet(),
    );

Map<String, dynamic> _$PostToJson(Post instance) {
  final val = <String, dynamic>{
    'id': instance.id,
  };

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('createdBy', instance.createdBy);
  writeNotNull('updatedBy', instance.updatedBy);
  writeNotNull('createdOn', instance.createdOn);
  writeNotNull('updatedOn', instance.updatedOn);
  val['deleted'] = instance.deleted;
  val['name'] = instance.name;
  writeNotNull('cover', instance.cover);
  writeNotNull('coverImage', instance.coverImage);
  writeNotNull(
      'coverImageType', _$FileTypeEnumEnumMap[instance.coverImageType]);
  writeNotNull('overview', instance.overview);
  writeNotNull('content', instance.content);
  writeNotNull('contentLink', instance.contentLink);
  val['disableComments'] = instance.disableComments;
  val['disableReplies'] = instance.disableReplies;
  writeNotNull('badges', instance.badges?.toList());
  writeNotNull('images', instance.images?.toList());
  val['states'] =
      instance.states.map((e) => _$PostStateEnumEnumMap[e]!).toList();
  val['reviewState'] = _$PostReviewStateEnumEnumMap[instance.reviewState]!;
  val['sortState'] = _$PostSortStateEnumEnumMap[instance.sortState]!;
  writeNotNull('allows', instance.allows?.toList());
  writeNotNull('blocks', instance.blocks?.toList());
  writeNotNull('accessKey', instance.accessKey);
  writeNotNull('styles', instance.styles);
  writeNotNull('classNames', instance.classNames);
  val['pageViews'] = instance.pageViews;
  val['commentsCount'] = instance.commentsCount;
  val['repliesCount'] = instance.repliesCount;
  val['likesCount'] = instance.likesCount;
  val['followersCount'] = instance.followersCount;
  val['favoritesCount'] = instance.favoritesCount;
  writeNotNull('section', instance.section);
  val['tags'] = instance.tags.toList();
  writeNotNull('user', instance.user);
  writeNotNull('liked', instance.liked);
  writeNotNull('followed', instance.followed);
  writeNotNull('favorited', instance.favorited);
  writeNotNull(
      'comments',
      instance.comments?.toJson(
        (value) => value,
      ));
  writeNotNull('postReviewQueue', instance.postReviewQueue);
  return val;
}

const _$PostStateEnumEnumMap = {
  PostStateEnum.show: 'SHOW',
  PostStateEnum.hide: 'HIDE',
  PostStateEnum.lock: 'LOCK',
  PostStateEnum.allow: 'ALLOW',
  PostStateEnum.block: 'BLOCK',
  PostStateEnum.visibleAfterLogin: 'VISIBLE_AFTER_LOGIN',
};

const _$PostReviewStateEnumEnumMap = {
  PostReviewStateEnum.approved: 'APPROVED',
  PostReviewStateEnum.rejected: 'REJECTED',
  PostReviewStateEnum.pendingReview: 'PENDING_REVIEW',
};

const _$PostSortStateEnumEnumMap = {
  PostSortStateEnum.defaultSort: 'DEFAULT',
  PostSortStateEnum.popular: 'POPULAR',
  PostSortStateEnum.currentTop: 'CURRENT_TOP',
  PostSortStateEnum.globalTop: 'GLOBAL_TOP',
};

const _$FileTypeEnumEnumMap = {
  FileTypeEnum.png: 'PNG',
  FileTypeEnum.jpg: 'JPG',
};
