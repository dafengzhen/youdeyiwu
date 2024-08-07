// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PostCWProxy {
  Post id(int id);

  Post deleted(bool deleted);

  Post createdBy(int? createdBy);

  Post updatedBy(int? updatedBy);

  Post createdOn(String? createdOn);

  Post updatedOn(String? updatedOn);

  Post name(String name);

  Post disableComments(bool disableComments);

  Post disableReplies(bool disableReplies);

  Post states(Set<PostStateEnum> states);

  Post reviewState(PostReviewStateEnum reviewState);

  Post sortState(PostSortStateEnum sortState);

  Post pageViews(int pageViews);

  Post commentsCount(int commentsCount);

  Post repliesCount(int repliesCount);

  Post likesCount(int likesCount);

  Post followersCount(int followersCount);

  Post favoritesCount(int favoritesCount);

  Post tags(Set<Tag>? tags);

  Post cover(String? cover);

  Post coverImage(List<int>? coverImage);

  Post coverImageType(FileTypeEnum? coverImageType);

  Post overview(String? overview);

  Post content(String? content);

  Post plainTextContent(String? plainTextContent);

  Post markdownContent(String? markdownContent);

  Post deltaContent(String? deltaContent);

  Post contentLink(String? contentLink);

  Post accessKey(String? accessKey);

  Post styles(String? styles);

  Post classNames(String? classNames);

  Post section(Section? section);

  Post user(User? user);

  Post liked(bool? liked);

  Post followed(bool? followed);

  Post favorited(bool? favorited);

  Post comments(Page<CommentReply>? comments);

  Post postReviewQueue(PostReviewQueue? postReviewQueue);

  Post badges(Set<PostBadge>? badges);

  Post images(Set<PostImage>? images);

  Post allows(Set<User>? allows);

  Post blocks(Set<User>? blocks);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Post(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Post(...).copyWith(id: 12, name: "My name")
  /// ````
  Post call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    bool? disableComments,
    bool? disableReplies,
    Set<PostStateEnum>? states,
    PostReviewStateEnum? reviewState,
    PostSortStateEnum? sortState,
    int? pageViews,
    int? commentsCount,
    int? repliesCount,
    int? likesCount,
    int? followersCount,
    int? favoritesCount,
    Set<Tag>? tags,
    String? cover,
    List<int>? coverImage,
    FileTypeEnum? coverImageType,
    String? overview,
    String? content,
    String? plainTextContent,
    String? markdownContent,
    String? deltaContent,
    String? contentLink,
    String? accessKey,
    String? styles,
    String? classNames,
    Section? section,
    User? user,
    bool? liked,
    bool? followed,
    bool? favorited,
    Page<CommentReply>? comments,
    PostReviewQueue? postReviewQueue,
    Set<PostBadge>? badges,
    Set<PostImage>? images,
    Set<User>? allows,
    Set<User>? blocks,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPost.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPost.copyWith.fieldName(...)`
class _$PostCWProxyImpl implements _$PostCWProxy {
  const _$PostCWProxyImpl(this._value);

  final Post _value;

  @override
  Post id(int id) => this(id: id);

  @override
  Post deleted(bool deleted) => this(deleted: deleted);

  @override
  Post createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Post updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Post createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Post updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Post name(String name) => this(name: name);

  @override
  Post disableComments(bool disableComments) =>
      this(disableComments: disableComments);

  @override
  Post disableReplies(bool disableReplies) =>
      this(disableReplies: disableReplies);

  @override
  Post states(Set<PostStateEnum> states) => this(states: states);

  @override
  Post reviewState(PostReviewStateEnum reviewState) =>
      this(reviewState: reviewState);

  @override
  Post sortState(PostSortStateEnum sortState) => this(sortState: sortState);

  @override
  Post pageViews(int pageViews) => this(pageViews: pageViews);

  @override
  Post commentsCount(int commentsCount) => this(commentsCount: commentsCount);

  @override
  Post repliesCount(int repliesCount) => this(repliesCount: repliesCount);

  @override
  Post likesCount(int likesCount) => this(likesCount: likesCount);

  @override
  Post followersCount(int followersCount) =>
      this(followersCount: followersCount);

  @override
  Post favoritesCount(int favoritesCount) =>
      this(favoritesCount: favoritesCount);

  @override
  Post tags(Set<Tag>? tags) => this(tags: tags);

  @override
  Post cover(String? cover) => this(cover: cover);

  @override
  Post coverImage(List<int>? coverImage) => this(coverImage: coverImage);

  @override
  Post coverImageType(FileTypeEnum? coverImageType) =>
      this(coverImageType: coverImageType);

  @override
  Post overview(String? overview) => this(overview: overview);

  @override
  Post content(String? content) => this(content: content);

  @override
  Post plainTextContent(String? plainTextContent) =>
      this(plainTextContent: plainTextContent);

  @override
  Post markdownContent(String? markdownContent) =>
      this(markdownContent: markdownContent);

  @override
  Post deltaContent(String? deltaContent) => this(deltaContent: deltaContent);

  @override
  Post contentLink(String? contentLink) => this(contentLink: contentLink);

  @override
  Post accessKey(String? accessKey) => this(accessKey: accessKey);

  @override
  Post styles(String? styles) => this(styles: styles);

  @override
  Post classNames(String? classNames) => this(classNames: classNames);

  @override
  Post section(Section? section) => this(section: section);

  @override
  Post user(User? user) => this(user: user);

  @override
  Post liked(bool? liked) => this(liked: liked);

  @override
  Post followed(bool? followed) => this(followed: followed);

  @override
  Post favorited(bool? favorited) => this(favorited: favorited);

  @override
  Post comments(Page<CommentReply>? comments) => this(comments: comments);

  @override
  Post postReviewQueue(PostReviewQueue? postReviewQueue) =>
      this(postReviewQueue: postReviewQueue);

  @override
  Post badges(Set<PostBadge>? badges) => this(badges: badges);

  @override
  Post images(Set<PostImage>? images) => this(images: images);

  @override
  Post allows(Set<User>? allows) => this(allows: allows);

  @override
  Post blocks(Set<User>? blocks) => this(blocks: blocks);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Post(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Post(...).copyWith(id: 12, name: "My name")
  /// ````
  Post call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? disableComments = const $CopyWithPlaceholder(),
    Object? disableReplies = const $CopyWithPlaceholder(),
    Object? states = const $CopyWithPlaceholder(),
    Object? reviewState = const $CopyWithPlaceholder(),
    Object? sortState = const $CopyWithPlaceholder(),
    Object? pageViews = const $CopyWithPlaceholder(),
    Object? commentsCount = const $CopyWithPlaceholder(),
    Object? repliesCount = const $CopyWithPlaceholder(),
    Object? likesCount = const $CopyWithPlaceholder(),
    Object? followersCount = const $CopyWithPlaceholder(),
    Object? favoritesCount = const $CopyWithPlaceholder(),
    Object? tags = const $CopyWithPlaceholder(),
    Object? cover = const $CopyWithPlaceholder(),
    Object? coverImage = const $CopyWithPlaceholder(),
    Object? coverImageType = const $CopyWithPlaceholder(),
    Object? overview = const $CopyWithPlaceholder(),
    Object? content = const $CopyWithPlaceholder(),
    Object? plainTextContent = const $CopyWithPlaceholder(),
    Object? markdownContent = const $CopyWithPlaceholder(),
    Object? deltaContent = const $CopyWithPlaceholder(),
    Object? contentLink = const $CopyWithPlaceholder(),
    Object? accessKey = const $CopyWithPlaceholder(),
    Object? styles = const $CopyWithPlaceholder(),
    Object? classNames = const $CopyWithPlaceholder(),
    Object? section = const $CopyWithPlaceholder(),
    Object? user = const $CopyWithPlaceholder(),
    Object? liked = const $CopyWithPlaceholder(),
    Object? followed = const $CopyWithPlaceholder(),
    Object? favorited = const $CopyWithPlaceholder(),
    Object? comments = const $CopyWithPlaceholder(),
    Object? postReviewQueue = const $CopyWithPlaceholder(),
    Object? badges = const $CopyWithPlaceholder(),
    Object? images = const $CopyWithPlaceholder(),
    Object? allows = const $CopyWithPlaceholder(),
    Object? blocks = const $CopyWithPlaceholder(),
  }) {
    return Post(
      id: id == const $CopyWithPlaceholder() || id == null
          ? _value.id
          // ignore: cast_nullable_to_non_nullable
          : id as int,
      deleted: deleted == const $CopyWithPlaceholder() || deleted == null
          ? _value.deleted
          // ignore: cast_nullable_to_non_nullable
          : deleted as bool,
      createdBy: createdBy == const $CopyWithPlaceholder()
          ? _value.createdBy
          // ignore: cast_nullable_to_non_nullable
          : createdBy as int?,
      updatedBy: updatedBy == const $CopyWithPlaceholder()
          ? _value.updatedBy
          // ignore: cast_nullable_to_non_nullable
          : updatedBy as int?,
      createdOn: createdOn == const $CopyWithPlaceholder()
          ? _value.createdOn
          // ignore: cast_nullable_to_non_nullable
          : createdOn as String?,
      updatedOn: updatedOn == const $CopyWithPlaceholder()
          ? _value.updatedOn
          // ignore: cast_nullable_to_non_nullable
          : updatedOn as String?,
      name: name == const $CopyWithPlaceholder() || name == null
          ? _value.name
          // ignore: cast_nullable_to_non_nullable
          : name as String,
      disableComments: disableComments == const $CopyWithPlaceholder() ||
              disableComments == null
          ? _value.disableComments
          // ignore: cast_nullable_to_non_nullable
          : disableComments as bool,
      disableReplies: disableReplies == const $CopyWithPlaceholder() ||
              disableReplies == null
          ? _value.disableReplies
          // ignore: cast_nullable_to_non_nullable
          : disableReplies as bool,
      states: states == const $CopyWithPlaceholder() || states == null
          ? _value.states
          // ignore: cast_nullable_to_non_nullable
          : states as Set<PostStateEnum>,
      reviewState:
          reviewState == const $CopyWithPlaceholder() || reviewState == null
              ? _value.reviewState
              // ignore: cast_nullable_to_non_nullable
              : reviewState as PostReviewStateEnum,
      sortState: sortState == const $CopyWithPlaceholder() || sortState == null
          ? _value.sortState
          // ignore: cast_nullable_to_non_nullable
          : sortState as PostSortStateEnum,
      pageViews: pageViews == const $CopyWithPlaceholder() || pageViews == null
          ? _value.pageViews
          // ignore: cast_nullable_to_non_nullable
          : pageViews as int,
      commentsCount:
          commentsCount == const $CopyWithPlaceholder() || commentsCount == null
              ? _value.commentsCount
              // ignore: cast_nullable_to_non_nullable
              : commentsCount as int,
      repliesCount:
          repliesCount == const $CopyWithPlaceholder() || repliesCount == null
              ? _value.repliesCount
              // ignore: cast_nullable_to_non_nullable
              : repliesCount as int,
      likesCount:
          likesCount == const $CopyWithPlaceholder() || likesCount == null
              ? _value.likesCount
              // ignore: cast_nullable_to_non_nullable
              : likesCount as int,
      followersCount: followersCount == const $CopyWithPlaceholder() ||
              followersCount == null
          ? _value.followersCount
          // ignore: cast_nullable_to_non_nullable
          : followersCount as int,
      favoritesCount: favoritesCount == const $CopyWithPlaceholder() ||
              favoritesCount == null
          ? _value.favoritesCount
          // ignore: cast_nullable_to_non_nullable
          : favoritesCount as int,
      tags: tags == const $CopyWithPlaceholder()
          ? _value.tags
          // ignore: cast_nullable_to_non_nullable
          : tags as Set<Tag>?,
      cover: cover == const $CopyWithPlaceholder()
          ? _value.cover
          // ignore: cast_nullable_to_non_nullable
          : cover as String?,
      coverImage: coverImage == const $CopyWithPlaceholder()
          ? _value.coverImage
          // ignore: cast_nullable_to_non_nullable
          : coverImage as List<int>?,
      coverImageType: coverImageType == const $CopyWithPlaceholder()
          ? _value.coverImageType
          // ignore: cast_nullable_to_non_nullable
          : coverImageType as FileTypeEnum?,
      overview: overview == const $CopyWithPlaceholder()
          ? _value.overview
          // ignore: cast_nullable_to_non_nullable
          : overview as String?,
      content: content == const $CopyWithPlaceholder()
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as String?,
      plainTextContent: plainTextContent == const $CopyWithPlaceholder()
          ? _value.plainTextContent
          // ignore: cast_nullable_to_non_nullable
          : plainTextContent as String?,
      markdownContent: markdownContent == const $CopyWithPlaceholder()
          ? _value.markdownContent
          // ignore: cast_nullable_to_non_nullable
          : markdownContent as String?,
      deltaContent: deltaContent == const $CopyWithPlaceholder()
          ? _value.deltaContent
          // ignore: cast_nullable_to_non_nullable
          : deltaContent as String?,
      contentLink: contentLink == const $CopyWithPlaceholder()
          ? _value.contentLink
          // ignore: cast_nullable_to_non_nullable
          : contentLink as String?,
      accessKey: accessKey == const $CopyWithPlaceholder()
          ? _value.accessKey
          // ignore: cast_nullable_to_non_nullable
          : accessKey as String?,
      styles: styles == const $CopyWithPlaceholder()
          ? _value.styles
          // ignore: cast_nullable_to_non_nullable
          : styles as String?,
      classNames: classNames == const $CopyWithPlaceholder()
          ? _value.classNames
          // ignore: cast_nullable_to_non_nullable
          : classNames as String?,
      section: section == const $CopyWithPlaceholder()
          ? _value.section
          // ignore: cast_nullable_to_non_nullable
          : section as Section?,
      user: user == const $CopyWithPlaceholder()
          ? _value.user
          // ignore: cast_nullable_to_non_nullable
          : user as User?,
      liked: liked == const $CopyWithPlaceholder()
          ? _value.liked
          // ignore: cast_nullable_to_non_nullable
          : liked as bool?,
      followed: followed == const $CopyWithPlaceholder()
          ? _value.followed
          // ignore: cast_nullable_to_non_nullable
          : followed as bool?,
      favorited: favorited == const $CopyWithPlaceholder()
          ? _value.favorited
          // ignore: cast_nullable_to_non_nullable
          : favorited as bool?,
      comments: comments == const $CopyWithPlaceholder()
          ? _value.comments
          // ignore: cast_nullable_to_non_nullable
          : comments as Page<CommentReply>?,
      postReviewQueue: postReviewQueue == const $CopyWithPlaceholder()
          ? _value.postReviewQueue
          // ignore: cast_nullable_to_non_nullable
          : postReviewQueue as PostReviewQueue?,
      badges: badges == const $CopyWithPlaceholder()
          ? _value.badges
          // ignore: cast_nullable_to_non_nullable
          : badges as Set<PostBadge>?,
      images: images == const $CopyWithPlaceholder()
          ? _value.images
          // ignore: cast_nullable_to_non_nullable
          : images as Set<PostImage>?,
      allows: allows == const $CopyWithPlaceholder()
          ? _value.allows
          // ignore: cast_nullable_to_non_nullable
          : allows as Set<User>?,
      blocks: blocks == const $CopyWithPlaceholder()
          ? _value.blocks
          // ignore: cast_nullable_to_non_nullable
          : blocks as Set<User>?,
    );
  }
}

extension $PostCopyWith on Post {
  /// Returns a callable class that can be used as follows: `instanceOfPost.copyWith(...)` or like so:`instanceOfPost.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PostCWProxy get copyWith => _$PostCWProxyImpl(this);
}

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
      tags: (json['tags'] as List<dynamic>?)
          ?.map((e) => Tag.fromJson(e as Map<String, dynamic>))
          .toSet(),
      cover: json['cover'] as String?,
      coverImage: (json['coverImage'] as List<dynamic>?)
          ?.map((e) => (e as num).toInt())
          .toList(),
      coverImageType:
          $enumDecodeNullable(_$FileTypeEnumEnumMap, json['coverImageType']),
      overview: json['overview'] as String?,
      content: json['content'] as String?,
      plainTextContent: json['plainTextContent'] as String?,
      markdownContent: json['markdownContent'] as String?,
      deltaContent: json['deltaContent'] as String?,
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
  writeNotNull('plainTextContent', instance.plainTextContent);
  writeNotNull('markdownContent', instance.markdownContent);
  writeNotNull('deltaContent', instance.deltaContent);
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
  writeNotNull('tags', instance.tags?.toList());
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
