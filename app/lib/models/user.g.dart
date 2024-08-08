// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'user.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$UserCWProxy {
  User id(int id);

  User deleted(bool deleted);

  User createdBy(int? createdBy);

  User updatedBy(int? updatedBy);

  User createdOn(String? createdOn);

  User updatedOn(String? updatedOn);

  User username(String username);

  User lastLoginTime(String lastLoginTime);

  User accountNonExpired(bool accountNonExpired);

  User credentialsNonExpired(bool credentialsNonExpired);

  User accountNonLocked(bool accountNonLocked);

  User enabled(bool enabled);

  User alias(String? alias);

  User avatar(String? avatar);

  User oneSentence(String? oneSentence);

  User email(String? email);

  User temporaryStorage(String? temporaryStorage);

  User root(bool? root);

  User noPostingAllowed(bool? noPostingAllowed);

  User disableComments(bool? disableComments);

  User disableReplies(bool? disableReplies);

  User noPostingReason(String? noPostingReason);

  User commentDisableReason(String? commentDisableReason);

  User replyDisableReason(String? replyDisableReason);

  User roles(Set<Role>? roles);

  User sections(Set<Section>? sections);

  User posts(Set<Post>? posts);

  User relatedSections(Set<Section>? relatedSections);

  User relatedTags(Set<Tag>? relatedTags);

  User relatedStatistics(RelatedStatistics? relatedStatistics);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `User(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// User(...).copyWith(id: 12, name: "My name")
  /// ````
  User call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? username,
    String? lastLoginTime,
    bool? accountNonExpired,
    bool? credentialsNonExpired,
    bool? accountNonLocked,
    bool? enabled,
    String? alias,
    String? avatar,
    String? oneSentence,
    String? email,
    String? temporaryStorage,
    bool? root,
    bool? noPostingAllowed,
    bool? disableComments,
    bool? disableReplies,
    String? noPostingReason,
    String? commentDisableReason,
    String? replyDisableReason,
    Set<Role>? roles,
    Set<Section>? sections,
    Set<Post>? posts,
    Set<Section>? relatedSections,
    Set<Tag>? relatedTags,
    RelatedStatistics? relatedStatistics,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfUser.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfUser.copyWith.fieldName(...)`
class _$UserCWProxyImpl implements _$UserCWProxy {
  const _$UserCWProxyImpl(this._value);

  final User _value;

  @override
  User id(int id) => this(id: id);

  @override
  User deleted(bool deleted) => this(deleted: deleted);

  @override
  User createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  User updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  User createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  User updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  User username(String username) => this(username: username);

  @override
  User lastLoginTime(String lastLoginTime) =>
      this(lastLoginTime: lastLoginTime);

  @override
  User accountNonExpired(bool accountNonExpired) =>
      this(accountNonExpired: accountNonExpired);

  @override
  User credentialsNonExpired(bool credentialsNonExpired) =>
      this(credentialsNonExpired: credentialsNonExpired);

  @override
  User accountNonLocked(bool accountNonLocked) =>
      this(accountNonLocked: accountNonLocked);

  @override
  User enabled(bool enabled) => this(enabled: enabled);

  @override
  User alias(String? alias) => this(alias: alias);

  @override
  User avatar(String? avatar) => this(avatar: avatar);

  @override
  User oneSentence(String? oneSentence) => this(oneSentence: oneSentence);

  @override
  User email(String? email) => this(email: email);

  @override
  User temporaryStorage(String? temporaryStorage) =>
      this(temporaryStorage: temporaryStorage);

  @override
  User root(bool? root) => this(root: root);

  @override
  User noPostingAllowed(bool? noPostingAllowed) =>
      this(noPostingAllowed: noPostingAllowed);

  @override
  User disableComments(bool? disableComments) =>
      this(disableComments: disableComments);

  @override
  User disableReplies(bool? disableReplies) =>
      this(disableReplies: disableReplies);

  @override
  User noPostingReason(String? noPostingReason) =>
      this(noPostingReason: noPostingReason);

  @override
  User commentDisableReason(String? commentDisableReason) =>
      this(commentDisableReason: commentDisableReason);

  @override
  User replyDisableReason(String? replyDisableReason) =>
      this(replyDisableReason: replyDisableReason);

  @override
  User roles(Set<Role>? roles) => this(roles: roles);

  @override
  User sections(Set<Section>? sections) => this(sections: sections);

  @override
  User posts(Set<Post>? posts) => this(posts: posts);

  @override
  User relatedSections(Set<Section>? relatedSections) =>
      this(relatedSections: relatedSections);

  @override
  User relatedTags(Set<Tag>? relatedTags) => this(relatedTags: relatedTags);

  @override
  User relatedStatistics(RelatedStatistics? relatedStatistics) =>
      this(relatedStatistics: relatedStatistics);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `User(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// User(...).copyWith(id: 12, name: "My name")
  /// ````
  User call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? username = const $CopyWithPlaceholder(),
    Object? lastLoginTime = const $CopyWithPlaceholder(),
    Object? accountNonExpired = const $CopyWithPlaceholder(),
    Object? credentialsNonExpired = const $CopyWithPlaceholder(),
    Object? accountNonLocked = const $CopyWithPlaceholder(),
    Object? enabled = const $CopyWithPlaceholder(),
    Object? alias = const $CopyWithPlaceholder(),
    Object? avatar = const $CopyWithPlaceholder(),
    Object? oneSentence = const $CopyWithPlaceholder(),
    Object? email = const $CopyWithPlaceholder(),
    Object? temporaryStorage = const $CopyWithPlaceholder(),
    Object? root = const $CopyWithPlaceholder(),
    Object? noPostingAllowed = const $CopyWithPlaceholder(),
    Object? disableComments = const $CopyWithPlaceholder(),
    Object? disableReplies = const $CopyWithPlaceholder(),
    Object? noPostingReason = const $CopyWithPlaceholder(),
    Object? commentDisableReason = const $CopyWithPlaceholder(),
    Object? replyDisableReason = const $CopyWithPlaceholder(),
    Object? roles = const $CopyWithPlaceholder(),
    Object? sections = const $CopyWithPlaceholder(),
    Object? posts = const $CopyWithPlaceholder(),
    Object? relatedSections = const $CopyWithPlaceholder(),
    Object? relatedTags = const $CopyWithPlaceholder(),
    Object? relatedStatistics = const $CopyWithPlaceholder(),
  }) {
    return User(
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
      username: username == const $CopyWithPlaceholder() || username == null
          ? _value.username
          // ignore: cast_nullable_to_non_nullable
          : username as String,
      lastLoginTime:
          lastLoginTime == const $CopyWithPlaceholder() || lastLoginTime == null
              ? _value.lastLoginTime
              // ignore: cast_nullable_to_non_nullable
              : lastLoginTime as String,
      accountNonExpired: accountNonExpired == const $CopyWithPlaceholder() ||
              accountNonExpired == null
          ? _value.accountNonExpired
          // ignore: cast_nullable_to_non_nullable
          : accountNonExpired as bool,
      credentialsNonExpired:
          credentialsNonExpired == const $CopyWithPlaceholder() ||
                  credentialsNonExpired == null
              ? _value.credentialsNonExpired
              // ignore: cast_nullable_to_non_nullable
              : credentialsNonExpired as bool,
      accountNonLocked: accountNonLocked == const $CopyWithPlaceholder() ||
              accountNonLocked == null
          ? _value.accountNonLocked
          // ignore: cast_nullable_to_non_nullable
          : accountNonLocked as bool,
      enabled: enabled == const $CopyWithPlaceholder() || enabled == null
          ? _value.enabled
          // ignore: cast_nullable_to_non_nullable
          : enabled as bool,
      alias: alias == const $CopyWithPlaceholder()
          ? _value.alias
          // ignore: cast_nullable_to_non_nullable
          : alias as String?,
      avatar: avatar == const $CopyWithPlaceholder()
          ? _value.avatar
          // ignore: cast_nullable_to_non_nullable
          : avatar as String?,
      oneSentence: oneSentence == const $CopyWithPlaceholder()
          ? _value.oneSentence
          // ignore: cast_nullable_to_non_nullable
          : oneSentence as String?,
      email: email == const $CopyWithPlaceholder()
          ? _value.email
          // ignore: cast_nullable_to_non_nullable
          : email as String?,
      temporaryStorage: temporaryStorage == const $CopyWithPlaceholder()
          ? _value.temporaryStorage
          // ignore: cast_nullable_to_non_nullable
          : temporaryStorage as String?,
      root: root == const $CopyWithPlaceholder()
          ? _value.root
          // ignore: cast_nullable_to_non_nullable
          : root as bool?,
      noPostingAllowed: noPostingAllowed == const $CopyWithPlaceholder()
          ? _value.noPostingAllowed
          // ignore: cast_nullable_to_non_nullable
          : noPostingAllowed as bool?,
      disableComments: disableComments == const $CopyWithPlaceholder()
          ? _value.disableComments
          // ignore: cast_nullable_to_non_nullable
          : disableComments as bool?,
      disableReplies: disableReplies == const $CopyWithPlaceholder()
          ? _value.disableReplies
          // ignore: cast_nullable_to_non_nullable
          : disableReplies as bool?,
      noPostingReason: noPostingReason == const $CopyWithPlaceholder()
          ? _value.noPostingReason
          // ignore: cast_nullable_to_non_nullable
          : noPostingReason as String?,
      commentDisableReason: commentDisableReason == const $CopyWithPlaceholder()
          ? _value.commentDisableReason
          // ignore: cast_nullable_to_non_nullable
          : commentDisableReason as String?,
      replyDisableReason: replyDisableReason == const $CopyWithPlaceholder()
          ? _value.replyDisableReason
          // ignore: cast_nullable_to_non_nullable
          : replyDisableReason as String?,
      roles: roles == const $CopyWithPlaceholder()
          ? _value.roles
          // ignore: cast_nullable_to_non_nullable
          : roles as Set<Role>?,
      sections: sections == const $CopyWithPlaceholder()
          ? _value.sections
          // ignore: cast_nullable_to_non_nullable
          : sections as Set<Section>?,
      posts: posts == const $CopyWithPlaceholder()
          ? _value.posts
          // ignore: cast_nullable_to_non_nullable
          : posts as Set<Post>?,
      relatedSections: relatedSections == const $CopyWithPlaceholder()
          ? _value.relatedSections
          // ignore: cast_nullable_to_non_nullable
          : relatedSections as Set<Section>?,
      relatedTags: relatedTags == const $CopyWithPlaceholder()
          ? _value.relatedTags
          // ignore: cast_nullable_to_non_nullable
          : relatedTags as Set<Tag>?,
      relatedStatistics: relatedStatistics == const $CopyWithPlaceholder()
          ? _value.relatedStatistics
          // ignore: cast_nullable_to_non_nullable
          : relatedStatistics as RelatedStatistics?,
    );
  }
}

extension $UserCopyWith on User {
  /// Returns a callable class that can be used as follows: `instanceOfUser.copyWith(...)` or like so:`instanceOfUser.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$UserCWProxy get copyWith => _$UserCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

User _$UserFromJson(Map<String, dynamic> json) => User(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      username: json['username'] as String,
      lastLoginTime: json['lastLoginTime'] as String,
      accountNonExpired: json['accountNonExpired'] as bool,
      credentialsNonExpired: json['credentialsNonExpired'] as bool,
      accountNonLocked: json['accountNonLocked'] as bool,
      enabled: json['enabled'] as bool,
      alias: json['alias'] as String?,
      avatar: json['avatar'] as String?,
      oneSentence: json['oneSentence'] as String?,
      email: json['email'] as String?,
      temporaryStorage: json['temporaryStorage'] as String?,
      root: json['root'] as bool?,
      noPostingAllowed: json['noPostingAllowed'] as bool?,
      disableComments: json['disableComments'] as bool?,
      disableReplies: json['disableReplies'] as bool?,
      noPostingReason: json['noPostingReason'] as String?,
      commentDisableReason: json['commentDisableReason'] as String?,
      replyDisableReason: json['replyDisableReason'] as String?,
      roles: (json['roles'] as List<dynamic>?)
          ?.map((e) => Role.fromJson(e as Map<String, dynamic>))
          .toSet(),
      sections: (json['sections'] as List<dynamic>?)
          ?.map((e) => Section.fromJson(e as Map<String, dynamic>))
          .toSet(),
      posts: (json['posts'] as List<dynamic>?)
          ?.map((e) => Post.fromJson(e as Map<String, dynamic>))
          .toSet(),
      relatedSections: (json['relatedSections'] as List<dynamic>?)
          ?.map((e) => Section.fromJson(e as Map<String, dynamic>))
          .toSet(),
      relatedTags: (json['relatedTags'] as List<dynamic>?)
          ?.map((e) => Tag.fromJson(e as Map<String, dynamic>))
          .toSet(),
      relatedStatistics: json['relatedStatistics'] == null
          ? null
          : RelatedStatistics.fromJson(
              json['relatedStatistics'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$UserToJson(User instance) {
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
  writeNotNull('alias', instance.alias);
  writeNotNull('avatar', instance.avatar);
  writeNotNull('oneSentence', instance.oneSentence);
  val['username'] = instance.username;
  writeNotNull('email', instance.email);
  writeNotNull('temporaryStorage', instance.temporaryStorage);
  val['lastLoginTime'] = instance.lastLoginTime;
  writeNotNull('root', instance.root);
  writeNotNull('noPostingAllowed', instance.noPostingAllowed);
  writeNotNull('disableComments', instance.disableComments);
  writeNotNull('disableReplies', instance.disableReplies);
  writeNotNull('noPostingReason', instance.noPostingReason);
  writeNotNull('commentDisableReason', instance.commentDisableReason);
  writeNotNull('replyDisableReason', instance.replyDisableReason);
  val['accountNonExpired'] = instance.accountNonExpired;
  val['credentialsNonExpired'] = instance.credentialsNonExpired;
  val['accountNonLocked'] = instance.accountNonLocked;
  val['enabled'] = instance.enabled;
  writeNotNull('roles', instance.roles?.toList());
  writeNotNull('sections', instance.sections?.toList());
  writeNotNull('posts', instance.posts?.toList());
  writeNotNull('relatedSections', instance.relatedSections?.toList());
  writeNotNull('relatedTags', instance.relatedTags?.toList());
  writeNotNull('relatedStatistics', instance.relatedStatistics);
  return val;
}
