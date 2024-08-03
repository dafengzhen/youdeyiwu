// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'user.dart';

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
