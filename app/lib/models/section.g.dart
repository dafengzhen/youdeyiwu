// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'section.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$SectionCWProxy {
  Section id(int id);

  Section deleted(bool deleted);

  Section createdBy(int? createdBy);

  Section updatedBy(int? updatedBy);

  Section createdOn(String? createdOn);

  Section updatedOn(String? updatedOn);

  Section name(String name);

  Section sort(int sort);

  Section states(Set<SectionStateEnum> states);

  Section accessKey(String? accessKey);

  Section accessPoints(int accessPoints);

  Section admins(Set<User>? admins);

  Section tagGroups(Set<TagGroup>? tagGroups);

  Section tags(Set<Tag>? tags);

  Section sectionGroups(Set<SectionGroup>? sectionGroups);

  Section cover(String? cover);

  Section coverImage(List<int>? coverImage);

  Section coverImageType(FileTypeEnum? coverImageType);

  Section overview(String? overview);

  Section content(String? content);

  Section allows(Set<User>? allows);

  Section blocks(Set<User>? blocks);

  Section createPostGuide(String? createPostGuide);

  Section user(User? user);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Section(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Section(...).copyWith(id: 12, name: "My name")
  /// ````
  Section call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    int? sort,
    Set<SectionStateEnum>? states,
    String? accessKey,
    int? accessPoints,
    Set<User>? admins,
    Set<TagGroup>? tagGroups,
    Set<Tag>? tags,
    Set<SectionGroup>? sectionGroups,
    String? cover,
    List<int>? coverImage,
    FileTypeEnum? coverImageType,
    String? overview,
    String? content,
    Set<User>? allows,
    Set<User>? blocks,
    String? createPostGuide,
    User? user,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfSection.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfSection.copyWith.fieldName(...)`
class _$SectionCWProxyImpl implements _$SectionCWProxy {
  const _$SectionCWProxyImpl(this._value);

  final Section _value;

  @override
  Section id(int id) => this(id: id);

  @override
  Section deleted(bool deleted) => this(deleted: deleted);

  @override
  Section createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Section updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Section createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Section updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Section name(String name) => this(name: name);

  @override
  Section sort(int sort) => this(sort: sort);

  @override
  Section states(Set<SectionStateEnum> states) => this(states: states);

  @override
  Section accessKey(String? accessKey) => this(accessKey: accessKey);

  @override
  Section accessPoints(int accessPoints) => this(accessPoints: accessPoints);

  @override
  Section admins(Set<User>? admins) => this(admins: admins);

  @override
  Section tagGroups(Set<TagGroup>? tagGroups) => this(tagGroups: tagGroups);

  @override
  Section tags(Set<Tag>? tags) => this(tags: tags);

  @override
  Section sectionGroups(Set<SectionGroup>? sectionGroups) =>
      this(sectionGroups: sectionGroups);

  @override
  Section cover(String? cover) => this(cover: cover);

  @override
  Section coverImage(List<int>? coverImage) => this(coverImage: coverImage);

  @override
  Section coverImageType(FileTypeEnum? coverImageType) =>
      this(coverImageType: coverImageType);

  @override
  Section overview(String? overview) => this(overview: overview);

  @override
  Section content(String? content) => this(content: content);

  @override
  Section allows(Set<User>? allows) => this(allows: allows);

  @override
  Section blocks(Set<User>? blocks) => this(blocks: blocks);

  @override
  Section createPostGuide(String? createPostGuide) =>
      this(createPostGuide: createPostGuide);

  @override
  Section user(User? user) => this(user: user);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Section(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Section(...).copyWith(id: 12, name: "My name")
  /// ````
  Section call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? states = const $CopyWithPlaceholder(),
    Object? accessKey = const $CopyWithPlaceholder(),
    Object? accessPoints = const $CopyWithPlaceholder(),
    Object? admins = const $CopyWithPlaceholder(),
    Object? tagGroups = const $CopyWithPlaceholder(),
    Object? tags = const $CopyWithPlaceholder(),
    Object? sectionGroups = const $CopyWithPlaceholder(),
    Object? cover = const $CopyWithPlaceholder(),
    Object? coverImage = const $CopyWithPlaceholder(),
    Object? coverImageType = const $CopyWithPlaceholder(),
    Object? overview = const $CopyWithPlaceholder(),
    Object? content = const $CopyWithPlaceholder(),
    Object? allows = const $CopyWithPlaceholder(),
    Object? blocks = const $CopyWithPlaceholder(),
    Object? createPostGuide = const $CopyWithPlaceholder(),
    Object? user = const $CopyWithPlaceholder(),
  }) {
    return Section(
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
      sort: sort == const $CopyWithPlaceholder() || sort == null
          ? _value.sort
          // ignore: cast_nullable_to_non_nullable
          : sort as int,
      states: states == const $CopyWithPlaceholder() || states == null
          ? _value.states
          // ignore: cast_nullable_to_non_nullable
          : states as Set<SectionStateEnum>,
      accessKey: accessKey == const $CopyWithPlaceholder()
          ? _value.accessKey
          // ignore: cast_nullable_to_non_nullable
          : accessKey as String?,
      accessPoints:
          accessPoints == const $CopyWithPlaceholder() || accessPoints == null
              ? _value.accessPoints
              // ignore: cast_nullable_to_non_nullable
              : accessPoints as int,
      admins: admins == const $CopyWithPlaceholder()
          ? _value.admins
          // ignore: cast_nullable_to_non_nullable
          : admins as Set<User>?,
      tagGroups: tagGroups == const $CopyWithPlaceholder()
          ? _value.tagGroups
          // ignore: cast_nullable_to_non_nullable
          : tagGroups as Set<TagGroup>?,
      tags: tags == const $CopyWithPlaceholder()
          ? _value.tags
          // ignore: cast_nullable_to_non_nullable
          : tags as Set<Tag>?,
      sectionGroups: sectionGroups == const $CopyWithPlaceholder()
          ? _value.sectionGroups
          // ignore: cast_nullable_to_non_nullable
          : sectionGroups as Set<SectionGroup>?,
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
      allows: allows == const $CopyWithPlaceholder()
          ? _value.allows
          // ignore: cast_nullable_to_non_nullable
          : allows as Set<User>?,
      blocks: blocks == const $CopyWithPlaceholder()
          ? _value.blocks
          // ignore: cast_nullable_to_non_nullable
          : blocks as Set<User>?,
      createPostGuide: createPostGuide == const $CopyWithPlaceholder()
          ? _value.createPostGuide
          // ignore: cast_nullable_to_non_nullable
          : createPostGuide as String?,
      user: user == const $CopyWithPlaceholder()
          ? _value.user
          // ignore: cast_nullable_to_non_nullable
          : user as User?,
    );
  }
}

extension $SectionCopyWith on Section {
  /// Returns a callable class that can be used as follows: `instanceOfSection.copyWith(...)` or like so:`instanceOfSection.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$SectionCWProxy get copyWith => _$SectionCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Section _$SectionFromJson(Map<String, dynamic> json) => Section(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      sort: (json['sort'] as num).toInt(),
      states: (json['states'] as List<dynamic>)
          .map((e) => $enumDecode(_$SectionStateEnumEnumMap, e))
          .toSet(),
      accessKey: json['accessKey'] as String?,
      accessPoints: (json['accessPoints'] as num).toInt(),
      admins: (json['admins'] as List<dynamic>?)
          ?.map((e) => User.fromJson(e as Map<String, dynamic>))
          .toSet(),
      tagGroups: (json['tagGroups'] as List<dynamic>?)
          ?.map((e) => TagGroup.fromJson(e as Map<String, dynamic>))
          .toSet(),
      tags: (json['tags'] as List<dynamic>?)
          ?.map((e) => Tag.fromJson(e as Map<String, dynamic>))
          .toSet(),
      sectionGroups: (json['sectionGroups'] as List<dynamic>?)
          ?.map((e) => SectionGroup.fromJson(e as Map<String, dynamic>))
          .toSet(),
      cover: json['cover'] as String?,
      coverImage: (json['coverImage'] as List<dynamic>?)
          ?.map((e) => (e as num).toInt())
          .toList(),
      coverImageType:
          $enumDecodeNullable(_$FileTypeEnumEnumMap, json['coverImageType']),
      overview: json['overview'] as String?,
      content: json['content'] as String?,
      allows: (json['allows'] as List<dynamic>?)
          ?.map((e) => User.fromJson(e as Map<String, dynamic>))
          .toSet(),
      blocks: (json['blocks'] as List<dynamic>?)
          ?.map((e) => User.fromJson(e as Map<String, dynamic>))
          .toSet(),
      createPostGuide: json['createPostGuide'] as String?,
      user: json['user'] == null
          ? null
          : User.fromJson(json['user'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$SectionToJson(Section instance) {
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
  writeNotNull('createPostGuide', instance.createPostGuide);
  val['sort'] = instance.sort;
  val['states'] =
      instance.states.map((e) => _$SectionStateEnumEnumMap[e]!).toList();
  writeNotNull('admins', instance.admins?.toList());
  writeNotNull('allows', instance.allows?.toList());
  writeNotNull('blocks', instance.blocks?.toList());
  writeNotNull('accessKey', instance.accessKey);
  val['accessPoints'] = instance.accessPoints;
  writeNotNull('tagGroups', instance.tagGroups?.toList());
  writeNotNull('tags', instance.tags?.toList());
  writeNotNull('sectionGroups', instance.sectionGroups?.toList());
  writeNotNull('user', instance.user);
  return val;
}

const _$SectionStateEnumEnumMap = {
  SectionStateEnum.show: 'SHOW',
  SectionStateEnum.hide: 'HIDE',
  SectionStateEnum.lock: 'LOCK',
  SectionStateEnum.allow: 'ALLOW',
  SectionStateEnum.block: 'BLOCK',
  SectionStateEnum.visibleAfterLogin: 'VISIBLE_AFTER_LOGIN',
};

const _$FileTypeEnumEnumMap = {
  FileTypeEnum.png: 'PNG',
  FileTypeEnum.jpg: 'JPG',
};
