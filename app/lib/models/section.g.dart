// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'section.dart';

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
