// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'file.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

File _$FileFromJson(Map<String, dynamic> json) => File(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      url: json['url'] as String,
      name: json['name'] as String,
      originalName: json['originalName'] as String,
      fileCategory:
          $enumDecode(_$FileCategoryEnumEnumMap, json['fileCategory']),
      storageServiceType: $enumDecode(
          _$StorageServiceTypeEnumEnumMap, json['storageServiceType']),
      businessType:
          $enumDecode(_$BusinessTypeEnumEnumMap, json['businessType']),
      contentType: json['contentType'] as String,
      mediaType: json['mediaType'] as String,
      size: (json['size'] as num).toInt(),
      viewCount: (json['viewCount'] as num).toInt(),
      digest: json['digest'] as String,
      urls: json['urls'] == null
          ? null
          : FileUrls.fromJson(json['urls'] as Map<String, dynamic>),
      overview: json['overview'] as String?,
      bucketName: json['bucketName'] as String?,
      objectName: json['objectName'] as String?,
      objectKey: json['objectKey'] as String?,
      user: json['user'] == null
          ? null
          : User.fromJson(json['user'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$FileToJson(File instance) {
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
  val['url'] = instance.url;
  writeNotNull('urls', instance.urls);
  val['name'] = instance.name;
  val['originalName'] = instance.originalName;
  writeNotNull('overview', instance.overview);
  val['fileCategory'] = _$FileCategoryEnumEnumMap[instance.fileCategory]!;
  val['storageServiceType'] =
      _$StorageServiceTypeEnumEnumMap[instance.storageServiceType]!;
  val['businessType'] = _$BusinessTypeEnumEnumMap[instance.businessType]!;
  val['contentType'] = instance.contentType;
  val['mediaType'] = instance.mediaType;
  val['size'] = instance.size;
  writeNotNull('bucketName', instance.bucketName);
  writeNotNull('objectName', instance.objectName);
  val['viewCount'] = instance.viewCount;
  val['digest'] = instance.digest;
  writeNotNull('objectKey', instance.objectKey);
  writeNotNull('user', instance.user);
  return val;
}

const _$FileCategoryEnumEnumMap = {
  FileCategoryEnum.image: 'IMAGE',
  FileCategoryEnum.zip: 'ZIP',
  FileCategoryEnum.text: 'TEXT',
};

const _$StorageServiceTypeEnumEnumMap = {
  StorageServiceTypeEnum.db: 'DB',
  StorageServiceTypeEnum.oss: 'OSS',
};

const _$BusinessTypeEnumEnumMap = {
  BusinessTypeEnum.system: 'SYSTEM',
  BusinessTypeEnum.section: 'SECTION',
  BusinessTypeEnum.post: 'POST',
  BusinessTypeEnum.tag: 'TAG',
  BusinessTypeEnum.comment: 'COMMENT',
  BusinessTypeEnum.reply: 'REPLY',
  BusinessTypeEnum.message: 'MESSAGE',
  BusinessTypeEnum.user: 'USER',
  BusinessTypeEnum.other: 'OTHER',
};
