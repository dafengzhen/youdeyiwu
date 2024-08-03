// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post_badge.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

PostBadge _$PostBadgeFromJson(Map<String, dynamic> json) => PostBadge(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      sort: (json['sort'] as num).toInt(),
      styles: json['styles'] as String?,
      classes: json['classes'] as String?,
    );

Map<String, dynamic> _$PostBadgeToJson(PostBadge instance) {
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
  val['sort'] = instance.sort;
  writeNotNull('styles', instance.styles);
  writeNotNull('classes', instance.classes);
  return val;
}
