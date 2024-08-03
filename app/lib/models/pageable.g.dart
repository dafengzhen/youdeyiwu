// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'pageable.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Pageable _$PageableFromJson(Map<String, dynamic> json) => Pageable(
      page: (json['page'] as num).toInt(),
      size: (json['size'] as num).toInt(),
      previous: json['previous'] as bool,
      next: json['next'] as bool,
      pages: (json['pages'] as num).toInt(),
      elements: (json['elements'] as num?)?.toInt(),
    );

Map<String, dynamic> _$PageableToJson(Pageable instance) {
  final val = <String, dynamic>{
    'page': instance.page,
    'size': instance.size,
    'previous': instance.previous,
    'next': instance.next,
    'pages': instance.pages,
  };

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('elements', instance.elements);
  return val;
}
