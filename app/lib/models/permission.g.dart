// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'permission.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Permission _$PermissionFromJson(Map<String, dynamic> json) => Permission(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      method: $enumDecode(_$MethodTypeEnumEnumMap, json['method']),
      type: $enumDecode(_$MatcherTypeEnumEnumMap, json['type']),
      caseInsensitive: json['caseInsensitive'] as bool,
      sort: (json['sort'] as num).toInt(),
      matchers: (json['matchers'] as List<dynamic>)
          .map((e) => Permission.fromJson(e as Map<String, dynamic>))
          .toSet(),
      alias: json['alias'] as String?,
      overview: json['overview'] as String?,
      matcher: json['matcher'] == null
          ? null
          : Permission.fromJson(json['matcher'] as Map<String, dynamic>),
      role: json['role'] == null
          ? null
          : Role.fromJson(json['role'] as Map<String, dynamic>),
      roles: (json['roles'] as List<dynamic>?)
          ?.map((e) => Role.fromJson(e as Map<String, dynamic>))
          .toSet(),
    );

Map<String, dynamic> _$PermissionToJson(Permission instance) {
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
  writeNotNull('alias', instance.alias);
  writeNotNull('overview', instance.overview);
  val['method'] = _$MethodTypeEnumEnumMap[instance.method]!;
  val['type'] = _$MatcherTypeEnumEnumMap[instance.type]!;
  val['caseInsensitive'] = instance.caseInsensitive;
  val['sort'] = instance.sort;
  writeNotNull('matcher', instance.matcher);
  val['matchers'] = instance.matchers.toList();
  writeNotNull('role', instance.role);
  writeNotNull('roles', instance.roles?.toList());
  return val;
}

const _$MethodTypeEnumEnumMap = {
  MethodTypeEnum.get: 'GET',
  MethodTypeEnum.head: 'HEAD',
  MethodTypeEnum.post: 'POST',
  MethodTypeEnum.put: 'PUT',
  MethodTypeEnum.patch: 'PATCH',
  MethodTypeEnum.delete: 'DELETE',
  MethodTypeEnum.options: 'OPTIONS',
  MethodTypeEnum.trace: 'TRACE',
};

const _$MatcherTypeEnumEnumMap = {
  MatcherTypeEnum.ant: 'ANT',
  MatcherTypeEnum.regex: 'REGEX',
};
