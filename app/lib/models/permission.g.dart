// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'permission.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PermissionCWProxy {
  Permission id(int id);

  Permission deleted(bool deleted);

  Permission createdBy(int? createdBy);

  Permission updatedBy(int? updatedBy);

  Permission createdOn(String? createdOn);

  Permission updatedOn(String? updatedOn);

  Permission name(String name);

  Permission method(MethodTypeEnum method);

  Permission type(MatcherTypeEnum type);

  Permission caseInsensitive(bool caseInsensitive);

  Permission sort(int sort);

  Permission matchers(Set<Permission> matchers);

  Permission alias(String? alias);

  Permission overview(String? overview);

  Permission matcher(Permission? matcher);

  Permission role(Role? role);

  Permission roles(Set<Role>? roles);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Permission(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Permission(...).copyWith(id: 12, name: "My name")
  /// ````
  Permission call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    MethodTypeEnum? method,
    MatcherTypeEnum? type,
    bool? caseInsensitive,
    int? sort,
    Set<Permission>? matchers,
    String? alias,
    String? overview,
    Permission? matcher,
    Role? role,
    Set<Role>? roles,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPermission.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPermission.copyWith.fieldName(...)`
class _$PermissionCWProxyImpl implements _$PermissionCWProxy {
  const _$PermissionCWProxyImpl(this._value);

  final Permission _value;

  @override
  Permission id(int id) => this(id: id);

  @override
  Permission deleted(bool deleted) => this(deleted: deleted);

  @override
  Permission createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Permission updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Permission createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Permission updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Permission name(String name) => this(name: name);

  @override
  Permission method(MethodTypeEnum method) => this(method: method);

  @override
  Permission type(MatcherTypeEnum type) => this(type: type);

  @override
  Permission caseInsensitive(bool caseInsensitive) =>
      this(caseInsensitive: caseInsensitive);

  @override
  Permission sort(int sort) => this(sort: sort);

  @override
  Permission matchers(Set<Permission> matchers) => this(matchers: matchers);

  @override
  Permission alias(String? alias) => this(alias: alias);

  @override
  Permission overview(String? overview) => this(overview: overview);

  @override
  Permission matcher(Permission? matcher) => this(matcher: matcher);

  @override
  Permission role(Role? role) => this(role: role);

  @override
  Permission roles(Set<Role>? roles) => this(roles: roles);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Permission(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Permission(...).copyWith(id: 12, name: "My name")
  /// ````
  Permission call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? method = const $CopyWithPlaceholder(),
    Object? type = const $CopyWithPlaceholder(),
    Object? caseInsensitive = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? matchers = const $CopyWithPlaceholder(),
    Object? alias = const $CopyWithPlaceholder(),
    Object? overview = const $CopyWithPlaceholder(),
    Object? matcher = const $CopyWithPlaceholder(),
    Object? role = const $CopyWithPlaceholder(),
    Object? roles = const $CopyWithPlaceholder(),
  }) {
    return Permission(
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
      method: method == const $CopyWithPlaceholder() || method == null
          ? _value.method
          // ignore: cast_nullable_to_non_nullable
          : method as MethodTypeEnum,
      type: type == const $CopyWithPlaceholder() || type == null
          ? _value.type
          // ignore: cast_nullable_to_non_nullable
          : type as MatcherTypeEnum,
      caseInsensitive: caseInsensitive == const $CopyWithPlaceholder() ||
              caseInsensitive == null
          ? _value.caseInsensitive
          // ignore: cast_nullable_to_non_nullable
          : caseInsensitive as bool,
      sort: sort == const $CopyWithPlaceholder() || sort == null
          ? _value.sort
          // ignore: cast_nullable_to_non_nullable
          : sort as int,
      matchers: matchers == const $CopyWithPlaceholder() || matchers == null
          ? _value.matchers
          // ignore: cast_nullable_to_non_nullable
          : matchers as Set<Permission>,
      alias: alias == const $CopyWithPlaceholder()
          ? _value.alias
          // ignore: cast_nullable_to_non_nullable
          : alias as String?,
      overview: overview == const $CopyWithPlaceholder()
          ? _value.overview
          // ignore: cast_nullable_to_non_nullable
          : overview as String?,
      matcher: matcher == const $CopyWithPlaceholder()
          ? _value.matcher
          // ignore: cast_nullable_to_non_nullable
          : matcher as Permission?,
      role: role == const $CopyWithPlaceholder()
          ? _value.role
          // ignore: cast_nullable_to_non_nullable
          : role as Role?,
      roles: roles == const $CopyWithPlaceholder()
          ? _value.roles
          // ignore: cast_nullable_to_non_nullable
          : roles as Set<Role>?,
    );
  }
}

extension $PermissionCopyWith on Permission {
  /// Returns a callable class that can be used as follows: `instanceOfPermission.copyWith(...)` or like so:`instanceOfPermission.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PermissionCWProxy get copyWith => _$PermissionCWProxyImpl(this);
}

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
