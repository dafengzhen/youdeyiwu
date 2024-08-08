// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'role.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$RoleCWProxy {
  Role id(int id);

  Role deleted(bool deleted);

  Role createdBy(int? createdBy);

  Role updatedBy(int? updatedBy);

  Role createdOn(String? createdOn);

  Role updatedOn(String? updatedOn);

  Role name(String name);

  Role sort(int sort);

  Role display(bool display);

  Role overview(String? overview);

  Role permissions(Set<Permission>? permissions);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Role(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Role(...).copyWith(id: 12, name: "My name")
  /// ````
  Role call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    int? sort,
    bool? display,
    String? overview,
    Set<Permission>? permissions,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfRole.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfRole.copyWith.fieldName(...)`
class _$RoleCWProxyImpl implements _$RoleCWProxy {
  const _$RoleCWProxyImpl(this._value);

  final Role _value;

  @override
  Role id(int id) => this(id: id);

  @override
  Role deleted(bool deleted) => this(deleted: deleted);

  @override
  Role createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Role updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Role createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Role updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Role name(String name) => this(name: name);

  @override
  Role sort(int sort) => this(sort: sort);

  @override
  Role display(bool display) => this(display: display);

  @override
  Role overview(String? overview) => this(overview: overview);

  @override
  Role permissions(Set<Permission>? permissions) =>
      this(permissions: permissions);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Role(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Role(...).copyWith(id: 12, name: "My name")
  /// ````
  Role call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? display = const $CopyWithPlaceholder(),
    Object? overview = const $CopyWithPlaceholder(),
    Object? permissions = const $CopyWithPlaceholder(),
  }) {
    return Role(
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
      display: display == const $CopyWithPlaceholder() || display == null
          ? _value.display
          // ignore: cast_nullable_to_non_nullable
          : display as bool,
      overview: overview == const $CopyWithPlaceholder()
          ? _value.overview
          // ignore: cast_nullable_to_non_nullable
          : overview as String?,
      permissions: permissions == const $CopyWithPlaceholder()
          ? _value.permissions
          // ignore: cast_nullable_to_non_nullable
          : permissions as Set<Permission>?,
    );
  }
}

extension $RoleCopyWith on Role {
  /// Returns a callable class that can be used as follows: `instanceOfRole.copyWith(...)` or like so:`instanceOfRole.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$RoleCWProxy get copyWith => _$RoleCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Role _$RoleFromJson(Map<String, dynamic> json) => Role(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      sort: (json['sort'] as num).toInt(),
      display: json['display'] as bool,
      overview: json['overview'] as String?,
      permissions: (json['permissions'] as List<dynamic>?)
          ?.map((e) => Permission.fromJson(e as Map<String, dynamic>))
          .toSet(),
    );

Map<String, dynamic> _$RoleToJson(Role instance) {
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
  writeNotNull('overview', instance.overview);
  val['sort'] = instance.sort;
  val['display'] = instance.display;
  writeNotNull('permissions', instance.permissions?.toList());
  return val;
}
