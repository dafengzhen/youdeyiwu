// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'tag.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$TagCWProxy {
  Tag id(int id);

  Tag deleted(bool deleted);

  Tag createdBy(int? createdBy);

  Tag updatedBy(int? updatedBy);

  Tag createdOn(String? createdOn);

  Tag updatedOn(String? updatedOn);

  Tag name(String name);

  Tag sort(int sort);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Tag(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Tag(...).copyWith(id: 12, name: "My name")
  /// ````
  Tag call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    int? sort,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfTag.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfTag.copyWith.fieldName(...)`
class _$TagCWProxyImpl implements _$TagCWProxy {
  const _$TagCWProxyImpl(this._value);

  final Tag _value;

  @override
  Tag id(int id) => this(id: id);

  @override
  Tag deleted(bool deleted) => this(deleted: deleted);

  @override
  Tag createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Tag updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Tag createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Tag updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Tag name(String name) => this(name: name);

  @override
  Tag sort(int sort) => this(sort: sort);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Tag(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Tag(...).copyWith(id: 12, name: "My name")
  /// ````
  Tag call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
  }) {
    return Tag(
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
    );
  }
}

extension $TagCopyWith on Tag {
  /// Returns a callable class that can be used as follows: `instanceOfTag.copyWith(...)` or like so:`instanceOfTag.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$TagCWProxy get copyWith => _$TagCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Tag _$TagFromJson(Map<String, dynamic> json) => Tag(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      sort: (json['sort'] as num).toInt(),
    );

Map<String, dynamic> _$TagToJson(Tag instance) {
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
  return val;
}
