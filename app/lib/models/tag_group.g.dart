// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'tag_group.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$TagGroupCWProxy {
  TagGroup id(int id);

  TagGroup deleted(bool deleted);

  TagGroup createdBy(int? createdBy);

  TagGroup updatedBy(int? updatedBy);

  TagGroup createdOn(String? createdOn);

  TagGroup updatedOn(String? updatedOn);

  TagGroup name(String name);

  TagGroup sort(int sort);

  TagGroup tags(Set<Tag>? tags);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `TagGroup(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// TagGroup(...).copyWith(id: 12, name: "My name")
  /// ````
  TagGroup call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    int? sort,
    Set<Tag>? tags,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfTagGroup.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfTagGroup.copyWith.fieldName(...)`
class _$TagGroupCWProxyImpl implements _$TagGroupCWProxy {
  const _$TagGroupCWProxyImpl(this._value);

  final TagGroup _value;

  @override
  TagGroup id(int id) => this(id: id);

  @override
  TagGroup deleted(bool deleted) => this(deleted: deleted);

  @override
  TagGroup createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  TagGroup updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  TagGroup createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  TagGroup updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  TagGroup name(String name) => this(name: name);

  @override
  TagGroup sort(int sort) => this(sort: sort);

  @override
  TagGroup tags(Set<Tag>? tags) => this(tags: tags);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `TagGroup(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// TagGroup(...).copyWith(id: 12, name: "My name")
  /// ````
  TagGroup call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? tags = const $CopyWithPlaceholder(),
  }) {
    return TagGroup(
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
      tags: tags == const $CopyWithPlaceholder()
          ? _value.tags
          // ignore: cast_nullable_to_non_nullable
          : tags as Set<Tag>?,
    );
  }
}

extension $TagGroupCopyWith on TagGroup {
  /// Returns a callable class that can be used as follows: `instanceOfTagGroup.copyWith(...)` or like so:`instanceOfTagGroup.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$TagGroupCWProxy get copyWith => _$TagGroupCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

TagGroup _$TagGroupFromJson(Map<String, dynamic> json) => TagGroup(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      sort: (json['sort'] as num).toInt(),
      tags: (json['tags'] as List<dynamic>?)
          ?.map((e) => Tag.fromJson(e as Map<String, dynamic>))
          .toSet(),
    );

Map<String, dynamic> _$TagGroupToJson(TagGroup instance) {
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
  writeNotNull('tags', instance.tags?.toList());
  return val;
}
