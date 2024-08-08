// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'section_group.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$SectionGroupCWProxy {
  SectionGroup id(int id);

  SectionGroup deleted(bool deleted);

  SectionGroup createdBy(int? createdBy);

  SectionGroup updatedBy(int? updatedBy);

  SectionGroup createdOn(String? createdOn);

  SectionGroup updatedOn(String? updatedOn);

  SectionGroup name(String name);

  SectionGroup sort(int sort);

  SectionGroup sections(Set<Section>? sections);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `SectionGroup(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// SectionGroup(...).copyWith(id: 12, name: "My name")
  /// ````
  SectionGroup call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    int? sort,
    Set<Section>? sections,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfSectionGroup.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfSectionGroup.copyWith.fieldName(...)`
class _$SectionGroupCWProxyImpl implements _$SectionGroupCWProxy {
  const _$SectionGroupCWProxyImpl(this._value);

  final SectionGroup _value;

  @override
  SectionGroup id(int id) => this(id: id);

  @override
  SectionGroup deleted(bool deleted) => this(deleted: deleted);

  @override
  SectionGroup createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  SectionGroup updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  SectionGroup createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  SectionGroup updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  SectionGroup name(String name) => this(name: name);

  @override
  SectionGroup sort(int sort) => this(sort: sort);

  @override
  SectionGroup sections(Set<Section>? sections) => this(sections: sections);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `SectionGroup(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// SectionGroup(...).copyWith(id: 12, name: "My name")
  /// ````
  SectionGroup call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? sections = const $CopyWithPlaceholder(),
  }) {
    return SectionGroup(
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
      sections: sections == const $CopyWithPlaceholder()
          ? _value.sections
          // ignore: cast_nullable_to_non_nullable
          : sections as Set<Section>?,
    );
  }
}

extension $SectionGroupCopyWith on SectionGroup {
  /// Returns a callable class that can be used as follows: `instanceOfSectionGroup.copyWith(...)` or like so:`instanceOfSectionGroup.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$SectionGroupCWProxy get copyWith => _$SectionGroupCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

SectionGroup _$SectionGroupFromJson(Map<String, dynamic> json) => SectionGroup(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      sort: (json['sort'] as num).toInt(),
      sections: (json['sections'] as List<dynamic>?)
          ?.map((e) => Section.fromJson(e as Map<String, dynamic>))
          .toSet(),
    );

Map<String, dynamic> _$SectionGroupToJson(SectionGroup instance) {
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
  writeNotNull('sections', instance.sections?.toList());
  return val;
}
