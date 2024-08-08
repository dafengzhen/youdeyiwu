// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post_badge.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PostBadgeCWProxy {
  PostBadge id(int id);

  PostBadge deleted(bool deleted);

  PostBadge createdBy(int? createdBy);

  PostBadge updatedBy(int? updatedBy);

  PostBadge createdOn(String? createdOn);

  PostBadge updatedOn(String? updatedOn);

  PostBadge name(String name);

  PostBadge sort(int sort);

  PostBadge styles(String? styles);

  PostBadge classes(String? classes);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `PostBadge(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// PostBadge(...).copyWith(id: 12, name: "My name")
  /// ````
  PostBadge call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    int? sort,
    String? styles,
    String? classes,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPostBadge.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPostBadge.copyWith.fieldName(...)`
class _$PostBadgeCWProxyImpl implements _$PostBadgeCWProxy {
  const _$PostBadgeCWProxyImpl(this._value);

  final PostBadge _value;

  @override
  PostBadge id(int id) => this(id: id);

  @override
  PostBadge deleted(bool deleted) => this(deleted: deleted);

  @override
  PostBadge createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  PostBadge updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  PostBadge createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  PostBadge updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  PostBadge name(String name) => this(name: name);

  @override
  PostBadge sort(int sort) => this(sort: sort);

  @override
  PostBadge styles(String? styles) => this(styles: styles);

  @override
  PostBadge classes(String? classes) => this(classes: classes);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `PostBadge(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// PostBadge(...).copyWith(id: 12, name: "My name")
  /// ````
  PostBadge call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? styles = const $CopyWithPlaceholder(),
    Object? classes = const $CopyWithPlaceholder(),
  }) {
    return PostBadge(
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
      styles: styles == const $CopyWithPlaceholder()
          ? _value.styles
          // ignore: cast_nullable_to_non_nullable
          : styles as String?,
      classes: classes == const $CopyWithPlaceholder()
          ? _value.classes
          // ignore: cast_nullable_to_non_nullable
          : classes as String?,
    );
  }
}

extension $PostBadgeCopyWith on PostBadge {
  /// Returns a callable class that can be used as follows: `instanceOfPostBadge.copyWith(...)` or like so:`instanceOfPostBadge.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PostBadgeCWProxy get copyWith => _$PostBadgeCWProxyImpl(this);
}

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
