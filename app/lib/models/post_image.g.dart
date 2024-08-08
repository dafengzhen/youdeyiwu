// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post_image.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PostImageCWProxy {
  PostImage id(int id);

  PostImage deleted(bool deleted);

  PostImage createdBy(int? createdBy);

  PostImage updatedBy(int? updatedBy);

  PostImage createdOn(String? createdOn);

  PostImage updatedOn(String? updatedOn);

  PostImage url(String url);

  PostImage sort(int sort);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `PostImage(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// PostImage(...).copyWith(id: 12, name: "My name")
  /// ````
  PostImage call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? url,
    int? sort,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPostImage.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPostImage.copyWith.fieldName(...)`
class _$PostImageCWProxyImpl implements _$PostImageCWProxy {
  const _$PostImageCWProxyImpl(this._value);

  final PostImage _value;

  @override
  PostImage id(int id) => this(id: id);

  @override
  PostImage deleted(bool deleted) => this(deleted: deleted);

  @override
  PostImage createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  PostImage updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  PostImage createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  PostImage updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  PostImage url(String url) => this(url: url);

  @override
  PostImage sort(int sort) => this(sort: sort);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `PostImage(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// PostImage(...).copyWith(id: 12, name: "My name")
  /// ````
  PostImage call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? url = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
  }) {
    return PostImage(
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
      url: url == const $CopyWithPlaceholder() || url == null
          ? _value.url
          // ignore: cast_nullable_to_non_nullable
          : url as String,
      sort: sort == const $CopyWithPlaceholder() || sort == null
          ? _value.sort
          // ignore: cast_nullable_to_non_nullable
          : sort as int,
    );
  }
}

extension $PostImageCopyWith on PostImage {
  /// Returns a callable class that can be used as follows: `instanceOfPostImage.copyWith(...)` or like so:`instanceOfPostImage.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PostImageCWProxy get copyWith => _$PostImageCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

PostImage _$PostImageFromJson(Map<String, dynamic> json) => PostImage(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      url: json['url'] as String,
      sort: (json['sort'] as num).toInt(),
    );

Map<String, dynamic> _$PostImageToJson(PostImage instance) {
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
  val['sort'] = instance.sort;
  return val;
}
