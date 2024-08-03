// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'query_parameters_dto.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$QueryParametersDtoCWProxy {
  QueryParametersDto id(String? id);

  QueryParametersDto tagId(String? tagId);

  QueryParametersDto sectionId(String? sectionId);

  QueryParametersDto postId(String? postId);

  QueryParametersDto tagGroupId(String? tagGroupId);

  QueryParametersDto sectionGroupId(String? sectionGroupId);

  QueryParametersDto page(String? page);

  QueryParametersDto size(String? size);

  QueryParametersDto sort(String? sort);

  QueryParametersDto name(String? name);

  QueryParametersDto sectionSecret(String? sectionSecret);

  QueryParametersDto postSecret(String? postSecret);

  QueryParametersDto secret(String? secret);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `QueryParametersDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// QueryParametersDto(...).copyWith(id: 12, name: "My name")
  /// ````
  QueryParametersDto call({
    String? id,
    String? tagId,
    String? sectionId,
    String? postId,
    String? tagGroupId,
    String? sectionGroupId,
    String? page,
    String? size,
    String? sort,
    String? name,
    String? sectionSecret,
    String? postSecret,
    String? secret,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfQueryParametersDto.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfQueryParametersDto.copyWith.fieldName(...)`
class _$QueryParametersDtoCWProxyImpl implements _$QueryParametersDtoCWProxy {
  const _$QueryParametersDtoCWProxyImpl(this._value);

  final QueryParametersDto _value;

  @override
  QueryParametersDto id(String? id) => this(id: id);

  @override
  QueryParametersDto tagId(String? tagId) => this(tagId: tagId);

  @override
  QueryParametersDto sectionId(String? sectionId) => this(sectionId: sectionId);

  @override
  QueryParametersDto postId(String? postId) => this(postId: postId);

  @override
  QueryParametersDto tagGroupId(String? tagGroupId) =>
      this(tagGroupId: tagGroupId);

  @override
  QueryParametersDto sectionGroupId(String? sectionGroupId) =>
      this(sectionGroupId: sectionGroupId);

  @override
  QueryParametersDto page(String? page) => this(page: page);

  @override
  QueryParametersDto size(String? size) => this(size: size);

  @override
  QueryParametersDto sort(String? sort) => this(sort: sort);

  @override
  QueryParametersDto name(String? name) => this(name: name);

  @override
  QueryParametersDto sectionSecret(String? sectionSecret) =>
      this(sectionSecret: sectionSecret);

  @override
  QueryParametersDto postSecret(String? postSecret) =>
      this(postSecret: postSecret);

  @override
  QueryParametersDto secret(String? secret) => this(secret: secret);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `QueryParametersDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// QueryParametersDto(...).copyWith(id: 12, name: "My name")
  /// ````
  QueryParametersDto call({
    Object? id = const $CopyWithPlaceholder(),
    Object? tagId = const $CopyWithPlaceholder(),
    Object? sectionId = const $CopyWithPlaceholder(),
    Object? postId = const $CopyWithPlaceholder(),
    Object? tagGroupId = const $CopyWithPlaceholder(),
    Object? sectionGroupId = const $CopyWithPlaceholder(),
    Object? page = const $CopyWithPlaceholder(),
    Object? size = const $CopyWithPlaceholder(),
    Object? sort = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? sectionSecret = const $CopyWithPlaceholder(),
    Object? postSecret = const $CopyWithPlaceholder(),
    Object? secret = const $CopyWithPlaceholder(),
  }) {
    return QueryParametersDto(
      id: id == const $CopyWithPlaceholder()
          ? _value.id
          // ignore: cast_nullable_to_non_nullable
          : id as String?,
      tagId: tagId == const $CopyWithPlaceholder()
          ? _value.tagId
          // ignore: cast_nullable_to_non_nullable
          : tagId as String?,
      sectionId: sectionId == const $CopyWithPlaceholder()
          ? _value.sectionId
          // ignore: cast_nullable_to_non_nullable
          : sectionId as String?,
      postId: postId == const $CopyWithPlaceholder()
          ? _value.postId
          // ignore: cast_nullable_to_non_nullable
          : postId as String?,
      tagGroupId: tagGroupId == const $CopyWithPlaceholder()
          ? _value.tagGroupId
          // ignore: cast_nullable_to_non_nullable
          : tagGroupId as String?,
      sectionGroupId: sectionGroupId == const $CopyWithPlaceholder()
          ? _value.sectionGroupId
          // ignore: cast_nullable_to_non_nullable
          : sectionGroupId as String?,
      page: page == const $CopyWithPlaceholder()
          ? _value.page
          // ignore: cast_nullable_to_non_nullable
          : page as String?,
      size: size == const $CopyWithPlaceholder()
          ? _value.size
          // ignore: cast_nullable_to_non_nullable
          : size as String?,
      sort: sort == const $CopyWithPlaceholder()
          ? _value.sort
          // ignore: cast_nullable_to_non_nullable
          : sort as String?,
      name: name == const $CopyWithPlaceholder()
          ? _value.name
          // ignore: cast_nullable_to_non_nullable
          : name as String?,
      sectionSecret: sectionSecret == const $CopyWithPlaceholder()
          ? _value.sectionSecret
          // ignore: cast_nullable_to_non_nullable
          : sectionSecret as String?,
      postSecret: postSecret == const $CopyWithPlaceholder()
          ? _value.postSecret
          // ignore: cast_nullable_to_non_nullable
          : postSecret as String?,
      secret: secret == const $CopyWithPlaceholder()
          ? _value.secret
          // ignore: cast_nullable_to_non_nullable
          : secret as String?,
    );
  }
}

extension $QueryParametersDtoCopyWith on QueryParametersDto {
  /// Returns a callable class that can be used as follows: `instanceOfQueryParametersDto.copyWith(...)` or like so:`instanceOfQueryParametersDto.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$QueryParametersDtoCWProxy get copyWith =>
      _$QueryParametersDtoCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

QueryParametersDto _$QueryParametersDtoFromJson(Map<String, dynamic> json) =>
    QueryParametersDto(
      id: json['id'] as String?,
      tagId: json['tagId'] as String?,
      sectionId: json['sectionId'] as String?,
      postId: json['postId'] as String?,
      tagGroupId: json['tagGroupId'] as String?,
      sectionGroupId: json['sectionGroupId'] as String?,
      page: json['page'] as String?,
      size: json['size'] as String?,
      sort: json['sort'] as String?,
      name: json['name'] as String?,
      sectionSecret: json['sectionSecret'] as String?,
      postSecret: json['postSecret'] as String?,
      secret: json['secret'] as String?,
    );

Map<String, dynamic> _$QueryParametersDtoToJson(QueryParametersDto instance) {
  final val = <String, dynamic>{};

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('id', instance.id);
  writeNotNull('tagId', instance.tagId);
  writeNotNull('sectionId', instance.sectionId);
  writeNotNull('postId', instance.postId);
  writeNotNull('tagGroupId', instance.tagGroupId);
  writeNotNull('sectionGroupId', instance.sectionGroupId);
  writeNotNull('page', instance.page);
  writeNotNull('size', instance.size);
  writeNotNull('sort', instance.sort);
  writeNotNull('name', instance.name);
  writeNotNull('sectionSecret', instance.sectionSecret);
  writeNotNull('postSecret', instance.postSecret);
  writeNotNull('secret', instance.secret);
  return val;
}
