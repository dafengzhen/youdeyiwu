// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'create_comment_dto.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$CreateCommentDtoCWProxy {
  CreateCommentDto content(String content);

  CreateCommentDto postId(String postId);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `CreateCommentDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// CreateCommentDto(...).copyWith(id: 12, name: "My name")
  /// ````
  CreateCommentDto call({
    String? content,
    String? postId,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfCreateCommentDto.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfCreateCommentDto.copyWith.fieldName(...)`
class _$CreateCommentDtoCWProxyImpl implements _$CreateCommentDtoCWProxy {
  const _$CreateCommentDtoCWProxyImpl(this._value);

  final CreateCommentDto _value;

  @override
  CreateCommentDto content(String content) => this(content: content);

  @override
  CreateCommentDto postId(String postId) => this(postId: postId);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `CreateCommentDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// CreateCommentDto(...).copyWith(id: 12, name: "My name")
  /// ````
  CreateCommentDto call({
    Object? content = const $CopyWithPlaceholder(),
    Object? postId = const $CopyWithPlaceholder(),
  }) {
    return CreateCommentDto(
      content: content == const $CopyWithPlaceholder() || content == null
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as String,
      postId: postId == const $CopyWithPlaceholder() || postId == null
          ? _value.postId
          // ignore: cast_nullable_to_non_nullable
          : postId as String,
    );
  }
}

extension $CreateCommentDtoCopyWith on CreateCommentDto {
  /// Returns a callable class that can be used as follows: `instanceOfCreateCommentDto.copyWith(...)` or like so:`instanceOfCreateCommentDto.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$CreateCommentDtoCWProxy get copyWith => _$CreateCommentDtoCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

CreateCommentDto _$CreateCommentDtoFromJson(Map<String, dynamic> json) =>
    CreateCommentDto(
      content: json['content'] as String,
      postId: json['postId'] as String,
    );

Map<String, dynamic> _$CreateCommentDtoToJson(CreateCommentDto instance) =>
    <String, dynamic>{
      'content': instance.content,
      'postId': instance.postId,
    };
