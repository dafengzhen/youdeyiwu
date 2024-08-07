// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'create_reply_dto.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$CreateReplyDtoCWProxy {
  CreateReplyDto content(String content);

  CreateReplyDto postId(String postId);

  CreateReplyDto commentId(String? commentId);

  CreateReplyDto replyId(String? replyId);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `CreateReplyDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// CreateReplyDto(...).copyWith(id: 12, name: "My name")
  /// ````
  CreateReplyDto call({
    String? content,
    String? postId,
    String? commentId,
    String? replyId,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfCreateReplyDto.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfCreateReplyDto.copyWith.fieldName(...)`
class _$CreateReplyDtoCWProxyImpl implements _$CreateReplyDtoCWProxy {
  const _$CreateReplyDtoCWProxyImpl(this._value);

  final CreateReplyDto _value;

  @override
  CreateReplyDto content(String content) => this(content: content);

  @override
  CreateReplyDto postId(String postId) => this(postId: postId);

  @override
  CreateReplyDto commentId(String? commentId) => this(commentId: commentId);

  @override
  CreateReplyDto replyId(String? replyId) => this(replyId: replyId);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `CreateReplyDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// CreateReplyDto(...).copyWith(id: 12, name: "My name")
  /// ````
  CreateReplyDto call({
    Object? content = const $CopyWithPlaceholder(),
    Object? postId = const $CopyWithPlaceholder(),
    Object? commentId = const $CopyWithPlaceholder(),
    Object? replyId = const $CopyWithPlaceholder(),
  }) {
    return CreateReplyDto(
      content: content == const $CopyWithPlaceholder() || content == null
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as String,
      postId: postId == const $CopyWithPlaceholder() || postId == null
          ? _value.postId
          // ignore: cast_nullable_to_non_nullable
          : postId as String,
      commentId: commentId == const $CopyWithPlaceholder()
          ? _value.commentId
          // ignore: cast_nullable_to_non_nullable
          : commentId as String?,
      replyId: replyId == const $CopyWithPlaceholder()
          ? _value.replyId
          // ignore: cast_nullable_to_non_nullable
          : replyId as String?,
    );
  }
}

extension $CreateReplyDtoCopyWith on CreateReplyDto {
  /// Returns a callable class that can be used as follows: `instanceOfCreateReplyDto.copyWith(...)` or like so:`instanceOfCreateReplyDto.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$CreateReplyDtoCWProxy get copyWith => _$CreateReplyDtoCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

CreateReplyDto _$CreateReplyDtoFromJson(Map<String, dynamic> json) =>
    CreateReplyDto(
      content: json['content'] as String,
      postId: json['postId'] as String,
      commentId: json['commentId'] as String?,
      replyId: json['replyId'] as String?,
    );

Map<String, dynamic> _$CreateReplyDtoToJson(CreateReplyDto instance) {
  final val = <String, dynamic>{
    'content': instance.content,
  };

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('commentId', instance.commentId);
  writeNotNull('replyId', instance.replyId);
  val['postId'] = instance.postId;
  return val;
}
