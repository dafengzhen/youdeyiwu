// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'comment_reply.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$CommentReplyCWProxy {
  CommentReply comment(Comment? comment);

  CommentReply reply(QuoteReply? reply);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `CommentReply(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// CommentReply(...).copyWith(id: 12, name: "My name")
  /// ````
  CommentReply call({
    Comment? comment,
    QuoteReply? reply,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfCommentReply.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfCommentReply.copyWith.fieldName(...)`
class _$CommentReplyCWProxyImpl implements _$CommentReplyCWProxy {
  const _$CommentReplyCWProxyImpl(this._value);

  final CommentReply _value;

  @override
  CommentReply comment(Comment? comment) => this(comment: comment);

  @override
  CommentReply reply(QuoteReply? reply) => this(reply: reply);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `CommentReply(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// CommentReply(...).copyWith(id: 12, name: "My name")
  /// ````
  CommentReply call({
    Object? comment = const $CopyWithPlaceholder(),
    Object? reply = const $CopyWithPlaceholder(),
  }) {
    return CommentReply(
      comment: comment == const $CopyWithPlaceholder()
          ? _value.comment
          // ignore: cast_nullable_to_non_nullable
          : comment as Comment?,
      reply: reply == const $CopyWithPlaceholder()
          ? _value.reply
          // ignore: cast_nullable_to_non_nullable
          : reply as QuoteReply?,
    );
  }
}

extension $CommentReplyCopyWith on CommentReply {
  /// Returns a callable class that can be used as follows: `instanceOfCommentReply.copyWith(...)` or like so:`instanceOfCommentReply.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$CommentReplyCWProxy get copyWith => _$CommentReplyCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

CommentReply _$CommentReplyFromJson(Map<String, dynamic> json) => CommentReply(
      comment: json['comment'] == null
          ? null
          : Comment.fromJson(json['comment'] as Map<String, dynamic>),
      reply: json['reply'] == null
          ? null
          : QuoteReply.fromJson(json['reply'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$CommentReplyToJson(CommentReply instance) {
  final val = <String, dynamic>{};

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('comment', instance.comment);
  writeNotNull('reply', instance.reply);
  return val;
}
