// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'quote_reply.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$QuoteReplyCWProxy {
  QuoteReply id(int id);

  QuoteReply deleted(bool deleted);

  QuoteReply createdBy(int? createdBy);

  QuoteReply updatedBy(int? updatedBy);

  QuoteReply createdOn(String? createdOn);

  QuoteReply updatedOn(String? updatedOn);

  QuoteReply content(String content);

  QuoteReply likesCount(int likesCount);

  QuoteReply reviewState(QuoteReplyReviewStateEnum reviewState);

  QuoteReply uniqueIdentifier(String uniqueIdentifier);

  QuoteReply liked(bool? liked);

  QuoteReply comment(Comment? comment);

  QuoteReply quoteReply(QuoteReply? quoteReply);

  QuoteReply user(User? user);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `QuoteReply(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// QuoteReply(...).copyWith(id: 12, name: "My name")
  /// ````
  QuoteReply call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? content,
    int? likesCount,
    QuoteReplyReviewStateEnum? reviewState,
    String? uniqueIdentifier,
    bool? liked,
    Comment? comment,
    QuoteReply? quoteReply,
    User? user,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfQuoteReply.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfQuoteReply.copyWith.fieldName(...)`
class _$QuoteReplyCWProxyImpl implements _$QuoteReplyCWProxy {
  const _$QuoteReplyCWProxyImpl(this._value);

  final QuoteReply _value;

  @override
  QuoteReply id(int id) => this(id: id);

  @override
  QuoteReply deleted(bool deleted) => this(deleted: deleted);

  @override
  QuoteReply createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  QuoteReply updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  QuoteReply createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  QuoteReply updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  QuoteReply content(String content) => this(content: content);

  @override
  QuoteReply likesCount(int likesCount) => this(likesCount: likesCount);

  @override
  QuoteReply reviewState(QuoteReplyReviewStateEnum reviewState) =>
      this(reviewState: reviewState);

  @override
  QuoteReply uniqueIdentifier(String uniqueIdentifier) =>
      this(uniqueIdentifier: uniqueIdentifier);

  @override
  QuoteReply liked(bool? liked) => this(liked: liked);

  @override
  QuoteReply comment(Comment? comment) => this(comment: comment);

  @override
  QuoteReply quoteReply(QuoteReply? quoteReply) => this(quoteReply: quoteReply);

  @override
  QuoteReply user(User? user) => this(user: user);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `QuoteReply(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// QuoteReply(...).copyWith(id: 12, name: "My name")
  /// ````
  QuoteReply call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? content = const $CopyWithPlaceholder(),
    Object? likesCount = const $CopyWithPlaceholder(),
    Object? reviewState = const $CopyWithPlaceholder(),
    Object? uniqueIdentifier = const $CopyWithPlaceholder(),
    Object? liked = const $CopyWithPlaceholder(),
    Object? comment = const $CopyWithPlaceholder(),
    Object? quoteReply = const $CopyWithPlaceholder(),
    Object? user = const $CopyWithPlaceholder(),
  }) {
    return QuoteReply(
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
      content: content == const $CopyWithPlaceholder() || content == null
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as String,
      likesCount:
          likesCount == const $CopyWithPlaceholder() || likesCount == null
              ? _value.likesCount
              // ignore: cast_nullable_to_non_nullable
              : likesCount as int,
      reviewState:
          reviewState == const $CopyWithPlaceholder() || reviewState == null
              ? _value.reviewState
              // ignore: cast_nullable_to_non_nullable
              : reviewState as QuoteReplyReviewStateEnum,
      uniqueIdentifier: uniqueIdentifier == const $CopyWithPlaceholder() ||
              uniqueIdentifier == null
          ? _value.uniqueIdentifier
          // ignore: cast_nullable_to_non_nullable
          : uniqueIdentifier as String,
      liked: liked == const $CopyWithPlaceholder()
          ? _value.liked
          // ignore: cast_nullable_to_non_nullable
          : liked as bool?,
      comment: comment == const $CopyWithPlaceholder()
          ? _value.comment
          // ignore: cast_nullable_to_non_nullable
          : comment as Comment?,
      quoteReply: quoteReply == const $CopyWithPlaceholder()
          ? _value.quoteReply
          // ignore: cast_nullable_to_non_nullable
          : quoteReply as QuoteReply?,
      user: user == const $CopyWithPlaceholder()
          ? _value.user
          // ignore: cast_nullable_to_non_nullable
          : user as User?,
    );
  }
}

extension $QuoteReplyCopyWith on QuoteReply {
  /// Returns a callable class that can be used as follows: `instanceOfQuoteReply.copyWith(...)` or like so:`instanceOfQuoteReply.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$QuoteReplyCWProxy get copyWith => _$QuoteReplyCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

QuoteReply _$QuoteReplyFromJson(Map<String, dynamic> json) => QuoteReply(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      content: json['content'] as String,
      likesCount: (json['likesCount'] as num).toInt(),
      reviewState:
          $enumDecode(_$QuoteReplyReviewStateEnumEnumMap, json['reviewState']),
      uniqueIdentifier: json['uniqueIdentifier'] as String,
      liked: json['liked'] as bool?,
      comment: json['comment'] == null
          ? null
          : Comment.fromJson(json['comment'] as Map<String, dynamic>),
      quoteReply: json['quoteReply'] == null
          ? null
          : QuoteReply.fromJson(json['quoteReply'] as Map<String, dynamic>),
      user: json['user'] == null
          ? null
          : User.fromJson(json['user'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$QuoteReplyToJson(QuoteReply instance) {
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
  val['content'] = instance.content;
  val['likesCount'] = instance.likesCount;
  writeNotNull('liked', instance.liked);
  val['reviewState'] =
      _$QuoteReplyReviewStateEnumEnumMap[instance.reviewState]!;
  writeNotNull('comment', instance.comment);
  writeNotNull('quoteReply', instance.quoteReply);
  writeNotNull('user', instance.user);
  val['uniqueIdentifier'] = instance.uniqueIdentifier;
  return val;
}

const _$QuoteReplyReviewStateEnumEnumMap = {
  QuoteReplyReviewStateEnum.approved: 'APPROVED',
  QuoteReplyReviewStateEnum.rejected: 'REJECTED',
  QuoteReplyReviewStateEnum.pendingReview: 'PENDING_REVIEW',
};
