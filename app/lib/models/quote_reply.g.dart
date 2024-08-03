// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'quote_reply.dart';

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
