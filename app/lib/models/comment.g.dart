// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'comment.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Comment _$CommentFromJson(Map<String, dynamic> json) => Comment(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      content: json['content'] as String,
      likesCount: (json['likesCount'] as num).toInt(),
      reviewState:
          $enumDecode(_$CommentReviewStateEnumEnumMap, json['reviewState']),
      uniqueIdentifier: json['uniqueIdentifier'] as String,
      liked: json['liked'] as bool?,
      user: json['user'] == null
          ? null
          : User.fromJson(json['user'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$CommentToJson(Comment instance) {
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
  val['reviewState'] = _$CommentReviewStateEnumEnumMap[instance.reviewState]!;
  writeNotNull('user', instance.user);
  val['uniqueIdentifier'] = instance.uniqueIdentifier;
  return val;
}

const _$CommentReviewStateEnumEnumMap = {
  CommentReviewStateEnum.approved: 'APPROVED',
  CommentReviewStateEnum.rejected: 'REJECTED',
  CommentReviewStateEnum.pendingReview: 'PENDING_REVIEW',
};
