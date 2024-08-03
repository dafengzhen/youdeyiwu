// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post_review_queue.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

PostReviewQueue _$PostReviewQueueFromJson(Map<String, dynamic> json) =>
    PostReviewQueue(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      received: json['received'] as bool,
      latestReviewResultTime:
          DateTime.parse(json['latestReviewResultTime'] as String),
      receiver: User.fromJson(json['receiver'] as Map<String, dynamic>),
      post: Post.fromJson(json['post'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$PostReviewQueueToJson(PostReviewQueue instance) {
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
  val['received'] = instance.received;
  val['latestReviewResultTime'] =
      instance.latestReviewResultTime.toIso8601String();
  val['receiver'] = instance.receiver;
  val['post'] = instance.post;
  return val;
}
