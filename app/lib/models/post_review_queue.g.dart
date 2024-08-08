// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'post_review_queue.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PostReviewQueueCWProxy {
  PostReviewQueue id(int id);

  PostReviewQueue deleted(bool deleted);

  PostReviewQueue createdBy(int? createdBy);

  PostReviewQueue updatedBy(int? updatedBy);

  PostReviewQueue createdOn(String? createdOn);

  PostReviewQueue updatedOn(String? updatedOn);

  PostReviewQueue received(bool received);

  PostReviewQueue latestReviewResultTime(DateTime latestReviewResultTime);

  PostReviewQueue receiver(User receiver);

  PostReviewQueue post(Post post);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `PostReviewQueue(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// PostReviewQueue(...).copyWith(id: 12, name: "My name")
  /// ````
  PostReviewQueue call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    bool? received,
    DateTime? latestReviewResultTime,
    User? receiver,
    Post? post,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPostReviewQueue.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPostReviewQueue.copyWith.fieldName(...)`
class _$PostReviewQueueCWProxyImpl implements _$PostReviewQueueCWProxy {
  const _$PostReviewQueueCWProxyImpl(this._value);

  final PostReviewQueue _value;

  @override
  PostReviewQueue id(int id) => this(id: id);

  @override
  PostReviewQueue deleted(bool deleted) => this(deleted: deleted);

  @override
  PostReviewQueue createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  PostReviewQueue updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  PostReviewQueue createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  PostReviewQueue updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  PostReviewQueue received(bool received) => this(received: received);

  @override
  PostReviewQueue latestReviewResultTime(DateTime latestReviewResultTime) =>
      this(latestReviewResultTime: latestReviewResultTime);

  @override
  PostReviewQueue receiver(User receiver) => this(receiver: receiver);

  @override
  PostReviewQueue post(Post post) => this(post: post);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `PostReviewQueue(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// PostReviewQueue(...).copyWith(id: 12, name: "My name")
  /// ````
  PostReviewQueue call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? received = const $CopyWithPlaceholder(),
    Object? latestReviewResultTime = const $CopyWithPlaceholder(),
    Object? receiver = const $CopyWithPlaceholder(),
    Object? post = const $CopyWithPlaceholder(),
  }) {
    return PostReviewQueue(
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
      received: received == const $CopyWithPlaceholder() || received == null
          ? _value.received
          // ignore: cast_nullable_to_non_nullable
          : received as bool,
      latestReviewResultTime:
          latestReviewResultTime == const $CopyWithPlaceholder() ||
                  latestReviewResultTime == null
              ? _value.latestReviewResultTime
              // ignore: cast_nullable_to_non_nullable
              : latestReviewResultTime as DateTime,
      receiver: receiver == const $CopyWithPlaceholder() || receiver == null
          ? _value.receiver
          // ignore: cast_nullable_to_non_nullable
          : receiver as User,
      post: post == const $CopyWithPlaceholder() || post == null
          ? _value.post
          // ignore: cast_nullable_to_non_nullable
          : post as Post,
    );
  }
}

extension $PostReviewQueueCopyWith on PostReviewQueue {
  /// Returns a callable class that can be used as follows: `instanceOfPostReviewQueue.copyWith(...)` or like so:`instanceOfPostReviewQueue.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PostReviewQueueCWProxy get copyWith => _$PostReviewQueueCWProxyImpl(this);
}

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
