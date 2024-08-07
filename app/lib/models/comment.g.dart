// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'comment.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$CommentCWProxy {
  Comment id(int id);

  Comment deleted(bool deleted);

  Comment createdBy(int? createdBy);

  Comment updatedBy(int? updatedBy);

  Comment createdOn(String? createdOn);

  Comment updatedOn(String? updatedOn);

  Comment content(String content);

  Comment likesCount(int likesCount);

  Comment reviewState(CommentReviewStateEnum reviewState);

  Comment uniqueIdentifier(String uniqueIdentifier);

  Comment liked(bool? liked);

  Comment user(User? user);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Comment(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Comment(...).copyWith(id: 12, name: "My name")
  /// ````
  Comment call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? content,
    int? likesCount,
    CommentReviewStateEnum? reviewState,
    String? uniqueIdentifier,
    bool? liked,
    User? user,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfComment.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfComment.copyWith.fieldName(...)`
class _$CommentCWProxyImpl implements _$CommentCWProxy {
  const _$CommentCWProxyImpl(this._value);

  final Comment _value;

  @override
  Comment id(int id) => this(id: id);

  @override
  Comment deleted(bool deleted) => this(deleted: deleted);

  @override
  Comment createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Comment updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Comment createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Comment updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Comment content(String content) => this(content: content);

  @override
  Comment likesCount(int likesCount) => this(likesCount: likesCount);

  @override
  Comment reviewState(CommentReviewStateEnum reviewState) =>
      this(reviewState: reviewState);

  @override
  Comment uniqueIdentifier(String uniqueIdentifier) =>
      this(uniqueIdentifier: uniqueIdentifier);

  @override
  Comment liked(bool? liked) => this(liked: liked);

  @override
  Comment user(User? user) => this(user: user);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Comment(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Comment(...).copyWith(id: 12, name: "My name")
  /// ````
  Comment call({
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
    Object? user = const $CopyWithPlaceholder(),
  }) {
    return Comment(
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
              : reviewState as CommentReviewStateEnum,
      uniqueIdentifier: uniqueIdentifier == const $CopyWithPlaceholder() ||
              uniqueIdentifier == null
          ? _value.uniqueIdentifier
          // ignore: cast_nullable_to_non_nullable
          : uniqueIdentifier as String,
      liked: liked == const $CopyWithPlaceholder()
          ? _value.liked
          // ignore: cast_nullable_to_non_nullable
          : liked as bool?,
      user: user == const $CopyWithPlaceholder()
          ? _value.user
          // ignore: cast_nullable_to_non_nullable
          : user as User?,
    );
  }
}

extension $CommentCopyWith on Comment {
  /// Returns a callable class that can be used as follows: `instanceOfComment.copyWith(...)` or like so:`instanceOfComment.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$CommentCWProxy get copyWith => _$CommentCWProxyImpl(this);
}

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
