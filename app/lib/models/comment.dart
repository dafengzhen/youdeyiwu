import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import '../enums/comment_review_state_enum.dart';
import 'base.dart';
import 'user.dart';

part 'comment.g.dart';

/// Comment
@JsonSerializable()
class Comment extends Base {
  /// content
  final String content;

  /// likes count
  final int likesCount;

  /// liked
  final bool? liked;

  /// reviewState
  final CommentReviewStateEnum reviewState;

  /// user
  final User? user;

  /// unique identifier
  final String uniqueIdentifier;

  const Comment({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.content,
    required this.likesCount,
    required this.reviewState,
    required this.uniqueIdentifier,
    this.liked,
    this.user,
  });

  factory Comment.withResponse(Response response) {
    return Comment.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Comment.fromJsonString(String json) =>
      Comment.fromJson(jsonDecode(json));

  factory Comment.fromJson(Map<String, dynamic> json) =>
      _$CommentFromJson(json);

  Map<String, dynamic> toJson() => _$CommentToJson(this);

  @override
  String toString() {
    return 'Comment{content: $content, likesCount: $likesCount, liked: $liked, reviewState: $reviewState, user: $user, uniqueIdentifier: $uniqueIdentifier}';
  }
}
