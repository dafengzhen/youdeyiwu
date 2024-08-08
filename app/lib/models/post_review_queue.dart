import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';
import 'post.dart';
import 'user.dart';

part 'post_review_queue.g.dart';

/// PostReviewQueue
@CopyWith()
@JsonSerializable()
class PostReviewQueue extends Base {
  /// received
  final bool received;

  /// latest review result time
  final DateTime latestReviewResultTime;

  /// receiver
  final User receiver;

  /// post
  final Post post;

  const PostReviewQueue({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.received,
    required this.latestReviewResultTime,
    required this.receiver,
    required this.post,
  });

  factory PostReviewQueue.withResponse(Response response) {
    return PostReviewQueue.fromJson(
        jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory PostReviewQueue.fromJsonString(String json) =>
      PostReviewQueue.fromJson(jsonDecode(json));

  factory PostReviewQueue.fromJson(Map<String, dynamic> json) =>
      _$PostReviewQueueFromJson(json);

  Map<String, dynamic> toJson() => _$PostReviewQueueToJson(this);

  @override
  String toString() {
    return 'PostReviewQueue{received: $received, latestReviewResultTime: $latestReviewResultTime, receiver: $receiver, post: $post}';
  }
}
