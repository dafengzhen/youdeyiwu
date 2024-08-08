import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'related_statistics.g.dart';

/// RelatedStatistics
@CopyWith()
@JsonSerializable()
class RelatedStatistics {
  /// sections
  final int sections;

  /// tags
  final int tags;

  /// posts
  final int posts;

  /// comments
  final int comments;

  /// replies
  final int replies;

  /// views
  final int views;

  const RelatedStatistics({
    required this.sections,
    required this.tags,
    required this.posts,
    required this.comments,
    required this.replies,
    required this.views,
  });

  factory RelatedStatistics.empty() => const RelatedStatistics(
        sections: 0,
        tags: 0,
        posts: 0,
        comments: 0,
        replies: 0,
        views: 0,
      );

  factory RelatedStatistics.fromJsonString(String json) =>
      RelatedStatistics.fromJson(jsonDecode(json));

  factory RelatedStatistics.fromJson(Map<String, dynamic> json) =>
      _$RelatedStatisticsFromJson(json);

  Map<String, dynamic> toJson() => _$RelatedStatisticsToJson(this);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is RelatedStatistics &&
          runtimeType == other.runtimeType &&
          sections == other.sections &&
          tags == other.tags &&
          posts == other.posts &&
          comments == other.comments &&
          replies == other.replies &&
          views == other.views;

  @override
  int get hashCode =>
      sections.hashCode ^
      tags.hashCode ^
      posts.hashCode ^
      comments.hashCode ^
      replies.hashCode ^
      views.hashCode;

  @override
  String toString() {
    return 'RelatedStatistics{sections: $sections, tags: $tags, posts: $posts, comments: $comments, replies: $replies, views: $views}';
  }
}
