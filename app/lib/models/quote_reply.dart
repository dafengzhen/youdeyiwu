import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import '../enums/quote_reply_review_state_enum.dart';
import 'base.dart';
import 'comment.dart';
import 'user.dart';

part 'quote_reply.g.dart';

/// QuoteReply
@JsonSerializable()
class QuoteReply extends Base {
  /// content
  final String content;

  /// likes count
  final int likesCount;

  /// liked
  final bool? liked;

  /// reviewState
  final QuoteReplyReviewStateEnum reviewState;

  /// comment
  final Comment? comment;

  /// quote reply
  final QuoteReply? quoteReply;

  /// user
  final User? user;

  /// unique identifier
  final String uniqueIdentifier;

  const QuoteReply({
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
    this.comment,
    this.quoteReply,
    this.user,
  });

  factory QuoteReply.withResponse(Response response) {
    return QuoteReply.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory QuoteReply.fromJsonString(String json) =>
      QuoteReply.fromJson(jsonDecode(json));

  factory QuoteReply.fromJson(Map<String, dynamic> json) =>
      _$QuoteReplyFromJson(json);

  Map<String, dynamic> toJson() => _$QuoteReplyToJson(this);

  @override
  String toString() {
    return 'QuoteReply{content: $content, likesCount: $likesCount, liked: $liked, reviewState: $reviewState, comment: $comment, quoteReply: $quoteReply, user: $user, uniqueIdentifier: $uniqueIdentifier}';
  }
}
