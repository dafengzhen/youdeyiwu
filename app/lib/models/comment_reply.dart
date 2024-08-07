import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'comment.dart';
import 'quote_reply.dart';

part 'comment_reply.g.dart';

/// CommentReply
@CopyWith()
@JsonSerializable()
class CommentReply {
  /// comment
  final Comment? comment;

  /// reply
  final QuoteReply? reply;

  const CommentReply({
    this.comment,
    this.reply,
  });

  factory CommentReply.withResponse(Response response) {
    return CommentReply.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory CommentReply.fromJsonString(String json) =>
      CommentReply.fromJson(jsonDecode(json));

  factory CommentReply.fromJson(Map<String, dynamic> json) =>
      _$CommentReplyFromJson(json);

  Map<String, dynamic> toJson() => _$CommentReplyToJson(this);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is CommentReply &&
          runtimeType == other.runtimeType &&
          comment == other.comment &&
          reply == other.reply;

  @override
  int get hashCode => comment.hashCode ^ reply.hashCode;

  @override
  String toString() {
    return 'CommentReply{comment: $comment, reply: $reply}';
  }
}
