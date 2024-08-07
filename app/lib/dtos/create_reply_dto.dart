import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'create_reply_dto.g.dart';

/// CreateReplyDto
@CopyWith()
@JsonSerializable()
class CreateReplyDto {
  /// content
  final String content;

  /// commentId
  final String? commentId;

  /// replyId
  final String? replyId;

  /// postId
  final String postId;

  factory CreateReplyDto.fromJsonString(String json) =>
      CreateReplyDto.fromJson(jsonDecode(json));

  factory CreateReplyDto.fromJson(Map<String, dynamic> json) =>
      _$CreateReplyDtoFromJson(json);

  Map<String, dynamic> toJson() => _$CreateReplyDtoToJson(this);

  const CreateReplyDto({
    required this.content,
    required this.postId,
    this.commentId,
    this.replyId,
  });
}
