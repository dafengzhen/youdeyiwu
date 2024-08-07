import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'create_comment_dto.g.dart';

/// CreateCommentDto
@CopyWith()
@JsonSerializable()
class CreateCommentDto {
  /// content
  final String content;

  /// postId
  final String postId;

  factory CreateCommentDto.fromJsonString(String json) =>
      CreateCommentDto.fromJson(jsonDecode(json));

  factory CreateCommentDto.fromJson(Map<String, dynamic> json) =>
      _$CreateCommentDtoFromJson(json);

  Map<String, dynamic> toJson() => _$CreateCommentDtoToJson(this);

  const CreateCommentDto({
    required this.content,
    required this.postId,
  });
}
