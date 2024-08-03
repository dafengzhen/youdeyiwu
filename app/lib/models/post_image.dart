import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';

part 'post_image.g.dart';

/// PostImage
@JsonSerializable()
class PostImage extends Base {
  /// url
  final String url;

  /// sort
  final int sort;

  const PostImage({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.url,
    required this.sort,
  });

  factory PostImage.withResponse(Response response) {
    return PostImage.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory PostImage.fromJsonString(String json) =>
      PostImage.fromJson(jsonDecode(json));

  factory PostImage.fromJson(Map<String, dynamic> json) =>
      _$PostImageFromJson(json);

  Map<String, dynamic> toJson() => _$PostImageToJson(this);

  @override
  String toString() {
    return 'PostImage{url: $url, sort: $sort}';
  }
}
