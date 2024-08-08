import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';

part 'post_badge.g.dart';

/// PostBadge
@CopyWith()
@JsonSerializable()
class PostBadge extends Base {
  /// name
  final String name;

  /// sort
  final int sort;

  /// styles
  final String? styles;

  /// classes
  final String? classes;

  const PostBadge({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.sort,
    this.styles,
    this.classes,
  });

  factory PostBadge.withResponse(Response response) {
    return PostBadge.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory PostBadge.fromJsonString(String json) =>
      PostBadge.fromJson(jsonDecode(json));

  factory PostBadge.fromJson(Map<String, dynamic> json) =>
      _$PostBadgeFromJson(json);

  Map<String, dynamic> toJson() => _$PostBadgeToJson(this);

  @override
  String toString() {
    return 'PostBadge{name: $name, sort: $sort, styles: $styles, classes: $classes}';
  }
}
