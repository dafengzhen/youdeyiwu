import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';

part 'tag.g.dart';

/// Tag
@CopyWith()
@JsonSerializable()
class Tag extends Base {
  /// name
  final String name;

  /// sort
  final int sort;

  const Tag({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.sort,
  });

  factory Tag.withResponse(Response response) {
    return Tag.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Tag.fromJsonString(String json) => Tag.fromJson(jsonDecode(json));

  factory Tag.fromJson(Map<String, dynamic> json) => _$TagFromJson(json);

  Map<String, dynamic> toJson() => _$TagToJson(this);

  @override
  String toString() {
    return 'Tag{name: $name, sort: $sort}';
  }
}
