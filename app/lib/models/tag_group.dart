import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';
import 'tag.dart';

part 'tag_group.g.dart';

/// TagGroup
@CopyWith()
@JsonSerializable()
class TagGroup extends Base {
  /// name
  final String name;

  /// sort
  final int sort;

  /// tags
  final Set<Tag>? tags;

  const TagGroup({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.sort,
    this.tags,
  });

  factory TagGroup.withResponse(Response response) {
    return TagGroup.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory TagGroup.fromJsonString(String json) =>
      TagGroup.fromJson(jsonDecode(json));

  factory TagGroup.fromJson(Map<String, dynamic> json) =>
      _$TagGroupFromJson(json);

  Map<String, dynamic> toJson() => _$TagGroupToJson(this);

  @override
  String toString() {
    return 'TagGroup{name: $name, sort: $sort, tags: $tags}';
  }
}
