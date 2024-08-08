import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';
import 'section.dart';

part 'section_group.g.dart';

/// SectionGroup
@CopyWith()
@JsonSerializable()
class SectionGroup extends Base {
  /// name
  final String name;

  /// sort
  final int sort;

  /// sections
  final Set<Section>? sections;

  const SectionGroup({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.sort,
    this.sections,
  });

  factory SectionGroup.others() => const SectionGroup(
        id: -1,
        deleted: false,
        name: "Others",
        sort: 0,
      );

  factory SectionGroup.withResponse(Response response) {
    return SectionGroup.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory SectionGroup.fromJsonString(String json) =>
      SectionGroup.fromJson(jsonDecode(json));

  factory SectionGroup.fromJson(Map<String, dynamic> json) =>
      _$SectionGroupFromJson(json);

  Map<String, dynamic> toJson() => _$SectionGroupToJson(this);

  @override
  String toString() {
    return 'SectionGroup{name: $name, sort: $sort, sections: $sections}';
  }
}
