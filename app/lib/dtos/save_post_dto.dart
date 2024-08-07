import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'save_post_dto.g.dart';

/// SavePostDto
@CopyWith()
@JsonSerializable()
class SavePostDto {
  /// name
  final String? name;

  /// cover
  final String? cover;

  /// overview
  final String? overview;

  /// content
  final String? content;

  /// plainTextContent
  final String? plainTextContent;

  /// markdownContent
  final String? markdownContent;

  /// deltaContent
  final String? deltaContent;

  /// contentLink
  final String? contentLink;

  /// tags
  final List<String>? tags;

  /// sectionId
  final int? sectionId;

  /// removeSection
  final bool? removeSection;

  factory SavePostDto.fromJsonString(String json) =>
      SavePostDto.fromJson(jsonDecode(json));

  factory SavePostDto.fromJson(Map<String, dynamic> json) =>
      _$SavePostDtoFromJson(json);

  Map<String, dynamic> toJson() => _$SavePostDtoToJson(this);

  const SavePostDto({
    this.name,
    this.cover,
    this.overview,
    this.content,
    this.plainTextContent,
    this.markdownContent,
    this.deltaContent,
    this.contentLink,
    this.tags,
    this.sectionId,
    this.removeSection,
  });

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is SavePostDto &&
          runtimeType == other.runtimeType &&
          name == other.name &&
          cover == other.cover &&
          overview == other.overview &&
          content == other.content &&
          plainTextContent == other.plainTextContent &&
          markdownContent == other.markdownContent &&
          deltaContent == other.deltaContent &&
          contentLink == other.contentLink &&
          tags == other.tags &&
          sectionId == other.sectionId &&
          removeSection == other.removeSection;

  @override
  int get hashCode =>
      name.hashCode ^
      cover.hashCode ^
      overview.hashCode ^
      content.hashCode ^
      plainTextContent.hashCode ^
      markdownContent.hashCode ^
      deltaContent.hashCode ^
      contentLink.hashCode ^
      tags.hashCode ^
      sectionId.hashCode ^
      removeSection.hashCode;

  @override
  String toString() {
    return 'SavePostDto{name: $name, cover: $cover, overview: $overview, content: $content, plainTextContent: $plainTextContent, markdownContent: $markdownContent, deltaContent: $deltaContent, contentLink: $contentLink, tags: $tags, sectionId: $sectionId, removeSection: $removeSection}';
  }
}
