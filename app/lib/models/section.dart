import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';
import 'package:youdeyiwu_app/utils/tools.dart';

import '../enums/file_type_enum.dart';
import '../enums/section_state_enum.dart';
import 'base.dart';
import 'section_group.dart';
import 'tag.dart';
import 'tag_group.dart';
import 'user.dart';

part 'section.g.dart';

/// Section
@CopyWith()
@JsonSerializable()
class Section extends Base {
  /// name
  final String name;

  /// cover
  final String? cover;

  /// coverImage
  final List<int>? coverImage;

  /// coverImageType
  final FileTypeEnum? coverImageType;

  /// overview
  final String? overview;

  /// content
  final String? content;

  /// createPostGuide
  final String? createPostGuide;

  /// sort
  final int sort;

  /// states
  final Set<SectionStateEnum> states;

  /// admins
  final Set<User>? admins;

  /// allows
  final Set<User>? allows;

  /// blocks
  final Set<User>? blocks;

  /// accessKey
  final String? accessKey;

  /// access points
  final int accessPoints;

  /// tagGroups
  final Set<TagGroup>? tagGroups;

  /// tags
  final Set<Tag>? tags;

  /// sectionGroups
  final Set<SectionGroup>? sectionGroups;

  /// user
  final User? user;

  const Section({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.sort,
    required this.states,
    required this.accessKey,
    required this.accessPoints,
    this.admins,
    this.tagGroups,
    this.tags,
    this.sectionGroups,
    this.cover,
    this.coverImage,
    this.coverImageType,
    this.overview,
    this.content,
    this.allows,
    this.blocks,
    this.createPostGuide,
    this.user,
  });

  factory Section.withResponse(Response response) {
    return Section.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  static List<Section> fromList(Response response) {
    List<dynamic> list = jsonDecode(utf8.decode(response.bodyBytes));
    return list.map((json) => Section.fromJson(json)).toList();
  }

  factory Section.fromJsonString(String json) =>
      Section.fromJson(jsonDecode(json));

  factory Section.fromJson(Map<String, dynamic> json) =>
      _$SectionFromJson(json);

  Map<String, dynamic> toJson() => _$SectionToJson(this);

  @override
  String toString() {
    return 'Section{name: $name, cover: $cover, coverImage: $coverImage, coverImageType: $coverImageType, overview: $overview, content: $content, createPostGuide: $createPostGuide, sort: $sort, states: $states, admins: $admins, allows: $allows, blocks: $blocks, accessKey: $accessKey, accessPoints: $accessPoints, tagGroups: $tagGroups, tags: $tags, sectionGroups: $sectionGroups, user: $user}';
  }
}
