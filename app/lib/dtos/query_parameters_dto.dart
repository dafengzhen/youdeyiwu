import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'query_parameters_dto.g.dart';

/// QueryParametersDto
@CopyWith()
@JsonSerializable()
class QueryParametersDto {
  /// id
  final String? id;

  /// tagId
  final String? tagId;

  /// sectionId
  final String? sectionId;

  /// postId
  final String? postId;

  /// tagGroupId
  final String? tagGroupId;

  /// sectionGroupId
  final String? sectionGroupId;

  /// page
  final String? page;

  /// size
  final String? size;

  /// sort
  final String? sort;

  /// name
  final String? name;

  /// sectionSecret
  final String? sectionSecret;

  /// postSecret
  final String? postSecret;

  /// secret
  final String? secret;

  factory QueryParametersDto.merge(
    QueryParametersDto base,
    QueryParametersDto other,
  ) {
    return QueryParametersDto(
      id: other.id ?? base.id,
      tagId: other.tagId ?? base.tagId,
      sectionId: other.sectionId ?? base.sectionId,
      postId: other.postId ?? base.postId,
      tagGroupId: other.tagGroupId ?? base.tagGroupId,
      sectionGroupId: other.sectionGroupId ?? base.sectionGroupId,
      page: other.page ?? base.page,
      size: other.size ?? base.size,
      sort: other.sort ?? base.sort,
      name: other.name ?? base.name,
      sectionSecret: other.sectionSecret ?? base.sectionSecret,
      postSecret: other.postSecret ?? base.postSecret,
      secret: other.secret ?? base.secret,
    );
  }

  factory QueryParametersDto.fromJsonString(String json) =>
      QueryParametersDto.fromJson(jsonDecode(json));

  factory QueryParametersDto.fromJson(Map<String, dynamic> json) =>
      _$QueryParametersDtoFromJson(json);

  Map<String, dynamic> toJson() => _$QueryParametersDtoToJson(this);

  const QueryParametersDto({
    this.id,
    this.tagId,
    this.sectionId,
    this.postId,
    this.tagGroupId,
    this.sectionGroupId,
    this.page,
    this.size,
    this.sort,
    this.name,
    this.sectionSecret,
    this.postSecret,
    this.secret,
  });

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is QueryParametersDto &&
          runtimeType == other.runtimeType &&
          id == other.id &&
          tagId == other.tagId &&
          sectionId == other.sectionId &&
          postId == other.postId &&
          tagGroupId == other.tagGroupId &&
          sectionGroupId == other.sectionGroupId &&
          page == other.page &&
          size == other.size &&
          sort == other.sort &&
          name == other.name &&
          sectionSecret == other.sectionSecret &&
          postSecret == other.postSecret &&
          secret == other.secret;

  @override
  int get hashCode =>
      id.hashCode ^
      tagId.hashCode ^
      sectionId.hashCode ^
      postId.hashCode ^
      tagGroupId.hashCode ^
      sectionGroupId.hashCode ^
      page.hashCode ^
      size.hashCode ^
      sort.hashCode ^
      name.hashCode ^
      sectionSecret.hashCode ^
      postSecret.hashCode ^
      secret.hashCode;

  @override
  String toString() {
    return 'QueryParametersDto{id: $id, tagId: $tagId, sectionId: $sectionId, postId: $postId, tagGroupId: $tagGroupId, sectionGroupId: $sectionGroupId, page: $page, size: $size, sort: $sort, name: $name, sectionSecret: $sectionSecret, postSecret: $postSecret, secret: $secret}';
  }
}
