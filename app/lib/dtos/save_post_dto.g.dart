// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'save_post_dto.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$SavePostDtoCWProxy {
  SavePostDto name(String? name);

  SavePostDto cover(String? cover);

  SavePostDto overview(String? overview);

  SavePostDto content(String? content);

  SavePostDto plainTextContent(String? plainTextContent);

  SavePostDto markdownContent(String? markdownContent);

  SavePostDto deltaContent(String? deltaContent);

  SavePostDto contentLink(String? contentLink);

  SavePostDto tags(List<String>? tags);

  SavePostDto sectionId(int? sectionId);

  SavePostDto removeSection(bool? removeSection);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `SavePostDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// SavePostDto(...).copyWith(id: 12, name: "My name")
  /// ````
  SavePostDto call({
    String? name,
    String? cover,
    String? overview,
    String? content,
    String? plainTextContent,
    String? markdownContent,
    String? deltaContent,
    String? contentLink,
    List<String>? tags,
    int? sectionId,
    bool? removeSection,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfSavePostDto.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfSavePostDto.copyWith.fieldName(...)`
class _$SavePostDtoCWProxyImpl implements _$SavePostDtoCWProxy {
  const _$SavePostDtoCWProxyImpl(this._value);

  final SavePostDto _value;

  @override
  SavePostDto name(String? name) => this(name: name);

  @override
  SavePostDto cover(String? cover) => this(cover: cover);

  @override
  SavePostDto overview(String? overview) => this(overview: overview);

  @override
  SavePostDto content(String? content) => this(content: content);

  @override
  SavePostDto plainTextContent(String? plainTextContent) =>
      this(plainTextContent: plainTextContent);

  @override
  SavePostDto markdownContent(String? markdownContent) =>
      this(markdownContent: markdownContent);

  @override
  SavePostDto deltaContent(String? deltaContent) =>
      this(deltaContent: deltaContent);

  @override
  SavePostDto contentLink(String? contentLink) =>
      this(contentLink: contentLink);

  @override
  SavePostDto tags(List<String>? tags) => this(tags: tags);

  @override
  SavePostDto sectionId(int? sectionId) => this(sectionId: sectionId);

  @override
  SavePostDto removeSection(bool? removeSection) =>
      this(removeSection: removeSection);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `SavePostDto(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// SavePostDto(...).copyWith(id: 12, name: "My name")
  /// ````
  SavePostDto call({
    Object? name = const $CopyWithPlaceholder(),
    Object? cover = const $CopyWithPlaceholder(),
    Object? overview = const $CopyWithPlaceholder(),
    Object? content = const $CopyWithPlaceholder(),
    Object? plainTextContent = const $CopyWithPlaceholder(),
    Object? markdownContent = const $CopyWithPlaceholder(),
    Object? deltaContent = const $CopyWithPlaceholder(),
    Object? contentLink = const $CopyWithPlaceholder(),
    Object? tags = const $CopyWithPlaceholder(),
    Object? sectionId = const $CopyWithPlaceholder(),
    Object? removeSection = const $CopyWithPlaceholder(),
  }) {
    return SavePostDto(
      name: name == const $CopyWithPlaceholder()
          ? _value.name
          // ignore: cast_nullable_to_non_nullable
          : name as String?,
      cover: cover == const $CopyWithPlaceholder()
          ? _value.cover
          // ignore: cast_nullable_to_non_nullable
          : cover as String?,
      overview: overview == const $CopyWithPlaceholder()
          ? _value.overview
          // ignore: cast_nullable_to_non_nullable
          : overview as String?,
      content: content == const $CopyWithPlaceholder()
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as String?,
      plainTextContent: plainTextContent == const $CopyWithPlaceholder()
          ? _value.plainTextContent
          // ignore: cast_nullable_to_non_nullable
          : plainTextContent as String?,
      markdownContent: markdownContent == const $CopyWithPlaceholder()
          ? _value.markdownContent
          // ignore: cast_nullable_to_non_nullable
          : markdownContent as String?,
      deltaContent: deltaContent == const $CopyWithPlaceholder()
          ? _value.deltaContent
          // ignore: cast_nullable_to_non_nullable
          : deltaContent as String?,
      contentLink: contentLink == const $CopyWithPlaceholder()
          ? _value.contentLink
          // ignore: cast_nullable_to_non_nullable
          : contentLink as String?,
      tags: tags == const $CopyWithPlaceholder()
          ? _value.tags
          // ignore: cast_nullable_to_non_nullable
          : tags as List<String>?,
      sectionId: sectionId == const $CopyWithPlaceholder()
          ? _value.sectionId
          // ignore: cast_nullable_to_non_nullable
          : sectionId as int?,
      removeSection: removeSection == const $CopyWithPlaceholder()
          ? _value.removeSection
          // ignore: cast_nullable_to_non_nullable
          : removeSection as bool?,
    );
  }
}

extension $SavePostDtoCopyWith on SavePostDto {
  /// Returns a callable class that can be used as follows: `instanceOfSavePostDto.copyWith(...)` or like so:`instanceOfSavePostDto.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$SavePostDtoCWProxy get copyWith => _$SavePostDtoCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

SavePostDto _$SavePostDtoFromJson(Map<String, dynamic> json) => SavePostDto(
      name: json['name'] as String?,
      cover: json['cover'] as String?,
      overview: json['overview'] as String?,
      content: json['content'] as String?,
      plainTextContent: json['plainTextContent'] as String?,
      markdownContent: json['markdownContent'] as String?,
      deltaContent: json['deltaContent'] as String?,
      contentLink: json['contentLink'] as String?,
      tags: (json['tags'] as List<dynamic>?)?.map((e) => e as String).toList(),
      sectionId: (json['sectionId'] as num?)?.toInt(),
      removeSection: json['removeSection'] as bool?,
    );

Map<String, dynamic> _$SavePostDtoToJson(SavePostDto instance) {
  final val = <String, dynamic>{};

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('name', instance.name);
  writeNotNull('cover', instance.cover);
  writeNotNull('overview', instance.overview);
  writeNotNull('content', instance.content);
  writeNotNull('plainTextContent', instance.plainTextContent);
  writeNotNull('markdownContent', instance.markdownContent);
  writeNotNull('deltaContent', instance.deltaContent);
  writeNotNull('contentLink', instance.contentLink);
  writeNotNull('tags', instance.tags);
  writeNotNull('sectionId', instance.sectionId);
  writeNotNull('removeSection', instance.removeSection);
  return val;
}
