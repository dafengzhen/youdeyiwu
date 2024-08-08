// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'related_statistics.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$RelatedStatisticsCWProxy {
  RelatedStatistics sections(int sections);

  RelatedStatistics tags(int tags);

  RelatedStatistics posts(int posts);

  RelatedStatistics comments(int comments);

  RelatedStatistics replies(int replies);

  RelatedStatistics views(int views);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `RelatedStatistics(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// RelatedStatistics(...).copyWith(id: 12, name: "My name")
  /// ````
  RelatedStatistics call({
    int? sections,
    int? tags,
    int? posts,
    int? comments,
    int? replies,
    int? views,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfRelatedStatistics.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfRelatedStatistics.copyWith.fieldName(...)`
class _$RelatedStatisticsCWProxyImpl implements _$RelatedStatisticsCWProxy {
  const _$RelatedStatisticsCWProxyImpl(this._value);

  final RelatedStatistics _value;

  @override
  RelatedStatistics sections(int sections) => this(sections: sections);

  @override
  RelatedStatistics tags(int tags) => this(tags: tags);

  @override
  RelatedStatistics posts(int posts) => this(posts: posts);

  @override
  RelatedStatistics comments(int comments) => this(comments: comments);

  @override
  RelatedStatistics replies(int replies) => this(replies: replies);

  @override
  RelatedStatistics views(int views) => this(views: views);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `RelatedStatistics(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// RelatedStatistics(...).copyWith(id: 12, name: "My name")
  /// ````
  RelatedStatistics call({
    Object? sections = const $CopyWithPlaceholder(),
    Object? tags = const $CopyWithPlaceholder(),
    Object? posts = const $CopyWithPlaceholder(),
    Object? comments = const $CopyWithPlaceholder(),
    Object? replies = const $CopyWithPlaceholder(),
    Object? views = const $CopyWithPlaceholder(),
  }) {
    return RelatedStatistics(
      sections: sections == const $CopyWithPlaceholder() || sections == null
          ? _value.sections
          // ignore: cast_nullable_to_non_nullable
          : sections as int,
      tags: tags == const $CopyWithPlaceholder() || tags == null
          ? _value.tags
          // ignore: cast_nullable_to_non_nullable
          : tags as int,
      posts: posts == const $CopyWithPlaceholder() || posts == null
          ? _value.posts
          // ignore: cast_nullable_to_non_nullable
          : posts as int,
      comments: comments == const $CopyWithPlaceholder() || comments == null
          ? _value.comments
          // ignore: cast_nullable_to_non_nullable
          : comments as int,
      replies: replies == const $CopyWithPlaceholder() || replies == null
          ? _value.replies
          // ignore: cast_nullable_to_non_nullable
          : replies as int,
      views: views == const $CopyWithPlaceholder() || views == null
          ? _value.views
          // ignore: cast_nullable_to_non_nullable
          : views as int,
    );
  }
}

extension $RelatedStatisticsCopyWith on RelatedStatistics {
  /// Returns a callable class that can be used as follows: `instanceOfRelatedStatistics.copyWith(...)` or like so:`instanceOfRelatedStatistics.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$RelatedStatisticsCWProxy get copyWith =>
      _$RelatedStatisticsCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

RelatedStatistics _$RelatedStatisticsFromJson(Map<String, dynamic> json) =>
    RelatedStatistics(
      sections: (json['sections'] as num).toInt(),
      tags: (json['tags'] as num).toInt(),
      posts: (json['posts'] as num).toInt(),
      comments: (json['comments'] as num).toInt(),
      replies: (json['replies'] as num).toInt(),
      views: (json['views'] as num).toInt(),
    );

Map<String, dynamic> _$RelatedStatisticsToJson(RelatedStatistics instance) =>
    <String, dynamic>{
      'sections': instance.sections,
      'tags': instance.tags,
      'posts': instance.posts,
      'comments': instance.comments,
      'replies': instance.replies,
      'views': instance.views,
    };
