// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'related_statistics.dart';

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
