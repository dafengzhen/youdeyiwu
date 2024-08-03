// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'file_urls.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

FileUrls _$FileUrlsFromJson(Map<String, dynamic> json) => FileUrls(
      url: json['url'] as String,
      urls: json['urls'] == null
          ? null
          : FileUrl.fromJson(json['urls'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$FileUrlsToJson(FileUrls instance) {
  final val = <String, dynamic>{
    'url': instance.url,
  };

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('urls', instance.urls);
  return val;
}
