import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'file_url.dart';

part 'file_urls.g.dart';

/// FileUrls
@JsonSerializable()
class FileUrls {
  /// url
  final String url;

  /// urls
  final FileUrl? urls;

  const FileUrls({
    required this.url,
    this.urls,
  });

  factory FileUrls.withResponse(Response response) {
    return FileUrls.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory FileUrls.fromJsonString(String json) =>
      FileUrls.fromJson(jsonDecode(json));

  factory FileUrls.fromJson(Map<String, dynamic> json) =>
      _$FileUrlsFromJson(json);

  Map<String, dynamic> toJson() => _$FileUrlsToJson(this);

  @override
  String toString() {
    return 'FileUrls{url: $url, urls: $urls}';
  }
}
