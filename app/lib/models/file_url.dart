import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

part 'file_url.g.dart';

/// FileUrl
@CopyWith()
@JsonSerializable()
class FileUrl {
  /// default url
  @JsonKey(name: 'default')
  final String defaultUrl;

  const FileUrl({
    required this.defaultUrl,
  });

  factory FileUrl.withResponse(Response response) {
    return FileUrl.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory FileUrl.fromJsonString(String json) =>
      FileUrl.fromJson(jsonDecode(json));

  factory FileUrl.fromJson(Map<String, dynamic> json) =>
      _$FileUrlFromJson(json);

  Map<String, dynamic> toJson() => _$FileUrlToJson(this);

  @override
  String toString() {
    return 'FileUrl{defaultUrl: $defaultUrl}';
  }
}
