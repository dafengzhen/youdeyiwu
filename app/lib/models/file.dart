import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import '../enums/business_type_enum.dart';
import '../enums/file_category_enum.dart';
import '../enums/storage_service_type_enum.dart';
import 'base.dart';
import 'file_urls.dart';
import 'user.dart';

part 'file.g.dart';

/// File
@CopyWith()
@JsonSerializable()
class File extends Base {
  /// url
  final String url;

  /// urls
  final FileUrls? urls;

  /// name
  final String name;

  /// original name
  final String originalName;

  /// overview
  final String? overview;

  /// file category
  final FileCategoryEnum fileCategory;

  /// storage service type
  final StorageServiceTypeEnum storageServiceType;

  /// business type
  final BusinessTypeEnum businessType;

  /// content type
  final String contentType;

  /// media type
  final String mediaType;

  /// size
  final int size;

  /// bucket name
  final String? bucketName;

  /// object name
  final String? objectName;

  /// view count
  final int viewCount;

  /// digest
  final String digest;

  /// object key
  final String? objectKey;

  /// user
  final User? user;

  const File({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.url,
    required this.name,
    required this.originalName,
    required this.fileCategory,
    required this.storageServiceType,
    required this.businessType,
    required this.contentType,
    required this.mediaType,
    required this.size,
    required this.viewCount,
    required this.digest,
    this.urls,
    this.overview,
    this.bucketName,
    this.objectName,
    this.objectKey,
    this.user,
  });

  factory File.withResponse(Response response) {
    return File.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory File.fromJsonString(String json) => File.fromJson(jsonDecode(json));

  factory File.fromJson(Map<String, dynamic> json) => _$FileFromJson(json);

  Map<String, dynamic> toJson() => _$FileToJson(this);

  @override
  String toString() {
    return 'File{url: $url, urls: $urls, name: $name, originalName: $originalName, overview: $overview, fileCategory: $fileCategory, storageServiceType: $storageServiceType, businessType: $businessType, contentType: $contentType, mediaType: $mediaType, size: $size, bucketName: $bucketName, objectName: $objectName, viewCount: $viewCount, digest: $digest, objectKey: $objectKey, user: $user}';
  }
}
