// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'file_urls.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$FileUrlsCWProxy {
  FileUrls url(String url);

  FileUrls urls(FileUrl? urls);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `FileUrls(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// FileUrls(...).copyWith(id: 12, name: "My name")
  /// ````
  FileUrls call({
    String? url,
    FileUrl? urls,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfFileUrls.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfFileUrls.copyWith.fieldName(...)`
class _$FileUrlsCWProxyImpl implements _$FileUrlsCWProxy {
  const _$FileUrlsCWProxyImpl(this._value);

  final FileUrls _value;

  @override
  FileUrls url(String url) => this(url: url);

  @override
  FileUrls urls(FileUrl? urls) => this(urls: urls);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `FileUrls(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// FileUrls(...).copyWith(id: 12, name: "My name")
  /// ````
  FileUrls call({
    Object? url = const $CopyWithPlaceholder(),
    Object? urls = const $CopyWithPlaceholder(),
  }) {
    return FileUrls(
      url: url == const $CopyWithPlaceholder() || url == null
          ? _value.url
          // ignore: cast_nullable_to_non_nullable
          : url as String,
      urls: urls == const $CopyWithPlaceholder()
          ? _value.urls
          // ignore: cast_nullable_to_non_nullable
          : urls as FileUrl?,
    );
  }
}

extension $FileUrlsCopyWith on FileUrls {
  /// Returns a callable class that can be used as follows: `instanceOfFileUrls.copyWith(...)` or like so:`instanceOfFileUrls.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$FileUrlsCWProxy get copyWith => _$FileUrlsCWProxyImpl(this);
}

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
