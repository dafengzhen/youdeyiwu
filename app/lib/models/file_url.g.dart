// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'file_url.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$FileUrlCWProxy {
  FileUrl defaultUrl(String defaultUrl);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `FileUrl(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// FileUrl(...).copyWith(id: 12, name: "My name")
  /// ````
  FileUrl call({
    String? defaultUrl,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfFileUrl.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfFileUrl.copyWith.fieldName(...)`
class _$FileUrlCWProxyImpl implements _$FileUrlCWProxy {
  const _$FileUrlCWProxyImpl(this._value);

  final FileUrl _value;

  @override
  FileUrl defaultUrl(String defaultUrl) => this(defaultUrl: defaultUrl);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `FileUrl(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// FileUrl(...).copyWith(id: 12, name: "My name")
  /// ````
  FileUrl call({
    Object? defaultUrl = const $CopyWithPlaceholder(),
  }) {
    return FileUrl(
      defaultUrl:
          defaultUrl == const $CopyWithPlaceholder() || defaultUrl == null
              ? _value.defaultUrl
              // ignore: cast_nullable_to_non_nullable
              : defaultUrl as String,
    );
  }
}

extension $FileUrlCopyWith on FileUrl {
  /// Returns a callable class that can be used as follows: `instanceOfFileUrl.copyWith(...)` or like so:`instanceOfFileUrl.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$FileUrlCWProxy get copyWith => _$FileUrlCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

FileUrl _$FileUrlFromJson(Map<String, dynamic> json) => FileUrl(
      defaultUrl: json['default'] as String,
    );

Map<String, dynamic> _$FileUrlToJson(FileUrl instance) => <String, dynamic>{
      'default': instance.defaultUrl,
    };
