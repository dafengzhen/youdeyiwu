// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'page.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PageCWProxy<T> {
  Page<T> content(List<T> content);

  Page<T> pageable(Pageable pageable);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Page<T>(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Page<T>(...).copyWith(id: 12, name: "My name")
  /// ````
  Page<T> call({
    List<T>? content,
    Pageable? pageable,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPage.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPage.copyWith.fieldName(...)`
class _$PageCWProxyImpl<T> implements _$PageCWProxy<T> {
  const _$PageCWProxyImpl(this._value);

  final Page<T> _value;

  @override
  Page<T> content(List<T> content) => this(content: content);

  @override
  Page<T> pageable(Pageable pageable) => this(pageable: pageable);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Page<T>(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Page<T>(...).copyWith(id: 12, name: "My name")
  /// ````
  Page<T> call({
    Object? content = const $CopyWithPlaceholder(),
    Object? pageable = const $CopyWithPlaceholder(),
  }) {
    return Page<T>(
      content: content == const $CopyWithPlaceholder() || content == null
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as List<T>,
      pageable: pageable == const $CopyWithPlaceholder() || pageable == null
          ? _value.pageable
          // ignore: cast_nullable_to_non_nullable
          : pageable as Pageable,
    );
  }
}

extension $PageCopyWith<T> on Page<T> {
  /// Returns a callable class that can be used as follows: `instanceOfPage.copyWith(...)` or like so:`instanceOfPage.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PageCWProxy<T> get copyWith => _$PageCWProxyImpl<T>(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Page<T> _$PageFromJson<T>(
  Map<String, dynamic> json,
  T Function(Object? json) fromJsonT,
) =>
    Page<T>(
      content: (json['content'] as List<dynamic>).map(fromJsonT).toList(),
      pageable: Pageable.fromJson(json['pageable'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$PageToJson<T>(
  Page<T> instance,
  Object? Function(T value) toJsonT,
) =>
    <String, dynamic>{
      'content': instance.content.map(toJsonT).toList(),
      'pageable': instance.pageable,
    };
