// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'pageable.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PageableCWProxy {
  Pageable page(int page);

  Pageable size(int size);

  Pageable previous(bool previous);

  Pageable next(bool next);

  Pageable pages(int pages);

  Pageable elements(int? elements);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Pageable(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Pageable(...).copyWith(id: 12, name: "My name")
  /// ````
  Pageable call({
    int? page,
    int? size,
    bool? previous,
    bool? next,
    int? pages,
    int? elements,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPageable.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPageable.copyWith.fieldName(...)`
class _$PageableCWProxyImpl implements _$PageableCWProxy {
  const _$PageableCWProxyImpl(this._value);

  final Pageable _value;

  @override
  Pageable page(int page) => this(page: page);

  @override
  Pageable size(int size) => this(size: size);

  @override
  Pageable previous(bool previous) => this(previous: previous);

  @override
  Pageable next(bool next) => this(next: next);

  @override
  Pageable pages(int pages) => this(pages: pages);

  @override
  Pageable elements(int? elements) => this(elements: elements);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Pageable(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Pageable(...).copyWith(id: 12, name: "My name")
  /// ````
  Pageable call({
    Object? page = const $CopyWithPlaceholder(),
    Object? size = const $CopyWithPlaceholder(),
    Object? previous = const $CopyWithPlaceholder(),
    Object? next = const $CopyWithPlaceholder(),
    Object? pages = const $CopyWithPlaceholder(),
    Object? elements = const $CopyWithPlaceholder(),
  }) {
    return Pageable(
      page: page == const $CopyWithPlaceholder() || page == null
          ? _value.page
          // ignore: cast_nullable_to_non_nullable
          : page as int,
      size: size == const $CopyWithPlaceholder() || size == null
          ? _value.size
          // ignore: cast_nullable_to_non_nullable
          : size as int,
      previous: previous == const $CopyWithPlaceholder() || previous == null
          ? _value.previous
          // ignore: cast_nullable_to_non_nullable
          : previous as bool,
      next: next == const $CopyWithPlaceholder() || next == null
          ? _value.next
          // ignore: cast_nullable_to_non_nullable
          : next as bool,
      pages: pages == const $CopyWithPlaceholder() || pages == null
          ? _value.pages
          // ignore: cast_nullable_to_non_nullable
          : pages as int,
      elements: elements == const $CopyWithPlaceholder()
          ? _value.elements
          // ignore: cast_nullable_to_non_nullable
          : elements as int?,
    );
  }
}

extension $PageableCopyWith on Pageable {
  /// Returns a callable class that can be used as follows: `instanceOfPageable.copyWith(...)` or like so:`instanceOfPageable.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PageableCWProxy get copyWith => _$PageableCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Pageable _$PageableFromJson(Map<String, dynamic> json) => Pageable(
      page: (json['page'] as num).toInt(),
      size: (json['size'] as num).toInt(),
      previous: json['previous'] as bool,
      next: json['next'] as bool,
      pages: (json['pages'] as num).toInt(),
      elements: (json['elements'] as num?)?.toInt(),
    );

Map<String, dynamic> _$PageableToJson(Pageable instance) {
  final val = <String, dynamic>{
    'page': instance.page,
    'size': instance.size,
    'previous': instance.previous,
    'next': instance.next,
    'pages': instance.pages,
  };

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('elements', instance.elements);
  return val;
}
