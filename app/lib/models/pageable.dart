import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pageable.g.dart';

/// Pageable
@CopyWith()
@JsonSerializable()
class Pageable {
  /// page
  final int page;

  /// size
  final int size;

  /// previous
  final bool previous;

  /// next
  final bool next;

  /// pages
  final int pages;

  /// elements
  final int? elements;

  const Pageable({
    required this.page,
    required this.size,
    required this.previous,
    required this.next,
    required this.pages,
    this.elements,
  });

  factory Pageable.withResponse(Response response) {
    return Pageable.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Pageable.fromJsonString(String json) =>
      Pageable.fromJson(jsonDecode(json));

  factory Pageable.fromJson(Map<String, dynamic> json) =>
      _$PageableFromJson(json);

  Map<String, dynamic> toJson() => _$PageableToJson(this);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Pageable &&
          runtimeType == other.runtimeType &&
          page == other.page &&
          size == other.size &&
          previous == other.previous &&
          next == other.next &&
          pages == other.pages &&
          elements == other.elements;

  @override
  int get hashCode =>
      page.hashCode ^
      size.hashCode ^
      previous.hashCode ^
      next.hashCode ^
      pages.hashCode ^
      elements.hashCode;

  @override
  String toString() {
    return 'Pageable{page: $page, size: $size, previous: $previous, next: $next, pages: $pages, elements: $elements}';
  }
}
