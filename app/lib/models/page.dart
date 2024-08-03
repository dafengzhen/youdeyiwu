import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'pageable.dart';

part 'page.g.dart';

/// Page
@JsonSerializable(genericArgumentFactories: true)
class Page<T> {
  /// content
  final List<T> content;

  /// pageable
  final Pageable pageable;

  const Page({
    required this.content,
    required this.pageable,
  });

  factory Page.withResponse(
      Response response, T Function(Object? json) fromJsonT) {
    return Page.fromJson(
        jsonDecode(utf8.decode(response.bodyBytes)), fromJsonT);
  }

  factory Page.fromJsonString(
          String json, T Function(Object? json) fromJsonT) =>
      Page.fromJson(jsonDecode(json), fromJsonT);

  factory Page.fromJson(
    Map<String, dynamic> json,
    T Function(Object? json) fromJsonT,
  ) =>
      _$PageFromJson(json, fromJsonT);

  Map<String, dynamic> toJson(Object? Function(T value) toJsonT) =>
      _$PageToJson(this, toJsonT);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Page &&
          runtimeType == other.runtimeType &&
          content == other.content;

  @override
  int get hashCode => content.hashCode;

  @override
  String toString() {
    return 'Page{content: $content, pageable: $pageable}';
  }
}
