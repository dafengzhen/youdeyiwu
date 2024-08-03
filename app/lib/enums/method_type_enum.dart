import 'package:json_annotation/json_annotation.dart';

enum MethodTypeEnum {
  /// get.
  @JsonValue('GET')
  get,

  /// head.
  @JsonValue('HEAD')
  head,

  /// post.
  @JsonValue('POST')
  post,

  /// put.
  @JsonValue('PUT')
  put,

  /// patch.
  @JsonValue('PATCH')
  patch,

  /// delete.
  @JsonValue('DELETE')
  delete,

  /// options.
  @JsonValue('OPTIONS')
  options,

  /// trace.
  @JsonValue('TRACE')
  trace
}
