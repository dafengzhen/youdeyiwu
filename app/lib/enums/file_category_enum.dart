import 'package:json_annotation/json_annotation.dart';

enum FileCategoryEnum {
  /// image.
  @JsonValue('IMAGE')
  image,

  /// zip.
  @JsonValue('ZIP')
  zip,

  /// text.
  @JsonValue('TEXT')
  text
}
