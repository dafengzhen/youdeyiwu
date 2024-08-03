import 'package:json_annotation/json_annotation.dart';

enum FileTypeEnum {
  /// png.
  @JsonValue('PNG')
  png,

  /// jpg.
  @JsonValue('JPG')
  jpg
}
