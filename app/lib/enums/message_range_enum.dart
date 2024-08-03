import 'package:json_annotation/json_annotation.dart';

enum MessageRangeEnum {
  /// all user.
  @JsonValue('ALL_USER')
  allUser,

  /// user.
  @JsonValue('USER')
  user,
}
