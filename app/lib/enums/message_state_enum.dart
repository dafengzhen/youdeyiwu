import 'package:json_annotation/json_annotation.dart';

enum MessageStateEnum {
  /// unread.
  @JsonValue('UNREAD')
  unread,

  /// read.
  @JsonValue('READ')
  read,
}
