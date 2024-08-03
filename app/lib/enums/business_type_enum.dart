import 'package:json_annotation/json_annotation.dart';

enum BusinessTypeEnum {
  /// system.
  @JsonValue('SYSTEM')
  system,

  /// section.
  @JsonValue('SECTION')
  section,

  /// post.
  @JsonValue('POST')
  post,

  /// tag.
  @JsonValue('TAG')
  tag,

  /// comment.
  @JsonValue('COMMENT')
  comment,

  /// reply.
  @JsonValue('REPLY')
  reply,

  /// message.
  @JsonValue('MESSAGE')
  message,

  /// user.
  @JsonValue('USER')
  user,

  /// other.
  @JsonValue('OTHER')
  other
}
