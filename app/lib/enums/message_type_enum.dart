import 'package:json_annotation/json_annotation.dart';

enum MessageTypeEnum {
  /// system.
  @JsonValue('SYSTEM')
  system,

  /// global message.
  @JsonValue('GLOBAL_MESSAGE')
  globalMessage,

  /// message.
  @JsonValue('MESSAGE')
  message,

  /// config.
  @JsonValue('CONFIG')
  config,

  /// file.
  @JsonValue('FILE')
  file,

  /// user.
  @JsonValue('USER')
  user,

  /// role.
  @JsonValue('ROLE')
  role,

  /// permission.
  @JsonValue('PERMISSION')
  permission,

  /// menu.
  @JsonValue('MENU')
  menu,

  /// submenu.
  @JsonValue('SUBMENU')
  submenu,

  /// action.
  @JsonValue('ACTION')
  action,

  /// section.
  @JsonValue('SECTION')
  section,

  /// section group.
  @JsonValue('SECTION_GROUP')
  sectionGroup,

  /// post.
  @JsonValue('POST')
  post,

  /// tag.
  @JsonValue('TAG')
  tag,

  /// tag group.
  @JsonValue('TAG_GROUP')
  tagGroup,

  /// comment.
  @JsonValue('COMMENT')
  comment,

  /// reply.
  @JsonValue('REPLY')
  reply,

  /// point.
  @JsonValue('POINT')
  point
}
