import 'package:json_annotation/json_annotation.dart';

enum PostStateEnum {
  /// show (Default).
  @JsonValue('SHOW')
  show,

  /// hide (Forum Administrator, Access to Section Management).
  @JsonValue('HIDE')
  hide,

  /// lock (The forum administrators, section moderators,
  /// as well as the thread authors and users with access keys, can access).
  @JsonValue('LOCK')
  lock,

  /// allow (The forum administrators, section moderators,
  /// as well as the thread authors and whitelisted users can access).
  @JsonValue('ALLOW')
  allow,

  /// block (Forum administrators, section administrators,
  /// and post authors have access, while users on the blacklist cannot access).
  @JsonValue('BLOCK')
  block,

  /// visible after login.
  @JsonValue('VISIBLE_AFTER_LOGIN')
  visibleAfterLogin
}
