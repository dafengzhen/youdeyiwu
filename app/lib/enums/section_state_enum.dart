import 'package:json_annotation/json_annotation.dart';

enum SectionStateEnum {
  /// show (Default).
  @JsonValue('SHOW')
  show,

  /// hide (Forum Administrator, Access to Section Management).
  @JsonValue('HIDE')
  hide,

  /// lock (The forum administrator, section manager, and users with access keys can access).
  @JsonValue('LOCK')
  lock,

  /// allow (The forum administrator, section administrator, and whitelisted users have access).
  @JsonValue('ALLOW')
  allow,

  /// block (The forum administrators and section moderators have access,
  /// while users on the blacklist are not allowed to access).
  @JsonValue('BLOCK')
  block,

  /// visible after login.
  @JsonValue('VISIBLE_AFTER_LOGIN')
  visibleAfterLogin
}
