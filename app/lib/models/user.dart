import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';
import 'role.dart';
import 'section.dart';

part 'user.g.dart';

/// User
@JsonSerializable()
class User extends Base {
  /// alias
  final String? alias;

  /// avatar
  final String? avatar;

  /// oneSentence
  final String? oneSentence;

  /// username
  final String username;

  /// email
  final String? email;

  /// temporaryStorage
  final String? temporaryStorage;

  /// lastLoginTime
  final String lastLoginTime;

  /// root
  final bool? root;

  /// noPostingAllowed
  final bool? noPostingAllowed;

  /// disableComments
  final bool? disableComments;

  /// disableReplies
  final bool? disableReplies;

  /// noPostingReason
  final String? noPostingReason;

  /// commentDisableReason
  final String? commentDisableReason;

  /// replyDisableReason
  final String? replyDisableReason;

  /// accountNonExpired
  final bool accountNonExpired;

  /// credentialsNonExpired
  final bool credentialsNonExpired;

  /// accountNonLocked
  final bool accountNonLocked;

  /// enabled
  final bool enabled;

  /// roles
  final Set<Role>? roles;

  /// sections
  final Set<Section>? sections;

  const User({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.username,
    required this.lastLoginTime,
    required this.accountNonExpired,
    required this.credentialsNonExpired,
    required this.accountNonLocked,
    required this.enabled,
    this.alias,
    this.avatar,
    this.oneSentence,
    this.email,
    this.temporaryStorage,
    this.root,
    this.noPostingAllowed,
    this.disableComments,
    this.disableReplies,
    this.noPostingReason,
    this.commentDisableReason,
    this.replyDisableReason,
    this.roles,
    this.sections,
  });

  factory User.withResponse(Response response) {
    return User.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory User.fromJsonString(String json) => User.fromJson(jsonDecode(json));

  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);

  Map<String, dynamic> toJson() => _$UserToJson(this);

  @override
  String toString() {
    return 'User{alias: $alias, avatar: $avatar, oneSentence: $oneSentence, username: $username, email: $email, temporaryStorage: $temporaryStorage, lastLoginTime: $lastLoginTime, root: $root, noPostingAllowed: $noPostingAllowed, disableComments: $disableComments, disableReplies: $disableReplies, noPostingReason: $noPostingReason, commentDisableReason: $commentDisableReason, replyDisableReason: $replyDisableReason, accountNonExpired: $accountNonExpired, credentialsNonExpired: $credentialsNonExpired, accountNonLocked: $accountNonLocked, enabled: $enabled, roles: $roles, sections: $sections}';
  }
}
