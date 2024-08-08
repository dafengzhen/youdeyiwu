import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import '../enums/matcher_type_enum.dart';
import '../enums/method_type_enum.dart';
import 'base.dart';
import 'role.dart';

part 'permission.g.dart';

/// Role
@CopyWith()
@JsonSerializable()
class Permission extends Base {
  /// name
  final String name;

  /// alias
  final String? alias;

  /// overview
  final String? overview;

  /// method
  final MethodTypeEnum method;

  /// type
  final MatcherTypeEnum type;

  /// caseInsensitive
  final bool caseInsensitive;

  /// sort
  final int sort;

  /// matcher
  final Permission? matcher;

  /// matchers
  final Set<Permission> matchers;

  /// role
  final Role? role;

  /// roles
  final Set<Role>? roles;

  const Permission({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.method,
    required this.type,
    required this.caseInsensitive,
    required this.sort,
    required this.matchers,
    this.alias,
    this.overview,
    this.matcher,
    this.role,
    this.roles,
  });

  factory Permission.withResponse(Response response) {
    return Permission.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Permission.fromJsonString(String json) =>
      Permission.fromJson(jsonDecode(json));

  factory Permission.fromJson(Map<String, dynamic> json) =>
      _$PermissionFromJson(json);

  Map<String, dynamic> toJson() => _$PermissionToJson(this);

  @override
  String toString() {
    return 'Permission{name: $name, alias: $alias, overview: $overview, method: $method, type: $type, caseInsensitive: $caseInsensitive, sort: $sort, matcher: $matcher, matchers: $matchers, role: $role, roles: $roles}';
  }
}
