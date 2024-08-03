import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';
import 'permission.dart';

part 'role.g.dart';

/// Role
@JsonSerializable()
class Role extends Base {
  /// name
  final String name;

  /// overview
  final String? overview;

  /// sort
  final int sort;

  /// display
  final bool display;

  /// permissions
  final Set<Permission>? permissions;

  const Role({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.sort,
    required this.display,
    this.overview,
    this.permissions,
  });

  factory Role.withResponse(Response response) {
    return Role.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Role.fromJsonString(String json) => Role.fromJson(jsonDecode(json));

  factory Role.fromJson(Map<String, dynamic> json) => _$RoleFromJson(json);

  Map<String, dynamic> toJson() => _$RoleToJson(this);

  @override
  String toString() {
    return 'Role{name: $name, overview: $overview, sort: $sort, display: $display, permissions: $permissions}';
  }
}
