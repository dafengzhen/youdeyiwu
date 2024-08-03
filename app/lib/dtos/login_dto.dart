import 'dart:convert';

import 'package:json_annotation/json_annotation.dart';

part 'login_dto.g.dart';

/// LoginDto
@JsonSerializable()
class LoginDto {
  /// username
  final String username;

  /// password
  final String password;

  factory LoginDto.fromJsonString(String json) =>
      LoginDto.fromJson(jsonDecode(json));

  factory LoginDto.fromJson(Map<String, dynamic> json) =>
      _$LoginDtoFromJson(json);

  Map<String, dynamic> toJson() => _$LoginDtoToJson(this);

  const LoginDto({required this.username, required this.password});

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is LoginDto &&
          runtimeType == other.runtimeType &&
          username == other.username &&
          password == other.password;

  @override
  int get hashCode => username.hashCode ^ password.hashCode;

  @override
  String toString() {
    return 'LoginDto{username: $username, password: $password}';
  }
}
