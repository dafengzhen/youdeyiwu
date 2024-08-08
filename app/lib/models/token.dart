import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'data.dart';

part 'token.g.dart';

/// Token
@CopyWith()
@JsonSerializable()
class Token {
  /// id.
  final int id;

  /// alias.
  final String alias;

  /// token.
  final String token;

  /// expDays.
  final int expDays;

  Token({
    required this.id,
    required this.alias,
    required this.token,
    required this.expDays,
  });

  factory Token.withResponse(Response response) {
    return Token.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Token.withData(Map<String, dynamic> json) {
    return Token.fromJson(Data.fromJson(json).data);
  }

  factory Token.fromJsonString(String json) => Token.fromJson(jsonDecode(json));

  factory Token.fromJson(Map<String, dynamic> json) => _$TokenFromJson(json);

  Map<String, dynamic> toJson() => _$TokenToJson(this);

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Token && runtimeType == other.runtimeType && id == other.id;

  @override
  int get hashCode => id.hashCode;

  @override
  String toString() {
    return 'Token{id: $id, alias: $alias, token: $token, expDays: $expDays}';
  }
}
