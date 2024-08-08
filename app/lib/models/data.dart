import 'dart:convert';

import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

import '../utils/tools.dart';

part 'data.g.dart';

/// Data
@CopyWith()
@JsonSerializable()
class Data {
  /// code
  final int? code;

  /// status
  final int status;

  /// message
  final String message;

  /// error
  final String? error;

  /// data
  final dynamic data;

  const Data({
    required this.status,
    this.code,
    this.error,
    this.data,
    String? message,
  }) : message = message ?? error ?? 'No messages available';

  factory Data.fromJsonString(String json) {
    logDebug(
      json,
      sourceType: SourceType.jsonString,
    );
    return Data.fromJson(jsonDecode(json));
  }

  factory Data.fromJson(Map<String, dynamic> json) {
    logDebug(
      json,
      sourceType: SourceType.json,
    );
    return _$DataFromJson(json);
  }

  Map<String, dynamic> toJson() => _$DataToJson(this);

  @override
  String toString() {
    return 'Data{code: $code, status: $status, message: $message, data: $data}';
  }
}
