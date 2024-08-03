import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import 'base.dart';
import 'user.dart';

part 'point.g.dart';

/// Point
@JsonSerializable()
class Point extends Base {
  /// points
  final int points;

  /// min points
  final int minPoints;

  /// max points
  final int maxPoints;

  /// user
  final User user;

  const Point({
    required super.id,
    required super.deleted,
    required this.points,
    required this.minPoints,
    required this.maxPoints,
    required this.user,
  });

  factory Point.withResponse(Response response) {
    return Point.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Point.fromJsonString(String json) => Point.fromJson(jsonDecode(json));

  factory Point.fromJson(Map<String, dynamic> json) => _$PointFromJson(json);

  Map<String, dynamic> toJson() => _$PointToJson(this);

  @override
  String toString() {
    return 'Point{points: $points, minPoints: $minPoints, maxPoints: $maxPoints, user: $user}';
  }
}
