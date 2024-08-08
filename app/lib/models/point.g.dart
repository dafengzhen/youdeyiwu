// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'point.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$PointCWProxy {
  Point id(int id);

  Point deleted(bool deleted);

  Point points(int points);

  Point minPoints(int minPoints);

  Point maxPoints(int maxPoints);

  Point user(User user);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Point(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Point(...).copyWith(id: 12, name: "My name")
  /// ````
  Point call({
    int? id,
    bool? deleted,
    int? points,
    int? minPoints,
    int? maxPoints,
    User? user,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfPoint.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfPoint.copyWith.fieldName(...)`
class _$PointCWProxyImpl implements _$PointCWProxy {
  const _$PointCWProxyImpl(this._value);

  final Point _value;

  @override
  Point id(int id) => this(id: id);

  @override
  Point deleted(bool deleted) => this(deleted: deleted);

  @override
  Point points(int points) => this(points: points);

  @override
  Point minPoints(int minPoints) => this(minPoints: minPoints);

  @override
  Point maxPoints(int maxPoints) => this(maxPoints: maxPoints);

  @override
  Point user(User user) => this(user: user);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Point(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Point(...).copyWith(id: 12, name: "My name")
  /// ````
  Point call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? points = const $CopyWithPlaceholder(),
    Object? minPoints = const $CopyWithPlaceholder(),
    Object? maxPoints = const $CopyWithPlaceholder(),
    Object? user = const $CopyWithPlaceholder(),
  }) {
    return Point(
      id: id == const $CopyWithPlaceholder() || id == null
          ? _value.id
          // ignore: cast_nullable_to_non_nullable
          : id as int,
      deleted: deleted == const $CopyWithPlaceholder() || deleted == null
          ? _value.deleted
          // ignore: cast_nullable_to_non_nullable
          : deleted as bool,
      points: points == const $CopyWithPlaceholder() || points == null
          ? _value.points
          // ignore: cast_nullable_to_non_nullable
          : points as int,
      minPoints: minPoints == const $CopyWithPlaceholder() || minPoints == null
          ? _value.minPoints
          // ignore: cast_nullable_to_non_nullable
          : minPoints as int,
      maxPoints: maxPoints == const $CopyWithPlaceholder() || maxPoints == null
          ? _value.maxPoints
          // ignore: cast_nullable_to_non_nullable
          : maxPoints as int,
      user: user == const $CopyWithPlaceholder() || user == null
          ? _value.user
          // ignore: cast_nullable_to_non_nullable
          : user as User,
    );
  }
}

extension $PointCopyWith on Point {
  /// Returns a callable class that can be used as follows: `instanceOfPoint.copyWith(...)` or like so:`instanceOfPoint.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$PointCWProxy get copyWith => _$PointCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Point _$PointFromJson(Map<String, dynamic> json) => Point(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      points: (json['points'] as num).toInt(),
      minPoints: (json['minPoints'] as num).toInt(),
      maxPoints: (json['maxPoints'] as num).toInt(),
      user: User.fromJson(json['user'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$PointToJson(Point instance) => <String, dynamic>{
      'id': instance.id,
      'deleted': instance.deleted,
      'points': instance.points,
      'minPoints': instance.minPoints,
      'maxPoints': instance.maxPoints,
      'user': instance.user,
    };
