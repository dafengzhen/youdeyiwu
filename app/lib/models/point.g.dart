// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'point.dart';

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
