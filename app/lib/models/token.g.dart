// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'token.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Token _$TokenFromJson(Map<String, dynamic> json) => Token(
      id: (json['id'] as num).toInt(),
      alias: json['alias'] as String,
      token: json['token'] as String,
      expDays: (json['expDays'] as num).toInt(),
    );

Map<String, dynamic> _$TokenToJson(Token instance) => <String, dynamic>{
      'id': instance.id,
      'alias': instance.alias,
      'token': instance.token,
      'expDays': instance.expDays,
    };
