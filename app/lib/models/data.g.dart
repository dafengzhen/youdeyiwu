// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'data.dart';

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Data _$DataFromJson(Map<String, dynamic> json) => Data(
      status: (json['status'] as num).toInt(),
      code: (json['code'] as num?)?.toInt(),
      error: json['error'] as String?,
      data: json['data'],
      message: json['message'] as String?,
    );

Map<String, dynamic> _$DataToJson(Data instance) {
  final val = <String, dynamic>{};

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('code', instance.code);
  val['status'] = instance.status;
  val['message'] = instance.message;
  writeNotNull('error', instance.error);
  writeNotNull('data', instance.data);
  return val;
}
