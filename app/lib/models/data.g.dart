// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'data.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$DataCWProxy {
  Data status(int status);

  Data code(int? code);

  Data error(String? error);

  Data data(dynamic data);

  Data message(String? message);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Data(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Data(...).copyWith(id: 12, name: "My name")
  /// ````
  Data call({
    int? status,
    int? code,
    String? error,
    dynamic data,
    String? message,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfData.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfData.copyWith.fieldName(...)`
class _$DataCWProxyImpl implements _$DataCWProxy {
  const _$DataCWProxyImpl(this._value);

  final Data _value;

  @override
  Data status(int status) => this(status: status);

  @override
  Data code(int? code) => this(code: code);

  @override
  Data error(String? error) => this(error: error);

  @override
  Data data(dynamic data) => this(data: data);

  @override
  Data message(String? message) => this(message: message);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Data(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Data(...).copyWith(id: 12, name: "My name")
  /// ````
  Data call({
    Object? status = const $CopyWithPlaceholder(),
    Object? code = const $CopyWithPlaceholder(),
    Object? error = const $CopyWithPlaceholder(),
    Object? data = const $CopyWithPlaceholder(),
    Object? message = const $CopyWithPlaceholder(),
  }) {
    return Data(
      status: status == const $CopyWithPlaceholder() || status == null
          ? _value.status
          // ignore: cast_nullable_to_non_nullable
          : status as int,
      code: code == const $CopyWithPlaceholder()
          ? _value.code
          // ignore: cast_nullable_to_non_nullable
          : code as int?,
      error: error == const $CopyWithPlaceholder()
          ? _value.error
          // ignore: cast_nullable_to_non_nullable
          : error as String?,
      data: data == const $CopyWithPlaceholder() || data == null
          ? _value.data
          // ignore: cast_nullable_to_non_nullable
          : data as dynamic,
      message: message == const $CopyWithPlaceholder()
          ? _value.message
          // ignore: cast_nullable_to_non_nullable
          : message as String?,
    );
  }
}

extension $DataCopyWith on Data {
  /// Returns a callable class that can be used as follows: `instanceOfData.copyWith(...)` or like so:`instanceOfData.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$DataCWProxy get copyWith => _$DataCWProxyImpl(this);
}

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
