// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'api_exception.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$ApiExceptionCWProxy {
  ApiException code(int? code);

  ApiException status(int status);

  ApiException message(String message);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `ApiException(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// ApiException(...).copyWith(id: 12, name: "My name")
  /// ````
  ApiException call({
    int? code,
    int? status,
    String? message,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfApiException.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfApiException.copyWith.fieldName(...)`
class _$ApiExceptionCWProxyImpl implements _$ApiExceptionCWProxy {
  const _$ApiExceptionCWProxyImpl(this._value);

  final ApiException _value;

  @override
  ApiException code(int? code) => this(code: code);

  @override
  ApiException status(int status) => this(status: status);

  @override
  ApiException message(String message) => this(message: message);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `ApiException(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// ApiException(...).copyWith(id: 12, name: "My name")
  /// ````
  ApiException call({
    Object? code = const $CopyWithPlaceholder(),
    Object? status = const $CopyWithPlaceholder(),
    Object? message = const $CopyWithPlaceholder(),
  }) {
    return ApiException(
      code: code == const $CopyWithPlaceholder()
          ? _value.code
          // ignore: cast_nullable_to_non_nullable
          : code as int?,
      status: status == const $CopyWithPlaceholder() || status == null
          ? _value.status
          // ignore: cast_nullable_to_non_nullable
          : status as int,
      message: message == const $CopyWithPlaceholder() || message == null
          ? _value.message
          // ignore: cast_nullable_to_non_nullable
          : message as String,
    );
  }
}

extension $ApiExceptionCopyWith on ApiException {
  /// Returns a callable class that can be used as follows: `instanceOfApiException.copyWith(...)` or like so:`instanceOfApiException.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$ApiExceptionCWProxy get copyWith => _$ApiExceptionCWProxyImpl(this);
}
