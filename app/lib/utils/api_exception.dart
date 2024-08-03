import 'package:copy_with_extension/copy_with_extension.dart';

import '../models/data.dart';

part 'api_exception.g.dart';

/// ApiException
@CopyWith()
class ApiException implements Exception {
  /// code
  final int? code;

  /// status
  final int status;

  /// message
  final String message;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ApiException &&
          runtimeType == other.runtimeType &&
          code == other.code &&
          status == other.status &&
          message == other.message;

  ApiException.withData(Data data)
      : code = data.code,
        status = data.status,
        message = data.message;

  ApiException({
    this.code,
    this.status = 500,
    this.message = 'Internal server error',
  });

  factory ApiException.internalServerError() => ApiException();

  factory ApiException.unknownError() => ApiException(
        message: 'Unknown error',
      );

  factory ApiException.notFound() => ApiException(
        status: 404,
        message: 'Not found',
      );

  factory ApiException.invalidParameter() => ApiException(
        status: 400,
        message: 'Invalid parameter',
      );

  factory ApiException.unauthorized() => ApiException(
        status: 401,
        message: 'Unauthorized',
      );

  factory ApiException.forbidden() => ApiException(
        status: 403,
        message: 'Forbidden',
      );

  factory ApiException.conflict() => ApiException(
        status: 409,
        message: 'Conflict',
      );

  factory ApiException.tooManyRequests() => ApiException(
        status: 429,
        message: 'Too many requests',
      );

  @override
  int get hashCode => code.hashCode ^ status.hashCode ^ message.hashCode;

  @override
  String toString() {
    return 'ApiException{code: $code, status: $status, message: $message}';
  }
}
