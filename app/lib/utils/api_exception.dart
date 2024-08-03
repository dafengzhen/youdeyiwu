import '../models/data.dart';

/// ApiException
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

  factory ApiException.unknownError() => ApiException(
        message: 'Unknown error',
      );

  factory ApiException.internalServerError() => ApiException();

  factory ApiException.notFound() => ApiException(
        status: 404,
        message: 'Not found',
      );

  ApiException({
    this.code,
    this.status = 500,
    this.message = 'Internal server error',
  });

  @override
  int get hashCode => code.hashCode ^ status.hashCode ^ message.hashCode;

  @override
  String toString() {
    return 'ApiException{code: $code, status: $status, message: $message}';
  }
}
