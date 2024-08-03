import 'dart:convert';

import 'package:http/http.dart' as http;
import 'package:shared_preferences/shared_preferences.dart';

import '../configs/configs.dart';
import '../models/data.dart';
import 'api_exception.dart';
import 'constants.dart';
import 'tools.dart';

/// ApiClient
class ApiClient extends http.BaseClient {
  /// baseUri
  final Uri _baseUri = Uri.parse(appApiServer);

  /// _inner
  final http.Client _inner;

  /// _sharedPreferences
  final SharedPreferences _sharedPreferences;

  ApiClient({http.Client? client, required SharedPreferences sharedPreferences})
      : _inner = client ?? http.Client(),
        _sharedPreferences = sharedPreferences;

  @override
  Future<http.StreamedResponse> send(http.BaseRequest request) {
    final tokenData = hasToken(_sharedPreferences);
    if (tokenData != null) {
      request.headers
          .putIfAbsent(authHeader, () => '$bearerToken ${tokenData.token}');
    }

    return _inner.send(request).then((response) async {
      if (response.statusCode >= 200 && response.statusCode < 300) {
        return response;
      } else {
        final contentType = response.headers[contentTypeHeader];
        if (contentType != null && contentType.contains(jsonContentType)) {
          final responseBody = await response.stream.bytesToString();
          final data = Data.fromJsonString(responseBody);
          if (data.status == 401 ||
              (data.code != null && (data.code == 4010 || data.code == 4011))) {
            await _removeToken();
          }
          throw ApiException.withData(data);
        }
        return response;
      }
    });
  }

  @override
  Future<http.Response> get(Uri url, {Map<String, String>? headers}) {
    return super.get(
      _resolveUri(url),
      headers: headers,
    );
  }

  @override
  Future<http.Response> post(
    Uri url, {
    Map<String, String>? headers,
    dynamic body,
    Encoding? encoding,
  }) {
    final newHeaders = (headers ?? {})..[contentTypeHeader] ??= jsonContentType;
    final newBody = body != null ? jsonEncode(body.toJson()) : body;
    return super.post(
      _resolveUri(url),
      headers: newHeaders,
      body: newBody,
      encoding: encoding,
    );
  }

  @override
  Future<http.Response> put(
    Uri url, {
    Map<String, String>? headers,
    dynamic body,
    Encoding? encoding,
  }) {
    final newHeaders = (headers ?? {})..[contentTypeHeader] ??= jsonContentType;
    final newBody = body != null ? jsonEncode(body.toJson()) : body;
    return super.put(
      _resolveUri(url),
      headers: newHeaders,
      body: newBody,
      encoding: encoding,
    );
  }

  @override
  Future<http.Response> patch(
    Uri url, {
    Map<String, String>? headers,
    dynamic body,
    Encoding? encoding,
  }) {
    final newHeaders = (headers ?? {})..[contentTypeHeader] ??= jsonContentType;
    final newBody = body != null ? jsonEncode(body.toJson()) : body;
    return super.patch(
      _resolveUri(url),
      headers: newHeaders,
      body: newBody,
      encoding: encoding,
    );
  }

  @override
  Future<http.Response> delete(
    Uri url, {
    Map<String, String>? headers,
    dynamic body,
    Encoding? encoding,
  }) {
    final newHeaders = (headers ?? {})..[contentTypeHeader] ??= jsonContentType;
    final newBody = body != null ? jsonEncode(body.toJson()) : body;
    return super.delete(
      _resolveUri(url),
      headers: newHeaders,
      body: newBody,
      encoding: encoding,
    );
  }

  @override
  void close() {
    _inner.close();
  }

  Uri _resolveUri(Uri url) {
    return _baseUri.resolve(_baseUri.path + Uri.encodeFull(url.toString()));
  }

  Future<bool> _removeToken() {
    return _sharedPreferences.remove(tk);
  }
}
