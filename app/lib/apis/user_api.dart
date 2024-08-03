import 'dart:convert';

import 'package:shared_preferences/shared_preferences.dart';

import '../dtos/login_dto.dart';
import '../models/token.dart';
import '../models/user.dart';
import '../utils/api_client.dart';
import '../utils/constants.dart';
import '../utils/tools.dart';

/// UserApi
class UserApi {
  /// _apiClient
  final ApiClient _apiClient;

  /// _sharedPreferences
  final SharedPreferences _sharedPreferences;

  Future<Token> login({
    required LoginDto dto,
    bool isReqLogin = true,
  }) async {
    final response = await _apiClient.post(
      Uri.parse(isReqLogin ? '/users/login' : '/users/register'),
      body: dto,
    );
    final token = Token.withResponse(response);
    await _setToken(token);
    return token;
  }

  Future<User?> loginInfo() async {
    var credentials = hasToken(_sharedPreferences);
    if (credentials == null) {
      return null;
    }

    final response = await _apiClient.get(Uri.parse('/users/login-info'));
    final user = User.withResponse(response);
    return user;
  }

  UserApi({
    required ApiClient apiClient,
    required SharedPreferences sharedPreferences,
  })  : _apiClient = apiClient,
        _sharedPreferences = sharedPreferences;

  Future<bool> _setToken(Token value) async {
    return _sharedPreferences.setString(
      tk,
      jsonEncode(value),
    );
  }
}
