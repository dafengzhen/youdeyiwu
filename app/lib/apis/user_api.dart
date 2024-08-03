import 'dart:convert';

import 'package:shared_preferences/shared_preferences.dart';
import 'package:youdeyiwu_app/utils/api_exception.dart';

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
    if (response.bodyBytes.isEmpty) {
      await removeToken(_sharedPreferences);
      return null;
    }

    final user = User.withResponse(response);
    return user;
  }

  Future<User> queryDetails({String? id}) async {
    String id0;
    if (id != null && id.isNotEmpty) {
      id0 = id;
    } else {
      var credentials = hasToken(_sharedPreferences);
      if (credentials != null) {
        id0 = credentials.id.toString();
      } else {
        throw ApiException.invalidParameter()
            .copyWith(message: "userId does not exist");
      }
    }

    final response = await _apiClient.get(Uri.parse('/users/$id0/details'));
    User user = User.withResponse(response);
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
