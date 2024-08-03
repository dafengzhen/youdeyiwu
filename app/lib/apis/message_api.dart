import 'package:youdeyiwu_app/utils/tools.dart';

import '../dtos/query_parameters_dto.dart';
import '../models/message.dart';
import '../models/page.dart';
import '../models/post.dart';
import '../utils/api_client.dart';

/// MessageApi
class MessageApi {
  /// _apiClient
  final ApiClient _apiClient;

  Future<Page<Message>> queryPosts({QueryParametersDto? dto}) async {
    final response = await _apiClient.get(
      Uri.parse('/messages').replace(queryParameters: dto?.toJson()),
    );
    Page<Message> page = Page.withResponse(
      response,
      (json) => Message.fromJson(json as Map<String, dynamic>),
    );
    return page;
  }

  MessageApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
