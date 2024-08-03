import 'package:youdeyiwu_app/utils/tools.dart';

import '../dtos/query_parameters_dto.dart';
import '../models/page.dart';
import '../models/post.dart';
import '../utils/api_client.dart';

/// PostApi
class PostApi {
  /// _apiClient
  final ApiClient _apiClient;

  Future<Page<Post>> queryPosts({QueryParametersDto? dto}) async {
    final response = await _apiClient.get(
      Uri.parse('/posts/select-all').replace(queryParameters: dto?.toJson()),
    );
    Page<Post> page = Page.withResponse(
      response,
      (json) => Post.fromJson(json as Map<String, dynamic>),
    );
    return page;
  }

  PostApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
