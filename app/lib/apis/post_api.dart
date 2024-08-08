import 'package:http/src/response.dart';

import '../dtos/query_parameters_dto.dart';
import '../dtos/save_post_dto.dart';
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

  Future<Post> queryDetails(String id, {QueryParametersDto? dto}) async {
    final response = await _apiClient.get(
      Uri.parse('/posts/$id/details').replace(queryParameters: dto?.toJson()),
    );
    var post = Post.withResponse(response);
    return post;
  }

  Future<void> like(String id) async {
    await _apiClient.put(Uri.parse('/posts/$id/like'));
  }

  Future<Response> save(
    String? id, {
    required SavePostDto dto,
    bool? isCreate = true,
  }) async {
    if (isCreate == true) {
      return await _apiClient.post(Uri.parse('/posts'), body: dto);
    } else {
      return await _apiClient.put(Uri.parse('/posts/$id'), body: dto);
    }
  }

  PostApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
