import '../dtos/create_comment_dto.dart';
import '../utils/api_client.dart';

/// CommentApi
class CommentApi {
  /// _apiClient
  final ApiClient _apiClient;

  Future<void> like(String id) async {
    await _apiClient.put(Uri.parse('/comments/$id/like'));
  }

  Future<void> create(CreateCommentDto dto) async {
    await _apiClient.post(Uri.parse('/comments'), body: dto);
  }

  CommentApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
