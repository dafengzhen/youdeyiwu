import '../dtos/create_reply_dto.dart';
import '../utils/api_client.dart';

/// ReplyApi
class ReplyApi {
  /// _apiClient
  final ApiClient _apiClient;

  Future<void> like(String id) async {
    await _apiClient.put(Uri.parse('/replies/$id/like'));
  }

  Future<void> create(CreateReplyDto dto) async {
    await _apiClient.post(Uri.parse('/replies'), body: dto);
  }

  ReplyApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
