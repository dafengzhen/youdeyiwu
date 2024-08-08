import '../dtos/query_parameters_dto.dart';
import '../enums/message_range_enum.dart';
import '../models/message.dart';
import '../models/page.dart';
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

  Future<void> delete(String id, MessageRangeEnum range) async {
    await _apiClient.delete(Uri.parse('/messages/$id'));
  }

  Future<void> read(String id, MessageRangeEnum range) async {
    if (range == MessageRangeEnum.allUser) {
      await _apiClient.put(Uri.parse('/messages/$id/global-messages/state'));
    } else {
      await _apiClient.put(Uri.parse('/messages/$id/state'));
    }
  }

  MessageApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
