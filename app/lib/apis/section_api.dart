import 'package:youdeyiwu_app/models/section.dart';

import '../dtos/query_parameters_dto.dart';
import '../models/page.dart';
import '../utils/api_client.dart';

/// SectionApi
class SectionApi {
  /// _apiClient
  final ApiClient _apiClient;

  Future<List<Section>> querySections() async {
    final response = await _apiClient.get(Uri.parse('/sections/select-all'));
    List<Section> sections = Section.fromList(response);
    return sections;
  }

  Future<Section> queryDetails(String id) async {
    final response = await _apiClient.get(Uri.parse('/sections/$id/details'));
    var section = Section.withResponse(response);
    return section;
  }

  SectionApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
