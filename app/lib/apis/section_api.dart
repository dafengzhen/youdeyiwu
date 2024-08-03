import 'package:youdeyiwu_app/models/section.dart';

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

  SectionApi({
    required ApiClient apiClient,
  }) : _apiClient = apiClient;
}
