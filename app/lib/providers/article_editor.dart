import 'package:flutter/material.dart';

import '../models/user.dart';

/// ArticleEditor
class ArticleEditor extends ChangeNotifier {
  /// _deltaContent
  String? _deltaContent;

  String? get deltaContent => _deltaContent;

  void setDeltaContent(String? content) {
    if (_deltaContent == null && content == null) {
      return;
    }

    _deltaContent = content;
    notifyListeners();
  }
}
