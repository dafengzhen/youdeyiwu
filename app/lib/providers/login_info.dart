import 'package:flutter/material.dart';

import '../models/user.dart';

/// LoginInfo
class LoginInfo extends ChangeNotifier {
  /// _id
  int? _id;

  bool get isLoggedIn => _id != null;

  int? get loginId => _id;

  void setUser(User? user) {
    setUserId(user?.id);
  }

  void setUserId(int? userId) {
    if ((_id == null && userId == null) ||
        (_id != null && userId != null && _id == userId)) {
      return;
    }

    _id = userId;
    notifyListeners();
  }
}
