import 'package:flutter/material.dart';

import '../models/user.dart';

/// LoginInfo
class LoginInfo extends ChangeNotifier {
  /// _id
  int? _id;

  bool get isLoggedIn => _id != null;

  void setUser(User? user) {
    if ((_id == null && user == null) ||
        (_id != null && user != null && _id == user.id)) {
      return;
    }

    _id = user?.id;
    notifyListeners();
  }
}
