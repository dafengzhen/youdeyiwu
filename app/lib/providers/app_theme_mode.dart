import 'package:flutter/material.dart';

/// AppThemeMode
class AppThemeMode extends ChangeNotifier {
  /// _isDarkMode
  bool _isDarkMode = false;

  bool get isDarkMode => _isDarkMode;

  void toggleTheme() {
    _isDarkMode = !_isDarkMode;
    notifyListeners();
  }
}
