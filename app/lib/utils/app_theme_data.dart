import 'package:flutter/material.dart';
import 'package:google_fonts/google_fonts.dart';

import 'app_theme_colors.dart';

class AppThemeData {
  AppThemeData._();

  static final ThemeData _baseLightThemeData =
      ThemeData.light(useMaterial3: true);

  static final ThemeData _baseDarkThemeData =
      ThemeData.dark(useMaterial3: true);

  static ThemeData lightTheme = getThemeData(ThemeMode.light);

  static ThemeData darkTheme = getThemeData(ThemeMode.dark);

  static ThemeData getThemeData(ThemeMode themeMode) {
    switch (themeMode) {
      case ThemeMode.light:
      case ThemeMode.system:
        return _baseLightThemeData.copyWith(
          colorScheme: ColorScheme.fromSeed(
            seedColor: AppThemeColors.seedColor,
          ),
          textTheme: getTextTheme(ThemeMode.light),
        );
      case ThemeMode.dark:
        return _baseDarkThemeData.copyWith(
          colorScheme: ColorScheme.fromSeed(
            brightness: Brightness.dark,
            seedColor: AppThemeColors.seedColor,
          ),
          textTheme: getTextTheme(ThemeMode.dark),
        );
    }
  }

  static TextTheme getTextTheme(ThemeMode themeMode) {
    TextTheme baseTextTheme = (themeMode == ThemeMode.light)
        ? _baseLightThemeData.textTheme
        : _baseDarkThemeData.textTheme;
    return GoogleFonts.ralewayTextTheme(baseTextTheme);
  }
}
