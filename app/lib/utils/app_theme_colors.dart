import 'package:flutter/material.dart';

class AppThemeColors {
  AppThemeColors._();

  /// common
  static const Color red = Colors.red;
  static const Color blue = Colors.blue;
  static const Color yellow = Colors.yellow;
  static const Color green = Colors.green;
  static const Color grey = Colors.grey;
  static const Color white = Colors.white;
  static const Color black = Colors.black;

  /// primary
  static const Color seedColor = Color(0xFF0A6CFF);
  static const Color primary = seedColor;
  static const Color secondary = Color(0xFF585C5F);
  static const Color success = Color(0xFF27FB6B);
  static const Color info = Color(0xFF00BFFF);
  static const Color warning = Color(0xFFFCF300);
  static const Color danger = Color(0xFFD7263D);

  /// mode
  static const Color baseColorLight = Color(0xFF212529);
  static const Color baseBgLight = Color(0xFFFFFFFF);
  static const Color secondaryColorLight = Color(0xFF757575);
  static const Color secondaryBgLight = Color(0xFFE9ECEF);
  static const Color tertiaryColorLight = Color(0xFFA0A0A0);
  static const Color tertiaryBgLight = Color(0xFFF8F9FA);

  static const Color baseColorDark = Color(0xFFDEE2E6);
  static const Color baseBgDark = Color(0xFF212529);
  static const Color secondaryColorDark = Color(0xFFA7ACB1);
  static const Color secondaryBgDark = Color(0xFF343A40);
  static const Color tertiaryColorDark = Color(0xFF808080);
  static const Color tertiaryBgDark = Color(0xFF2B3035);

  /// danger #D7263D
  static const Color baseColorDangerBgLight = Color(0xFFF5C7CD);
  static const Color baseBgDangerColorLight = Color(0xFF821725);
  static const Color secondaryColorDangerBgLight = Color(0xFFFFFFFF);
  static const Color secondaryBgDangerColorLight = Color(0xFF6C131F);
  static const Color tertiaryColorDangerBgLight = Color(0xFF000000);
  static const Color tertiaryBgDangerColorLight = Color(0xFF791523);

  static const Color baseColorDangerBgDark = Color(0xFF5F111B);
  static const Color baseBgDangerColorDark = Color(0xFFF5C7CD);
  static const Color secondaryColorDangerBgDark = Color(0xFF000000);
  static const Color secondaryBgDangerColorDark = Color(0xFFFCEEF0);
  static const Color tertiaryColorDangerBgDark = Color(0xFF000000);
  static const Color tertiaryBgDangerColorDark = Color(0xFFF8D8DC);

  /// other
  static const Map<int, Color> primaryColor = {
    0: Color(0xFFF5F9FF),
    50: Color(0xFFD2E5FF),
    100: Color(0xFFAED0FF),
    150: Color(0xFF8ABBFF),
    200: Color(0xFF65A6FF),
    300: Color(0xFF2F81FD),
    400: Color(0xFF0262F2),
    500: Color(0xFF0051CA),
    600: Color(0xFF0041A2),
    700: Color(0xFF02327A),
    800: Color(0xFF042352),
    900: Color(0xFF04132A),
    1000: Color(0xFF000103),
  };

  static const Map<int, Color> secondaryColor = {
    0: Color(0xFFEBF6FF),
    50: Color(0xFFE0E9F1),
    100: Color(0xFFD4DDE3),
    150: Color(0xFFC9D0D5),
    200: Color(0xFFBEC3C6),
    300: Color(0xFFA0A7AD),
    400: Color(0xFF858D93),
    500: Color(0xFF6E7479),
    600: Color(0xFF585C5F),
    700: Color(0xFF3A444C),
    800: Color(0xFF23303A),
    900: Color(0xFF121E27),
    1000: Color(0xFF070E14),
  };

  static const Map<int, Color> successColor = {
    0: Color(0xFFF5FFF8),
    50: Color(0xFFCFFFDF),
    100: Color(0xFFA9FFC6),
    150: Color(0xFF83FFAC),
    200: Color(0xFF5DFF93),
    300: Color(0xFF27FB6B),
    400: Color(0xFF1CD958),
    500: Color(0xFF16B84A),
    600: Color(0xFF13963D),
    700: Color(0xFF117431),
    800: Color(0xFF0F5325),
    900: Color(0xFF0B3117),
    1000: Color(0xFF040F08),
  };

  static const Map<int, Color> infoColor = {
    0: Color(0xFFF0FBFF),
    50: Color(0xFFC4F2FF),
    100: Color(0xFF97E8FF),
    150: Color(0xFF6BDFFF),
    200: Color(0xFF3DD5FF),
    300: Color(0xFF00BFFF),
    400: Color(0xFF00A6DE),
    500: Color(0xFF008EBD),
    600: Color(0xFF00759D),
    700: Color(0xFF025D7C),
    800: Color(0xFF06465B),
    900: Color(0xFF072D3A),
    1000: Color(0xFF051419),
  };

  static const Map<int, Color> warningColor = {
    0: Color(0xFFFFFEE6),
    50: Color(0xFFFFFBBB),
    100: Color(0xFFFFF991),
    150: Color(0xFFFFF966),
    200: Color(0xFFFFF93A),
    300: Color(0xFFFCF300),
    400: Color(0xFFD8D100),
    500: Color(0xFFB5AE00),
    600: Color(0xFF918C00),
    700: Color(0xFF6D6A00),
    800: Color(0xFF4A4702),
    900: Color(0xFF262503),
    1000: Color(0xFF030200),
  };

  static const Map<int, Color> dangerColor = {
    0: Color(0xFFFFFFFF),
    50: Color(0xFFFCDDE1),
    100: Color(0xFFF9BCC4),
    150: Color(0xFFF79CA8),
    200: Color(0xFFF47C8C),
    300: Color(0xFFE54C60),
    400: Color(0xFFD7263D),
    500: Color(0xFFB71A2E),
    600: Color(0xFF971324),
    700: Color(0xFF770F1D),
    800: Color(0xFF570C16),
    900: Color(0xFF37090F),
    1000: Color(0xFF170507),
  };
}
