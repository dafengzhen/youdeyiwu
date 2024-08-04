import 'package:flutter/material.dart';

import '../utils/app_theme_colors.dart';

Widget buildCenteredLoadingIndicator() {
  return const Center(child: CircularProgressIndicator());
}

Widget buildCenteredNoMoreDataMessage(bool isDarkMode) {
  return Center(
    child: Text(
      "No more data available",
      style: TextStyle(
        color: isDarkMode
            ? AppThemeColors.tertiaryColorDark
            : AppThemeColors.tertiaryColorLight,
      ),
    ),
  );
}
