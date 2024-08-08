import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';

import 'api_exception.dart';
import 'app_theme_colors.dart';
import 'tools.dart';

enum PromptType { success, failure, warning, info, secondary, defaultType }

void showSystemPromptBottomSheet(
  bool isDarkMode,
  BuildContext context, {
  String title = 'System prompt',
  String description = 'OK',
  Object? exception,
  String buttonText = 'Confirm',
  VoidCallback? onButtonPressed,
  VoidCallback? onClosePressed,
  VoidCallback? onBottomSheetClosed,
  PromptType promptType = PromptType.defaultType,
}) {
  if (exception != null) {
    if (exception is ApiException) {
      description = exception.message;
    } else {
      description = exception.toString();
    }
    promptType = PromptType.failure;
  }

  /// logDebug
  logDebug(description, promptType: promptType);

  Color getBackgroundColor() {
    switch (promptType) {
      case PromptType.success:
        return isDarkMode
            ? AppThemeColors.successColor[800]!
            : AppThemeColors.successColor[200]!;
      case PromptType.failure:
        return isDarkMode
            ? AppThemeColors.dangerColor[800]!
            : AppThemeColors.dangerColor[200]!;
      case PromptType.warning:
        return isDarkMode
            ? AppThemeColors.warningColor[800]!
            : AppThemeColors.warningColor[200]!;
      case PromptType.info:
        return isDarkMode
            ? AppThemeColors.infoColor[800]!
            : AppThemeColors.infoColor[200]!;
      case PromptType.secondary:
        return isDarkMode
            ? AppThemeColors.secondaryColor[800]!
            : AppThemeColors.secondaryColor[200]!;
      case PromptType.defaultType:
      default:
        return isDarkMode
            ? AppThemeColors.baseBgDark
            : AppThemeColors.baseBgLight;
    }
  }

  Color getButtonColor() {
    switch (promptType) {
      case PromptType.success:
        return isDarkMode
            ? AppThemeColors.successColor[700]!
            : AppThemeColors.successColor[300]!;
      case PromptType.failure:
        return isDarkMode
            ? AppThemeColors.dangerColor[700]!
            : AppThemeColors.dangerColor[300]!;
      case PromptType.warning:
        return isDarkMode
            ? AppThemeColors.warningColor[700]!
            : AppThemeColors.warningColor[300]!;
      case PromptType.info:
        return isDarkMode
            ? AppThemeColors.infoColor[700]!
            : AppThemeColors.infoColor[300]!;
      case PromptType.secondary:
        return isDarkMode
            ? AppThemeColors.secondaryColor[700]!
            : AppThemeColors.secondaryColor[300]!;
      case PromptType.defaultType:
      default:
        return isDarkMode
            ? AppThemeColors.tertiaryBgDark
            : AppThemeColors.tertiaryBgLight;
    }
  }

  Color getTextColor() {
    switch (promptType) {
      case PromptType.success:
      case PromptType.failure:
      case PromptType.warning:
      case PromptType.info:
      case PromptType.secondary:
        return isDarkMode ? AppThemeColors.white : AppThemeColors.black;
      case PromptType.defaultType:
      default:
        return isDarkMode
            ? AppThemeColors.baseColorDark
            : AppThemeColors.baseColorLight;
    }
  }

  defaultPopAction() {
    popIfMounted(context);
  }

  showModalBottomSheet(
    context: context,
    shape: const RoundedRectangleBorder(
      borderRadius: BorderRadius.vertical(top: Radius.circular(21)),
    ),
    builder: (BuildContext context) {
      return Container(
        width: double.infinity,
        padding:
            const EdgeInsets.only(left: 19, right: 19, top: 19, bottom: 60),
        decoration: BoxDecoration(
          color: getBackgroundColor(),
          borderRadius: const BorderRadius.vertical(top: Radius.circular(21)),
        ),
        child: Stack(
          children: [
            Column(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                const SizedBox(height: 41),
                Text(
                  title,
                  style: TextStyle(
                    fontSize: 19,
                    fontWeight: FontWeight.bold,
                    color: getTextColor(),
                  ),
                ),
                const SizedBox(height: 11),
                Text(
                  description,
                  style: TextStyle(
                    fontSize: 17,
                    color: getTextColor(),
                  ),
                ),
                const SizedBox(height: 21),
                Center(
                  child: SizedBox(
                    width: double.infinity,
                    child: ElevatedButton(
                      onPressed: onButtonPressed != null
                          ? () {
                              onButtonPressed();
                              defaultPopAction();
                            }
                          : defaultPopAction,
                      style: ElevatedButton.styleFrom(
                        backgroundColor: getButtonColor(),
                        padding: const EdgeInsets.symmetric(vertical: 13),
                      ),
                      child: Text(
                        buttonText,
                        style: TextStyle(
                          color: getTextColor(),
                        ),
                      ),
                    ),
                  ),
                ),
              ],
            ),
            Positioned(
              top: 0,
              right: 0,
              child: IconButton(
                icon: FaIcon(
                  FontAwesomeIcons.xmark,
                  color: getTextColor(),
                ),
                onPressed: onClosePressed != null
                    ? () {
                        onClosePressed();
                        defaultPopAction();
                      }
                    : defaultPopAction,
              ),
            ),
          ],
        ),
      );
    },
  ).whenComplete(() {
    if (onBottomSheetClosed != null) {
      onBottomSheetClosed();
      defaultPopAction();
    }
  });
}
