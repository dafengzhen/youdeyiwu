import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:timeago/timeago.dart' as timeago;

import '../models/token.dart';
import 'bottom_sheet_utils.dart';
import 'constants.dart';

enum SourceType { jsonString, json }

/// logDebug
void logDebug(
  dynamic message, {
  PromptType? promptType,
  SourceType? sourceType,
  String? customType,
}) {
  if (kDebugMode) {
    final timestamp = DateFormat('yyyy-MM-dd HH:mm:ss').format(DateTime.now());
    if (message is! String) {
      message = message.toString();
    }

    if (sourceType != null) {
      debugPrint(
        '[DEBUG] [$timestamp] [${sourceType.name.toUpperCase()}] - $message',
        wrapWidth: 1024,
      );
    } else if (promptType != null) {
      debugPrint(
        '[DEBUG] [$timestamp] [${promptType.name.toUpperCase()}] - $message',
        wrapWidth: 1024,
      );
    } else if (customType != null) {
      debugPrint(
        '[DEBUG] [$timestamp] [${customType.toUpperCase()}] - $message',
        wrapWidth: 1024,
      );
    } else {
      debugPrint(
        '[DEBUG] [$timestamp] [INFO] - $message',
        wrapWidth: 1024,
      );
    }
  }
}

/// hasToken
Token? hasToken(SharedPreferences sharedPreferences) {
  final value = sharedPreferences.getString(tk);
  if (value != null) {
    return Token.fromJsonString(value);
  }
  return null;
}

/// removeToken
Future<bool> removeToken(SharedPreferences sharedPreferences) {
  return sharedPreferences.remove(tk);
}

/// navigateIfMounted
void navigateIfMounted(BuildContext context, String name) {
  if (context.mounted) {
    context.goNamed(name);
  }
}

/// popIfMounted
void popIfMounted(BuildContext context) {
  if (context.mounted) {
    context.pop();
  }
}

/// formatRelativeTime
String formatRelativeTime(String? createdOn) {
  DateTime parsedDate = createdOn != null
      ? DateTime.parse(createdOn).toLocal()
      : DateTime.now().toLocal();
  return timeago.format(parsedDate, locale: 'en_short');
}

/// formatCount
String formatCount(int? count) {
  if (count == null) {
    return "0";
  }

  if (count < 1000) {
    return count.toString();
  }

  var suffix = '';
  var divisor = 1;

  if (count >= 1000000000) {
    suffix = 'B';
    divisor = 1000000000;
  } else if (count >= 1000000) {
    suffix = 'M';
    divisor = 1000000;
  } else if (count >= 1000) {
    suffix = 'K';
    divisor = 1000;
  }

  return (count / divisor).toStringAsFixed(1) + suffix;
}

/// isHttpOrHttps
bool isHttpOrHttps(String? url) {
  if (url == null || url.isEmpty) {
    return false;
  }

  final RegExp urlPattern = RegExp(r'^(http|https)://');
  return urlPattern.hasMatch(url);
}

/// getUsernameOrAnonymous
String getUsernameOrAnonymous(String? username) {
  return username?.isNotEmpty == true ? username! : 'Anonymous';
}

/// getAvatarOrDefault
ImageProvider getAvatarOrDefault(String? avatar) {
  return isHttpOrHttps(avatar)
      ? NetworkImage(avatar!)
      : const AssetImage("assets/images/avatar.png");
}

/// extractIdFromUrl
String? extractIdFromUrl(String url, String pathSegment) {
  String pattern = '/$pathSegment/(\\d+)';
  RegExp regExp = RegExp(pattern);
  RegExpMatch? match = regExp.firstMatch(url);

  if (match != null) {
    return match.group(1);
  } else {
    return null;
  }
}
