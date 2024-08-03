import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/user_api.dart';
import '../dtos/login_dto.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';

enum _PageType { signIn, signUp }

class SignInPage extends StatefulWidget {
  const SignInPage({super.key});

  @override
  State<SignInPage> createState() => _SignInPageState();
}

class _SignInPageState extends State<SignInPage> {
  _PageType pageType = _PageType.signIn;

  final TextEditingController _usernameController = TextEditingController();
  final TextEditingController _passwordController = TextEditingController();

  @override
  void dispose() {
    _usernameController.dispose();
    _passwordController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    return Scaffold(
      backgroundColor:
          isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
      appBar: AppBar(
        backgroundColor: barBackgroundColor,
        surfaceTintColor: barBackgroundColor,
        leading: IconButton(
          icon: const FaIcon(
            FontAwesomeIcons.arrowLeft,
            size: 20,
          ),
          onPressed: () {
            context.pop();
          },
        ),
        title: Row(
          mainAxisAlignment: MainAxisAlignment.end,
          children: [
            Switch(
              value: isDarkMode,
              onChanged: (bool value) {
                Provider.of<AppThemeMode>(context, listen: false).toggleTheme();
              },
            )
          ],
        ),
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(15),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              pageType == _PageType.signIn ? "Welcome back." : 'Welcome join.',
              style: TextStyle(
                fontWeight: FontWeight.bold,
                height: 1.5,
                letterSpacing: 2,
                fontSize: 28,
                color: isDarkMode
                    ? AppThemeColors.baseColorDark
                    : AppThemeColors.baseColorLight,
              ),
            ),
            const SizedBox(height: 9),
            Text(
              pageType == _PageType.signIn
                  ? "Please sign in to continue using our services."
                  : "Sign up to experience more exciting content.",
              style: TextStyle(
                height: 1.5,
                letterSpacing: 2,
                fontSize: 27,
                color: isDarkMode
                    ? AppThemeColors.baseColorDark
                    : AppThemeColors.baseColorLight,
              ),
            ),
            const SizedBox(height: 79),
            TextField(
              controller: _usernameController,
              maxLength: 16,
              maxLines: 1,
              decoration: InputDecoration(
                  hintText: 'Enter username',
                  hintStyle: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.secondaryColorDark
                        : AppThemeColors.secondaryColorLight,
                  ),
                  prefixIcon: Padding(
                    padding: const EdgeInsets.all(15),
                    child: FaIcon(
                      FontAwesomeIcons.solidUser,
                      size: 20,
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                  filled: true,
                  fillColor: isDarkMode
                      ? AppThemeColors.tertiaryBgDark
                      : AppThemeColors.tertiaryBgLight,
                  counterText: '',
                  contentPadding: const EdgeInsets.symmetric(vertical: 15),
                  border: OutlineInputBorder(
                    borderSide: BorderSide.none,
                    borderRadius: BorderRadius.circular(15),
                  )),
            ),
            const SizedBox(height: 15),
            TextField(
              controller: _passwordController,
              obscureText: true,
              maxLength: 18,
              maxLines: 1,
              decoration: InputDecoration(
                  hintText: 'Enter password',
                  hintStyle: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.secondaryColorDark
                        : AppThemeColors.secondaryColorLight,
                  ),
                  prefixIcon: Padding(
                    padding: const EdgeInsets.all(15),
                    child: FaIcon(
                      FontAwesomeIcons.lock,
                      size: 20,
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                  filled: true,
                  fillColor: isDarkMode
                      ? AppThemeColors.tertiaryBgDark
                      : AppThemeColors.tertiaryBgLight,
                  counterText: '',
                  contentPadding: const EdgeInsets.symmetric(vertical: 15),
                  border: OutlineInputBorder(
                    borderSide: BorderSide.none,
                    borderRadius: BorderRadius.circular(15),
                  )),
            ),
            const SizedBox(height: 99),
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Text(
                  pageType == _PageType.signIn
                      ? "Don't have an account yet? "
                      : "Already have an account? ",
                  style: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.secondaryColorDark
                        : AppThemeColors.secondaryColorLight,
                  ),
                ),
                GestureDetector(
                  onTap: () {
                    setState(() {
                      switch (pageType) {
                        case _PageType.signIn:
                          pageType = _PageType.signUp;
                          break;
                        case _PageType.signUp:
                          pageType = _PageType.signIn;
                          break;
                      }
                    });
                  },
                  child: Text(
                    pageType == _PageType.signIn ? "Sign up" : "Sign in",
                    style: TextStyle(
                      fontWeight: FontWeight.bold,
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                ),
              ],
            ),
            const SizedBox(height: 19),
            SizedBox(
              width: double.infinity,
              child: ElevatedButton.icon(
                onPressed: () {
                  var username = _usernameController.text.trim();
                  var password = _passwordController.text.trim();

                  if (username.isEmpty) {
                    showSystemPromptBottomSheet(
                      isDarkMode,
                      context,
                      description: "Username cannot be empty",
                    );
                    return;
                  }

                  if (password.isEmpty) {
                    showSystemPromptBottomSheet(
                      isDarkMode,
                      context,
                      description: "Password cannot be empty",
                    );
                    return;
                  }

                  var dto = LoginDto(
                    username: username,
                    password: password,
                  );
                  var userApi = context.read<UserApi>();
                  var isReqLogin = pageType == _PageType.signIn;

                  userApi
                      .login(
                    dto: dto,
                    isReqLogin: isReqLogin,
                  )
                      .then((value) {
                    String description;
                    if (isReqLogin) {
                      description = 'Login successful, welcome, $username';
                    } else {
                      description =
                          'Registration successful, welcome, $username';
                    }

                    onPressed() {
                      navigateIfMounted(context, "home");
                    }

                    showSystemPromptBottomSheet(
                      isDarkMode,
                      context,
                      description: description,
                      promptType: PromptType.success,
                      onButtonPressed: onPressed,
                      onBottomSheetClosed: onPressed,
                      onClosePressed: onPressed,
                    );
                  }).catchError((e) {
                    showSystemPromptBottomSheet(
                      isDarkMode,
                      context,
                      exception: e,
                    );
                  });
                },
                icon: FaIcon(
                  pageType == _PageType.signIn
                      ? FontAwesomeIcons.solidUser
                      : FontAwesomeIcons.userPlus,
                  size: 17,
                ),
                style: ButtonStyle(
                  shape: WidgetStateProperty.all<RoundedRectangleBorder>(
                      RoundedRectangleBorder(
                    borderRadius: BorderRadius.circular(15),
                  )),
                  padding: const WidgetStatePropertyAll(
                    EdgeInsets.symmetric(
                      vertical: 15,
                    ),
                  ),
                ),
                label: Text(
                  pageType == _PageType.signIn ? 'Sign In' : 'Sign Up',
                  style: const TextStyle(
                    fontWeight: FontWeight.bold,
                  ),
                ),
              ),
            ),
            const SizedBox(height: 19),
          ],
        ),
      ),
    );
  }
}
