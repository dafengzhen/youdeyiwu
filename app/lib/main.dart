import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:google_fonts/google_fonts.dart';
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';

import 'apis/message_api.dart';
import 'apis/post_api.dart';
import 'apis/section_api.dart';
import 'apis/user_api.dart';
import 'configs/configs.dart';
import 'pages/article_comment_page.dart';
import 'pages/article_details_page.dart';
import 'pages/article_edit_page.dart';
import 'pages/article_editor_page.dart';
import 'pages/content_details_page.dart';
import 'pages/content_page.dart';
import 'pages/home_page.dart';
import 'pages/message_page.dart';
import 'pages/sign_in_page.dart';
import 'pages/user_articles_page.dart';
import 'pages/user_contents_page.dart';
import 'pages/user_page.dart';
import 'pages/user_statistics_page.dart';
import 'pages/user_tags_page.dart';
import 'pages/user_view_articles_page.dart';
import 'providers/app_theme_mode.dart';
import 'providers/login_info.dart';
import 'utils/api_client.dart';
import 'utils/app_theme_data.dart';
import 'utils/bottom_sheet_utils.dart';

/// rootNavigatorKey
final GlobalKey<NavigatorState> rootNavigatorKey =
    GlobalKey<NavigatorState>(debugLabel: 'root');

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  GoogleFonts.config.allowRuntimeFetching = false;
  LicenseRegistry.addLicense(() async* {
    final license = await rootBundle.loadString('fonts/OFL.txt');
    yield LicenseEntryWithLineBreaks(['fonts'], license);
  });

  SharedPreferences sharedPreferences = await SharedPreferences.getInstance();
  ApiClient apiClient = ApiClient(sharedPreferences: sharedPreferences);

  runApp(
    MultiProvider(
      providers: [
        ChangeNotifierProvider<AppThemeMode>(create: (_) => AppThemeMode()),
        ChangeNotifierProvider<LoginInfo>(create: (_) => LoginInfo()),
        Provider<SharedPreferences>(create: (_) => sharedPreferences),
        Provider<ApiClient>(create: (_) => apiClient),
        Provider<UserApi>(
          create: (_) => UserApi(
            apiClient: apiClient,
            sharedPreferences: sharedPreferences,
          ),
        ),
        Provider<PostApi>(
          create: (_) => PostApi(apiClient: apiClient),
        ),
        Provider<SectionApi>(
          create: (_) => SectionApi(apiClient: apiClient),
        ),
        Provider<MessageApi>(
          create: (_) => MessageApi(apiClient: apiClient),
        ),
      ],
      child: const MyApp(),
    ),
  );
}

class MyApp extends StatefulWidget {
  const MyApp({super.key});

  @override
  State<MyApp> createState() => _MyAppState();
}

class _MyAppState extends State<MyApp> {
  final GoRouter _router = GoRouter(
    debugLogDiagnostics: true,
    navigatorKey: rootNavigatorKey,
    initialLocation: '/home',
    routes: <RouteBase>[
      StatefulShellRoute.indexedStack(
        builder: (BuildContext context, GoRouterState state,
            StatefulNavigationShell navigationShell) {
          return ScaffoldWithNavBar(
            navigationShell: navigationShell,
          );
        },
        branches: <StatefulShellBranch>[
          StatefulShellBranch(
            routes: <RouteBase>[
              GoRoute(
                path: '/home',
                name: 'home',
                builder: (BuildContext context, GoRouterState state) =>
                    const HomePage(),
                routes: const <RouteBase>[],
              ),
            ],
          ),
          StatefulShellBranch(
            routes: <RouteBase>[
              GoRoute(
                path: '/content',
                name: 'content',
                builder: (BuildContext context, GoRouterState state) =>
                    const ContentPage(),
                routes: const <RouteBase>[],
              ),
            ],
          ),
          StatefulShellBranch(
            routes: <RouteBase>[
              GoRoute(
                path: '/message',
                name: 'message',
                builder: (BuildContext context, GoRouterState state) =>
                    const MessagePage(),
                routes: const <RouteBase>[],
              ),
            ],
          ),
          StatefulShellBranch(
            routes: <RouteBase>[
              GoRoute(
                path: '/user',
                name: 'user',
                builder: (BuildContext context, GoRouterState state) =>
                    const UserPage(),
                routes: <RouteBase>[
                  GoRoute(
                    path: 'articles',
                    name: 'userArticles',
                    parentNavigatorKey: rootNavigatorKey,
                    builder: (BuildContext context, GoRouterState state) =>
                        UserArticlesPage(id: state.uri.queryParameters['id']!),
                    routes: const <RouteBase>[],
                  ),
                  GoRoute(
                    path: 'contents',
                    name: 'userContents',
                    parentNavigatorKey: rootNavigatorKey,
                    builder: (BuildContext context, GoRouterState state) =>
                        UserContentsPage(id: state.uri.queryParameters['id']!),
                    routes: const <RouteBase>[],
                  ),
                  GoRoute(
                    path: 'tags',
                    name: 'userTags',
                    parentNavigatorKey: rootNavigatorKey,
                    builder: (BuildContext context, GoRouterState state) =>
                        UserTagsPage(id: state.uri.queryParameters['id']!),
                    routes: const <RouteBase>[],
                  ),
                  GoRoute(
                    path: 'statistics',
                    name: 'userStatistics',
                    parentNavigatorKey: rootNavigatorKey,
                    builder: (BuildContext context, GoRouterState state) =>
                        UserStatisticsPage(
                            id: state.uri.queryParameters['id']!),
                    routes: const <RouteBase>[],
                  ),
                ],
              ),
            ],
          ),
        ],
      ),
      GoRoute(
        path: '/user/details/:id',
        name: 'userDetails',
        builder: (BuildContext context, GoRouterState state) =>
            UserPage(id: state.pathParameters['id']!),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/user/view/articles',
        name: 'userViewArticles',
        builder: (BuildContext context, GoRouterState state) =>
            UserViewArticlesPage(
          sectionId: state.uri.queryParameters['sectionId'],
          tagId: state.uri.queryParameters['tagId'],
        ),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/content/details/:id',
        name: 'contentDetails',
        builder: (BuildContext context, GoRouterState state) =>
            ContentDetailsPage(id: state.pathParameters['id']!),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/article/details/:id',
        name: 'articleDetails',
        builder: (BuildContext context, GoRouterState state) =>
            ArticleDetailsPage(id: state.pathParameters['id']!),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/article/comment/:id',
        name: 'articleComment',
        builder: (BuildContext context, GoRouterState state) =>
            ArticleCommentPage(id: state.pathParameters['id']!),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/article/edit',
        name: 'articleEdit',
        builder: (BuildContext context, GoRouterState state) =>
            ArticleEditPage(id: state.uri.queryParameters['id']),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/article/editor',
        name: 'articleEditor',
        builder: (BuildContext context, GoRouterState state) =>
            ArticleEditorPage(id: state.uri.queryParameters['id']),
        routes: const <RouteBase>[],
      ),
      GoRoute(
        path: '/signIn',
        name: 'signIn',
        builder: (BuildContext context, GoRouterState state) =>
            const SignInPage(),
        routes: const <RouteBase>[],
      ),
    ],
  );

  @override
  void initState() {
    super.initState();
    _fetchLoginInfo();
  }

  void _fetchLoginInfo() async {
    try {
      var userApi = context.read<UserApi>();
      var loginInfo = context.read<LoginInfo>();
      var value = await userApi.loginInfo();
      loginInfo.setUser(value);
    } catch (e) {
      if (mounted) {
        _showErrorPrompt(e);
      }
    }
  }

  void _showErrorPrompt(dynamic e) {
    showSystemPromptBottomSheet(
      context.read<AppThemeMode>().isDarkMode,
      context,
      exception: e,
    );
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);

    return MaterialApp.router(
      title: appTitle,
      routerConfig: _router,
      debugShowCheckedModeBanner: false,
      localizationsDelegates: AppLocalizations.localizationsDelegates,
      supportedLocales: AppLocalizations.supportedLocales,
      locale: const Locale('en'),
      themeMode: isDarkMode ? ThemeMode.dark : ThemeMode.light,
      theme: AppThemeData.lightTheme,
      darkTheme: AppThemeData.darkTheme,
    );
  }
}

/// ScaffoldWithNavBar
class ScaffoldWithNavBar extends StatelessWidget {
  const ScaffoldWithNavBar({
    required this.navigationShell,
    Key? key,
  }) : super(key: key ?? const ValueKey<String>('ScaffoldWithNavBar'));

  final StatefulNavigationShell navigationShell;

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    return Scaffold(
      body: navigationShell,
      floatingActionButtonLocation: FloatingActionButtonLocation.centerDocked,
      floatingActionButton: Stack(
        clipBehavior: Clip.none,
        children: [
          SizedBox(
            width: 48,
            height: 48,
            child: FloatingActionButton(
              tooltip: "Create Article",
              elevation: 0,
              shape: RoundedRectangleBorder(
                borderRadius: BorderRadius.circular(50),
              ),
              child: const FaIcon(
                FontAwesomeIcons.plus,
                size: 20,
              ),
              onPressed: () {
                context.pushNamed("articleEdit");
              },
            ),
          ),
          Positioned(
            left: -5,
            right: -5,
            top: -5,
            child: IgnorePointer(
              ignoring: true,
              child: Container(
                width: 48,
                height: 29,
                decoration: BoxDecoration(
                  color: Colors.transparent,
                  borderRadius: const BorderRadius.vertical(
                    top: Radius.circular(29),
                  ),
                  border: Border(
                    top: BorderSide(color: barBackgroundColor, width: 5),
                    left: BorderSide(color: barBackgroundColor, width: 5),
                    right: BorderSide(color: barBackgroundColor, width: 5),
                  ),
                ),
              ),
            ),
          ),
        ],
      ),
      bottomNavigationBar: NavigationBar(
        height: 60,
        labelBehavior: NavigationDestinationLabelBehavior.alwaysHide,
        destinations: [
          _createNavigationDestination(
            icon: FontAwesomeIcons.house,
            label: "Home",
          ),
          _createNavigationDestination(
            icon: FontAwesomeIcons.tableColumns,
            label: "Content",
          ),
          _createNavigationDestination(
            icon: FontAwesomeIcons.solidMessage,
            label: "Message",
          ),
          _createNavigationDestination(
            icon: FontAwesomeIcons.solidUser,
            label: "User",
          ),
        ],
        selectedIndex: navigationShell.currentIndex,
        onDestinationSelected: (index) => _onTap(context, index),
      ),
    );
  }

  NavigationDestination _createNavigationDestination({
    required IconData icon,
    IconData? selectedIcon,
    required String label,
  }) {
    return NavigationDestination(
      icon: FaIcon(
        icon,
        size: 20,
      ),
      selectedIcon:
          selectedIcon != null ? FaIcon(selectedIcon, size: 20) : null,
      label: label,
    );
  }

  void _onTap(BuildContext context, int index) {
    navigationShell.goBranch(
      index,
      initialLocation: index == navigationShell.currentIndex,
    );
  }
}
