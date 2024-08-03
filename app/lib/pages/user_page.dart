import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../configs/configs.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';

class UserPage extends StatefulWidget {
  final String? id;

  const UserPage({this.id, super.key});

  @override
  State<UserPage> createState() => _UserPageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _UserPageState extends State<UserPage> {
  List<Article> articles = List.generate(
    5,
    (index) => Article(
      'Article Title $index',
      'Subtitle for Article $index',
    ),
  );

  ScrollController _scrollController = ScrollController();
  bool _isLoadingMore = false;
  int _page = 1;
  bool _hasMore = true;

  @override
  void initState() {
    super.initState();
    _loadData();
    _scrollController.addListener(() {
      if (_scrollController.position.pixels ==
          _scrollController.position.maxScrollExtent) {
        _loadMore();
      }
    });
  }

  @override
  void dispose() {
    _scrollController.dispose();
    super.dispose();
  }

  Future<void> _loadData() async {
    await Future.delayed(const Duration(seconds: 2));
    setState(() {
      articles = List.generate(
          10, (index) => Article('Title $index', 'Subtitle $index'));
      _page = 1;
      _hasMore = true;
    });
  }

  Future<void> _refresh() async {
    await Future.delayed(const Duration(seconds: 2));
  }

  void _loadMore() async {
    if (_isLoadingMore || !_hasMore) return;

    setState(() {
      _isLoadingMore = true;
    });

    await Future.delayed(const Duration(seconds: 2));
    setState(() {
      _page++;
      final newArticles = List.generate(
          10,
          (index) => Article(
              'Title ${index + _page * 10}', 'Subtitle ${index + _page * 10}'));
      articles.addAll(newArticles);

      if (newArticles.length < 10) {
        _hasMore = false;
      }

      _isLoadingMore = false;
    });
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    return Scaffold(
      body: Stack(
        children: [
          Container(
            color: barBackgroundColor,
            child: Container(
              padding: const EdgeInsets.all(15),
              margin: const EdgeInsets.symmetric(horizontal: 3),
              decoration: BoxDecoration(
                color: isDarkMode
                    ? AppThemeColors.baseBgDark
                    : AppThemeColors.baseBgLight,
                borderRadius: const BorderRadius.vertical(
                  bottom: Radius.circular(17),
                ),
              ),
            ),
          ),
          RefreshIndicator(
            onRefresh: _refresh,
            child: CustomScrollView(
              controller: _scrollController,
              slivers: [
                SliverAppBar(
                  backgroundColor: barBackgroundColor,
                  surfaceTintColor: barBackgroundColor,
                  title: Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Text(
                        appTitle,
                        style: TextStyle(
                          color: isDarkMode
                              ? AppThemeColors.baseColorDark
                              : AppThemeColors.baseColorLight,
                        ),
                      ),
                      Switch(
                        value: isDarkMode,
                        onChanged: (bool value) {
                          Provider.of<AppThemeMode>(context, listen: false)
                              .toggleTheme();
                        },
                      )
                    ],
                  ),
                  floating: true,
                ),
                SliverPadding(
                  padding: const EdgeInsets.all(15),
                  sliver: SliverList(
                    delegate: SliverChildListDelegate([
                      Stack(
                        clipBehavior: Clip.none,
                        children: [
                          Container(
                            height: 130,
                            padding: const EdgeInsets.only(left: 12, right: 0),
                            decoration: BoxDecoration(
                              image: const DecorationImage(
                                fit: BoxFit.cover,
                                image: NetworkImage(
                                  "https://images.unsplash.com/photo-1696733585001-868eb49cbfa6?w=640",
                                ),
                              ),
                              color: isDarkMode
                                  ? AppThemeColors.tertiaryBgDark
                                  : AppThemeColors.tertiaryBgLight,
                              borderRadius: BorderRadius.circular(17),
                            ),
                          ),
                          Positioned(
                            left: 0,
                            right: 0,
                            bottom: -23,
                            child: CircleAvatar(
                              radius: 35,
                              backgroundColor: isDarkMode
                                  ? AppThemeColors.baseBgDark
                                  : AppThemeColors.baseBgLight,
                              child: const CircleAvatar(
                                backgroundImage:
                                    AssetImage('assets/images/avatar.png'),
                                radius: 29,
                              ),
                            ),
                          ),
                        ],
                      ),
                      const SizedBox(
                        height: 39,
                      ),
                      Column(
                        children: [
                          Row(
                            mainAxisAlignment: MainAxisAlignment.center,
                            children: [
                              Text(
                                "dafengzhen",
                                maxLines: 1,
                                style: TextStyle(
                                  color: isDarkMode
                                      ? AppThemeColors.baseColorDark
                                      : AppThemeColors.baseColorLight,
                                  fontWeight: FontWeight.bold,
                                  fontSize: 17,
                                  overflow: TextOverflow.ellipsis,
                                ),
                              ),
                              const SizedBox(
                                width: 7,
                              ),
                              Text(
                                "⌈ ID.1 ⌋",
                                style: TextStyle(
                                  color: isDarkMode
                                      ? AppThemeColors.secondaryColorDark
                                      : AppThemeColors.secondaryColorLight,
                                ),
                              ),
                            ],
                          ),
                          const SizedBox(
                            height: 5,
                          ),
                          Text(
                            "He didn't leave behind a single word",
                            maxLines: 2,
                            style: TextStyle(
                              color: isDarkMode
                                  ? AppThemeColors.baseColorDark
                                  : AppThemeColors.baseColorLight,
                              overflow: TextOverflow.ellipsis,
                            ),
                          ),
                        ],
                      ),
                      const SizedBox(
                        height: 21,
                      ),
                      IntrinsicHeight(
                        child: Row(
                          children: [
                            Expanded(
                              child: Column(
                                children: [
                                  Text(
                                    '56',
                                    style: TextStyle(
                                      color: isDarkMode
                                          ? AppThemeColors.baseColorDark
                                          : AppThemeColors.baseColorLight,
                                      fontWeight: FontWeight.bold,
                                      fontSize: 19,
                                    ),
                                  ),
                                  const SizedBox(height: 5),
                                  Text(
                                    'Articles',
                                    style: TextStyle(
                                      color: isDarkMode
                                          ? AppThemeColors.baseColorDark
                                          : AppThemeColors.baseColorLight,
                                    ),
                                  ),
                                ],
                              ),
                            ),
                            Padding(
                              padding: const EdgeInsets.symmetric(
                                vertical: 5,
                              ),
                              child: VerticalDivider(
                                color: isDarkMode
                                    ? AppThemeColors.secondaryColor[700]!
                                    : AppThemeColors.secondaryColor[150]!,
                                thickness: 1,
                              ),
                            ),
                            Expanded(
                              child: Column(
                                children: [
                                  Text(
                                    '47',
                                    style: TextStyle(
                                      color: isDarkMode
                                          ? AppThemeColors.baseColorDark
                                          : AppThemeColors.baseColorLight,
                                      fontWeight: FontWeight.bold,
                                      fontSize: 19,
                                    ),
                                  ),
                                  const SizedBox(height: 5),
                                  Text(
                                    'Comments',
                                    style: TextStyle(
                                      color: isDarkMode
                                          ? AppThemeColors.baseColorDark
                                          : AppThemeColors.baseColorLight,
                                    ),
                                  ),
                                ],
                              ),
                            ),
                            Padding(
                              padding: const EdgeInsets.symmetric(
                                vertical: 5,
                              ),
                              child: VerticalDivider(
                                color: isDarkMode
                                    ? AppThemeColors.secondaryColor[700]!
                                    : AppThemeColors.secondaryColor[150]!,
                                thickness: 1,
                              ),
                            ),
                            Expanded(
                              child: Column(
                                children: [
                                  Text(
                                    '91',
                                    style: TextStyle(
                                      color: isDarkMode
                                          ? AppThemeColors.baseColorDark
                                          : AppThemeColors.baseColorLight,
                                      fontWeight: FontWeight.bold,
                                      fontSize: 19,
                                    ),
                                  ),
                                  const SizedBox(height: 5),
                                  Text(
                                    'Replies',
                                    style: TextStyle(
                                      color: isDarkMode
                                          ? AppThemeColors.baseColorDark
                                          : AppThemeColors.baseColorLight,
                                    ),
                                  ),
                                ],
                              ),
                            ),
                          ],
                        ),
                      ),
                      const SizedBox(
                        height: 39,
                      ),
                      Column(
                        children: [
                          _createMenuItem(
                            isDarkMode,
                            icon: FontAwesomeIcons.newspaper,
                            text: "Articles",
                            onTap: () {
                              // showSystemPromptBottomSheet(isDarkMode, context);
                              context.pushNamed(
                                "userArticles",
                                queryParameters: {'id': "4"},
                              );
                            },
                          ),
                          _createMenuItem(
                            isDarkMode,
                            icon: FontAwesomeIcons.tableColumns,
                            text: "Contents",
                            onTap: () {
                              context.pushNamed(
                                "userContents",
                                queryParameters: {'id': "4"},
                              );
                            },
                          ),
                          _createMenuItem(
                            isDarkMode,
                            icon: FontAwesomeIcons.tags,
                            text: "Tags",
                            onTap: () {
                              context.pushNamed(
                                "userTags",
                                queryParameters: {'id': "4"},
                              );
                            },
                          ),
                          _createMenuItem(
                            isDarkMode,
                            icon: FontAwesomeIcons.chartSimple,
                            text: "Statistics",
                            onTap: () {
                              context.pushNamed(
                                "userStatistics",
                                queryParameters: {'id': "4"},
                              );
                            },
                          ),
                          _createMenuItem(
                            isDarkMode,
                            icon: FontAwesomeIcons.rightFromBracket,
                            text: "Logout",
                            onTap: () {},
                          ),
                          const SizedBox(
                            height: 19,
                          ),
                        ],
                      ),
                    ]),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _createMenuItem(
    bool isDarkMode, {
    required IconData icon,
    required String text,
    required Function() onTap,
  }) {
    return Container(
      margin: const EdgeInsets.symmetric(vertical: 7),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.tertiaryBgDark
            : AppThemeColors.tertiaryBgLight,
        borderRadius: BorderRadius.circular(11),
        boxShadow: [
          BoxShadow(
            color: isDarkMode
                ? AppThemeColors.secondaryBgDark
                : AppThemeColors.secondaryBgLight,
            blurRadius: 5,
            offset: const Offset(0, 2),
          ),
        ],
      ),
      child: Material(
        color: Colors.transparent,
        child: InkWell(
          borderRadius: BorderRadius.circular(11),
          overlayColor: WidgetStatePropertyAll<Color>(
            isDarkMode
                ? AppThemeColors.secondaryBgDark
                : AppThemeColors.secondaryBgLight,
          ),
          onTap: onTap,
          child: Padding(
            padding: const EdgeInsets.all(9),
            child: Row(
              children: [
                FaIcon(
                  icon,
                  size: 17,
                  color: isDarkMode
                      ? AppThemeColors.baseColorDark
                      : AppThemeColors.baseColorLight,
                ),
                const SizedBox(width: 15),
                Expanded(
                  child: Text(
                    text,
                    style: TextStyle(
                      fontSize: 17,
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
                    ),
                  ),
                ),
                FaIcon(
                  FontAwesomeIcons.angleRight,
                  size: 17,
                  color: isDarkMode
                      ? AppThemeColors.baseColorDark
                      : AppThemeColors.baseColorLight,
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
