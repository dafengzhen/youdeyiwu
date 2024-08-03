import 'dart:developer';

import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../configs/configs.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';

class UserArticlesPage extends StatefulWidget {
  final String id;

  const UserArticlesPage({required this.id, super.key});

  @override
  State<UserArticlesPage> createState() => _UserArticlesPageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _UserArticlesPageState extends State<UserArticlesPage> {
  List<Article> articles = List.generate(
    20,
    (index) => Article(
      'Article Title $index',
      'Subtitle for Article $index',
    ),
  );

  final TextEditingController _controller = TextEditingController();
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

  void _sendMessage() {
    print('发送的消息: ${_controller.text}');
    _controller.clear();
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    return Scaffold(
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
      body: Container(
        color: barBackgroundColor,
        child: Container(
          padding: const EdgeInsets.symmetric(
            horizontal: 15,
          ),
          color: isDarkMode
              ? AppThemeColors.baseBgDark
              : AppThemeColors.baseBgLight,
          child: ListView.separated(
            itemBuilder: (context, index) {
              if (index == 0) {
                return Padding(
                  padding: const EdgeInsets.only(
                    top: 15,
                  ),
                  child: _createArticleCard(isDarkMode),
                );
              } else if (index == 5 - 1) {
                return Padding(
                  padding: const EdgeInsets.only(
                    bottom: 41,
                  ),
                  child: _createArticleCard(isDarkMode),
                );
              } else {
                return _createArticleCard(isDarkMode);
              }
            },
            separatorBuilder: (context, index) {
              return const SizedBox(
                height: 15,
              );
            },
            itemCount: 5,
          ),
        ),
      ),
    );
  }

  Widget _createArticleCard(bool isDarkMode) {
    return Container(
      padding: const EdgeInsets.only(top: 17, bottom: 7, right: 12),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.tertiaryBgDark
            : AppThemeColors.tertiaryBgLight,
        borderRadius: BorderRadius.circular(17),
      ),
      child: Column(
        children: [
          Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Expanded(
                child: Column(
                  children: [
                    Text(
                      "890",
                      style: TextStyle(
                        color: isDarkMode
                            ? AppThemeColors.baseColorDark
                            : AppThemeColors.baseColorLight,
                      ),
                    ),
                    IconButton(
                      onPressed: () {},
                      icon: FaIcon(
                        FontAwesomeIcons.thumbsUp,
                        size: 20,
                        color: isDarkMode
                            ? AppThemeColors.baseColorDark
                            : AppThemeColors.baseColorLight,
                      ),
                    ),
                  ],
                ),
              ),
              Expanded(
                flex: 6,
                child: Row(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Expanded(
                      child: Text(
                        "Do it, and the difficult thing becomes easy; do not do it, and the easy thing becomes difficult",
                        softWrap: true,
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: isDarkMode
                              ? AppThemeColors.baseColorDark
                              : AppThemeColors.baseColorLight,
                        ),
                      ),
                    ),
                    Text(
                      "2 hours ago",
                      style: TextStyle(
                        color: isDarkMode
                            ? AppThemeColors.secondaryColorDark
                            : AppThemeColors.secondaryColorLight,
                      ),
                    ),
                  ],
                ),
              )
            ],
          ),
          const SizedBox(
            height: 9,
          ),
          Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              const Expanded(child: SizedBox()),
              Expanded(
                flex: 6,
                child: ClipRRect(
                  borderRadius: BorderRadius.circular(9),
                  child: CachedNetworkImage(
                    imageUrl: "http://via.placeholder.com/330x130",
                    progressIndicatorBuilder:
                        (context, url, downloadProgress) => Padding(
                      padding: const EdgeInsets.symmetric(vertical: 9),
                      child: Center(
                        child: CircularProgressIndicator(
                            value: downloadProgress.progress),
                      ),
                    ),
                    errorWidget: (context, url, error) {
                      return Row(
                        children: [
                          Icon(
                            Icons.error,
                            color: isDarkMode
                                ? AppThemeColors.tertiaryBgDangerColorDark
                                : AppThemeColors.tertiaryBgDangerColorLight,
                          ),
                          const SizedBox(
                            width: 3,
                          ),
                          Text(
                            "Failed to load image",
                            style: TextStyle(
                              color: isDarkMode
                                  ? AppThemeColors.tertiaryBgDangerColorDark
                                  : AppThemeColors.tertiaryBgDangerColorLight,
                            ),
                          )
                        ],
                      );
                    },
                  ),
                ),
              ),
            ],
          ),
          const SizedBox(
            height: 9,
          ),
          Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              const Expanded(child: SizedBox()),
              Expanded(
                flex: 6,
                child: Text(
                  """The Night Stay at a Mountain Temple:
      A towering temple stands a hundred feet high;
      I can almost touch the stars with my hand.
      I dare not speak loudly;
      I fear to startle the heavenly beings above.""",
                  style: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.baseColorDark
                        : AppThemeColors.baseColorLight,
                  ),
                ),
              ),
            ],
          ),
          const SizedBox(
            height: 15,
          ),
          Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              const Expanded(child: SizedBox()),
              Expanded(
                flex: 6,
                child: _showFirstCommentCard(isDarkMode),
              ),
            ],
          ),
          const SizedBox(
            height: 15,
          ),
          Row(
            crossAxisAlignment: CrossAxisAlignment.center,
            children: [
              Expanded(
                child: Column(
                  children: [
                    IconButton(
                      onPressed: () {},
                      icon: FaIcon(
                        FontAwesomeIcons.comment,
                        size: 20,
                        color: isDarkMode
                            ? AppThemeColors.baseColorDark.withOpacity(0.5)
                            : AppThemeColors.baseColorLight.withOpacity(0.5),
                      ),
                    ),
                    // Text(
                    //   "Comm",
                    //   style: TextStyle(
                    //     color: isDarkMode
                    //         ? AppThemeColors.defaultBgColor2Dark
                    //         .withOpacity(0.5)
                    //         : AppThemeColors.defaultBg2Color2Light
                    //         .withOpacity(0.5),
                    //   ),
                    // ),
                  ],
                ),
              ),
              Expanded(
                flex: 6,
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.end,
                  children: [
                    Text(
                      "by",
                      style: TextStyle(
                        color: isDarkMode
                            ? AppThemeColors.secondaryColorDark
                            : AppThemeColors.secondaryColorLight,
                      ),
                    ),
                    const SizedBox(
                      width: 5,
                    ),
                    Text(
                      "dafengzhen",
                      style: TextStyle(
                        fontWeight: FontWeight.bold,
                        color: isDarkMode
                            ? AppThemeColors.baseColorDark
                            : AppThemeColors.baseColorLight,
                      ),
                    ),
                    const SizedBox(
                      width: 9,
                    ),

                    ClipRRect(
                      borderRadius: BorderRadius.circular(11),
                      child: Material(
                        child: InkWell(
                          onTap: () {},
                          child: Ink.image(
                            image: const AssetImage("assets/images/avatar.png"),
                            width: 35,
                            height: 35,
                          ),
                        ),
                      ),
                    ),

                    // CircleAvatar(
                    //   backgroundImage:
                    //       AssetImage("assets/images/avatar.png"),
                    // ),

                    // Container(
                    //   width: 35,
                    //   height: 35,
                    //   decoration: BoxDecoration(
                    //     borderRadius: BorderRadius.circular(11),
                    //     image: const DecorationImage(
                    //       image: AssetImage("assets/images/avatar.png"),
                    //     ),
                    //   ),
                    // )
                  ],
                ),
              ),
            ],
          )
        ],
      ),
    );
  }

  Widget _showFirstCommentCard(bool isDarkMode) {
    return Container(
      decoration: BoxDecoration(
        border: Border(
          left: BorderSide(
            width: 1.5,
            color: isDarkMode
                ? AppThemeColors.secondaryColor[700]!
                : AppThemeColors.secondaryColor[150]!,
          ),
        ),
      ),
      child: Padding(
        padding: const EdgeInsets.only(left: 9),
        child: Column(
          children: [
            Row(
              children: [
                const CircleAvatar(
                  backgroundImage: AssetImage('assets/images/avatar.png'),
                  radius: 17,
                ),
                const SizedBox(width: 9),
                Text(
                  "@dafengzhen",
                  style: TextStyle(
                    fontWeight: FontWeight.bold,
                    color: isDarkMode
                        ? AppThemeColors.baseColorDark
                        : AppThemeColors.baseColorLight,
                  ),
                ),
              ],
            ),
            const SizedBox(height: 9),
            Text(
              "Show the first comment. What do you say, it's a great day",
              style: TextStyle(
                color: isDarkMode
                    ? AppThemeColors.baseColorDark
                    : AppThemeColors.baseColorLight,
              ),
            ),
            const SizedBox(height: 9),
            _showReplyForFirstCommentCard(isDarkMode),
          ],
        ),
      ),
    );
  }

  Widget _showReplyForFirstCommentCard(bool isDarkMode) {
    return Container(
      decoration: BoxDecoration(
        border: Border(
          right: BorderSide(
            width: 1.5,
            color: isDarkMode
                ? AppThemeColors.secondaryColor[700]!
                : AppThemeColors.secondaryColor[150]!,
          ),
        ),
      ),
      padding: const EdgeInsets.only(
        left: 9,
        right: 9,
      ),
      child: Text(
        "Show the first comment. What do you say, it's a great day",
        style: TextStyle(
          fontSize: 13,
          color: isDarkMode
              ? AppThemeColors.baseColorDark
              : AppThemeColors.baseColorLight,
        ),
      ),
    );
  }
}
