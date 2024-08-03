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

class ArticleCommentPage extends StatefulWidget {
  final String id;

  const ArticleCommentPage({required this.id, super.key});

  @override
  State<ArticleCommentPage> createState() => _ArticleCommentPageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _ArticleCommentPageState extends State<ArticleCommentPage> {
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
          color: isDarkMode
              ? AppThemeColors.baseBgDark
              : AppThemeColors.baseBgLight,
          child: Stack(
            children: [
              Padding(
                padding: const EdgeInsets.symmetric(
                  horizontal: 15,
                ),
                child: ListView.separated(
                  itemBuilder: (context, index) {
                    if (index == 9) {
                      return Column(
                        children: [
                          _buildArticleCommentItem(isDarkMode),
                          const SizedBox(
                            height: 15,
                          ),
                          const SizedBox(
                            // height: 90,
                            height: 130,
                          ),
                        ],
                      );
                    }

                    if (index == 0) {
                      return Padding(
                        padding: const EdgeInsets.only(
                          top: 15,
                        ),
                        child: _buildArticleCommentItem(isDarkMode),
                      );
                    }

                    return _buildArticleCommentItem(isDarkMode);
                  },
                  separatorBuilder: (context, index) {
                    return const SizedBox(
                      height: 15,
                    );
                  },
                  itemCount: 10,
                ),
              ),
              Positioned(
                bottom: 0,
                left: 0,
                right: 0,
                child: Container(
                  padding: const EdgeInsets.symmetric(
                    vertical: 15,
                    horizontal: 15,
                  ),
                  decoration: BoxDecoration(
                    color: isDarkMode
                        ? AppThemeColors.baseBgDark
                        : AppThemeColors.baseBgLight,
                    borderRadius: const BorderRadius.vertical(
                      bottom: Radius.circular(17),
                    ),
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Row(
                        children: [
                          TextButton.icon(
                            onPressed: () {},
                            label: Text(
                              "Cancel reply",
                              style: TextStyle(
                                color: isDarkMode
                                    ? AppThemeColors.secondaryColorDark
                                    : AppThemeColors.secondaryColorLight,
                                fontWeight: FontWeight.bold,
                              ),
                            ),
                            icon: FaIcon(
                              FontAwesomeIcons.xmark,
                              size: 17,
                              color: isDarkMode
                                  ? AppThemeColors.secondaryColorDark
                                  : AppThemeColors.secondaryColorLight,
                            ),
                          )
                        ],
                      ),
                      const SizedBox(
                        height: 7,
                      ),
                      TextField(
                        controller: _controller,
                        minLines: 1,
                        maxLines: 5,
                        decoration: InputDecoration(
                            hintText: 'Enter a comment',
                            border: OutlineInputBorder(
                              borderRadius: BorderRadius.circular(11),
                            ),
                            contentPadding: const EdgeInsets.symmetric(
                              horizontal: 15,
                              vertical: 11,
                            ),
                            prefixIcon: Row(
                              mainAxisSize: MainAxisSize.min,
                              children: [
                                IconButton(
                                  onPressed: () {},
                                  icon: const FaIcon(
                                    FontAwesomeIcons.at,
                                    size: 19,
                                  ),
                                )
                              ],
                            ),
                            suffixIcon: Row(
                              mainAxisSize: MainAxisSize.min,
                              children: [
                                IconButton(
                                  onPressed: () {},
                                  icon: const FaIcon(
                                    FontAwesomeIcons.solidPaperPlane,
                                    size: 19,
                                  ),
                                ),
                              ],
                            )),
                      )
                      // const SizedBox(width: 9),
                      // Row(
                      //   mainAxisSize: MainAxisSize.min,
                      //   children: [
                      //     IconButton(
                      //       onPressed: () {},
                      //       icon: FaIcon(
                      //         FontAwesomeIcons.xmark,
                      //         size: 19,
                      //         color: isDarkMode
                      //             ? AppThemeColors.defaultBgColorDark
                      //             : AppThemeColors.defaultBgColorLight,
                      //       ),
                      //     )
                      //   ],
                      // ),
                    ],
                  ),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildArticleCommentItem(bool isDarkMode) {
    return Container(
      padding: const EdgeInsets.only(top: 17, bottom: 5, left: 12, right: 12),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.tertiaryBgDark
            : AppThemeColors.tertiaryBgLight,
        borderRadius: BorderRadius.circular(17),
        // boxShadow: [
        //   BoxShadow(
        //     color: isDarkMode
        //         ? AppThemeColors.defaultBg3Dark
        //         : AppThemeColors.defaultBg2Light.withOpacity(0.5),
        //     spreadRadius: 5,
        //     blurRadius: 7,
        //     offset: Offset(0, 3), // changes position of shadow
        //   ),
        // ],
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              const CircleAvatar(
                backgroundImage: AssetImage('assets/images/avatar.png'),
                radius: 19,
              ),
              const SizedBox(width: 9),
              Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    "dafengzhen",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
                    ),
                  ),
                  Text(
                    "2024-07-09",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                ],
              ),
              const Spacer(),
              Column(
                children: [
                  Text(
                    "1#",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark.withOpacity(0.5)
                          : AppThemeColors.secondaryColorLight.withOpacity(0.5),
                    ),
                  ),
                  const Text(
                    "",
                  ),
                ],
              ),
            ],
          ),
          const SizedBox(height: 15),
          _buildArticleCommentReply(isDarkMode),
          const SizedBox(height: 15),
          Text(
            """Give textual form controls like <input>s and <textarea>s an upgrade with custom styles, sizing, focus states, and more.""",
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
            ),
          ),
          const SizedBox(height: 15),
          Row(
            children: [
              IconButton(
                onPressed: () {},
                icon: const FaIcon(
                  FontAwesomeIcons.thumbsUp,
                  size: 17,
                ),
              ),
              IconButton(
                onPressed: () {},
                icon: const FaIcon(
                  FontAwesomeIcons.reply,
                  size: 17,
                ),
              )
            ],
          ),
        ],
      ),
    );
  }

  Widget _buildArticleCommentReply(bool isDarkMode) {
    return Container(
      padding: const EdgeInsets.only(top: 7, bottom: 7, left: 12, right: 12),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.secondaryBgDark.withOpacity(0.7)
            : AppThemeColors.secondaryBgLight.withOpacity(0.7),
        border: Border(
          left: BorderSide(
            width: 2,
            color: isDarkMode
                ? AppThemeColors.secondaryColor[700]!
                : AppThemeColors.secondaryColor[150]!,
          ),
        ),
        // borderRadius: BorderRadius.circular(7),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              // const CircleAvatar(
              //   backgroundImage: AssetImage('assets/images/avatar.png'),
              //   radius: 19,
              // ),
              // const SizedBox(width: 9),
              Row(
                children: [
                  Icon(
                    Icons.reply,
                    color: isDarkMode
                        ? AppThemeColors.secondaryColorDark.withOpacity(0.7)
                        : AppThemeColors.secondaryColorLight.withOpacity(0.7),
                  ),
                  const SizedBox(
                    width: 7,
                  ),
                  Text(
                    "@dafengzhen",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                  Text(
                    " / ",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                  Text(
                    "2024-07-09",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                ],
              ),
            ],
          ),
          const SizedBox(height: 15),
          Text(
            """Give textual form controls like <input>s and <textarea>s an upgrade with custom styles, sizing, focus states, and more.""",
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
            ),
          ),
        ],
      ),
    );
  }
}
