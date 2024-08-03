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

class ArticleDetailsPage extends StatefulWidget {
  final String id;

  const ArticleDetailsPage({required this.id, super.key});

  @override
  State<ArticleDetailsPage> createState() => _ArticleDetailsPageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _ArticleDetailsPageState extends State<ArticleDetailsPage> {
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
              SingleChildScrollView(
                child: Padding(
                  padding: const EdgeInsets.symmetric(
                    horizontal: 15,
                    vertical: 15,
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      _buildArticleHeader(isDarkMode),
                      const SizedBox(
                        height: 15,
                      ),
                      _buildArticleTitle(isDarkMode),
                      const SizedBox(
                        height: 15,
                      ),
                      _buildArticleImages(),
                      const SizedBox(
                        height: 15,
                      ),
                      _buildArticleTags(isDarkMode),
                      const SizedBox(
                        height: 19,
                      ),
                      _buildArticleDeclaration(isDarkMode),
                      const SizedBox(
                        height: 25,
                      ),
                      _buildArticleContent(isDarkMode),
                      const SizedBox(
                        height: 15,
                      ),
                      SizedBox(
                        height: 27,
                        child: Center(
                          child: Container(
                            width: 50,
                            height: 1,
                            color: isDarkMode
                                ? AppThemeColors.secondaryColor[700]!
                                : AppThemeColors.secondaryColor[150]!,
                          ),
                        ),
                      ),
                      const SizedBox(
                        height: 5,
                      ),
                      Row(
                        mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                        children: [
                          ElevatedButton.icon(
                            icon: const FaIcon(
                              FontAwesomeIcons.thumbsUp,
                              size: 17,
                            ),
                            label: const Text(
                              "Like",
                            ),
                            onPressed: () {},
                          ),
                          ElevatedButton.icon(
                            icon: const FaIcon(
                              FontAwesomeIcons.shareNodes,
                              size: 17,
                            ),
                            label: const Text("Share"),
                            onPressed: () {},
                          ),
                          ElevatedButton.icon(
                            icon: const FaIcon(
                              FontAwesomeIcons.reply,
                              size: 17,
                            ),
                            label: const Text("Reply"),
                            onPressed: () {},
                          ),
                        ],
                      ),
                      const SizedBox(
                        height: 5,
                      ),
                      SizedBox(
                        height: 27,
                        child: Center(
                          child: Container(
                            width: 50,
                            height: 1,
                            color: isDarkMode
                                ? AppThemeColors.secondaryColor[700]!
                                : AppThemeColors.secondaryColor[150]!,
                          ),
                        ),
                      ),
                      const SizedBox(
                        height: 15,
                      ),
                      _buildArticleCommentItem(isDarkMode),
                      const SizedBox(
                        height: 15,
                      ),
                      _buildArticleCommentItem(isDarkMode),
                      const SizedBox(
                        height: 15,
                      ),
                      _createMenuItem(
                        isDarkMode,
                        icon: FontAwesomeIcons.solidCommentDots,
                        text: "View more comments",
                        onTap: () {
                          context.pushNamed(
                            "articleComment",
                            pathParameters: {'id': "3"},
                          );
                        },
                      ),
                      const SizedBox(
                        height: 15,
                      ),
                      const SizedBox(
                        height: 90,
                      ),
                    ],
                  ),
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
                  child: Row(
                    children: [
                      Expanded(
                        child: TextField(
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
                                  IconButton(
                                    onPressed: () {},
                                    icon: const FaIcon(
                                      FontAwesomeIcons.solidCommentDots,
                                      size: 19,
                                    ),
                                  ),
                                ],
                              )),
                        ),
                      ),
                      // const SizedBox(width: 9),
                      // Row(
                      //   mainAxisSize: MainAxisSize.min,
                      //   children: [
                      //     IconButton(
                      //       onPressed: () {},
                      //       icon: FaIcon(
                      //         FontAwesomeIcons.solidCommentDots,
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

  Widget _buildArticleHeader(bool isDarkMode) {
    return Row(
      children: [
        ClipRRect(
          borderRadius: BorderRadius.circular(11),
          child: Material(
            child: InkWell(
              onTap: () {},
              child: Ink.image(
                image: const AssetImage("assets/images/avatar.png"),
                width: 40,
                height: 40,
              ),
            ),
          ),
        ),
        const SizedBox(width: 19),
        Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Authored by',
              style: TextStyle(
                color: isDarkMode
                    ? AppThemeColors.secondaryColorDark
                    : AppThemeColors.secondaryColorLight,
              ),
            ),
            Text(
              'dafengzhen',
              style: TextStyle(
                color: isDarkMode
                    ? AppThemeColors.baseColorDark
                    : AppThemeColors.baseColorLight,
              ),
            ),
          ],
        ),
        const SizedBox(width: 19),
        Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text(
              'Published on',
              style: TextStyle(
                color: isDarkMode
                    ? AppThemeColors.secondaryColorDark
                    : AppThemeColors.secondaryColorLight,
              ),
            ),
            Text(
              '2024-07-09',
              style: TextStyle(
                color: isDarkMode
                    ? AppThemeColors.baseColorDark
                    : AppThemeColors.baseColorLight,
              ),
            ),
          ],
        ),
      ],
    );
  }

  Widget _buildArticleTitle(bool isDarkMode) {
    return Text(
      'Get started with Bootstrap',
      softWrap: true,
      style: TextStyle(
        fontSize: 22,
        fontWeight: FontWeight.bold,
        color: isDarkMode
            ? AppThemeColors.baseColorDark
            : AppThemeColors.baseColorLight,
      ),
    );
  }

  Widget _buildArticleImages() {
    return SizedBox(
      height: 260,
      child: ListView.separated(
        separatorBuilder: (context, index) {
          return const SizedBox(
            width: 15,
          );
        },
        scrollDirection: Axis.horizontal,
        itemCount: 5,
        itemBuilder: (BuildContext context, int index) {
          return ClipRRect(
            borderRadius: BorderRadius.circular(11),
            child: Image.network(
              'https://images.unsplash.com/photo-1696733585001-868eb49cbfa6?h=260',
              fit: BoxFit.cover,
            ),
          );
        },
      ),
    );
  }

  Widget _buildArticleTags(bool isDarkMode) {
    return Wrap(
      spacing: 11,
      runSpacing: 3,
      children: List.generate(
        7,
        (index) => ChoiceChip(
          backgroundColor: isDarkMode
              ? AppThemeColors.tertiaryBgDark
              : AppThemeColors.tertiaryBgLight,
          label: Text(
            '#Tag $index',
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
            ),
          ),
          selected: false,
          onSelected: (value) {},
        ),
      ),
    );
  }

  Widget _buildArticleDeclaration(bool isDarkMode) {
    return Container(
      padding: const EdgeInsets.symmetric(
        horizontal: 7,
      ),
      decoration: BoxDecoration(
        border: Border(
          left: BorderSide(
            width: 2,
            color: isDarkMode
                ? AppThemeColors.secondaryColor[700]!
                : AppThemeColors.secondaryColor[150]!,
          ),
        ),
      ),
      child: Text(
        '"This is a declaration"',
        style: TextStyle(
          fontSize: 17,
          fontStyle: FontStyle.italic,
          color: isDarkMode
              ? AppThemeColors.baseColorDark
              : AppThemeColors.baseColorLight,
        ),
      ),
    );
  }

  Widget _buildArticleContent(bool isDarkMode) {
    return Text(
      'Bootstrap is a powerful, feature-packed frontend toolkit. Build anything—from prototype to production—in minutes.Get started by including Bootstrap’s production-ready CSS and JavaScript via CDN without the need for any build steps. See it in practice with this Bootstrap CodePen demo.',
      style: TextStyle(
        fontSize: 17,
        color: isDarkMode
            ? AppThemeColors.baseColorDark
            : AppThemeColors.baseColorLight,
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
