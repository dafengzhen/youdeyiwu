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

class ContentDetailsPage extends StatefulWidget {
  final String id;

  const ContentDetailsPage({required this.id, super.key});

  @override
  State<ContentDetailsPage> createState() => _ContentDetailsPageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _ContentDetailsPageState extends State<ContentDetailsPage>
    with SingleTickerProviderStateMixin {
  List<Article> articles = List.generate(
    20,
    (index) => Article(
      'Article Title $index',
      'Subtitle for Article $index',
    ),
  );

  late TabController _tabController;
  PageStorageBucket _bucket = PageStorageBucket();
  ScrollController _scrollController = ScrollController();
  bool _isLoadingMore = false;
  int _page = 1;
  bool _hasMore = true;

  void _handleTabChange() {
    print(_tabController.index);
    print(_tabController.offset);

    if (_tabController.index == _tabController.length - 1 &&
        _tabController.offset > 0.5) {
      _tabController.animateTo(0);
    } else if (_tabController.index == 0 && _tabController.offset < -0.5) {
      _tabController.animateTo(_tabController.length - 1);
    }
  }

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

    _tabController = TabController(length: 8, vsync: this);
    _tabController.addListener(_handleTabChange);
  }

  @override
  void dispose() {
    _scrollController.dispose();
    _tabController.removeListener(_handleTabChange);
    _tabController.dispose();
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

    return DefaultTabController(
      length: 8,
      child: Scaffold(
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
                  Provider.of<AppThemeMode>(context, listen: false)
                      .toggleTheme();
                },
              )
            ],
          ),
        ),
        body: Container(
          color: barBackgroundColor,
          child: Container(
            padding: const EdgeInsets.only(
              top: 15,
              left: 15,
              right: 15,
            ),
            color: isDarkMode
                ? AppThemeColors.baseBgDark
                : AppThemeColors.baseBgLight,
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  "Javascript",
                  style: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.baseColorDark
                        : AppThemeColors.baseColorLight,
                    fontWeight: FontWeight.bold,
                    fontSize: 21,
                  ),
                ),
                const SizedBox(
                  height: 17,
                ),
                Text(
                  "JavaScript (JS) is a lightweight interpreted (or just-in-time compiled) programming language with first-class functions",
                  style: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.baseColorDark
                        : AppThemeColors.baseColorLight,
                  ),
                ),
                const SizedBox(
                  height: 17,
                ),
                Wrap(
                  spacing: 13,
                  runSpacing: 1,
                  children: [
                    TextButton(
                      onPressed: () {},
                      child: const Text('dafengzhen'),
                      style: OutlinedButton.styleFrom(
                        side: BorderSide(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColorDark
                              : AppThemeColors.secondaryColorLight,
                        ),
                        shape: RoundedRectangleBorder(
                          borderRadius: BorderRadius.circular(9),
                        ),
                      ),
                    ),
                    IconButton(
                      onPressed: () {},
                      icon: FaIcon(
                        FontAwesomeIcons.circleInfo,
                        size: 13,
                        color: isDarkMode
                            ? AppThemeColors.secondaryColorDark
                            : AppThemeColors.secondaryColorLight,
                      ),
                      style: OutlinedButton.styleFrom(
                        side: BorderSide(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColorDark
                                  .withOpacity(0.5)
                              : AppThemeColors.secondaryColorLight
                                  .withOpacity(0.5),
                        ),
                        shape: RoundedRectangleBorder(
                          borderRadius: BorderRadius.circular(9),
                        ),
                      ),
                    ),
                  ],
                ),
                const SizedBox(
                  height: 17,
                ),
                Expanded(
                  child: Column(
                    children: [
                      TabBar(
                        controller: _tabController,
                        tabs: const [
                          Tab(
                            text: "Function",
                          ),
                          Tab(
                            text: "Class",
                          ),
                          Tab(
                            text: "Const",
                          ),
                          Tab(
                            text: "Var",
                          ),
                          Tab(
                            text: "Function",
                          ),
                          Tab(
                            text: "Class",
                          ),
                          Tab(
                            text: "Const",
                          ),
                          Tab(
                            text: "Var",
                          ),
                        ],
                        isScrollable: true,
                        tabAlignment: TabAlignment.start,
                      ),
                      Expanded(
                        child: PageStorage(
                          bucket: _bucket,
                          child: TabBarView(
                            controller: _tabController,
                            children: [
                              // MyTabPage(PageStorageKey('page1')),
                              Container(
                                color: isDarkMode
                                    ? AppThemeColors.baseBgDark
                                    : AppThemeColors.baseBgLight,
                                child: RefreshIndicator(
                                  onRefresh: _refresh,
                                  child: ListView.separated(
                                    itemCount: articles.length +
                                        (_isLoadingMore ? 1 : 0),
                                    itemBuilder: (context, index) {
                                      if (index == articles.length) {
                                        return const Padding(
                                          padding: EdgeInsets.all(16.0),
                                          child: Center(
                                              child:
                                                  CircularProgressIndicator()),
                                        );
                                      }

                                      if (index == 0) {
                                        return Padding(
                                            padding: const EdgeInsets.only(
                                              top: 15,
                                            ),
                                            child:
                                                _createArticleCard(isDarkMode));
                                      }

                                      return _createArticleCard(isDarkMode);
                                    },
                                    separatorBuilder: (context, index) =>
                                        const SizedBox(
                                      height: 13,
                                    ),
                                  ),
                                ),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                              const SizedBox(
                                child: Icon(Icons.flight),
                              ),
                            ],
                          ),
                        ),
                      ),
                    ],
                  ),
                ),
              ],
            ),
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
