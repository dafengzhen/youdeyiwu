import 'dart:developer';

import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../configs/configs.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';

class MessagePage extends StatefulWidget {
  const MessagePage({super.key});

  @override
  State<MessagePage> createState() => _MessagePageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _MessagePageState extends State<MessagePage> {
  List<Article> articles = List.generate(
    5,
    (index) => Article(
      'Article Title $index',
      'Subtitle for Article $index',
    ),
  );

  late ScrollController _scrollController;
  bool _isLoadingMore = false;
  int _page = 1;
  bool _hasMore = true;

  @override
  void initState() {
    super.initState();
    _loadData();

    // _tabController = TabController(length: 2, vsync: this);
    _scrollController = ScrollController();
    _scrollController.addListener(() {
      if (_scrollController.position.pixels ==
          _scrollController.position.maxScrollExtent) {
        _loadMore();
      }
    });
  }

  @override
  void dispose() {
    // _tabController.dispose();
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

    return DefaultTabController(
      length: 2,
      child: Stack(
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
                  sliver: SliverList.separated(
                    itemCount: articles.length + (_isLoadingMore ? 1 : 0),
                    itemBuilder: (context, index) {
                      if (index == articles.length) {
                        return const Padding(
                          padding: EdgeInsets.all(16.0),
                          child: Center(child: CircularProgressIndicator()),
                        );
                      }

                      return _createMessageCard(isDarkMode);
                    },
                    separatorBuilder: (context, index) => const SizedBox(
                      height: 19,
                    ),
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _createMessageCard(bool isDarkMode) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(
          children: [
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
            const SizedBox(
              width: 9,
            ),
            Expanded(
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Text(
                    "Comment Notification",
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
                  Text(
                    "19/7/24",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                ],
              ),
            ),
          ],
        ),
        const SizedBox(
          height: 15,
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
          height: 15,
        ),
        Wrap(
          spacing: 11,
          runSpacing: 1,
          children: [
            ElevatedButton.icon(
              icon: const FaIcon(
                FontAwesomeIcons.circleInfo,
                size: 17,
              ),
              label: const Text("Details"),
              onPressed: () {},
            ),
            ElevatedButton.icon(
              icon: const FaIcon(
                FontAwesomeIcons.solidCircleCheck,
                size: 17,
              ),
              label: const Text("Read"),
              onPressed: () {},
            ),
            ElevatedButton.icon(
              icon: FaIcon(
                FontAwesomeIcons.trashCan,
                size: 17,
                color: isDarkMode
                    ? AppThemeColors.baseBgDangerColorDark
                    : AppThemeColors.baseBgDangerColorLight,
              ),
              label: Text(
                "Delete",
                style: TextStyle(
                  color: isDarkMode
                      ? AppThemeColors.baseBgDangerColorDark
                      : AppThemeColors.baseBgDangerColorLight,
                ),
              ),
              onPressed: () {},
            ),
          ],
        ),
      ],
    );
  }
}
