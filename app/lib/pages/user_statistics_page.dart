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

class UserStatisticsPage extends StatefulWidget {
  final String id;

  const UserStatisticsPage({required this.id, super.key});

  @override
  State<UserStatisticsPage> createState() => _UserStatisticsPageState();
}

class Article {
  final String title;
  final String subtitle;

  Article(this.title, this.subtitle);
}

class _UserStatisticsPageState extends State<UserStatisticsPage> {
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
                  child: _buildStatisticItem(isDarkMode),
                );
              } else if (index == 11 - 1) {
                return Padding(
                  padding: const EdgeInsets.only(
                    bottom: 41,
                  ),
                  child: _buildStatisticItem(isDarkMode),
                );
              } else {
                return _buildStatisticItem(isDarkMode);
              }
            },
            separatorBuilder: (context, index) {
              return const SizedBox(
                height: 15,
              );
            },
            itemCount: 11,
          ),
        ),
      ),
    );
  }

  Widget _buildStatisticItem(bool isDarkMode) {
    return Card(
      margin: const EdgeInsets.all(0),
      color: isDarkMode
          ? AppThemeColors.tertiaryBgDark
          : AppThemeColors.tertiaryBgLight,
      child: ListTile(
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(11),
        ),
        onTap: () {

        },
        leading: FaIcon(
          FontAwesomeIcons.chartSimple,
          size: 17,
          color: isDarkMode
              ? AppThemeColors.baseColorDark.withOpacity(0.7)
              : AppThemeColors.baseColorLight.withOpacity(0.7),
        ),
        title: Text(
          'Two-line ListTile',
          style: TextStyle(
            fontSize: 17,
            color: isDarkMode
                ? AppThemeColors.baseColorDark
                : AppThemeColors.baseColorLight,
          ),
        ),
        // subtitle: Text('Here is a second line',
        //     style: TextStyle(
        //       color: isDarkMode
        //           ? AppThemeColors.secondaryColorDark
        //           : AppThemeColors.secondaryColorLight,
        //     )),
        trailing: Text(
          '79',
          style: TextStyle(
            fontWeight: FontWeight.bold,
            fontSize: 17,
            color: isDarkMode
                ? AppThemeColors.baseColorDark
                : AppThemeColors.baseColorLight,
          ),
        ),
      ),
    );
  }
}
