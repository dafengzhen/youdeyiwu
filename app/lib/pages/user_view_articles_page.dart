import 'dart:math';

import 'package:flutter/material.dart';
import 'package:provider/provider.dart';

import '../apis/post_api.dart';
import '../dtos/query_parameters_dto.dart';
import '../enums/load_data_type_enum.dart';
import '../models/pageable.dart';
import '../models/post.dart';
import '../providers/app_theme_mode.dart';
import '../providers/login_info.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../widgets/common.dart';
import '../widgets/post.dart';

class UserViewArticlesPage extends StatefulWidget {
  final String? sectionId;

  final String? tagId;

  const UserViewArticlesPage({this.sectionId, this.tagId, super.key});

  @override
  State<UserViewArticlesPage> createState() => _UserViewArticlesPageState();
}

class _UserViewArticlesPageState extends State<UserViewArticlesPage> {
  final ScrollController _scrollController = ScrollController();

  List<Post> _list = [];
  Pageable? _pageable;
  bool _isLoadingInit = true;
  bool _isLoadingMore = false;
  bool _isLoading = false;
  bool _hasMore = false;

  @override
  void initState() {
    super.initState();
    _loadData();
    _scrollController.addListener(_onScroll);
  }

  @override
  void dispose() {
    _scrollController.dispose();
    super.dispose();
  }

  Future<void> _refresh() async {
    if (_isLoading == false) {
      await _loadData(
        type: LoadDataTypeEnum.refresh,
      );
    }
  }

  Future<void> _loadData({
    LoadDataTypeEnum type = LoadDataTypeEnum.initialize,
    QueryParametersDto? dto,
  }) async {
    var sectionId = widget.sectionId;
    var tagId = widget.tagId;

    if (sectionId == null && tagId == null) {
      return;
    }

    setState(() {
      _isLoading = true;
      if (type == LoadDataTypeEnum.initialize) {
        _isLoadingInit = true;
      } else if (type == LoadDataTypeEnum.loadMore) {
        _isLoadingMore = true;
      }
    });

    try {
      Map<String, String> parameters = {
        "sectionId": sectionId ?? '',
        "tagId": tagId ?? ''
      };
      if (type == LoadDataTypeEnum.loadMore && _pageable?.next == true) {
        parameters['page'] =
            min(_pageable!.page + 1, _pageable!.pages).toString();
      } else if (type == LoadDataTypeEnum.loadMore) {
        return;
      }

      var base = QueryParametersDto.fromJson(parameters);
      var dto0 =
          QueryParametersDto.merge(base, dto ?? const QueryParametersDto());
      var page = await context.read<PostApi>().queryPosts(dto: dto0);

      setState(() {
        if (type == LoadDataTypeEnum.loadMore) {
          _list.addAll(page.content);
        } else {
          _list = page.content;
        }
        _pageable = page.pageable;
        _hasMore = page.pageable.next;
      });
    } catch (e) {
      if (mounted) {
        _showErrorPrompt(e);
      }
    } finally {
      setState(() {
        _isLoading = false;
        if (type == LoadDataTypeEnum.initialize) {
          _isLoadingInit = false;
        } else if (type == LoadDataTypeEnum.loadMore) {
          _isLoadingMore = false;
        }
      });
    }
  }

  void _onScroll() {
    if (_scrollController.position.pixels ==
        _scrollController.position.maxScrollExtent) {
      if (!_isLoading && _hasMore) {
        _loadData(type: LoadDataTypeEnum.loadMore);
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
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;
    final bool isLoggedIn =
        context.select((LoginInfo value) => value.isLoggedIn);

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
                _buildSliverAppBar(isDarkMode, barBackgroundColor, isLoggedIn),
                if (_isLoadingInit)
                  SliverFillRemaining(child: buildCenteredLoadingIndicator()),
                _buildList(isDarkMode),
                if (_isLoadingMore) _buildLoadingIndicator(),
                if (_list.isNotEmpty && !_hasMore)
                  _buildNoMoreDataMessage(isDarkMode),
                if (!_isLoadingInit && _list.isEmpty)
                  SliverFillRemaining(
                    child: buildCenteredNoMoreDataMessage(isDarkMode),
                  ),
                const SliverToBoxAdapter(child: SizedBox(height: 35)),
              ],
            ),
          ),
        ],
      ),
    );
  }

  SliverAppBar _buildSliverAppBar(
    bool isDarkMode,
    Color barBackgroundColor,
    bool isLoggedIn,
  ) {
    return SliverAppBar(
      backgroundColor: barBackgroundColor,
      surfaceTintColor: barBackgroundColor,
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
      floating: true,
    );
  }

  SliverToBoxAdapter _buildLoadingIndicator() {
    return const SliverToBoxAdapter(
      child: Padding(
        padding: EdgeInsets.symmetric(vertical: 15),
        child: Center(child: CircularProgressIndicator()),
      ),
    );
  }

  SliverList _buildList(bool isDarkMode) {
    return SliverList.separated(
      itemCount: _list.length,
      itemBuilder: (context, index) {
        return Padding(
          padding: const EdgeInsets.symmetric(
            horizontal: 15,
          ),
          child: buildArticleCard(
            isDarkMode,
            context,
            item: _list[index],
            reload: (updateData) {
              updateData(index, _list);
            },
          ),
        );
      },
      separatorBuilder: (context, index) => const SizedBox(height: 13),
    );
  }

  SliverToBoxAdapter _buildNoMoreDataMessage(bool isDarkMode) {
    return SliverToBoxAdapter(
      child: Padding(
        padding: const EdgeInsets.symmetric(vertical: 15),
        child: Center(
          child: Text(
            "No more data available",
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.tertiaryColorDark
                  : AppThemeColors.tertiaryColorLight,
            ),
          ),
        ),
      ),
    );
  }
}
