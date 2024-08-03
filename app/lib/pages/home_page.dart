import 'dart:math';

import 'package:cached_network_image/cached_network_image.dart';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/post_api.dart';
import '../configs/configs.dart';
import '../dtos/query_parameters_dto.dart';
import '../enums/load_data_type_enum.dart';
import '../models/pageable.dart';
import '../models/post.dart';
import '../providers/app_theme_mode.dart';
import '../providers/login_info.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';

class HomePage extends StatefulWidget {
  const HomePage({super.key});

  @override
  State<HomePage> createState() => _HomePageState();
}

class _HomePageState extends State<HomePage> {
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
    setState(() {
      _isLoading = true;
      if (type == LoadDataTypeEnum.initialize) {
        _isLoadingInit = true;
      } else if (type == LoadDataTypeEnum.loadMore) {
        _isLoadingMore = true;
      }
    });

    try {
      Map<String, String> parameters = {};
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

    return Stack(
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
              _buildNewArticleTip(isDarkMode),
              if (_isLoadingInit) _buildLoadingIndicator(),
              _buildList(isDarkMode),
              if (!_isLoadingInit && _isLoadingMore && _hasMore)
                _buildLoadingIndicator(),
              if (!_isLoadingInit && !_isLoadingMore && !_hasMore)
                _buildNoMoreDataMessage(isDarkMode),
              const SliverToBoxAdapter(child: SizedBox(height: 35)),
            ],
          ),
        ),
      ],
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
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          Text(
            isLoggedIn ? "Welcome" : appTitle,
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
            ),
          ),
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

  Widget _buildNewArticleTip(bool isDarkMode) {
    return SliverToBoxAdapter(
      child: Padding(
        padding: const EdgeInsets.symmetric(
          horizontal: 15,
          vertical: 15,
        ),
        child: Container(
          height: 60,
          padding: const EdgeInsets.only(left: 12, right: 0),
          decoration: BoxDecoration(
            color: isDarkMode
                ? AppThemeColors.tertiaryBgDark
                : AppThemeColors.tertiaryBgLight,
            borderRadius: BorderRadius.circular(17),
          ),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Text(
                "Create a new article.",
                style: TextStyle(
                  color: isDarkMode
                      ? AppThemeColors.secondaryColorDark
                      : AppThemeColors.secondaryColorLight,
                ),
              ),
              IconButton(
                onPressed: () {
                  context.pushNamed("articleEdit");
                },
                icon: const FaIcon(
                  FontAwesomeIcons.solidSquarePlus,
                  color: AppThemeColors.info,
                ),
              )
            ],
          ),
        ),
      ),
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
          child: _createArticleCard(isDarkMode, item: _list[index]),
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

  Widget _createArticleCard(bool isDarkMode, {required Post item}) {
    final baseColor = isDarkMode
        ? AppThemeColors.baseColorDark
        : AppThemeColors.baseColorLight;
    final secondaryColor = isDarkMode
        ? AppThemeColors.secondaryColorDark
        : AppThemeColors.secondaryColorLight;
    final dangerColor = isDarkMode
        ? AppThemeColors.tertiaryBgDangerColorDark
        : AppThemeColors.tertiaryBgDangerColorLight;
    final bgColor = isDarkMode
        ? AppThemeColors.tertiaryBgDark
        : AppThemeColors.tertiaryBgLight;

    final id = item.id;
    final isLike = item.liked ?? false;
    final likesCount = formatCount(item.likesCount);
    final commentsCount = formatCount(item.commentsCount);
    final createdOn =
        item.createdOn != null ? formatRelativeTime(item.createdOn) : null;
    final cover = isHttpOrHttps(item.cover) ? item.cover : null;
    final username = getUsernameOrAnonymous(item.user?.username);
    final userId = item.user?.id;
    final avatar = getAvatarOrDefault(item.user?.avatar);
    final comments = item.comments;

    void onClickName() {
      context
          .pushNamed("articleDetails", pathParameters: {'id': id.toString()});
    }

    void onClickUsername() {
      if (userId != null) {
        context.pushNamed("userDetails",
            pathParameters: {'id': userId.toString()});
      }
    }

    return Container(
      padding: EdgeInsets.only(
        top: 17,
        bottom: 7,
        right: 12,
        left: likesCount == "0" && commentsCount == "0" ? 12 : 0,
      ),
      decoration: BoxDecoration(
        color: bgColor,
        borderRadius: BorderRadius.circular(17),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              if (likesCount != "0" || commentsCount != "0")
                Expanded(
                  child: Center(
                    child: Text(
                      likesCount,
                      style: TextStyle(color: baseColor),
                    ),
                  ),
                ),
              Expanded(
                flex: 6,
                child: Row(
                  children: [
                    Expanded(
                      child: GestureDetector(
                        onTap: onClickName,
                        child: Text(
                          item.name,
                          softWrap: true,
                          style: TextStyle(
                              fontWeight: FontWeight.bold, color: baseColor),
                        ),
                      ),
                    ),
                    if (createdOn != null) ...[
                      const SizedBox(width: 5),
                      GestureDetector(
                        onTap: onClickName,
                        child: Text(
                          createdOn,
                          style: TextStyle(color: secondaryColor),
                        ),
                      ),
                    ],
                  ],
                ),
              ),
            ],
          ),
          if (cover != null && cover.isNotEmpty) ...[
            const SizedBox(height: 9),
            Row(
              children: [
                Expanded(
                  child: Center(
                    child: IconButton(
                      onPressed: () {},
                      icon: FaIcon(
                        isLike
                            ? FontAwesomeIcons.solidThumbsUp
                            : FontAwesomeIcons.thumbsUp,
                        size: 20,
                        color: baseColor,
                      ),
                    ),
                  ),
                ),
                Expanded(
                  flex: 6,
                  child: ClipRRect(
                    borderRadius: BorderRadius.circular(9),
                    child: CachedNetworkImage(
                      imageUrl: cover,
                      progressIndicatorBuilder:
                          (context, url, downloadProgress) => Padding(
                        padding: const EdgeInsets.symmetric(vertical: 9),
                        child: Center(
                          child: CircularProgressIndicator(
                              value: downloadProgress.progress),
                        ),
                      ),
                      errorWidget: (context, url, error) => Row(
                        children: [
                          Icon(Icons.error, color: dangerColor),
                          const SizedBox(width: 3),
                          Text("Failed to load image",
                              style: TextStyle(color: dangerColor)),
                        ],
                      ),
                    ),
                  ),
                ),
              ],
            ),
          ],
          if (item.overview != null && item.overview!.isNotEmpty) ...[
            const SizedBox(height: 9),
            Row(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                if ((likesCount != "0" || commentsCount != "0") &&
                    cover != null)
                  const Expanded(child: SizedBox()),
                if ((likesCount != "0" || commentsCount != "0") &&
                    cover == null)
                  Expanded(
                    child: Center(
                      child: IconButton(
                        onPressed: () {},
                        icon: FaIcon(
                          isLike
                              ? FontAwesomeIcons.solidThumbsUp
                              : FontAwesomeIcons.thumbsUp,
                          size: 20,
                          color: baseColor,
                        ),
                      ),
                    ),
                  ),
                Expanded(
                  flex: 6,
                  child: GestureDetector(
                    onTap: onClickName,
                    child: Text(
                      item.overview!,
                      style: TextStyle(color: baseColor),
                    ),
                  ),
                ),
              ],
            ),
          ],
          if (comments != null && comments.content.isNotEmpty) ...[
            const SizedBox(
              height: 15,
            ),
            Row(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                const Expanded(child: SizedBox()),
                Expanded(
                  flex: 6,
                  child: _showFirstCommentCard(isDarkMode, item: item),
                ),
              ],
            ),
          ],
          const SizedBox(height: 15),
          if (commentsCount != "0")
            Row(
              children: [
                Expanded(
                  child: Center(
                    child: Text(
                      commentsCount,
                      style: TextStyle(color: baseColor),
                    ),
                  ),
                ),
                const Expanded(
                  flex: 6,
                  child: SizedBox(),
                ),
              ],
            ),
          Row(
            crossAxisAlignment: CrossAxisAlignment.center,
            children: [
              if (commentsCount != "0")
                Expanded(
                  child: Column(
                    children: [
                      IconButton(
                        onPressed: () {},
                        icon: FaIcon(
                          FontAwesomeIcons.comment,
                          size: 20,
                          color: baseColor.withOpacity(0.5),
                        ),
                      ),
                    ],
                  ),
                ),
              Expanded(
                flex: 6,
                child: Row(
                  mainAxisAlignment: MainAxisAlignment.end,
                  children: [
                    GestureDetector(
                      onTap: onClickUsername,
                      child: Text(
                        "by",
                        style: TextStyle(color: secondaryColor),
                      ),
                    ),
                    const SizedBox(width: 5),
                    GestureDetector(
                      onTap: onClickUsername,
                      child: Text(
                        username,
                        style: TextStyle(
                            fontWeight: FontWeight.bold, color: baseColor),
                      ),
                    ),
                    const SizedBox(width: 9),
                    ClipRRect(
                      borderRadius: BorderRadius.circular(11),
                      child: Material(
                        child: InkWell(
                          onTap: onClickUsername,
                          child: Ink.image(
                            image: avatar,
                            width: 35,
                            height: 35,
                          ),
                        ),
                      ),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ],
      ),
    );
  }

  Widget _showFirstCommentCard(bool isDarkMode, {required Post item}) {
    final baseColor = isDarkMode
        ? AppThemeColors.baseColorDark
        : AppThemeColors.baseColorLight;
    final secondaryColor = isDarkMode
        ? AppThemeColors.secondaryColor[700]!
        : AppThemeColors.secondaryColor[150]!;
    final secondaryColor2 = isDarkMode
        ? AppThemeColors.secondaryColorDark
        : AppThemeColors.secondaryColorLight;

    final id = item.id;
    final comment = item.comments!.content[0].comment!;
    final reply = item.comments!.content.length == 2
        ? item.comments!.content[1].reply
        : null;
    final avatar = getAvatarOrDefault(comment.user?.avatar);
    final username = getUsernameOrAnonymous(comment.user?.username);
    final commentContent = comment.content;

    void onClickComment() {
      context
          .pushNamed("articleComment", pathParameters: {'id': id.toString()});
    }

    return GestureDetector(
      onTap: onClickComment,
      child: Container(
        padding: const EdgeInsets.only(left: 9),
        decoration: BoxDecoration(
          border: Border(
            left: BorderSide(
              width: 1.5,
              color: secondaryColor,
            ),
          ),
        ),
        child: Column(
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
                        image: avatar,
                        width: 35,
                        height: 35,
                      ),
                    ),
                  ),
                ),
                const SizedBox(width: 9),
                Text(
                  "#",
                  style: TextStyle(color: secondaryColor2),
                ),
                Expanded(
                  child: Text(
                    username,
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                    style: TextStyle(
                      fontWeight: FontWeight.bold,
                      color: baseColor,
                    ),
                  ),
                ),
              ],
            ),
            const SizedBox(height: 11),
            Text(
              commentContent,
              style: TextStyle(
                color: baseColor,
              ),
            ),
            const SizedBox(height: 9),
            if (reply != null)
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  const SizedBox(),
                  _buildReplyCard(isDarkMode, item: item),
                ],
              ),
          ],
        ),
      ),
    );
  }

  Widget _buildReplyCard(bool isDarkMode, {required Post item}) {
    final baseColor = isDarkMode
        ? AppThemeColors.baseColorDark
        : AppThemeColors.baseColorLight;
    final secondaryColor = isDarkMode
        ? AppThemeColors.secondaryColor[700]!
        : AppThemeColors.secondaryColor[150]!;

    final reply = item.comments!.content[1].reply!;
    final replyContent = reply.content;

    return Container(
      decoration: BoxDecoration(
        border: Border(
          right: BorderSide(
            width: 1.5,
            color: secondaryColor,
          ),
        ),
      ),
      padding: const EdgeInsets.symmetric(horizontal: 9),
      child: Text(
        replyContent,
        style: TextStyle(
          fontSize: 13,
          color: baseColor,
        ),
      ),
    );
  }
}
