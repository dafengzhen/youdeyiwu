import 'dart:math';

import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/message_api.dart';
import '../configs/configs.dart';
import '../dtos/query_parameters_dto.dart';
import '../enums/load_data_type_enum.dart';
import '../enums/message_state_enum.dart';
import '../models/message.dart';
import '../models/pageable.dart';
import '../providers/app_theme_mode.dart';
import '../providers/login_info.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';

class MessagePage extends StatefulWidget {
  const MessagePage({super.key});

  @override
  State<MessagePage> createState() => _MessagePageState();
}

class _MessagePageState extends State<MessagePage> {
  final ScrollController _scrollController = ScrollController();

  List<Message> _list = [];
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
      var page = await context.read<MessageApi>().queryPosts(dto: dto0);

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
    final isDarkMode = context.select((AppThemeMode value) => value.isDarkMode);
    final barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;
    final isLoggedIn = context.select((LoginInfo value) => value.isLoggedIn);

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
              SliverAppBar(
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
                      onChanged: (value) {
                        Provider.of<AppThemeMode>(context, listen: false)
                            .toggleTheme();
                      },
                    )
                  ],
                ),
                floating: true,
              ),
              const SliverToBoxAdapter(child: SizedBox(height: 15)),
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
          padding: const EdgeInsets.symmetric(horizontal: 15),
          child: _createMessageCard(isDarkMode, item: _list[index]),
        );
      },
      separatorBuilder: (context, index) => const SizedBox(height: 19),
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

  Widget _createMessageCard(bool isDarkMode, {required Message item}) {
    final name = item.name;
    final overview = item.overview;
    final createdOn =
        item.createdOn != null ? formatRelativeTime(item.createdOn) : null;
    final state = item.state;
    final link = item.link;
    final senderUserId = item.sender?.id;
    final senderAvatar = getAvatarOrDefault(item.sender?.avatar);

    void onClickSenderAvatar() {
      if (senderUserId != null) {
        context.pushNamed("userDetails",
            pathParameters: {'id': senderUserId.toString()});
      }
    }

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(
          children: [
            ClipRRect(
              borderRadius: BorderRadius.circular(11),
              child: Material(
                child: InkWell(
                  onTap: onClickSenderAvatar,
                  child: Ink.image(
                    image: senderAvatar,
                    width: 35,
                    height: 35,
                  ),
                ),
              ),
            ),
            const SizedBox(width: 9),
            Expanded(
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Expanded(
                    child: Text(
                      name,
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
                  ),
                  if (createdOn != null)
                    Text(
                      createdOn,
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
        const SizedBox(height: 15),
        Text(
          overview,
          style: TextStyle(
            color: isDarkMode
                ? AppThemeColors.baseColorDark
                : AppThemeColors.baseColorLight,
          ),
        ),
        const SizedBox(height: 15),
        Wrap(
          spacing: 11,
          runSpacing: 1,
          children: [
            if (link != null)
              ElevatedButton.icon(
                icon: const FaIcon(FontAwesomeIcons.circleInfo, size: 17),
                label: const Text("Details"),
                onPressed: () => _handleDetailsClick(item),
              ),
            if (state == MessageStateEnum.unread)
              ElevatedButton.icon(
                icon: const FaIcon(FontAwesomeIcons.solidCircleCheck, size: 17),
                label: const Text("Read"),
                onPressed: () => _handleReadClick(item),
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
              onPressed: () => _handleDeleteClick(item),
            ),
          ],
        ),
      ],
    );
  }

  void _handleDetailsClick(Message item) {
    // Add your details click handling logic here
  }

  void _handleReadClick(Message item) {
    // Add your read click handling logic here
  }

  void _handleDeleteClick(Message item) {
    // Add your delete click handling logic here
  }
}
