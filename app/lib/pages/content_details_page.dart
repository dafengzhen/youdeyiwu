import 'dart:math';

import 'package:flutter/material.dart';
import 'package:flutter_widget_from_html/flutter_widget_from_html.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/post_api.dart';
import '../apis/section_api.dart';
import '../dtos/query_parameters_dto.dart';
import '../enums/load_data_type_enum.dart';
import '../models/pageable.dart';
import '../models/post.dart';
import '../models/section.dart';
import '../models/tag.dart';
import '../models/user.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';
import '../widgets/common.dart';
import '../widgets/post.dart';

class ContentDetailsPage extends StatefulWidget {
  final String id;

  const ContentDetailsPage({required this.id, super.key});

  @override
  State<ContentDetailsPage> createState() => _ContentDetailsPageState();
}

class TagPost {
  List<Post> list;
  Pageable? pageable;
  bool isLoadingInit;
  bool isLoadingMore;
  bool isLoading;
  bool hasMore;
  Tag? tag;

  TagPost({
    this.list = const [],
    this.pageable,
    this.isLoadingInit = true,
    this.isLoadingMore = false,
    this.isLoading = false,
    this.hasMore = false,
    this.tag,
  });
}

class _ContentDetailsPageState extends State<ContentDetailsPage>
    with SingleTickerProviderStateMixin {
  late TabController _tabController;
  final ScrollController _scrollController = ScrollController();

  final ValueNotifier<Map<Tag, TagPost>> _map = ValueNotifier({});
  final ValueNotifier<TagPost> _all = ValueNotifier(TagPost());
  Section? _section;
  bool _isLoading = false;
  bool _isLoadingInit = true;
  int _tabIndex = 0;

  @override
  void initState() {
    super.initState();
    _loadSectionData(completedCallback: (Section section) {
      var tags = _section?.tags ?? {};
      var content = _section?.content;
      var length = tags.isEmpty ? 0 : tags.length + (content != null ? 2 : 1);
      _tabController = TabController(length: length, vsync: this);
      _tabController.addListener(_handleTabChange);

      if (length - tags.length == 1) {
        print('_loadAllTagData');
        _loadAllTagData();
      }
    });
    _scrollController.addListener(_onScroll);
  }

  @override
  void dispose() {
    _scrollController.dispose();
    _tabController.removeListener(_handleTabChange);
    _tabController.dispose();
    super.dispose();
  }

  Future<void> _refresh() async {
    onClickTab(_tabIndex, type: LoadDataTypeEnum.refresh);
  }

  Future<void> _loadSectionData(
      {LoadDataTypeEnum type = LoadDataTypeEnum.initialize,
      void Function(Section section)? completedCallback}) async {
    setState(() {
      _isLoading = true;
      if (type == LoadDataTypeEnum.initialize) {
        _isLoadingInit = true;
      }
    });

    try {
      Section section =
          await context.read<SectionApi>().queryDetails(widget.id);
      setState(() {
        _section = section;
      });

      if (completedCallback != null) {
        completedCallback(section);
      }
    } catch (e) {
      if (mounted) {
        _showErrorPrompt(e);
      }
    } finally {
      setState(() {
        _isLoading = false;
        if (type == LoadDataTypeEnum.initialize) {
          _isLoadingInit = false;
        }
      });
    }
  }

  Future<void> _loadData({
    required TagPost currentItem,
    LoadDataTypeEnum type = LoadDataTypeEnum.initialize,
    QueryParametersDto? dto,
    String? tagId,
  }) async {
    if (type == LoadDataTypeEnum.initialize &&
        currentItem.isLoadingInit == false) {
      return;
    }

    _updateLoadingState(currentItem, type, isLoading: true);

    try {
      final Map<String, String> parameters = {"sectionId": widget.id};
      if (tagId != null) {
        parameters['tagId'] = tagId;
      }

      if (type == LoadDataTypeEnum.loadMore &&
          currentItem.pageable?.next == true) {
        parameters['page'] =
            min(currentItem.pageable!.page + 1, currentItem.pageable!.pages)
                .toString();
      } else if (type == LoadDataTypeEnum.loadMore) {
        return;
      }

      var base = QueryParametersDto.fromJson(parameters);
      var dto0 =
          QueryParametersDto.merge(base, dto ?? const QueryParametersDto());
      var page = await context.read<PostApi>().queryPosts(dto: dto0);

      if (type == LoadDataTypeEnum.loadMore) {
        currentItem.list.addAll(page.content);
      } else {
        currentItem.list = page.content;
      }
      currentItem.pageable = page.pageable;
      currentItem.hasMore = page.pageable.next;

      var tag = currentItem.tag;
      if (tag != null) {
        _map.value[tag] = currentItem;
      } else {
        _all.value = currentItem;
      }
    } catch (e) {
      if (mounted) {
        _showErrorPrompt(e);
      }
    } finally {
      _updateLoadingState(currentItem, type, isLoading: false);
      setState(() {});
    }
  }

  Future<void> _loadTagData({
    LoadDataTypeEnum type = LoadDataTypeEnum.initialize,
    int tagIndex = 0,
    QueryParametersDto? dto,
  }) async {
    var tags = _section?.tags ?? {};
    var tag = tags.isEmpty ? null : tags.elementAt(tagIndex);
    if (tag == null) {
      return;
    }

    await _loadData(
      currentItem: _map.value.putIfAbsent(tag, () => TagPost(tag: tag)),
      type: type,
      dto: dto,
      tagId: tag.id.toString(),
    );
  }

  Future<void> _loadAllTagData({
    LoadDataTypeEnum type = LoadDataTypeEnum.initialize,
    QueryParametersDto? dto,
  }) async {
    await _loadData(
      currentItem: _all.value,
      type: type,
      dto: dto,
    );
  }

  void _updateLoadingState(
    TagPost currentItem,
    LoadDataTypeEnum type, {
    required bool isLoading,
  }) {
    currentItem.isLoading = isLoading;
    if (type == LoadDataTypeEnum.initialize) {
      currentItem.isLoadingInit = isLoading;
    } else if (type == LoadDataTypeEnum.loadMore) {
      currentItem.isLoadingMore = isLoading;
    }

    var tag = currentItem.tag;
    if (tag != null) {
      _map.value[tag] = currentItem;
    } else {
      _all.value = currentItem;
    }
  }

  void _onScroll() {
    if (_scrollController.position.pixels ==
        _scrollController.position.maxScrollExtent) {
      onClickTab(_tabIndex, type: LoadDataTypeEnum.loadMore);
    }
  }

  void _showErrorPrompt(dynamic e) {
    showSystemPromptBottomSheet(
      context.read<AppThemeMode>().isDarkMode,
      context,
      exception: e,
    );
  }

  void _handleTabChange() {
    var index = _tabController.index;

    setState(() {
      _tabIndex = index;
    });

    if (!_tabController.indexIsChanging) {
      onClickTab(index);
    }
  }

  void onClickTab(
    int index, {
    LoadDataTypeEnum type = LoadDataTypeEnum.initialize,
  }) {
    var tags = _section?.tags ?? {};
    var content = _section?.content;
    var tabLength = tags.isEmpty ? 0 : tags.length + (content != null ? 2 : 1);

    if (index == 0 && content != null) {
      // start
    } else if ((index == 0 && content == null) ||
        (index == 1 && content != null)) {
      // all
      _loadAllTagData(
        type: type,
      );
    } else {
      // tag
      var value = tabLength - tags.length;
      var tagIndex = index - value;
      _loadTagData(
        tagIndex: tagIndex,
        type: type,
      );
    }
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    var name = _section?.name;
    var overview = _section?.overview;
    var content = _section?.content;
    var tags = _section?.tags ?? {};
    var admins = _section?.admins ?? {};

    return DefaultTabController(
      length: tags.isEmpty ? 0 : tags.length + (content != null ? 2 : 1),
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
            child: _isLoadingInit
                ? buildCenteredLoadingIndicator()
                : Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      if (name != null) ...[
                        Text(
                          name,
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
                      ],
                      if (overview != null && content == null) ...[
                        Text(
                          overview,
                          style: TextStyle(
                            color: isDarkMode
                                ? AppThemeColors.baseColorDark
                                : AppThemeColors.baseColorLight,
                          ),
                        ),
                        const SizedBox(
                          height: 17,
                        ),
                      ],
                      if (admins.isNotEmpty)
                        Wrap(
                          spacing: 13,
                          runSpacing: 1,
                          children: [
                            ..._buildAdmins(isDarkMode, admins: admins),
                            IconButton(
                              onPressed: () {
                                showSystemPromptBottomSheet(
                                  isDarkMode,
                                  context,
                                  description:
                                      "The current row displays the administrator of the current section.",
                                );
                              },
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
                      tags.isEmpty && content == null
                          ? Expanded(
                              child: buildCenteredNoMoreDataMessage(isDarkMode))
                          : Expanded(
                              child: Column(
                                children: [
                                  TabBar(
                                    controller: _tabController,
                                    tabs: _buildTags(
                                      isDarkMode,
                                      tags: tags,
                                      content: content,
                                    ),
                                    isScrollable: true,
                                    tabAlignment: TabAlignment.start,
                                  ),
                                  Expanded(
                                    child: TabBarView(
                                      controller: _tabController,
                                      children: _buildTagViews(
                                        isDarkMode,
                                        tags: tags,
                                        content: content,
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

  List<Widget> _buildTags(
    bool isDarkMode, {
    required Set<Tag> tags,
    required String? content,
  }) {
    List<Widget> list = [];

    if (content != null) {
      list.add(const Tab(
        text: "Start",
      ));
    }

    if (tags.isNotEmpty) {
      list.add(const Tab(
        text: "All",
      ));
    }

    for (var tag in tags) {
      list.add(Tab(
        text: tag.name,
      ));
    }

    return list;
  }

  List<Widget> _buildTagViews(
    bool isDarkMode, {
    required Set<Tag> tags,
    required String? content,
  }) {
    List<Widget> list = [];

    if (content != null) {
      list.add(Padding(
        padding: const EdgeInsets.symmetric(
          vertical: 15,
        ),
        child: HtmlWidget(
          key: const PageStorageKey("start"),
          content,
          renderMode: RenderMode.listView,
          textStyle: TextStyle(
            color: isDarkMode
                ? AppThemeColors.baseColorDark
                : AppThemeColors.baseColorLight,
          ),
        ),
      ));
    }

    if (tags.isNotEmpty) {
      list.add(Container(
        color:
            isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
        child: RefreshIndicator(
          onRefresh: _refresh,
          child: ValueListenableBuilder(
            valueListenable: _all,
            builder: (context, value, child) {
              return ListView.separated(
                controller: _scrollController,
                key: const PageStorageKey("all"),
                padding: const EdgeInsets.only(
                  top: 15,
                  bottom: 15,
                ),
                itemBuilder: (context, index) {
                  return buildArticleCard(
                    isDarkMode,
                    context,
                    item: value.list[index],
                    reload: (updateData) {
                      updateData(index, value.list);
                    },
                  );
                },
                separatorBuilder: (context, index) {
                  return const SizedBox(
                    height: 13,
                  );
                },
                itemCount: value.list.length,
              );
            },
          ),
        ),
      ));
    }

    for (var tag in tags) {
      list.add(Container(
        color:
            isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
        child: RefreshIndicator(
          onRefresh: _refresh,
          child: ValueListenableBuilder(
            valueListenable: _map,
            builder: (context, value, child) {
              var item = value[tag];

              return ListView.separated(
                controller: _scrollController,
                key: PageStorageKey(tag.name + tag.id.toString()),
                padding: const EdgeInsets.only(
                  top: 15,
                  bottom: 15,
                ),
                itemBuilder: (context, index) {
                  return buildArticleCard(
                    isDarkMode,
                    context,
                    item: item!.list[index],
                    reload: (updateData) {
                      updateData(index, item.list);
                    },
                  );
                },
                separatorBuilder: (context, index) {
                  return const SizedBox(
                    height: 13,
                  );
                },
                itemCount: item?.list.length ?? 0,
              );
            },
          ),
        ),
      ));
    }

    return list;
  }

  List<Widget> _buildAdmins(
    bool isDarkMode, {
    required Set<User> admins,
  }) {
    List<Widget> list = [];

    for (var admin in admins) {
      var username = getUsernameOrAnonymous(admin.username);

      list.add(TextButton(
        onPressed: () {
          context.pushNamed("userDetails", pathParameters: {
            'id': admin.id.toString(),
          });
        },
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
        child: Text(username),
      ));
    }

    return list;
  }
}
