import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/section_api.dart';
import '../configs/configs.dart';
import '../enums/load_data_type_enum.dart';
import '../models/section.dart';
import '../models/section_group.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';

class ContentPage extends StatefulWidget {
  const ContentPage({super.key});

  @override
  State<ContentPage> createState() => _ContentPageState();
}

class _ContentPageState extends State<ContentPage> {
  Map<SectionGroup, List<Section>> _map = {};
  bool _isLoadingInit = true;
  bool _isLoading = false;

  @override
  void initState() {
    super.initState();
    _loadData();
  }

  @override
  void dispose() {
    super.dispose();
  }

  Future<void> _refresh() async {
    if (_isLoading == false) {
      await _loadData(
        type: LoadDataTypeEnum.refresh,
      );
    }
  }

  Future<void> _loadData(
      {LoadDataTypeEnum type = LoadDataTypeEnum.initialize}) async {
    setState(() {
      _isLoading = true;
      if (type == LoadDataTypeEnum.initialize) {
        _isLoadingInit = true;
      }
    });

    try {
      _fetchAndGroupSections();
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

  void _fetchAndGroupSections() async {
    var list = await context.read<SectionApi>().querySections();
    var map = <SectionGroup, List<Section>>{};

    for (var element in list) {
      for (var sectionGroupElement in element.sectionGroups) {
        map.putIfAbsent(sectionGroupElement, () => []).add(element);
      }
    }

    setState(() {
      _map = map;
    });
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

    final keys = _map.keys;

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
                    ),
                  ],
                ),
                floating: true,
              ),
              const SliverToBoxAdapter(child: SizedBox(height: 15)),
              if (_isLoadingInit) _buildLoadingIndicator(),
              SliverList.separated(
                itemCount: keys.length,
                itemBuilder: (context, index) {
                  return Padding(
                    padding: const EdgeInsets.symmetric(horizontal: 15),
                    child: _createSectionGroupCard(isDarkMode, context,
                        item: keys.elementAt(index)),
                  );
                },
                separatorBuilder: (context, index) =>
                    const SizedBox(height: 13),
              ),
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

  Widget _createSectionGroupCard(bool isDarkMode, BuildContext context,
      {required SectionGroup item}) {
    final sections = _map[item] ?? [];

    return Container(
      padding: const EdgeInsets.all(15),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.tertiaryBgDark
            : AppThemeColors.tertiaryBgLight,
        borderRadius: BorderRadius.circular(17),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            item.name,
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
              fontWeight: FontWeight.bold,
              fontSize: 17,
            ),
          ),
          const SizedBox(height: 19),
          for (int index = 0; index < sections.length; index++) ...[
            _createSectionCard(isDarkMode, context, item: sections[index]),
            if (index < sections.length - 1) const SizedBox(height: 9),
          ],
        ],
      ),
    );
  }

  Widget _createSectionCard(bool isDarkMode, BuildContext context,
      {required Section item}) {
    final cover = isHttpOrHttps(item.cover) ? item.cover : null;

    return GestureDetector(
      onTap: () {
        context.pushNamed("contentDetails",
            pathParameters: {'id': item.id.toString()});
      },
      child: Container(
        padding: const EdgeInsets.all(19),
        decoration: BoxDecoration(
          color: isDarkMode
              ? AppThemeColors.secondaryBgDark.withOpacity(0.5)
              : AppThemeColors.secondaryBgLight.withOpacity(0.5),
          borderRadius: BorderRadius.circular(23),
        ),
        child: Row(
          crossAxisAlignment: item.overview == null
              ? CrossAxisAlignment.center
              : CrossAxisAlignment.start,
          children: [
            Container(
              width: 65,
              height: 65,
              decoration: BoxDecoration(
                borderRadius: BorderRadius.circular(70),
                image: cover != null
                    ? DecorationImage(image: NetworkImage(cover))
                    : null,
                color: isDarkMode
                    ? AppThemeColors.secondaryColor[700]!.withOpacity(0.3)
                    : AppThemeColors.secondaryColor[150]!.withOpacity(0.3),
              ),
            ),
            const SizedBox(width: 11),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    item.name,
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
                      fontWeight: FontWeight.bold,
                      fontSize: 17,
                    ),
                  ),
                  if (item.overview != null && item.overview!.isNotEmpty) ...[
                    const SizedBox(height: 9),
                    Text(
                      item.overview!,
                      maxLines: 1,
                      style: TextStyle(
                        color: isDarkMode
                            ? AppThemeColors.baseColorDark
                            : AppThemeColors.baseColorLight,
                        overflow: TextOverflow.ellipsis,
                      ),
                    ),
                    const SizedBox(height: 5),
                  ],
                  Row(
                    children: [
                      for (int index = 0;
                          index < item.tags.length;
                          index++) ...[
                        Container(
                          width: 31,
                          height: 5,
                          decoration: BoxDecoration(
                            color: isDarkMode
                                ? AppThemeColors.secondaryColor[700]!
                                    .withOpacity(0.3)
                                : AppThemeColors.secondaryColor[150]!
                                    .withOpacity(0.3),
                            borderRadius: const BorderRadius.horizontal(
                              left: Radius.circular(10),
                              right: Radius.circular(10),
                            ),
                          ),
                        ),
                        if (index < item.tags.length - 1)
                          const SizedBox(width: 3),
                      ],
                    ],
                  ),
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }
}
