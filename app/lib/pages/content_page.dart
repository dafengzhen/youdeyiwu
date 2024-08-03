import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/section_api.dart';
import '../configs/configs.dart';
import '../enums/load_data_type_enum.dart';
import '../models/section.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';

class ContentPage extends StatefulWidget {
  const ContentPage({super.key});

  @override
  State<ContentPage> createState() => _ContentPageState();
}

class _ContentPageState extends State<ContentPage> {
  List<Section> _list = [];
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
      var list = await context.read<SectionApi>().querySections();

      setState(() {
        _list.addAll(list);
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
        }
      });
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
                    )
                  ],
                ),
                floating: true,
              ),
              SliverPadding(
                padding: const EdgeInsets.all(15),
                sliver: SliverList.separated(
                  itemCount: _list.length,
                  itemBuilder: (context, index) {
                    return _createSectionGroupCard(isDarkMode, context);
                  },
                  separatorBuilder: (context, index) => const SizedBox(
                    height: 13,
                  ),
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _createSectionGroupCard(bool isDarkMode, BuildContext context) {
    return Container(
      padding: const EdgeInsets.only(top: 17, bottom: 15, left: 15, right: 15),
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
            "Programming language",
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
              fontWeight: FontWeight.bold,
              fontSize: 17,
            ),
          ),
          const SizedBox(
            height: 19,
          ),
          for (int index = 0; index < 3; index++)
            Column(
              children: [
                _createSectionCard(isDarkMode, context),
                if (index < 2) // 如果不是最后一个元素，添加分隔线
                  const SizedBox(
                    height: 9,
                  ),
              ],
            ),
        ],
      ),
    );
  }

  Widget _createSectionCard(bool isDarkMode, BuildContext context) {
    return GestureDetector(
      onTap: () {
        context.pushNamed(
          "contentDetails",
          pathParameters: {'id': "1"},
        );
      },
      child: Container(
        // height: 100,
        padding: const EdgeInsets.symmetric(
          vertical: 19,
          horizontal: 19,
        ),
        decoration: BoxDecoration(
          color: isDarkMode
              ? AppThemeColors.secondaryBgDark
              : AppThemeColors.secondaryBgLight,
          borderRadius: BorderRadius.circular(23),
        ),
        child: Row(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Container(
              width: 65,
              height: 65,
              decoration: BoxDecoration(
                borderRadius: const BorderRadius.all(Radius.circular(70)),
                // image: DecorationImage(
                //   image: AssetImage('assets/images/avatar.png'),
                // ),
                color: isDarkMode
                    ? AppThemeColors.secondaryColor[700]!
                    : AppThemeColors.secondaryColor[150]!,
              ),
            ),
            // CircleAvatar(
            //   backgroundImage: AssetImage('assets/images/avatar.png'),
            //   radius: 31,
            // ),
            const SizedBox(
              width: 11,
            ),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    "JavaScript",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
                      fontWeight: FontWeight.bold,
                      fontSize: 17,
                    ),
                  ),
                  const SizedBox(
                    height: 9,
                  ),
                  Text(
                    "JavaScript (JS) is a lightweight interpreted (or just-in-time compiled) programming language with first-class functions",
                    maxLines: 1,
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
                      overflow: TextOverflow.ellipsis,
                    ),
                  ),
                  const SizedBox(
                    height: 5,
                  ),
                  Row(
                    children: [
                      Container(
                        width: 31,
                        height: 5,
                        decoration: BoxDecoration(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColor[700]!
                              : AppThemeColors.secondaryColor[150]!,
                          borderRadius: const BorderRadius.horizontal(
                              left: Radius.circular(10),
                              right: Radius.circular(10)),
                        ),
                      ),
                      const SizedBox(
                        width: 3,
                      ),
                      Container(
                        width: 31,
                        height: 5,
                        decoration: BoxDecoration(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColor[700]!
                              : AppThemeColors.secondaryColor[150]!,
                          borderRadius: const BorderRadius.horizontal(
                              left: Radius.circular(10),
                              right: Radius.circular(10)),
                        ),
                      ),
                      const SizedBox(
                        width: 3,
                      ),
                      Container(
                        width: 31,
                        height: 5,
                        decoration: BoxDecoration(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColor[700]!
                              : AppThemeColors.secondaryColor[150]!,
                          borderRadius: const BorderRadius.horizontal(
                              left: Radius.circular(10),
                              right: Radius.circular(10)),
                        ),
                      ),
                    ],
                  )
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }
}
