import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/user_api.dart';
import '../configs/configs.dart';
import '../enums/load_data_type_enum.dart';
import '../models/related_statistics.dart';
import '../models/user.dart';
import '../providers/app_theme_mode.dart';
import '../providers/login_info.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';

enum MenuLabel { articles, contents, tags, statistics, logout }

const Map<MenuLabel, String> menuLabelToRoute = {
  MenuLabel.articles: "userArticles",
  MenuLabel.contents: "userContents",
  MenuLabel.tags: "userTags",
  MenuLabel.statistics: "userStatistics",
};

class UserPage extends StatefulWidget {
  final String? id;

  const UserPage({this.id, super.key});

  @override
  State<UserPage> createState() => _UserPageState();
}

class _UserPageState extends State<UserPage> {
  User? _user;
  bool _isLoadingInit = true;
  bool _isLoading = false;

  @override
  void initState() {
    super.initState();
    _loadData();
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
      var user = await context.read<UserApi>().queryDetails(id: widget.id);
      if (user != null) {
        setState(() {
          _user = user;
        });
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

    final username = getUsernameOrAnonymous(_user?.username);
    final userId = _user?.id;
    final avatar = getAvatarOrDefault(_user?.avatar);
    final oneSentence =
        _user?.oneSentence ?? "He didn't leave behind a single word";
    final relatedStatistics =
        _user?.relatedStatistics ?? RelatedStatistics.empty();

    void onClickLogout() {}

    void onClickMenuItem(MenuLabel label) {
      if (userId != null) {
        var userId0 = userId.toString();
        if (label == MenuLabel.logout) {
          onClickLogout();
        } else if (menuLabelToRoute.containsKey(label)) {
          context.pushNamed(
            menuLabelToRoute[label]!,
            queryParameters: {'id': userId0},
          );
        }
      } else {
        showSystemPromptBottomSheet(
          isDarkMode,
          context,
          description: "User does not exist",
        );
      }
    }

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
                borderRadius:
                    const BorderRadius.vertical(bottom: Radius.circular(17)),
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
                SliverPadding(
                  padding: const EdgeInsets.symmetric(horizontal: 15),
                  sliver: SliverList(
                    delegate: SliverChildListDelegate([
                      _buildUserInfoSection(
                        isDarkMode,
                        avatar,
                        username,
                        userId,
                        oneSentence,
                      ),
                      const SizedBox(height: 27),
                      _buildStatisticsSection(isDarkMode, relatedStatistics),
                      const SizedBox(height: 27),
                      _buildMenuSection(
                        isDarkMode,
                        onClickMenuItem,
                      ),
                    ]),
                  ),
                ),
                const SliverToBoxAdapter(child: SizedBox(height: 35)),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildLoadingIndicator() {
    return const SliverToBoxAdapter(
      child: Padding(
        padding: EdgeInsets.symmetric(vertical: 15),
        child: Center(child: CircularProgressIndicator()),
      ),
    );
  }

  Widget _buildUserInfoSection(
    bool isDarkMode,
    ImageProvider avatar,
    String username,
    int? userId,
    String oneSentence,
  ) {
    return Column(
      children: [
        Stack(
          clipBehavior: Clip.none,
          children: [
            Container(
              height: 130,
              padding: const EdgeInsets.only(left: 12, right: 0),
              decoration: BoxDecoration(
                image: const DecorationImage(
                  fit: BoxFit.cover,
                  image: AssetImage("assets/images/user-bg.jpg"),
                ),
                color: isDarkMode
                    ? AppThemeColors.tertiaryBgDark
                    : AppThemeColors.tertiaryBgLight,
                borderRadius: BorderRadius.circular(17),
              ),
            ),
            Positioned(
              left: 0,
              right: 0,
              bottom: -23,
              child: CircleAvatar(
                radius: 35,
                backgroundColor: isDarkMode
                    ? AppThemeColors.baseBgDark
                    : AppThemeColors.baseBgLight,
                child: CircleAvatar(
                  backgroundImage: avatar,
                  radius: 29,
                ),
              ),
            ),
          ],
        ),
        const SizedBox(height: 39),
        Column(
          children: [
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Text(
                  username,
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
                if (userId != null) ...[
                  const SizedBox(width: 7),
                  Text(
                    "⌈ ID.$userId ⌋",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark
                          : AppThemeColors.secondaryColorLight,
                    ),
                  ),
                ],
              ],
            ),
            const SizedBox(height: 5),
            Text(
              oneSentence,
              maxLines: 2,
              style: TextStyle(
                color: isDarkMode
                    ? AppThemeColors.baseColorDark
                    : AppThemeColors.baseColorLight,
                overflow: TextOverflow.ellipsis,
              ),
            ),
          ],
        ),
      ],
    );
  }

  Widget _buildStatisticsSection(
    bool isDarkMode,
    RelatedStatistics relatedStatistics,
  ) {
    return IntrinsicHeight(
      child: Row(
        children: [
          _buildStatisticItem(
              isDarkMode, relatedStatistics.posts.toString(), 'Articles'),
          _buildDivider(isDarkMode),
          _buildStatisticItem(
              isDarkMode, relatedStatistics.comments.toString(), 'Comments'),
          _buildDivider(isDarkMode),
          _buildStatisticItem(
              isDarkMode, relatedStatistics.replies.toString(), 'Replies'),
        ],
      ),
    );
  }

  Widget _buildStatisticItem(bool isDarkMode, String value, String label) {
    return Expanded(
      child: Column(
        children: [
          Text(
            value,
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
              fontWeight: FontWeight.bold,
              fontSize: 19,
            ),
          ),
          const SizedBox(height: 5),
          Text(
            label,
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

  Widget _buildDivider(bool isDarkMode) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 5),
      child: VerticalDivider(
        color: isDarkMode
            ? AppThemeColors.secondaryColor[700]!
            : AppThemeColors.secondaryColor[150]!,
        thickness: 1,
      ),
    );
  }

  Widget _buildMenuSection(
    bool isDarkMode,
    Function(MenuLabel) onClickMenuItem,
  ) {
    return Column(
      children: [
        _createMenuItem(
          isDarkMode,
          icon: FontAwesomeIcons.newspaper,
          text: "Articles",
          onTap: () => onClickMenuItem(MenuLabel.articles),
        ),
        _createMenuItem(
          isDarkMode,
          icon: FontAwesomeIcons.tableColumns,
          text: "Contents",
          onTap: () => onClickMenuItem(MenuLabel.contents),
        ),
        _createMenuItem(
          isDarkMode,
          icon: FontAwesomeIcons.tags,
          text: "Tags",
          onTap: () => onClickMenuItem(MenuLabel.tags),
        ),
        _createMenuItem(
          isDarkMode,
          icon: FontAwesomeIcons.chartSimple,
          text: "Statistics",
          onTap: () => onClickMenuItem(MenuLabel.statistics),
        ),
        _createMenuItem(
          isDarkMode,
          icon: FontAwesomeIcons.rightFromBracket,
          text: "Logout",
          onTap: () => onClickMenuItem(MenuLabel.logout),
        ),
      ],
    );
  }

  Widget _createMenuItem(bool isDarkMode,
      {required IconData icon,
      required String text,
      required Function() onTap}) {
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
