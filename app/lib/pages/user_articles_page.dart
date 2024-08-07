import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/user_api.dart';
import '../enums/load_data_type_enum.dart';
import '../models/post.dart';
import '../models/user.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../widgets/common.dart';
import '../widgets/post.dart';

class UserArticlesPage extends StatefulWidget {
  final String id;

  const UserArticlesPage({required this.id, super.key});

  @override
  State<UserArticlesPage> createState() => _UserArticlesPageState();
}

class _UserArticlesPageState extends State<UserArticlesPage> {
  User? _user;
  bool _isLoadingInit = true;
  bool _isLoading = false;

  @override
  void initState() {
    super.initState();
    _loadData();
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

    var list = (_user?.posts ?? {}).toList();

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
          padding: const EdgeInsets.only(
            left: 15,
            right: 15,
            top: 15,
            bottom: 45,
          ),
          color: isDarkMode
              ? AppThemeColors.baseBgDark
              : AppThemeColors.baseBgLight,
          child: _isLoadingInit
              ? buildCenteredLoadingIndicator()
              : _buildList(isDarkMode, list: list),
        ),
      ),
    );
  }

  Widget _buildList(bool isDarkMode, {required List<Post> list}) {
    if (list.isEmpty) {
      return buildCenteredNoMoreDataMessage(isDarkMode);
    }

    return ListView.separated(
      itemCount: list.length,
      itemBuilder: (context, index) {
        return buildArticleCard(
          isDarkMode,
          context,
          item: list[index],
          reload: (updateData) {
            updateData(index, list);
          },
        );
      },
      separatorBuilder: (context, index) => const SizedBox(height: 15),
    );
  }
}
