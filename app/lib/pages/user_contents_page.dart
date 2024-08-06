import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/user_api.dart';
import '../enums/load_data_type_enum.dart';
import '../models/section.dart';
import '../models/user.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../widgets/common.dart';

class UserContentsPage extends StatefulWidget {
  final String id;

  const UserContentsPage({required this.id, super.key});

  @override
  State<UserContentsPage> createState() => _UserContentsPageState();
}

class _UserContentsPageState extends State<UserContentsPage> {
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

    var list = (_user?.relatedSections ?? {}).toList();

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

  Widget _buildList(bool isDarkMode, {required List<Section> list}) {
    if (list.isEmpty) {
      return buildCenteredNoMoreDataMessage(isDarkMode);
    }

    return ListView.separated(
      itemCount: list.length,
      itemBuilder: (context, index) {
        return _buildContentItem(isDarkMode, item: list[index]);
      },
      separatorBuilder: (context, index) {
        return const SizedBox(
          height: 15,
        );
      },
    );
  }

  Widget _buildContentItem(bool isDarkMode, {required Section item}) {
    var name = item.name;

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
          context.pushNamed("userViewArticles", queryParameters: {
            'sectionId': item.id.toString(),
          });
        },
        title: Text(
          name,
          style: TextStyle(
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
