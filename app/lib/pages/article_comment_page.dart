import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../apis/post_api.dart';
import '../enums/load_data_type_enum.dart';
import '../models/comment_reply.dart';
import '../models/post.dart';
import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';
import '../widgets/common.dart';

class ArticleCommentPage extends StatefulWidget {
  final String id;

  const ArticleCommentPage({required this.id, super.key});

  @override
  State<ArticleCommentPage> createState() => _ArticleCommentPageState();
}

class _ArticleCommentPageState extends State<ArticleCommentPage> {
  final TextEditingController _controller = TextEditingController();

  Post? _post;
  bool _isLoadingInit = true;
  bool _isLoading = false;

  @override
  void initState() {
    super.initState();
    _loadData();
  }

  @override
  void dispose() {
    _controller.dispose();
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
  }) async {
    setState(() {
      _isLoading = true;
      if (type == LoadDataTypeEnum.initialize) {
        _isLoadingInit = true;
      }
    });

    try {
      var post = await context.read<PostApi>().queryDetails(widget.id);

      setState(() {
        _post = post;
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
    final isDarkMode = context.select((AppThemeMode value) => value.isDarkMode);
    final barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    final item = _post;

    return Scaffold(
      appBar: _buildAppBar(context, isDarkMode, barBackgroundColor),
      body: _buildBody(context, isDarkMode, item, barBackgroundColor),
    );
  }

  AppBar _buildAppBar(
    BuildContext context,
    bool isDarkMode,
    Color barBackgroundColor,
  ) {
    return AppBar(
      backgroundColor: barBackgroundColor,
      surfaceTintColor: barBackgroundColor,
      leading: IconButton(
        icon: const FaIcon(FontAwesomeIcons.arrowLeft, size: 20),
        onPressed: () => context.pop(),
      ),
      title: Row(
        mainAxisAlignment: MainAxisAlignment.end,
        children: [
          Switch(
            value: isDarkMode,
            onChanged: (value) =>
                Provider.of<AppThemeMode>(context, listen: false).toggleTheme(),
          ),
        ],
      ),
    );
  }

  Widget _buildBody(
    BuildContext context,
    bool isDarkMode,
    Post? item,
    Color barBackgroundColor,
  ) {
    return Container(
      color: barBackgroundColor,
      child: Container(
        color:
            isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
        child: Stack(
          children: [
            if (_isLoadingInit) buildCenteredLoadingIndicator(),
            if ((!_isLoadingInit && item == null) ||
                (item != null &&
                    item.comments != null &&
                    item.comments!.content.isEmpty))
              buildCenteredNoMoreDataMessage(isDarkMode),
            if (item != null &&
                item.comments != null &&
                item.comments!.content.isNotEmpty)
              ..._buildCommentsList(isDarkMode, item),
          ],
        ),
      ),
    );
  }

  List<Widget> _buildCommentsList(bool isDarkMode, Post item) {
    return [
      ListView.separated(
        padding: const EdgeInsets.symmetric(horizontal: 15),
        itemBuilder: (context, index) => Padding(
          padding: EdgeInsets.only(
            top: index == 0 ? 15 : 0,
            bottom: index == item.comments!.content.length - 1 ? 145 : 0,
          ),
          child: _buildArticleCommentItem(
            isDarkMode,
            post: item,
            item: item.comments!.content[index],
            index: index,
          ),
        ),
        separatorBuilder: (context, index) => const SizedBox(height: 15),
        itemCount: item.comments!.content.length,
      ),
      Positioned(
        bottom: 0,
        left: 0,
        right: 0,
        child: _buildBottomCommentBar(isDarkMode),
      ),
    ];
  }

  Widget _buildBottomCommentBar(bool isDarkMode) {
    return Container(
      padding: const EdgeInsets.symmetric(vertical: 15, horizontal: 15),
      decoration: BoxDecoration(
        color:
            isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
        borderRadius: const BorderRadius.vertical(bottom: Radius.circular(17)),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              TextButton.icon(
                onPressed: () {},
                label: Text(
                  "Cancel reply",
                  style: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.secondaryColorDark
                        : AppThemeColors.secondaryColorLight,
                    fontWeight: FontWeight.bold,
                  ),
                ),
                icon: FaIcon(
                  FontAwesomeIcons.xmark,
                  size: 17,
                  color: isDarkMode
                      ? AppThemeColors.secondaryColorDark
                      : AppThemeColors.secondaryColorLight,
                ),
              ),
            ],
          ),
          const SizedBox(height: 7),
          TextField(
            controller: _controller,
            minLines: 1,
            maxLines: 5,
            decoration: InputDecoration(
              hintText: 'Enter a comment',
              border: OutlineInputBorder(
                borderRadius: BorderRadius.circular(11),
              ),
              contentPadding:
                  const EdgeInsets.symmetric(horizontal: 15, vertical: 11),
              prefixIcon: IconButton(
                onPressed: () {},
                icon: const FaIcon(FontAwesomeIcons.at, size: 19),
              ),
              suffixIcon: IconButton(
                onPressed: () {},
                icon: const FaIcon(FontAwesomeIcons.solidPaperPlane, size: 19),
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildArticleCommentItem(
    bool isDarkMode, {
    required Post post,
    required CommentReply item,
    required int index,
  }) {
    final item0 = (item.comment ?? item.reply) as dynamic;
    final createdOn =
        item0.createdOn != null ? formatRelativeTime(item0.createdOn) : null;
    final username = getUsernameOrAnonymous(item0.user?.username);
    final userId = item0.user?.id;
    final avatar = getAvatarOrDefault(item0.user?.avatar);
    final content = item0.content;
    final index0 = index + 1;

    void onClickUsername() {
      if (userId != null) {
        context.pushNamed("userDetails",
            pathParameters: {'id': userId.toString()});
      }
    }

    return Container(
      padding: const EdgeInsets.only(top: 17, bottom: 5, left: 12, right: 12),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.tertiaryBgDark
            : AppThemeColors.tertiaryBgLight,
        borderRadius: BorderRadius.circular(17),
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
                    onTap: onClickUsername,
                    child: Ink.image(
                      image: avatar,
                      width: 40,
                      height: 40,
                    ),
                  ),
                ),
              ),
              const SizedBox(width: 9),
              Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    username,
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
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
              const Spacer(),
              Column(
                children: [
                  Text(
                    "$index0#",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark.withOpacity(0.5)
                          : AppThemeColors.secondaryColorLight.withOpacity(0.5),
                    ),
                  ),
                  const Text(
                    "",
                  ),
                ],
              ),
            ],
          ),
          const SizedBox(height: 15),
          if (item.comment == null && item.reply != null) ...[
            _buildArticleCommentReply(
              isDarkMode,
              post: post,
              item: item,
            ),
            const SizedBox(height: 15),
          ],
          Text(
            content,
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
            ),
          ),
          const SizedBox(height: 15),
          Row(
            children: [
              IconButton(
                onPressed: () {},
                icon: const FaIcon(
                  FontAwesomeIcons.thumbsUp,
                  size: 17,
                ),
              ),
              IconButton(
                onPressed: () {},
                icon: const FaIcon(
                  FontAwesomeIcons.reply,
                  size: 17,
                ),
              )
            ],
          ),
        ],
      ),
    );
  }

  Widget _buildArticleCommentReply(
    bool isDarkMode, {
    required Post post,
    required CommentReply item,
  }) {
    final reply = (item.reply!.comment ?? item.reply!.quoteReply) as dynamic;
    final createdOn =
        reply.createdOn != null ? formatRelativeTime(reply.createdOn) : null;
    final username = getUsernameOrAnonymous(reply.user?.username);
    final userId = reply.user?.id;
    final content = reply.content;

    void onClickUsername() {
      if (userId != null) {
        context.pushNamed("userDetails",
            pathParameters: {'id': userId.toString()});
      }
    }

    return Container(
      padding: const EdgeInsets.only(top: 7, bottom: 7, left: 12, right: 12),
      decoration: BoxDecoration(
        color: isDarkMode
            ? AppThemeColors.secondaryBgDark.withOpacity(0.7)
            : AppThemeColors.secondaryBgLight.withOpacity(0.7),
        border: Border(
          left: BorderSide(
            width: 2,
            color: isDarkMode
                ? AppThemeColors.secondaryColor[700]!
                : AppThemeColors.secondaryColor[150]!,
          ),
        ),
        // borderRadius: BorderRadius.circular(7),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(
            children: [
              GestureDetector(
                onTap: onClickUsername,
                child: Row(
                  children: [
                    Icon(
                      Icons.reply,
                      color: isDarkMode
                          ? AppThemeColors.secondaryColorDark.withOpacity(0.7)
                          : AppThemeColors.secondaryColorLight.withOpacity(0.7),
                    ),
                    const SizedBox(
                      width: 7,
                    ),
                    Text(
                      "#",
                      style: TextStyle(
                        color: isDarkMode
                            ? AppThemeColors.secondaryColorDark
                            : AppThemeColors.secondaryColorLight,
                      ),
                    ),
                    Text(
                      username,
                      style: TextStyle(
                        color: isDarkMode
                            ? AppThemeColors.secondaryColorDark
                            : AppThemeColors.secondaryColorLight,
                      ),
                    ),
                    if (createdOn != null) ...[
                      Text(
                        " / ",
                        style: TextStyle(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColorDark
                              : AppThemeColors.secondaryColorLight,
                        ),
                      ),
                      Text(
                        createdOn,
                        style: TextStyle(
                          color: isDarkMode
                              ? AppThemeColors.secondaryColorDark
                              : AppThemeColors.secondaryColorLight,
                        ),
                      ),
                    ],
                  ],
                ),
              ),
            ],
          ),
          const SizedBox(height: 15),
          Text(
            content,
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
}
