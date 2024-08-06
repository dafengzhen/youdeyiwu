import 'package:flutter/material.dart';
import 'package:flutter_widget_from_html/flutter_widget_from_html.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';
import 'package:youdeyiwu_app/models/comment_reply.dart';

import '../apis/post_api.dart';
import '../enums/load_data_type_enum.dart';
import '../models/post.dart';
import '../models/page.dart' as page_model;
import '../models/tag.dart';
import '../providers/app_theme_mode.dart';
import '../providers/login_info.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/tools.dart';
import '../widgets/common.dart';

class ArticleDetailsPage extends StatefulWidget {
  final String id;

  const ArticleDetailsPage({required this.id, super.key});

  @override
  State<ArticleDetailsPage> createState() => _ArticleDetailsPageState();
}

class _ArticleDetailsPageState extends State<ArticleDetailsPage> {
  final TextEditingController _controller = TextEditingController();
  final FocusNode _focusNode = FocusNode();

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
    _focusNode.dispose();
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
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;
    final bool isLoggedIn =
        context.select((LoginInfo value) => value.isLoggedIn);
    final int? loginId = context.select((LoginInfo value) => value.loginId);

    final item = _post;
    String? cover;
    String? declaration;
    Set<Tag> tags = {};
    String? content;
    page_model.Page<CommentReply>? comments;
    String? id;
    bool yourself = false;

    if (item != null) {
      id = item.id.toString();
      cover = isHttpOrHttps(item.cover) ? item.cover : null;
      declaration = null;
      tags = item.tags ?? {};
      content = item.content;
      comments = item.comments;
      yourself = isLoggedIn && loginId == item.user?.id;
    }

    void onClickLike() {}

    void onClickShare() {}

    void onClickReply() {
      if (_focusNode.hasFocus) {
        _focusNode.unfocus();
      } else {
        _focusNode.requestFocus();
      }
    }

    void onClickSend() {}

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
            yourself
                ? IconButton(
                    icon: const FaIcon(
                      FontAwesomeIcons.solidPenToSquare,
                      size: 20,
                    ),
                    onPressed: () {
                      context.pushNamed("articleEdit", queryParameters: {
                        'id': id.toString(),
                      });
                    },
                  )
                : Switch(
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
          color: isDarkMode
              ? AppThemeColors.baseBgDark
              : AppThemeColors.baseBgLight,
          child: Stack(
            children: [
              if (_isLoadingInit) buildCenteredLoadingIndicator(),
              if (!_isLoadingInit && item == null)
                buildCenteredNoMoreDataMessage(isDarkMode),
              if (!_isLoadingInit && item != null) ...[
                SingleChildScrollView(
                  padding: const EdgeInsets.symmetric(
                    horizontal: 15,
                    vertical: 15,
                  ),
                  child: GestureDetector(
                    onTap: onClickReply,
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        _buildArticleHeader(isDarkMode, item: item),
                        const SizedBox(
                          height: 15,
                        ),
                        _buildArticleTitle(isDarkMode, item: item),
                        const SizedBox(
                          height: 15,
                        ),
                        if (cover != null) ...[
                          _buildArticleImages(cover: cover),
                          const SizedBox(
                            height: 15,
                          ),
                        ],
                        if (tags.isNotEmpty) ...[
                          _buildArticleTags(isDarkMode, item: item),
                          const SizedBox(
                            height: 19,
                          ),
                        ],
                        if (declaration != null) ...[
                          _buildArticleDeclaration(isDarkMode),
                          const SizedBox(
                            height: 25,
                          ),
                        ],
                        if (content != null && content.isNotEmpty) ...[
                          _buildArticleContent(isDarkMode, content: content),
                          const SizedBox(
                            height: 15,
                          ),
                        ],
                        SizedBox(
                          height: 27,
                          child: Center(
                            child: Container(
                              width: 50,
                              height: 1,
                              color: isDarkMode
                                  ? AppThemeColors.secondaryColor[700]!
                                  : AppThemeColors.secondaryColor[150]!,
                            ),
                          ),
                        ),
                        const SizedBox(
                          height: 5,
                        ),
                        Row(
                          mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                          children: [
                            ElevatedButton.icon(
                              icon: const FaIcon(
                                FontAwesomeIcons.thumbsUp,
                                size: 17,
                              ),
                              label: const Text(
                                "Like",
                              ),
                              onPressed: onClickLike,
                            ),
                            ElevatedButton.icon(
                              icon: const FaIcon(
                                FontAwesomeIcons.shareNodes,
                                size: 17,
                              ),
                              label: const Text("Share"),
                              onPressed: onClickShare,
                            ),
                            ElevatedButton.icon(
                              icon: const FaIcon(
                                FontAwesomeIcons.reply,
                                size: 17,
                              ),
                              label: const Text("Reply"),
                              onPressed: onClickReply,
                            ),
                          ],
                        ),
                        const SizedBox(
                          height: 5,
                        ),
                        SizedBox(
                          height: 27,
                          child: Center(
                            child: Container(
                              width: 50,
                              height: 1,
                              color: isDarkMode
                                  ? AppThemeColors.secondaryColor[700]!
                                  : AppThemeColors.secondaryColor[150]!,
                            ),
                          ),
                        ),
                        const SizedBox(
                          height: 15,
                        ),
                        if (comments != null &&
                            comments.content.isNotEmpty) ...[
                          if (comments.content.length > 1) ...[
                            _buildArticleCommentItem(
                              isDarkMode,
                              post: item,
                              item: comments.content[0],
                              index: 0,
                            ),
                            const SizedBox(
                              height: 15,
                            ),
                          ],
                          if (comments.content.length >= 2) ...[
                            _buildArticleCommentItem(
                              isDarkMode,
                              post: item,
                              item: comments.content[1],
                              index: 1,
                            ),
                            const SizedBox(
                              height: 15,
                            ),
                          ],
                          if (comments.content.length >= 3) ...[
                            _createMenuItem(
                              isDarkMode,
                              icon: FontAwesomeIcons.solidCommentDots,
                              text: "View more comments",
                              onTap: () {
                                context.pushNamed(
                                  "articleComment",
                                  pathParameters: {'id': item.id.toString()},
                                );
                              },
                            ),
                            const SizedBox(
                              height: 15,
                            ),
                          ],
                        ],
                        const SizedBox(
                          height: 90,
                        ),
                      ],
                    ),
                  ),
                ),
                Positioned(
                  bottom: 0,
                  left: 0,
                  right: 0,
                  child: Container(
                    padding: const EdgeInsets.symmetric(
                      vertical: 15,
                      horizontal: 15,
                    ),
                    decoration: BoxDecoration(
                      color: isDarkMode
                          ? AppThemeColors.baseBgDark
                          : AppThemeColors.baseBgLight,
                      borderRadius: const BorderRadius.vertical(
                        bottom: Radius.circular(17),
                      ),
                    ),
                    child: Row(
                      children: [
                        Expanded(
                          child: TextField(
                            controller: _controller,
                            focusNode: _focusNode,
                            minLines: 1,
                            maxLines: 5,
                            decoration: InputDecoration(
                                hintText: 'Enter a comment',
                                border: OutlineInputBorder(
                                  borderRadius: BorderRadius.circular(11),
                                ),
                                contentPadding: const EdgeInsets.symmetric(
                                  horizontal: 15,
                                  vertical: 11,
                                ),
                                suffixIcon: Row(
                                  mainAxisSize: MainAxisSize.min,
                                  children: [
                                    IconButton(
                                      onPressed: onClickSend,
                                      icon: const FaIcon(
                                        FontAwesomeIcons.solidPaperPlane,
                                        size: 19,
                                      ),
                                    ),
                                    if (comments != null &&
                                        comments.content.isNotEmpty)
                                      IconButton(
                                        onPressed: () {
                                          context.pushNamed(
                                            "articleComment",
                                            pathParameters: {
                                              'id': item.id.toString()
                                            },
                                          );
                                        },
                                        icon: const FaIcon(
                                          FontAwesomeIcons.solidCommentDots,
                                          size: 19,
                                        ),
                                      ),
                                  ],
                                )),
                          ),
                        ),
                      ],
                    ),
                  ),
                ),
              ]
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildArticleHeader(bool isDarkMode, {required Post item}) {
    final createdOn =
        item.createdOn != null ? formatRelativeTime(item.createdOn) : null;
    final username = getUsernameOrAnonymous(item.user?.username);
    final userId = item.user?.id;
    final avatar = getAvatarOrDefault(item.user?.avatar);

    void onClickUsername() {
      if (userId != null) {
        context.pushNamed("userDetails",
            pathParameters: {'id': userId.toString()});
      }
    }

    return Row(
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
        const SizedBox(width: 19),
        GestureDetector(
          onTap: onClickUsername,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Authored by',
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
                      ? AppThemeColors.baseColorDark
                      : AppThemeColors.baseColorLight,
                ),
              ),
            ],
          ),
        ),
        if (createdOn != null) ...[
          const SizedBox(width: 19),
          Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Published on',
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
                      ? AppThemeColors.baseColorDark
                      : AppThemeColors.baseColorLight,
                ),
              ),
            ],
          ),
        ],
      ],
    );
  }

  Widget _buildArticleTitle(bool isDarkMode, {required Post item}) {
    var name = item.name;

    return Text(
      name,
      softWrap: true,
      style: TextStyle(
        fontSize: 22,
        fontWeight: FontWeight.bold,
        color: isDarkMode
            ? AppThemeColors.baseColorDark
            : AppThemeColors.baseColorLight,
      ),
    );
  }

  Widget _buildArticleImages({required String cover}) {
    return SizedBox(
      height: 260,
      child: ListView.separated(
        separatorBuilder: (context, index) {
          return const SizedBox(
            width: 15,
          );
        },
        scrollDirection: Axis.horizontal,
        itemCount: 5,
        itemBuilder: (BuildContext context, int index) {
          return ClipRRect(
            borderRadius: BorderRadius.circular(11),
            child: Image.network(
              cover,
              fit: BoxFit.cover,
            ),
          );
        },
      ),
    );
  }

  Widget _buildArticleTags(bool isDarkMode, {required Post item}) {
    var tags = item.tags ?? {};

    return Wrap(
      spacing: 11,
      runSpacing: 3,
      children: List.generate(
        tags.length,
        (index) => ChoiceChip(
          backgroundColor: isDarkMode
              ? AppThemeColors.tertiaryBgDark
              : AppThemeColors.tertiaryBgLight,
          label: Text(
            '#${tags.elementAt(index).name}',
            style: TextStyle(
              color: isDarkMode
                  ? AppThemeColors.baseColorDark
                  : AppThemeColors.baseColorLight,
            ),
          ),
          selected: false,
          onSelected: (value) {},
        ),
      ),
    );
  }

  Widget _buildArticleDeclaration(bool isDarkMode) {
    return Container(
      padding: const EdgeInsets.symmetric(
        horizontal: 7,
      ),
      decoration: BoxDecoration(
        border: Border(
          left: BorderSide(
            width: 2,
            color: isDarkMode
                ? AppThemeColors.secondaryColor[700]!
                : AppThemeColors.secondaryColor[150]!,
          ),
        ),
      ),
      child: Text(
        '"This is a declaration"',
        style: TextStyle(
          fontSize: 17,
          fontStyle: FontStyle.italic,
          color: isDarkMode
              ? AppThemeColors.baseColorDark
              : AppThemeColors.baseColorLight,
        ),
      ),
    );
  }

  Widget _buildArticleContent(bool isDarkMode, {required String content}) {
    return HtmlWidget(
      content,
      textStyle: TextStyle(
        fontSize: 17,
        color: isDarkMode
            ? AppThemeColors.baseColorDark
            : AppThemeColors.baseColorLight,
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

  Widget _createMenuItem(
    bool isDarkMode, {
    required IconData icon,
    required String text,
    required Function() onTap,
  }) {
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
