import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter_quill/flutter_quill.dart';
import 'package:flutter_quill_extensions/models/config/shared_configurations.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';
import 'package:youdeyiwu_app/utils/tools.dart';

import '../apis/post_api.dart';
import '../enums/load_data_type_enum.dart';
import '../models/post.dart';
import '../providers/app_theme_mode.dart';
import '../providers/article_editor.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/my_quill_editor.dart';
import '../utils/my_quill_toolbar.dart';

class ArticleEditorPage extends StatefulWidget {
  final String? id;

  const ArticleEditorPage({this.id, super.key});

  @override
  State<ArticleEditorPage> createState() => _ArticleEditorPageState();
}

class _ArticleEditorPageState extends State<ArticleEditorPage> {
  late final QuillController _controller;
  late final FocusNode _editorFocusNode;
  late final ScrollController _editorScrollController;
  bool _isReadOnly = false;

  Post? _post;
  bool _isLoadingInit = true;
  bool _isLoading = false;

  @override
  void initState() {
    super.initState();
    _controller = QuillController.basic();
    _editorFocusNode = FocusNode();
    _editorScrollController = ScrollController();
    _loadContent();
    // _loadData();
  }

  @override
  void dispose() {
    _controller.dispose();
    _editorFocusNode.dispose();
    _editorScrollController.dispose();
    super.dispose();
  }

  void _loadContent() {
    var articleEditor = context.read<ArticleEditor>();
    var deltaContent = articleEditor.deltaContent;

    if (deltaContent != null && deltaContent.isNotEmpty) {
      _controller.document = Document.fromJson(jsonDecode(deltaContent));
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
      var id = widget.id;
      if (id != null) {
        var post = await context.read<PostApi>().queryDetails(id);
        var deltaContent = post.deltaContent;
        if (deltaContent != null && deltaContent.isNotEmpty) {
          /// plainTextContent
          // _controller.replaceText(
          //   0,
          //   0,
          //   content,
          //   TextSelection.collapsed(offset: content.length),
          // );

          _controller.document = Document.fromJson(jsonDecode(deltaContent));
        }

        setState(() {
          _post = post;
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

  QuillSharedConfigurations get _sharedConfigurations =>
      const QuillSharedConfigurations(
        extraConfigurations: {
          QuillSharedExtensionsConfigurations.key:
              QuillSharedExtensionsConfigurations(
            assetsPrefix: 'assets', // Defaults to assets
          ),
        },
      );

  @override
  Widget build(BuildContext context) {
    final isDarkMode = context.select((AppThemeMode value) => value.isDarkMode);
    final barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    _controller.readOnly = _isReadOnly;

    return PopScope(
      onPopInvoked: (didPop) {
        final content = jsonEncode(_controller.document.toDelta().toJson());
        var articleEditor = context.read<ArticleEditor>();
        articleEditor.setDeltaContent(content);
      },
      child: Scaffold(
        backgroundColor:
            isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
        appBar: AppBar(
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
                onChanged: (bool value) {
                  Provider.of<AppThemeMode>(context, listen: false)
                      .toggleTheme();
                },
              ),
            ],
          ),
        ),
        floatingActionButton: Row(
          mainAxisSize: MainAxisSize.min,
          children: [
            FloatingActionButton.small(
              heroTag: "setEditorToReadOnly",
              child: FaIcon(
                _isReadOnly
                    ? FontAwesomeIcons.lock
                    : FontAwesomeIcons.solidPenToSquare,
                size: 17,
              ),
              onPressed: () => setState(() => _isReadOnly = !_isReadOnly),
            ),
            // FloatingActionButton.small(
            //   heroTag: "saveEditorDeltaContent",
            //   child: const FaIcon(
            //     FontAwesomeIcons.solidFloppyDisk,
            //     size: 17,
            //   ),
            //   onPressed: () {},
            // ),
          ],
        ),
        body: Column(
          children: [
            if (!_isReadOnly)
              MyQuillToolbar(
                controller: _controller,
                focusNode: _editorFocusNode,
              ),
            const SizedBox(height: 15),
            Expanded(
              child: Padding(
                padding: const EdgeInsets.symmetric(horizontal: 15),
                child: MyQuillEditor(
                  configurations: QuillEditorConfigurations(
                    sharedConfigurations: _sharedConfigurations,
                    controller: _controller,
                  ),
                  scrollController: _editorScrollController,
                  focusNode: _editorFocusNode,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }
}
