import 'package:flutter/material.dart';
import 'package:flutter_quill/flutter_quill.dart';
import 'package:flutter_quill_extensions/models/config/shared_configurations.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';

import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/my_quill_editor.dart';
import '../utils/my_quill_toolbar.dart';

class ArticleEditorPage extends StatefulWidget {
  final String? id;

  const ArticleEditorPage({this.id, super.key});

  @override
  State<ArticleEditorPage> createState() => _ArticleEditorPageState();
}

class _ArticleEditorPageState extends State<ArticleEditorPage> {
  final QuillController _controller = QuillController.basic();
  final _editorFocusNode = FocusNode();
  final _editorScrollController = ScrollController();
  var _isReadOnly = false;

  @override
  void initState() {
    super.initState();
  }

  @override
  void dispose() {
    _controller.dispose();
    _editorFocusNode.dispose();
    _editorScrollController.dispose();
    super.dispose();
  }

  QuillSharedConfigurations get _sharedConfigurations {
    return const QuillSharedConfigurations(
      // locale: Locale('en'),
      extraConfigurations: {
        QuillSharedExtensionsConfigurations.key:
            QuillSharedExtensionsConfigurations(
          assetsPrefix: 'assets', // Defaults to assets
        ),
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

    _controller.readOnly = _isReadOnly;

    return Scaffold(
      backgroundColor:
          isDarkMode ? AppThemeColors.baseBgDark : AppThemeColors.baseBgLight,
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
      floatingActionButton: FloatingActionButton.small(
        child: _isReadOnly
            ? const FaIcon(
                FontAwesomeIcons.lock,
                size: 17,
              )
            : const FaIcon(
                FontAwesomeIcons.solidPenToSquare,
                size: 17,
              ),
        onPressed: () => setState(() => _isReadOnly = !_isReadOnly),
      ),
      body: Column(
        children: [
          if (!_isReadOnly)
            MyQuillToolbar(
              controller: _controller,
              focusNode: _editorFocusNode,
            ),
          const SizedBox(
            height: 15,
          ),
          Expanded(
            child: Padding(
              padding: const EdgeInsets.symmetric(
                horizontal: 15,
              ),
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
    );
  }
}
