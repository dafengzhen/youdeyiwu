import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter_quill/flutter_quill.dart';
import 'package:flutter_quill_extensions/models/config/shared_configurations.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';
import 'package:image_picker/image_picker.dart';

import '../providers/app_theme_mode.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/my_quill_editor.dart';
import '../utils/my_quill_toolbar.dart';

class ArticleEditPage extends StatefulWidget {
  final String? id;

  const ArticleEditPage({this.id, super.key});

  @override
  State<ArticleEditPage> createState() => _ArticleEditPageState();
}

class _ArticleEditPageState extends State<ArticleEditPage> {
  final TextEditingController _titleController = TextEditingController();
  final TextEditingController _contentController = TextEditingController();
  final TextEditingController _tagsController = TextEditingController();
  final List<XFile> _images = [];

  Future<void> _pickImage() async {
    final ImagePicker picker = ImagePicker();
    final List<XFile>? pickedFiles = await picker.pickMultiImage();
    if (pickedFiles != null) {
      setState(() {
        _images.addAll(pickedFiles);
      });
    }
  }

  @override
  void initState() {
    super.initState();
  }

  @override
  void dispose() {
    _titleController.dispose();
    _contentController.dispose();
    _tagsController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final bool isDarkMode =
        context.select((AppThemeMode value) => value.isDarkMode);
    final Color barBackgroundColor = isDarkMode
        ? AppThemeData.darkTheme.colorScheme.surfaceContainer
        : AppThemeData.lightTheme.colorScheme.surfaceContainer;

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
      floatingActionButton: ElevatedButton.icon(
        icon: const FaIcon(
          FontAwesomeIcons.solidFloppyDisk,
          size: 17,
        ),
        label: const Text(
          "Publish",
          style: TextStyle(
            fontWeight: FontWeight.bold,
          ),
        ),
        onPressed: () {},
      ),
      body: SingleChildScrollView(
        padding: const EdgeInsets.symmetric(
          horizontal: 15,
        ),
        child: Column(
          children: [
            const SizedBox(
              height: 15,
            ),
            TextField(
              controller: _titleController,
              minLines: 1,
              maxLines: 3,
              decoration: InputDecoration(
                hintText: 'Name',
                border: OutlineInputBorder(
                  borderRadius: BorderRadius.circular(11),
                ),
                contentPadding: const EdgeInsets.symmetric(
                  horizontal: 15,
                  vertical: 11,
                ),
              ),
            ),
            const SizedBox(height: 15),
            TextField(
              controller: _contentController,
              minLines: 7,
              maxLines: 9,
              decoration: InputDecoration(
                hintText: 'Content',
                border: OutlineInputBorder(
                  borderRadius: BorderRadius.circular(11),
                ),
                contentPadding: const EdgeInsets.symmetric(
                  horizontal: 15,
                  vertical: 11,
                ),
                suffixIcon: IconButton(
                  icon: const FaIcon(
                    FontAwesomeIcons.solidPenToSquare,
                    size: 17,
                  ),
                  onPressed: () {
                    context.pushNamed(
                      "articleEditor",
                      pathParameters: {'id': "4"},
                    );
                  },
                ),
              ),
            ),
            const SizedBox(height: 15),
            TextField(
              controller: _tagsController,
              minLines: 1,
              maxLines: 3,
              decoration: InputDecoration(
                hintText: 'Tags (separated by commas)',
                border: OutlineInputBorder(
                  borderRadius: BorderRadius.circular(11),
                ),
                contentPadding: const EdgeInsets.symmetric(
                  horizontal: 15,
                  vertical: 11,
                ),
              ),
            ),
            const SizedBox(height: 15),
            SizedBox(
              height: 50,
              child: DropdownMenu(
                onSelected: (value) {
                  FocusManager.instance.primaryFocus?.unfocus();
                },
                dropdownMenuEntries: const [
                  DropdownMenuEntry(value: Colors.red, label: "Red"),
                  DropdownMenuEntry(value: Colors.blue, label: "Blue"),
                  DropdownMenuEntry(value: Colors.grey, label: "Grey"),
                  DropdownMenuEntry(value: Colors.green, label: "Green"),
                ],
                hintText: "Please select content",
                enableFilter: true,
                enableSearch: true,
                requestFocusOnTap: true,
                menuHeight: 330,
                expandedInsets: const EdgeInsets.all(0),
                inputDecorationTheme: InputDecorationTheme(
                  border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(11),
                  ),
                ),
              ),
            ),
            const SizedBox(height: 15),
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Text(
                  'Upload images',
                  style: TextStyle(
                    color: isDarkMode
                        ? AppThemeColors.baseColorDark
                        : AppThemeColors.baseColorLight,
                  ),
                ),
                TextButton.icon(
                  onPressed: _pickImage,
                  label: Text(
                    "Upload",
                    style: TextStyle(
                      color: isDarkMode
                          ? AppThemeColors.baseColorDark
                          : AppThemeColors.baseColorLight,
                      fontWeight: FontWeight.bold,
                    ),
                  ),
                  icon: FaIcon(
                    FontAwesomeIcons.upload,
                    size: 17,
                    color: isDarkMode
                        ? AppThemeColors.baseColorDark
                        : AppThemeColors.baseColorLight,
                  ),
                )
              ],
            ),
            const SizedBox(height: 15),
            _images.isNotEmpty
                ? SizedBox(
                    height: 190,
                    child: GridView.builder(
                      shrinkWrap: true,
                      gridDelegate:
                          const SliverGridDelegateWithFixedCrossAxisCount(
                        crossAxisCount: 3,
                        crossAxisSpacing: 9,
                        mainAxisSpacing: 9,
                      ),
                      itemCount: _images.length,
                      itemBuilder: (context, index) {
                        return Stack(
                          children: [
                            ClipRRect(
                              borderRadius: BorderRadius.circular(11),
                              child: Image.file(
                                File(_images[index].path),
                                width: double.infinity,
                                height: double.infinity,
                                fit: BoxFit.contain,
                              ),
                            ),
                            Positioned(
                              top: 0,
                              right: 0,
                              child: GestureDetector(
                                onTap: () {
                                  setState(() {
                                    _images.removeAt(index);
                                  });
                                },
                                child: FaIcon(
                                  FontAwesomeIcons.solidCircleXmark,
                                  size: 21,
                                  color: isDarkMode
                                      ? AppThemeColors.baseBgDangerColorDark
                                      : AppThemeColors.baseBgDangerColorLight,
                                ),
                              ),
                            ),
                          ],
                        );
                      },
                    ),
                  )
                : const SizedBox(
                    height: 190,
                  ),
            const SizedBox(height: 75),
          ],
        ),
      ),
    );
  }
}
