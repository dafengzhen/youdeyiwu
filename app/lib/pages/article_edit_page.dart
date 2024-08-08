import 'dart:io';
import 'dart:math';
import 'package:flutter/material.dart';
import 'package:font_awesome_flutter/font_awesome_flutter.dart';
import 'package:go_router/go_router.dart';
import 'package:provider/provider.dart';
import 'package:image_picker/image_picker.dart';

import '../apis/post_api.dart';
import '../apis/section_api.dart';
import '../dtos/save_post_dto.dart';
import '../enums/load_data_type_enum.dart';
import '../models/post.dart';
import '../models/section.dart';
import '../models/tag.dart';
import '../providers/app_theme_mode.dart';
import '../providers/article_editor.dart';
import '../providers/login_info.dart';
import '../utils/app_theme_colors.dart';
import '../utils/app_theme_data.dart';
import '../utils/bottom_sheet_utils.dart';
import '../utils/constants.dart';
import '../utils/tools.dart';
import '../widgets/common.dart';

class ArticleEditPage extends StatefulWidget {
  final String? id;

  const ArticleEditPage({this.id, super.key});

  @override
  State<ArticleEditPage> createState() => _ArticleEditPageState();
}

class _ArticleEditPageState extends State<ArticleEditPage> {
  final TextEditingController _nameController = TextEditingController();
  final TextEditingController _overviewController = TextEditingController();
  final TextEditingController _contentController = TextEditingController();
  final TextEditingController _tagsController = TextEditingController();
  final List<XFile> _images = [];

  List<Section> _sections = [];
  Post? _post;
  bool _isLoadingInit = true;
  bool _isLoading = false;

  int? _selectedSectionId;

  @override
  void initState() {
    super.initState();
    _loadData();
  }

  @override
  void dispose() {
    _nameController.dispose();
    _overviewController.dispose();
    _contentController.dispose();
    _tagsController.dispose();
    super.dispose();
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
      var postApi = context.read<PostApi>();
      var sectionApi = context.read<SectionApi>();

      Post? post;
      if (id != null) {
        post = await postApi.queryDetails(id);
      }

      var sections = await sectionApi.querySections();
      setState(() {
        if (post != null) {
          _post = post;
        }

        _sections = sections;
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

  Future<void> _pickImage() async {
    showSystemPromptBottomSheet(
      context.read<AppThemeMode>().isDarkMode,
      context,
      promptType: PromptType.info,
      description:
          "This feature is pending implementation (the current plan is to first complete a browsable app version, rather than focusing on a management-oriented app version).",
    );

    return;

    try {
      final ImagePicker picker = ImagePicker();
      final List<XFile> pickedFiles = await picker.pickMultiImage();
      setState(() {
        _images.addAll(pickedFiles);
      });
    } catch (e) {
      // Handle error
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Failed to pick images: $e')),
      );
    }
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

    final Post? item = _post;

    int? id;
    String? name;
    String? overview;
    String? plainTextContent;
    String? deltaContent;
    Set<Tag> tags = {};

    if (item != null) {
      id = item.id;
      name = item.name;
      plainTextContent = item.plainTextContent;
      deltaContent = item.deltaContent;
      overview = item.overview;
      tags = item.tags ?? {};
    }

    void onClickPublish() async {
      final name0 = _nameController.text;
      final overview0 = _overviewController.text;
      final plainTextContent0 = _contentController.text;
      final tags0 = _tagsController.text;

      if (name0.isEmpty) {
        showSystemPromptBottomSheet(
          isDarkMode,
          context,
          promptType: PromptType.warning,
          description: "Name cannot be empty",
        );
        return;
      }

      try {
        var id = widget.id;
        var postApi = context.read<PostApi>();
        var articleEditor = context.read<ArticleEditor>();
        var deltaContent0 = articleEditor.deltaContent;
        var sectionId0 = _selectedSectionId;

        var response = await postApi.save(
          id,
          dto: SavePostDto(
            name: name0,
            overview: overview0,
            plainTextContent: plainTextContent0,
            deltaContent: deltaContent0,
            tags: tags0.split(',').map((e) => e.trim()).toList(),
            sectionId: sectionId0,
            removeSection: sectionId0 == null,
          ),
          isCreate: id == null,
        );

        onClosePressed() {
          logDebug(response.headers);
          var location = response.headers[locationHeader.toLowerCase()];
          if (location != null) {
            var split = location.split('/');
            var newId = split[split.length - 1];
            context.replaceNamed("articleDetails", pathParameters: {'id': newId});
          }
        }

        if (id == null) {
          if (context.mounted) {
            showSystemPromptBottomSheet(
              isDarkMode,
              context,
              promptType: PromptType.success,
              description: "Create Successful",
              onBottomSheetClosed: onClosePressed,
              onClosePressed: onClosePressed,
              onButtonPressed: onClosePressed,
            );
          } else {
            onClosePressed();
          }
        } else {
          if (context.mounted) {
            showSystemPromptBottomSheet(
              isDarkMode,
              context,
              promptType: PromptType.success,
              description: "Update Successful",
            );
          }
        }
      } catch (e) {
        if (context.mounted) {
          showSystemPromptBottomSheet(
            isDarkMode,
            context,
            exception: e,
          );
        }
      }
    }

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
          onPressed: () => context.pop(),
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
          style: TextStyle(fontWeight: FontWeight.bold),
        ),
        onPressed: onClickPublish,
      ),
      body: _isLoadingInit
          ? buildCenteredLoadingIndicator()
          : SingleChildScrollView(
              padding: const EdgeInsets.symmetric(horizontal: 15),
              child: Column(
                children: [
                  const SizedBox(height: 15),
                  _buildTextField(
                    _nameController,
                    'Name',
                    1,
                    3,
                    initialValue: name,
                  ),
                  const SizedBox(height: 15),
                  _buildTextField(
                    _overviewController,
                    'Overview (Optional)',
                    1,
                    3,
                    initialValue: overview,
                  ),
                  const SizedBox(height: 15),
                  _buildTextField(
                    _contentController,
                    'Plain text content (Optional)',
                    7,
                    9,
                    suffixIcon: Row(
                      mainAxisSize: MainAxisSize.min,
                      children: [
                        IconButton(
                          icon: const FaIcon(
                            FontAwesomeIcons.circleInfo,
                            size: 17,
                          ),
                          onPressed: () {
                            showSystemPromptBottomSheet(isDarkMode, context,
                                description:
                                    "There are significant differences between the mobile and web versions. Therefore, if you need to modify HTML rich text content, please update it on the web version. You can understand it as the app and web versions storing different editor content separately, which will not be overwritten.");
                          },
                        ),
                        IconButton(
                          icon: const FaIcon(
                            FontAwesomeIcons.solidPenToSquare,
                            size: 17,
                          ),
                          onPressed: () {
                            var articleEditor = context.read<ArticleEditor>();
                            if (articleEditor.deltaContent == null) {
                              articleEditor.setDeltaContent(
                                  deltaContent ?? plainTextContent);
                            }

                            if (id == null) {
                              context.pushNamed("articleEditor");
                            } else {
                              context
                                  .pushNamed("articleEditor", queryParameters: {
                                'id': id.toString(),
                              });
                            }
                          },
                        ),
                      ],
                    ),
                    initialValue: plainTextContent,
                  ),
                  const SizedBox(height: 15),
                  _buildTextField(
                    _tagsController,
                    'Tags (separated by commas)',
                    1,
                    3,
                    initialValue: tags.map(
                      (e) {
                        return e.name;
                      },
                    ).join(', '),
                  ),
                  const SizedBox(height: 15),
                  if (_sections.isNotEmpty) ...[
                    _buildDropdownMenu(),
                    const SizedBox(height: 15),
                  ],
                  _buildImageUploadSection(isDarkMode),
                  const SizedBox(height: 75),
                ],
              ),
            ),
    );
  }

  Widget _buildTextField(
    TextEditingController controller,
    String hintText,
    int minLines,
    int maxLines, {
    Widget? suffixIcon,
    String? initialValue,
  }) {
    if (initialValue != null && initialValue.isNotEmpty) {
      controller.text = initialValue;
    }

    return TextField(
      controller: controller,
      minLines: minLines,
      maxLines: maxLines,
      decoration: InputDecoration(
        hintText: hintText,
        border: OutlineInputBorder(
          borderRadius: BorderRadius.circular(11),
        ),
        contentPadding:
            const EdgeInsets.symmetric(horizontal: 15, vertical: 11),
        suffixIcon: suffixIcon,
      ),
    );
  }

  Widget _buildDropdownMenu() {
    return SizedBox(
      height: 50,
      child: DropdownMenu(
        onSelected: (value) {
          FocusManager.instance.primaryFocus?.unfocus();
          setState(() {
            _selectedSectionId = value;
          });
        },
        dropdownMenuEntries: _sections.map(
          (item) {
            return DropdownMenuEntry(value: item.id, label: item.name);
          },
        ).toList(),
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
    );
  }

  Widget _buildImageUploadSection(bool isDarkMode) {
    return Column(
      children: [
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
            ),
          ],
        ),
        const SizedBox(height: 15),
        _images.isNotEmpty
            ? SizedBox(
                height: 190,
                child: GridView.builder(
                  shrinkWrap: true,
                  gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
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
            : const SizedBox(height: 190),
      ],
    );
  }
}
