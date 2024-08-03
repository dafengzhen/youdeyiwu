import 'dart:io' as io show File;

import 'package:flutter/material.dart';
import 'package:flutter_quill/extensions.dart' show isAndroid, isIOS, isWeb;
import 'package:flutter_quill/flutter_quill.dart';
import 'package:flutter_quill_extensions/flutter_quill_extensions.dart';
import 'package:google_fonts/google_fonts.dart';
import 'package:image_cropper/image_cropper.dart';
import 'package:path/path.dart' as path;
import 'package:path_provider/path_provider.dart'
    show getApplicationDocumentsDirectory;
import 'package:provider/provider.dart';

import './timestamp_embed.dart';

class MyQuillToolbar extends StatefulWidget {
  const MyQuillToolbar({
    required this.controller,
    required this.focusNode,
    super.key,
  });

  final QuillController controller;
  final FocusNode focusNode;

  @override
  State<MyQuillToolbar> createState() => _MyQuillToolbarState();
}

class _MyQuillToolbarState extends State<MyQuillToolbar> {
  bool useCustomQuillToolbar = true;

  Future<void> onImageInsertWithCropping(
    String image,
    QuillController controller,
    BuildContext context,
  ) async {
    final croppedFile = await ImageCropper().cropImage(
      sourcePath: image,
      // aspectRatioPresets: [
      //   CropAspectRatioPreset.square,
      //   CropAspectRatioPreset.ratio3x2,
      //   CropAspectRatioPreset.original,
      //   CropAspectRatioPreset.ratio4x3,
      //   CropAspectRatioPreset.ratio16x9
      // ],
      uiSettings: [
        AndroidUiSettings(
          toolbarTitle: 'Cropper',
          toolbarColor: Colors.deepOrange,
          toolbarWidgetColor: Colors.white,
          initAspectRatio: CropAspectRatioPreset.original,
          lockAspectRatio: false,
        ),
        IOSUiSettings(
          title: 'Cropper',
        ),
        WebUiSettings(
          context: context,
        ),
      ],
    );
    final newImage = croppedFile?.path;
    if (newImage == null) {
      return;
    }
    if (isWeb()) {
      controller.insertImageBlock(imageSource: newImage);
      return;
    }
    final newSavedImage = await saveImage(io.File(newImage));
    controller.insertImageBlock(imageSource: newSavedImage);
  }

  Future<void> onImageInsert(String image, QuillController controller) async {
    if (isWeb() || isHttpBasedUrl(image)) {
      controller.insertImageBlock(imageSource: image);
      return;
    }
    final newSavedImage = await saveImage(io.File(image));
    controller.insertImageBlock(imageSource: newSavedImage);
  }

  Future<String> saveImage(io.File file) async {
    final appDocDir = await getApplicationDocumentsDirectory();
    final fileExt = path.extension(file.path);
    final newFileName = '${DateTime.now().toIso8601String()}$fileExt';
    final newPath = path.join(
      appDocDir.path,
      newFileName,
    );
    final copiedFile = await file.copy(newPath);
    return copiedFile.path;
  }

  @override
  Widget build(BuildContext context) {
    if (useCustomQuillToolbar) {
      return QuillToolbar(
        configurations: const QuillToolbarConfigurations(),
        child: SingleChildScrollView(
          scrollDirection: Axis.horizontal,
          child: Wrap(
            children: [
              IconButton(
                onPressed: () {
                  // context.read<SettingsNotifier>().updateSettings(false);
                  setState(() {
                    useCustomQuillToolbar = false;
                  });
                },
                icon: const Icon(
                  Icons.width_normal,
                ),
              ),
              QuillToolbarHistoryButton(
                isUndo: true,
                controller: widget.controller,
              ),
              QuillToolbarHistoryButton(
                isUndo: false,
                controller: widget.controller,
              ),
              QuillToolbarToggleStyleButton(
                options: const QuillToolbarToggleStyleButtonOptions(),
                controller: widget.controller,
                attribute: Attribute.bold,
              ),
              QuillToolbarToggleStyleButton(
                options: const QuillToolbarToggleStyleButtonOptions(),
                controller: widget.controller,
                attribute: Attribute.italic,
              ),
              QuillToolbarToggleStyleButton(
                controller: widget.controller,
                attribute: Attribute.underline,
              ),
              QuillToolbarClearFormatButton(
                controller: widget.controller,
              ),
              const VerticalDivider(),
              QuillToolbarImageButton(
                controller: widget.controller,
              ),
              QuillToolbarCameraButton(
                controller: widget.controller,
              ),
              QuillToolbarVideoButton(
                controller: widget.controller,
              ),
              const VerticalDivider(),
              QuillToolbarColorButton(
                controller: widget.controller,
                isBackground: false,
              ),
              QuillToolbarColorButton(
                controller: widget.controller,
                isBackground: true,
              ),
              const VerticalDivider(),
              QuillToolbarSelectHeaderStyleDropdownButton(
                controller: widget.controller,
              ),
              const VerticalDivider(),
              QuillToolbarSelectLineHeightStyleDropdownButton(
                controller: widget.controller,
              ),
              const VerticalDivider(),
              QuillToolbarToggleCheckListButton(
                controller: widget.controller,
              ),
              QuillToolbarToggleStyleButton(
                controller: widget.controller,
                attribute: Attribute.ol,
              ),
              QuillToolbarToggleStyleButton(
                controller: widget.controller,
                attribute: Attribute.ul,
              ),
              QuillToolbarToggleStyleButton(
                controller: widget.controller,
                attribute: Attribute.inlineCode,
              ),
              QuillToolbarToggleStyleButton(
                controller: widget.controller,
                attribute: Attribute.blockQuote,
              ),
              QuillToolbarIndentButton(
                controller: widget.controller,
                isIncrease: true,
              ),
              QuillToolbarIndentButton(
                controller: widget.controller,
                isIncrease: false,
              ),
              const VerticalDivider(),
              QuillToolbarLinkStyleButton(controller: widget.controller),
            ],
          ),
        ),
      );
    }

    return QuillToolbar.simple(
      configurations: QuillSimpleToolbarConfigurations(
        controller: widget.controller,
        showAlignmentButtons: true,
        multiRowsDisplay: true,
        // fontFamilyValues: {
        //   'Amatic': GoogleFonts.amaticSc().fontFamily!,
        //   'Annie': GoogleFonts.annieUseYourTelescope().fontFamily!,
        //   'Formal': GoogleFonts.petitFormalScript().fontFamily!,
        //   'Roboto': GoogleFonts.roboto().fontFamily!
        // },
        fontSizesValues: const {
          '14': '14.0',
          '16': '16.0',
          '18': '18.0',
          '20': '20.0',
          '22': '22.0',
          '24': '24.0',
          '26': '26.0',
          '28': '28.0',
          '30': '30.0',
          '35': '35.0',
          '40': '40.0'
        },
        searchButtonType: SearchButtonType.modern,
        customButtons: [
          QuillToolbarCustomButtonOptions(
            icon: const Icon(Icons.add_alarm_rounded),
            onPressed: () {
              widget.controller.document
                  .insert(widget.controller.selection.extentOffset, '\n');
              widget.controller.updateSelection(
                TextSelection.collapsed(
                  offset: widget.controller.selection.extentOffset + 1,
                ),
                ChangeSource.local,
              );

              widget.controller.document.insert(
                widget.controller.selection.extentOffset,
                TimeStampEmbed(
                  DateTime.now().toString(),
                ),
              );

              widget.controller.updateSelection(
                TextSelection.collapsed(
                  offset: widget.controller.selection.extentOffset + 1,
                ),
                ChangeSource.local,
              );

              widget.controller.document
                  .insert(widget.controller.selection.extentOffset, ' ');
              widget.controller.updateSelection(
                TextSelection.collapsed(
                  offset: widget.controller.selection.extentOffset + 1,
                ),
                ChangeSource.local,
              );

              widget.controller.document
                  .insert(widget.controller.selection.extentOffset, '\n');
              widget.controller.updateSelection(
                TextSelection.collapsed(
                  offset: widget.controller.selection.extentOffset + 1,
                ),
                ChangeSource.local,
              );
            },
          ),
          QuillToolbarCustomButtonOptions(
            icon: const Icon(Icons.dashboard_customize),
            onPressed: () {
              // context.read<SettingsNotifier>().updateSettings(true);
              setState(() {
                useCustomQuillToolbar = true;
              });
            },
          ),
        ],
        embedButtons: FlutterQuillEmbeds.toolbarButtons(
          imageButtonOptions: QuillToolbarImageButtonOptions(
            imageButtonConfigurations: QuillToolbarImageConfigurations(
              onImageInsertCallback: isAndroid(supportWeb: false) ||
                      isIOS(supportWeb: false) ||
                      isWeb()
                  ? (image, controller) =>
                      onImageInsertWithCropping(image, controller, context)
                  : onImageInsert,
            ),
          ),
          tableButtonOptions: const QuillToolbarTableButtonOptions(),
        ),
      ),
    );
  }
}
