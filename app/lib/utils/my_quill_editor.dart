import 'dart:io' as io show Directory, File;

import 'package:cached_network_image/cached_network_image.dart'
    show CachedNetworkImageProvider;
import 'package:flutter/material.dart';
import 'package:flutter_quill/extensions.dart'
    show isAndroid, isDesktop, isIOS, isWeb;
import 'package:flutter_quill/flutter_quill.dart';
// ignore: implementation_imports
import 'package:flutter_quill_extensions/src/editor/image/widgets/image.dart'
    show getImageProviderByImageSource;
import 'package:flutter_quill_extensions/flutter_quill_extensions.dart';
import 'package:path/path.dart' as path;

import './timestamp_embed.dart';

class MyQuillEditor extends StatelessWidget {
  const MyQuillEditor({
    required this.controller,
    required this.configurations,
    required this.scrollController,
    required this.focusNode,
    super.key,
  });

  final QuillController controller;
  final QuillEditorConfigurations configurations;
  final ScrollController scrollController;
  final FocusNode focusNode;

  @override
  Widget build(BuildContext context) {
    // final defaultTextStyle = DefaultTextStyle.of(context);

    return QuillEditor(
      scrollController: scrollController,
      focusNode: focusNode,
      controller: controller,
      configurations: configurations.copyWith(
        // elementOptions: const QuillEditorElementOptions(
        //   codeBlock: QuillEditorCodeBlockElementOptions(
        //     enableLineNumbers: true,
        //   ),
        //   orderedList: QuillEditorOrderedListElementOptions(),
        //   unorderedList: QuillEditorUnOrderedListElementOptions(
        //     useTextColorForDot: true,
        //   ),
        // ),
        // customStyles: DefaultStyles(
        //   h1: DefaultTextBlockStyle(
        //     defaultTextStyle.style.copyWith(
        //       fontSize: 32,
        //       height: 1.15,
        //       fontWeight: FontWeight.w300,
        //     ),
        //     const HorizontalSpacing(0, 0),
        //     const VerticalSpacing(16, 0),
        //     const VerticalSpacing(0, 0),
        //     null,
        //   ),
        //   sizeSmall: defaultTextStyle.style.copyWith(fontSize: 9),
        // ),
        scrollable: true,
        placeholder: 'Start writing your notes...',
        onImagePaste: (imageBytes) async {
          if (isWeb()) {
            return null;
          }
          // We will save it to system temporary files
          final newFileName =
              'imageFile-${DateTime.now().toIso8601String()}.png';
          final newPath = path.join(
            io.Directory.systemTemp.path,
            newFileName,
          );
          final file = await io.File(
            newPath,
          ).writeAsBytes(imageBytes, flush: true);
          return file.path;
        },
        onGifPaste: (gifBytes) async {
          if (isWeb()) {
            return null;
          }
          // We will save it to system temporary files
          final newFileName = 'gifFile-${DateTime.now().toIso8601String()}.gif';
          final newPath = path.join(
            io.Directory.systemTemp.path,
            newFileName,
          );
          final file = await io.File(
            newPath,
          ).writeAsBytes(gifBytes, flush: true);
          return file.path;
        },
        embedBuilders: [
          ...(isWeb()
              ? FlutterQuillEmbeds.editorWebBuilders()
              : FlutterQuillEmbeds.editorBuilders(
                  imageEmbedConfigurations: QuillEditorImageEmbedConfigurations(
                    imageErrorWidgetBuilder: (context, error, stackTrace) {
                      return Text(
                        'Error while loading an image: ${error.toString()}',
                      );
                    },
                    imageProviderBuilder: (context, imageUrl) {
                      // cached_network_image is supported
                      // only for Android, iOS and web

                      // We will use it only if image from network
                      if (isAndroid(supportWeb: false) ||
                          isIOS(supportWeb: false) ||
                          isWeb()) {
                        if (isHttpBasedUrl(imageUrl)) {
                          return CachedNetworkImageProvider(
                            imageUrl,
                          );
                        }
                      }
                      return getImageProviderByImageSource(
                        imageUrl,
                        imageProviderBuilder: null,
                        context: context,
                        assetsPrefix: QuillSharedExtensionsConfigurations.get(
                                context: context)
                            .assetsPrefix,
                      );
                    },
                  ),
                  videoEmbedConfigurations: QuillEditorVideoEmbedConfigurations(
                    // Loading YouTube videos on Desktop is not supported yet
                    // when using iframe platform view
                    youtubeVideoSupportMode: isDesktop(supportWeb: false)
                        ? YoutubeVideoSupportMode.customPlayerWithDownloadUrl
                        : YoutubeVideoSupportMode.iframeView,
                  ),
                )),
          TimeStampEmbedBuilderWidget(),
        ],
      ),
    );
  }
}
