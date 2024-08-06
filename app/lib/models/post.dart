import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import '../enums/file_type_enum.dart';
import '../enums/post_review_state_enum.dart';
import '../enums/post_sort_state_enum.dart';
import '../enums/post_state_enum.dart';
import 'base.dart';
import 'comment_reply.dart';
import 'page.dart';
import 'post_badge.dart';
import 'post_image.dart';
import 'post_review_queue.dart';
import 'section.dart';
import 'tag.dart';
import 'user.dart';

part 'post.g.dart';

/// Post
@JsonSerializable()
class Post extends Base {
  /// name
  final String name;

  /// cover
  final String? cover;

  /// coverImage
  final List<int>? coverImage;

  /// coverImageType
  final FileTypeEnum? coverImageType;

  /// overview
  final String? overview;

  /// content
  final String? content;

  /// plainTextContent
  final String? plainTextContent;

  /// markdownContent
  final String? markdownContent;

  /// deltaContent
  final String? deltaContent;

  /// content link
  final String? contentLink;

  /// disable comments
  final bool disableComments;

  /// disable replies
  final bool disableReplies;

  /// badges
  final Set<PostBadge>? badges;

  /// images
  final Set<PostImage>? images;

  /// states
  final Set<PostStateEnum> states;

  /// reviewState
  final PostReviewStateEnum reviewState;

  /// sortState
  final PostSortStateEnum sortState;

  /// allows
  final Set<User>? allows;

  /// blocks
  final Set<User>? blocks;

  /// accessKey
  final String? accessKey;

  /// styles (To separate each item, use a semicolon).
  final String? styles;

  /// class names (To separate each item, use a space).
  final String? classNames;

  /// page views
  final int pageViews;

  /// comments count
  final int commentsCount;

  /// replies count
  final int repliesCount;

  /// likes count
  final int likesCount;

  /// followers count
  final int followersCount;

  /// favorites count
  final int favoritesCount;

  /// section
  final Section? section;

  /// tags
  final Set<Tag>? tags;

  /// user
  final User? user;

  /// liked
  final bool? liked;

  /// followed
  final bool? followed;

  /// favorited
  final bool? favorited;

  /// comments
  final Page<CommentReply>? comments;

  /// post review queue
  final PostReviewQueue? postReviewQueue;

  const Post({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.disableComments,
    required this.disableReplies,
    required this.states,
    required this.reviewState,
    required this.sortState,
    required this.pageViews,
    required this.commentsCount,
    required this.repliesCount,
    required this.likesCount,
    required this.followersCount,
    required this.favoritesCount,
    this.tags,
    this.cover,
    this.coverImage,
    this.coverImageType,
    this.overview,
    this.content,
    this.plainTextContent,
    this.markdownContent,
    this.deltaContent,
    this.contentLink,
    this.accessKey,
    this.styles,
    this.classNames,
    this.section,
    this.user,
    this.liked,
    this.followed,
    this.favorited,
    this.comments,
    this.postReviewQueue,
    this.badges,
    this.images,
    this.allows,
    this.blocks,
  });

  factory Post.withResponse(Response response) {
    return Post.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Post.fromJsonString(String json) => Post.fromJson(jsonDecode(json));

  factory Post.fromJson(Map<String, dynamic> json) => _$PostFromJson(json);

  Map<String, dynamic> toJson() => _$PostToJson(this);

  @override
  String toString() {
    return 'Post{name: $name, cover: $cover, coverImage: $coverImage, coverImageType: $coverImageType, overview: $overview, content: $content, contentLink: $contentLink, disableComments: $disableComments, disableReplies: $disableReplies, badges: $badges, images: $images, states: $states, reviewState: $reviewState, sortState: $sortState, allows: $allows, blocks: $blocks, accessKey: $accessKey, styles: $styles, classNames: $classNames, pageViews: $pageViews, commentsCount: $commentsCount, repliesCount: $repliesCount, likesCount: $likesCount, followersCount: $followersCount, favoritesCount: $favoritesCount, section: $section, tags: $tags, user: $user, liked: $liked, followed: $followed, favorited: $favorited, comments: $comments, postReviewQueue: $postReviewQueue}';
  }
}
