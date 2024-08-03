import 'package:json_annotation/json_annotation.dart';

enum PostReviewStateEnum {
  /// approved (Default).
  @JsonValue('APPROVED')
  approved,

  /// rejected (The forum administrators, section administrators, and post authors have access).
  @JsonValue('REJECTED')
  rejected,

  /// pending review (The forum administrators, section administrators, and post authors have access).
  @JsonValue('PENDING_REVIEW')
  pendingReview,
}
