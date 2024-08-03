import 'package:json_annotation/json_annotation.dart';

enum PostSortStateEnum {
  /// default.
  @JsonValue('DEFAULT')
  defaultSort,

  /// popular.
  @JsonValue('POPULAR')
  popular,

  /// current top (The thread is pinned in the current section).
  @JsonValue('CURRENT_TOP')
  currentTop,

  /// global top (The thread is pinned across all sections).
  @JsonValue('GLOBAL_TOP')
  globalTop,
}
