import 'package:json_annotation/json_annotation.dart';

enum MatcherTypeEnum {
  /// ant.
  @JsonValue('ANT')
  ant,

  /// regex.
  @JsonValue('REGEX')
  regex
}
