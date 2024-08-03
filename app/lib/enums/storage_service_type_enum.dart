import 'package:json_annotation/json_annotation.dart';

enum StorageServiceTypeEnum {
  /// db.
  @JsonValue('DB')
  db,

  /// oss.
  @JsonValue('OSS')
  oss
}
