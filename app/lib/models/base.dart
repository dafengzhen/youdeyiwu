/// Base
abstract class Base {
  /// id
  final int id;

  /// createdBy
  final int? createdBy;

  /// updatedBy
  final int? updatedBy;

  /// createdOn
  final String? createdOn;

  /// updatedOn
  final String? updatedOn;

  /// deleted
  final bool deleted;

  const Base({
    required this.id,
    required this.deleted,
    this.createdBy,
    this.updatedBy,
    this.createdOn,
    this.updatedOn,
  });

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Base && runtimeType == other.runtimeType && id == other.id;

  @override
  int get hashCode => id.hashCode;

  @override
  String toString() {
    return 'Base{id: $id, createdBy: $createdBy, updatedBy: $updatedBy, createdOn: $createdOn, updatedOn: $updatedOn, deleted: $deleted}';
  }
}
