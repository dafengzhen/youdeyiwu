// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'message.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$MessageCWProxy {
  Message id(int id);

  Message deleted(bool deleted);

  Message createdBy(int? createdBy);

  Message updatedBy(int? updatedBy);

  Message createdOn(String? createdOn);

  Message updatedOn(String? updatedOn);

  Message name(String name);

  Message overview(String overview);

  Message messageType(MessageTypeEnum messageType);

  Message messageRange(MessageRangeEnum messageRange);

  Message state(MessageStateEnum state);

  Message link(String? link);

  Message links(Map<String, String>? links);

  Message content(Map<String, String>? content);

  Message sender(User? sender);

  Message receiver(User? receiver);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Message(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Message(...).copyWith(id: 12, name: "My name")
  /// ````
  Message call({
    int? id,
    bool? deleted,
    int? createdBy,
    int? updatedBy,
    String? createdOn,
    String? updatedOn,
    String? name,
    String? overview,
    MessageTypeEnum? messageType,
    MessageRangeEnum? messageRange,
    MessageStateEnum? state,
    String? link,
    Map<String, String>? links,
    Map<String, String>? content,
    User? sender,
    User? receiver,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfMessage.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfMessage.copyWith.fieldName(...)`
class _$MessageCWProxyImpl implements _$MessageCWProxy {
  const _$MessageCWProxyImpl(this._value);

  final Message _value;

  @override
  Message id(int id) => this(id: id);

  @override
  Message deleted(bool deleted) => this(deleted: deleted);

  @override
  Message createdBy(int? createdBy) => this(createdBy: createdBy);

  @override
  Message updatedBy(int? updatedBy) => this(updatedBy: updatedBy);

  @override
  Message createdOn(String? createdOn) => this(createdOn: createdOn);

  @override
  Message updatedOn(String? updatedOn) => this(updatedOn: updatedOn);

  @override
  Message name(String name) => this(name: name);

  @override
  Message overview(String overview) => this(overview: overview);

  @override
  Message messageType(MessageTypeEnum messageType) =>
      this(messageType: messageType);

  @override
  Message messageRange(MessageRangeEnum messageRange) =>
      this(messageRange: messageRange);

  @override
  Message state(MessageStateEnum state) => this(state: state);

  @override
  Message link(String? link) => this(link: link);

  @override
  Message links(Map<String, String>? links) => this(links: links);

  @override
  Message content(Map<String, String>? content) => this(content: content);

  @override
  Message sender(User? sender) => this(sender: sender);

  @override
  Message receiver(User? receiver) => this(receiver: receiver);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Message(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Message(...).copyWith(id: 12, name: "My name")
  /// ````
  Message call({
    Object? id = const $CopyWithPlaceholder(),
    Object? deleted = const $CopyWithPlaceholder(),
    Object? createdBy = const $CopyWithPlaceholder(),
    Object? updatedBy = const $CopyWithPlaceholder(),
    Object? createdOn = const $CopyWithPlaceholder(),
    Object? updatedOn = const $CopyWithPlaceholder(),
    Object? name = const $CopyWithPlaceholder(),
    Object? overview = const $CopyWithPlaceholder(),
    Object? messageType = const $CopyWithPlaceholder(),
    Object? messageRange = const $CopyWithPlaceholder(),
    Object? state = const $CopyWithPlaceholder(),
    Object? link = const $CopyWithPlaceholder(),
    Object? links = const $CopyWithPlaceholder(),
    Object? content = const $CopyWithPlaceholder(),
    Object? sender = const $CopyWithPlaceholder(),
    Object? receiver = const $CopyWithPlaceholder(),
  }) {
    return Message(
      id: id == const $CopyWithPlaceholder() || id == null
          ? _value.id
          // ignore: cast_nullable_to_non_nullable
          : id as int,
      deleted: deleted == const $CopyWithPlaceholder() || deleted == null
          ? _value.deleted
          // ignore: cast_nullable_to_non_nullable
          : deleted as bool,
      createdBy: createdBy == const $CopyWithPlaceholder()
          ? _value.createdBy
          // ignore: cast_nullable_to_non_nullable
          : createdBy as int?,
      updatedBy: updatedBy == const $CopyWithPlaceholder()
          ? _value.updatedBy
          // ignore: cast_nullable_to_non_nullable
          : updatedBy as int?,
      createdOn: createdOn == const $CopyWithPlaceholder()
          ? _value.createdOn
          // ignore: cast_nullable_to_non_nullable
          : createdOn as String?,
      updatedOn: updatedOn == const $CopyWithPlaceholder()
          ? _value.updatedOn
          // ignore: cast_nullable_to_non_nullable
          : updatedOn as String?,
      name: name == const $CopyWithPlaceholder() || name == null
          ? _value.name
          // ignore: cast_nullable_to_non_nullable
          : name as String,
      overview: overview == const $CopyWithPlaceholder() || overview == null
          ? _value.overview
          // ignore: cast_nullable_to_non_nullable
          : overview as String,
      messageType:
          messageType == const $CopyWithPlaceholder() || messageType == null
              ? _value.messageType
              // ignore: cast_nullable_to_non_nullable
              : messageType as MessageTypeEnum,
      messageRange:
          messageRange == const $CopyWithPlaceholder() || messageRange == null
              ? _value.messageRange
              // ignore: cast_nullable_to_non_nullable
              : messageRange as MessageRangeEnum,
      state: state == const $CopyWithPlaceholder() || state == null
          ? _value.state
          // ignore: cast_nullable_to_non_nullable
          : state as MessageStateEnum,
      link: link == const $CopyWithPlaceholder()
          ? _value.link
          // ignore: cast_nullable_to_non_nullable
          : link as String?,
      links: links == const $CopyWithPlaceholder()
          ? _value.links
          // ignore: cast_nullable_to_non_nullable
          : links as Map<String, String>?,
      content: content == const $CopyWithPlaceholder()
          ? _value.content
          // ignore: cast_nullable_to_non_nullable
          : content as Map<String, String>?,
      sender: sender == const $CopyWithPlaceholder()
          ? _value.sender
          // ignore: cast_nullable_to_non_nullable
          : sender as User?,
      receiver: receiver == const $CopyWithPlaceholder()
          ? _value.receiver
          // ignore: cast_nullable_to_non_nullable
          : receiver as User?,
    );
  }
}

extension $MessageCopyWith on Message {
  /// Returns a callable class that can be used as follows: `instanceOfMessage.copyWith(...)` or like so:`instanceOfMessage.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$MessageCWProxy get copyWith => _$MessageCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Message _$MessageFromJson(Map<String, dynamic> json) => Message(
      id: (json['id'] as num).toInt(),
      deleted: json['deleted'] as bool,
      createdBy: (json['createdBy'] as num?)?.toInt(),
      updatedBy: (json['updatedBy'] as num?)?.toInt(),
      createdOn: json['createdOn'] as String?,
      updatedOn: json['updatedOn'] as String?,
      name: json['name'] as String,
      overview: json['overview'] as String,
      messageType: $enumDecode(_$MessageTypeEnumEnumMap, json['messageType']),
      messageRange:
          $enumDecode(_$MessageRangeEnumEnumMap, json['messageRange']),
      state: $enumDecode(_$MessageStateEnumEnumMap, json['state']),
      link: json['link'] as String?,
      links: (json['links'] as Map<String, dynamic>?)?.map(
        (k, e) => MapEntry(k, e as String),
      ),
      content: (json['content'] as Map<String, dynamic>?)?.map(
        (k, e) => MapEntry(k, e as String),
      ),
      sender: json['sender'] == null
          ? null
          : User.fromJson(json['sender'] as Map<String, dynamic>),
      receiver: json['receiver'] == null
          ? null
          : User.fromJson(json['receiver'] as Map<String, dynamic>),
    );

Map<String, dynamic> _$MessageToJson(Message instance) {
  final val = <String, dynamic>{
    'id': instance.id,
  };

  void writeNotNull(String key, dynamic value) {
    if (value != null) {
      val[key] = value;
    }
  }

  writeNotNull('createdBy', instance.createdBy);
  writeNotNull('updatedBy', instance.updatedBy);
  writeNotNull('createdOn', instance.createdOn);
  writeNotNull('updatedOn', instance.updatedOn);
  val['deleted'] = instance.deleted;
  val['name'] = instance.name;
  val['overview'] = instance.overview;
  writeNotNull('link', instance.link);
  writeNotNull('links', instance.links);
  writeNotNull('content', instance.content);
  val['messageType'] = _$MessageTypeEnumEnumMap[instance.messageType]!;
  val['messageRange'] = _$MessageRangeEnumEnumMap[instance.messageRange]!;
  val['state'] = _$MessageStateEnumEnumMap[instance.state]!;
  writeNotNull('sender', instance.sender);
  writeNotNull('receiver', instance.receiver);
  return val;
}

const _$MessageTypeEnumEnumMap = {
  MessageTypeEnum.system: 'SYSTEM',
  MessageTypeEnum.globalMessage: 'GLOBAL_MESSAGE',
  MessageTypeEnum.message: 'MESSAGE',
  MessageTypeEnum.config: 'CONFIG',
  MessageTypeEnum.file: 'FILE',
  MessageTypeEnum.user: 'USER',
  MessageTypeEnum.role: 'ROLE',
  MessageTypeEnum.permission: 'PERMISSION',
  MessageTypeEnum.menu: 'MENU',
  MessageTypeEnum.submenu: 'SUBMENU',
  MessageTypeEnum.action: 'ACTION',
  MessageTypeEnum.section: 'SECTION',
  MessageTypeEnum.sectionGroup: 'SECTION_GROUP',
  MessageTypeEnum.post: 'POST',
  MessageTypeEnum.tag: 'TAG',
  MessageTypeEnum.tagGroup: 'TAG_GROUP',
  MessageTypeEnum.comment: 'COMMENT',
  MessageTypeEnum.reply: 'REPLY',
  MessageTypeEnum.point: 'POINT',
};

const _$MessageRangeEnumEnumMap = {
  MessageRangeEnum.allUser: 'ALL_USER',
  MessageRangeEnum.user: 'USER',
};

const _$MessageStateEnumEnumMap = {
  MessageStateEnum.unread: 'UNREAD',
  MessageStateEnum.read: 'READ',
};
