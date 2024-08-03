// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'message.dart';

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
