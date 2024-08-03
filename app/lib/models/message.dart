import 'dart:convert';

import 'package:http/http.dart';
import 'package:json_annotation/json_annotation.dart';

import '../enums/message_range_enum.dart';
import '../enums/message_state_enum.dart';
import '../enums/message_type_enum.dart';
import 'base.dart';
import 'user.dart';

part 'message.g.dart';

/// Message
@JsonSerializable()
class Message extends Base {
  /// name
  final String name;

  /// overview
  final String overview;

  /// link (Can be an absolute or relative path).
  final String? link;

  /// links
  final Map<String, String>? links;

  /// content
  final Map<String, String>? content;

  /// message type
  final MessageTypeEnum messageType;

  /// message range
  final MessageRangeEnum messageRange;

  /// state
  final MessageStateEnum state;

  /// sender
  final User? sender;

  /// receiver
  final User? receiver;

  const Message({
    required super.id,
    required super.deleted,
    super.createdBy,
    super.updatedBy,
    super.createdOn,
    super.updatedOn,
    required this.name,
    required this.overview,
    required this.messageType,
    required this.messageRange,
    required this.state,
    this.link,
    this.links,
    this.content,
    this.sender,
    this.receiver,
  });

  factory Message.withResponse(Response response) {
    return Message.fromJson(jsonDecode(utf8.decode(response.bodyBytes)));
  }

  factory Message.fromJsonString(String json) =>
      Message.fromJson(jsonDecode(json));

  factory Message.fromJson(Map<String, dynamic> json) =>
      _$MessageFromJson(json);

  Map<String, dynamic> toJson() => _$MessageToJson(this);

  @override
  String toString() {
    return 'Message{name: $name, overview: $overview, link: $link, links: $links, content: $content, messageType: $messageType, messageRange: $messageRange, state: $state, sender: $sender, receiver: $receiver}';
  }
}
