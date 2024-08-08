// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'token.dart';

// **************************************************************************
// CopyWithGenerator
// **************************************************************************

abstract class _$TokenCWProxy {
  Token id(int id);

  Token alias(String alias);

  Token token(String token);

  Token expDays(int expDays);

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Token(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Token(...).copyWith(id: 12, name: "My name")
  /// ````
  Token call({
    int? id,
    String? alias,
    String? token,
    int? expDays,
  });
}

/// Proxy class for `copyWith` functionality. This is a callable class and can be used as follows: `instanceOfToken.copyWith(...)`. Additionally contains functions for specific fields e.g. `instanceOfToken.copyWith.fieldName(...)`
class _$TokenCWProxyImpl implements _$TokenCWProxy {
  const _$TokenCWProxyImpl(this._value);

  final Token _value;

  @override
  Token id(int id) => this(id: id);

  @override
  Token alias(String alias) => this(alias: alias);

  @override
  Token token(String token) => this(token: token);

  @override
  Token expDays(int expDays) => this(expDays: expDays);

  @override

  /// This function **does support** nullification of nullable fields. All `null` values passed to `non-nullable` fields will be ignored. You can also use `Token(...).copyWith.fieldName(...)` to override fields one at a time with nullification support.
  ///
  /// Usage
  /// ```dart
  /// Token(...).copyWith(id: 12, name: "My name")
  /// ````
  Token call({
    Object? id = const $CopyWithPlaceholder(),
    Object? alias = const $CopyWithPlaceholder(),
    Object? token = const $CopyWithPlaceholder(),
    Object? expDays = const $CopyWithPlaceholder(),
  }) {
    return Token(
      id: id == const $CopyWithPlaceholder() || id == null
          ? _value.id
          // ignore: cast_nullable_to_non_nullable
          : id as int,
      alias: alias == const $CopyWithPlaceholder() || alias == null
          ? _value.alias
          // ignore: cast_nullable_to_non_nullable
          : alias as String,
      token: token == const $CopyWithPlaceholder() || token == null
          ? _value.token
          // ignore: cast_nullable_to_non_nullable
          : token as String,
      expDays: expDays == const $CopyWithPlaceholder() || expDays == null
          ? _value.expDays
          // ignore: cast_nullable_to_non_nullable
          : expDays as int,
    );
  }
}

extension $TokenCopyWith on Token {
  /// Returns a callable class that can be used as follows: `instanceOfToken.copyWith(...)` or like so:`instanceOfToken.copyWith.fieldName(...)`.
  // ignore: library_private_types_in_public_api
  _$TokenCWProxy get copyWith => _$TokenCWProxyImpl(this);
}

// **************************************************************************
// JsonSerializableGenerator
// **************************************************************************

Token _$TokenFromJson(Map<String, dynamic> json) => Token(
      id: (json['id'] as num).toInt(),
      alias: json['alias'] as String,
      token: json['token'] as String,
      expDays: (json['expDays'] as num).toInt(),
    );

Map<String, dynamic> _$TokenToJson(Token instance) => <String, dynamic>{
      'id': instance.id,
      'alias': instance.alias,
      'token': instance.token,
      'expDays': instance.expDays,
    };
