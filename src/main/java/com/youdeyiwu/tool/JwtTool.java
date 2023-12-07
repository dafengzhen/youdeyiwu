package com.youdeyiwu.tool;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.io.Encoders;
import io.jsonwebtoken.security.Keys;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.TemporalUnit;
import java.util.Date;
import java.util.Map;
import javax.crypto.SecretKey;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * jwt.
 *
 * @author dafengzhen
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class JwtTool {

  /**
   * create key (signature algorithm used HS256).
   *
   * @return SecretKey
   */
  public static SecretKey createSecret() {
    return Jwts.SIG.HS256.key().build();
  }

  /**
   * encoding key.
   *
   * @return String
   */
  public static String encodeSecret() {
    return Encoders.BASE64.encode(createSecret().getEncoded());
  }

  /**
   * decoding key.
   *
   * @return SecretKey
   */
  public static SecretKey decodeSecret(String secret) {
    return Keys.hmacShaKeyFor(Decoders.BASE64.decode(secret));
  }

  /**
   * create jwt.
   *
   * @param key          key
   * @param sub          sub
   * @param amountToAdd  amountToAdd
   * @param temporalUnit temporalUnit
   * @return String
   */
  public static String createJwt(
      SecretKey key,
      Long sub,
      Long amountToAdd,
      TemporalUnit temporalUnit
  ) {
    return Jwts.builder()
        .signWith(key)
        .subject(String.valueOf(sub))
        .expiration(JwtTool.generateDate(amountToAdd, temporalUnit))
        .compact();
  }

  /**
   * create jwt.
   *
   * @param key key
   * @param sub sub
   * @param exp exp
   * @return String
   */
  public static String createJwt(SecretKey key, Long sub, Duration exp) {
    return Jwts.builder()
        .signWith(key)
        .subject(String.valueOf(sub))
        .expiration(generateDate(exp))
        .compact();
  }

  /**
   * create jwt.
   *
   * @param key    key
   * @param sub    sub
   * @param header header
   * @param exp    exp
   * @return String
   */
  public static String createJwt(
      SecretKey key,
      Long sub,
      Map<String, Object> header,
      Duration exp
  ) {
    return Jwts.builder()
        .signWith(key)
        .subject(String.valueOf(sub))
        .header()
        .add(header)
        .and()
        .expiration(generateDate(exp))
        .compact();
  }

  /**
   * verification jwt.
   *
   * @param token token
   * @param key   key
   * @return Claims
   */
  public static Claims verifyJwt(String token, SecretKey key) {
    return Jwts.parser().verifyWith(key).build().parseSignedClaims(token).getPayload();
  }

  /**
   * generate a specified time value.
   *
   * @param amountToAdd  amount needed to be increased
   * @param temporalUnit unit of increase
   * @return Date
   */
  public static Date generateDate(Long amountToAdd, TemporalUnit temporalUnit) {
    return Date.from(LocalDateTime.now(ZoneOffset.UTC).plus(amountToAdd, temporalUnit)
        .toInstant(ZoneOffset.UTC));
  }

  /**
   * generate a specified time value.
   *
   * @param duration duration
   * @return Date
   */
  public static Date generateDate(Duration duration) {
    return Date.from(LocalDateTime.now(ZoneOffset.UTC).plus(duration).toInstant(ZoneOffset.UTC));
  }
}