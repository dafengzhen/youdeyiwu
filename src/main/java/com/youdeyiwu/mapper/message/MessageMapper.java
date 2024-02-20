package com.youdeyiwu.mapper.message;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.dto.message.CreateGlobalMessageDto;
import com.youdeyiwu.model.dto.message.CreateMessageDto;
import com.youdeyiwu.model.entity.message.GlobalMessageEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.vo.message.GlobalMessageEntityVo;
import com.youdeyiwu.model.vo.message.MessageEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * message.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface MessageMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "link", ignore = true)
  @Mapping(target = "links", ignore = true)
  void dtoToEntity(CreateGlobalMessageDto dto, @MappingTarget GlobalMessageEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "link", ignore = true)
  @Mapping(target = "links", ignore = true)
  @Mapping(target = "receiver", ignore = true)
  void dtoToEntity(CreateMessageDto dto, @MappingTarget MessageEntity entity);

  /**
   * entityToVo.
   *
   * @param vo     vo
   * @param entity entity
   */
  @Mapping(target = "sender", ignore = true)
  void entityToVo(@MappingTarget MessageEntityVo vo, GlobalMessageEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return GlobalMessageEntityVo
   */
  @Mapping(target = "sender", ignore = true)
  GlobalMessageEntityVo entityToVo(GlobalMessageEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return MessageEntityVo
   */
  @Mapping(target = "sender", ignore = true)
  @Mapping(target = "receiver", ignore = true)
  MessageEntityVo entityToVo(MessageEntity entity);
}
