package com.youdeyiwu.service.message.impl;

import com.youdeyiwu.enums.message.MessageStateEnum;
import com.youdeyiwu.exception.GlobalMessageNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.message.MessageMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.message.CreateGlobalMessageDto;
import com.youdeyiwu.model.dto.message.CreateMessageDto;
import com.youdeyiwu.model.entity.message.GlobalMessageEntity;
import com.youdeyiwu.model.entity.message.GlobalMessageUserEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.message.GlobalMessageEntityVo;
import com.youdeyiwu.model.vo.message.MessageEntityVo;
import com.youdeyiwu.repository.message.GlobalMessageRepository;
import com.youdeyiwu.repository.message.MessageRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.message.MessageService;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.util.Streamable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * message.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class MessageServiceImpl implements MessageService {

  private final GlobalMessageRepository globalMessageRepository;

  private final MessageRepository messageRepository;

  private final UserRepository userRepository;

  private final MessageMapper messageMapper;

  private final SecurityService securityService;

  private final UserMapper userMapper;

  @Transactional
  @Override
  public GlobalMessageEntity createGlobalMessage(CreateGlobalMessageDto dto) {
    GlobalMessageEntity globalMessageEntity = new GlobalMessageEntity();
    messageMapper.dtoToEntity(dto, globalMessageEntity);

    if (securityService.isAuthenticated()) {
      globalMessageEntity.setSender(
          userRepository.findById(securityService.getUserId())
              .orElseThrow(UserNotFoundException::new)
      );
    }

    globalMessageRepository.save(globalMessageEntity);
    return globalMessageEntity;
  }

  @Transactional
  @Override
  public MessageEntity create(CreateMessageDto dto) {
    MessageEntity messageEntity = new MessageEntity();
    messageMapper.dtoToEntity(dto, messageEntity);

    if (securityService.isAuthenticated()) {
      messageEntity.setSender(
          userRepository.findById(securityService.getUserId())
              .orElseThrow(UserNotFoundException::new)
      );
    }

    messageEntity.setReceiver(
        userRepository.findById(dto.receiver())
            .orElseThrow(UserNotFoundException::new)
    );

    messageRepository.save(messageEntity);
    return messageEntity;
  }

  @Transactional
  @Override
  public void updateGlobalMessageState(Long id) {
    GlobalMessageEntity globalMessageEntity = globalMessageRepository.findById(id)
        .orElseThrow(GlobalMessageNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    Optional<GlobalMessageUserEntity> globalMessageUserEntityOptional = globalMessageEntity
        .getGlobalMessageUsers()
        .stream()
        .filter(globalMessageUserEntity ->
            globalMessageUserEntity.getGlobalMessage().equals(globalMessageEntity)
                && globalMessageUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    if (globalMessageUserEntityOptional.isPresent()) {
      GlobalMessageUserEntity globalMessageUserEntity = globalMessageUserEntityOptional.get();
      globalMessageUserEntity.setState(MessageStateEnum.READ);
    } else {
      GlobalMessageUserEntity globalMessageUserEntity = new GlobalMessageUserEntity();
      globalMessageUserEntity.setGlobalMessage(globalMessageEntity);
      globalMessageUserEntity.setUser(userEntity);
      globalMessageUserEntity.setState(MessageStateEnum.READ);
      globalMessageEntity.getGlobalMessageUsers().add(globalMessageUserEntity);
      userEntity.getUserGlobalMessages().add(globalMessageUserEntity);
    }
  }

  @Transactional
  @Override
  public void updateState(Long id) {
    MessageEntity messageEntity = messageRepository.findByIdAndReceiver(
        id,
        userRepository.findById(securityService.getUserId())
            .orElseThrow(UserNotFoundException::new)
    );
    messageEntity.setState(MessageStateEnum.READ);
  }

  @Override
  public GlobalMessageEntityVo queryGlobalMessage(Long id) {
    GlobalMessageEntity globalMessageEntity = globalMessageRepository.findById(id)
        .orElseThrow(GlobalMessageNotFoundException::new);

    GlobalMessageEntityVo vo = messageMapper.entityToVo(globalMessageEntity);
    setSender(vo, globalMessageEntity);
    return vo;
  }

  @Override
  public MessageEntityVo query(Long id) {
    MessageEntity messageEntity;
    if (securityService.isAuthenticated()) {
      messageEntity = messageRepository.findByIdAndReceiver(
          id,
          userRepository.findById(securityService.getUserId())
              .orElseThrow(UserNotFoundException::new)
      );
    } else {
      messageEntity = messageRepository.findByIdAndSenderIsNull(id);
    }

    MessageEntityVo vo = messageMapper.entityToVo(messageEntity);
    vo.setSender(userMapper.entityToVo(messageEntity.getSender()));
    vo.setReceiver(userMapper.entityToVo(messageEntity.getReceiver()));
    return vo;
  }

  @Override
  public PageVo<GlobalMessageEntityVo> queryAllGlobalMessage(Pageable pageable) {
    if (securityService.isAuthenticated()) {
      return new PageVo<>(globalMessageRepository.findAll(pageable).map(globalMessageEntity -> {
        GlobalMessageEntityVo vo = messageMapper.entityToVo(globalMessageEntity);
        setSender(vo, globalMessageEntity);
        return vo;
      }));
    }

    return new PageVo<>(
        globalMessageRepository.findAllBySenderIsNull(pageable)
            .map(messageMapper::entityToVo)
    );
  }

  @Override
  public PageVo<MessageEntityVo> queryAll(Pageable pageable) {
    if (securityService.isAuthenticated()) {
      return queryAllUserMessage(pageable);
    }

    return queryAllAnonymousMessage(pageable);
  }

  @Transactional
  @Override
  public void delete(Long id) {
    MessageEntity messageEntity;
    if (securityService.isAuthenticated()) {
      messageEntity = messageRepository.findByIdAndReceiver(
          id,
          userRepository.findById(securityService.getUserId())
              .orElseThrow(UserNotFoundException::new)
      );
    } else {
      messageEntity = messageRepository.findByIdAndSenderIsNull(id);
    }

    messageRepository.delete(messageEntity);
  }

  /**
   * set sender.
   *
   * @param vo                  vo
   * @param globalMessageEntity globalMessageEntity
   */
  private void setSender(GlobalMessageEntityVo vo, GlobalMessageEntity globalMessageEntity) {
    vo.setSender(userMapper.entityToVo(globalMessageEntity.getSender()));
  }

  /**
   * query all user message.
   *
   * @param pageable pageable
   * @return PageVo
   */
  private PageVo<MessageEntityVo> queryAllUserMessage(Pageable pageable) {
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Page<GlobalMessageUserEntity> globalMessageEntities = globalMessageRepository.findAllByReceiver(
        userEntity,
        new PaginationPositionDto(
            PageRequest.of(
                0,
                6,
                Sort.by(Sort.Order.desc("sort"), Sort.Order.desc("id"))
            )
        )
    );
    Page<MessageEntity> messageEntities = messageRepository.findAllByReceiver(userEntity, pageable);

    List<MessageEntityVo> vos = Stream.of(
            globalMessageEntities.map(globalMessageUserEntity -> {
              GlobalMessageEntity globalMessageEntity = globalMessageUserEntity.getGlobalMessage();
              MessageEntityVo vo = new MessageEntityVo();
              messageMapper.entityToVo(vo, globalMessageEntity);
              vo.setState(globalMessageUserEntity.getState());
              vo.setSender(userMapper.entityToVo(globalMessageUserEntity.getUser()));
              vo.setReceiver(userMapper.entityToVo(globalMessageEntity.getSender()));
              return vo;
            }),
            messageEntities.map(messageEntity -> {
              MessageEntityVo vo = messageMapper.entityToVo(messageEntity);
              vo.setSender(userMapper.entityToVo(messageEntity.getSender()));
              vo.setReceiver(userMapper.entityToVo(messageEntity.getReceiver()));
              return vo;
            })
        )
        .flatMap(Streamable::stream)
        .sorted(
            Comparator.comparing(MessageEntityVo::getMessageRange)
                .thenComparing(MessageEntityVo::getState)
        )
        .toList();

    return new PageVo<>(new PageImpl<>(vos, pageable, messageEntities.getTotalElements()));
  }

  /**
   * query all anonymous message.
   *
   * @param pageable pageable
   * @return PageVo
   */
  private PageVo<MessageEntityVo> queryAllAnonymousMessage(Pageable pageable) {
    Page<GlobalMessageEntity> globalMessageEntities = globalMessageRepository.findAllBySenderIsNull(
        PageRequest.of(
            0,
            6,
            Sort.by(Sort.Order.desc("sort"), Sort.Order.desc("id"))
        )
    );
    Page<MessageEntity> messageEntities = messageRepository.findAllBySenderIsNull(pageable);

    List<MessageEntityVo> vos = Stream.of(
            globalMessageEntities.map(globalMessageEntity -> {
              MessageEntityVo vo = new MessageEntityVo();
              messageMapper.entityToVo(vo, globalMessageEntity);
              return vo;
            }),
            messageEntities.map(messageEntity -> {
              MessageEntityVo vo = messageMapper.entityToVo(messageEntity);
              vo.setReceiver(userMapper.entityToVo(messageEntity.getReceiver()));
              return vo;
            })
        )
        .flatMap(Streamable::stream)
        .sorted(Comparator.comparing(MessageEntityVo::getMessageRange))
        .toList();

    return new PageVo<>(new PageImpl<>(vos, pageable, messageEntities.getTotalElements()));
  }
}