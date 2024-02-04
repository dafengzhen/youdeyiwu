package com.youdeyiwu.service.point.impl;

import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.point.PointMapper;
import com.youdeyiwu.model.dto.point.SavePointPermissionRuleDto;
import com.youdeyiwu.model.dto.point.SavePointRuleDto;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointPermissionRuleEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.model.vo.point.PointPermissionRuleEntityVo;
import com.youdeyiwu.model.vo.point.PointRuleEntityVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.repository.point.PointPermissionRuleRepository;
import com.youdeyiwu.repository.point.PointRepository;
import com.youdeyiwu.repository.point.PointRuleRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import java.util.List;
import java.util.Objects;
import java.util.stream.StreamSupport;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * point.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PointServiceImpl implements PointService {

  private final PointRepository pointRepository;

  private final PointRuleRepository pointRuleRepository;

  private final PointPermissionRuleRepository pointPermissionRuleRepository;

  private final PointHistoryRepository pointHistoryRepository;

  private final UserRepository userRepository;

  private final ConfigRepository configRepository;

  private final PointMapper pointMapper;

  private final PointCoreService pointCoreService;

  @Transactional
  @Override
  public void save(SavePointRuleDto dto) {
    PointRuleEntity pointRuleEntity = pointRuleRepository
        .findByRuleName(dto.ruleName())
        .orElseGet(PointRuleEntity::new);

    pointRuleEntity.setRuleName(dto.ruleName());
    if (Objects.nonNull(dto.initiatorRewardPoints())) {
      pointRuleEntity.setInitiatorRewardPoints(dto.initiatorRewardPoints());
    }
    if (Objects.nonNull(dto.receiverRewardPoints())) {
      pointRuleEntity.setReceiverRewardPoints(dto.receiverRewardPoints());
    }

    pointRuleRepository.save(pointRuleEntity);
  }

  @Transactional
  @Override
  public void save(SavePointPermissionRuleDto dto) {
    PointPermissionRuleEntity pointPermissionRuleEntity = pointPermissionRuleRepository
        .findByPermissionRuleName(dto.permissionRuleName())
        .orElseGet(PointPermissionRuleEntity::new);

    pointPermissionRuleEntity.setPermissionRuleName(dto.permissionRuleName());
    if (Objects.nonNull(dto.requiredPoints())) {
      pointPermissionRuleEntity.setRequiredPoints(dto.requiredPoints());
    }

    pointPermissionRuleRepository.save(pointPermissionRuleEntity);
  }

  @Override
  public List<PointRuleEntityVo> queryRules() {
    return StreamSupport.stream(pointRuleRepository.findAll().spliterator(), false)
        .map(pointMapper::entityToVo)
        .toList();
  }

  @Override
  public List<PointPermissionRuleEntityVo> queryPermissionRules() {
    return StreamSupport.stream(pointPermissionRuleRepository.findAll().spliterator(), false)
        .map(pointMapper::entityToVo)
        .toList();
  }

  @Override
  public PageVo<PointHistoryEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(pointHistoryRepository.findAll(pageable).map(pointMapper::entityToVo));
  }

  @Override
  public PointEntity findPointByUserId(Long userId) {
    UserEntity userEntity = userRepository.findById(userId)
        .orElseThrow(UserNotFoundException::new);

    PointEntity pointEntity;
    if (Objects.isNull(userEntity.getPoint())) {
      return pointCoreService.create(userEntity);
    } else {
      pointEntity = userEntity.getPoint();
    }

    return pointEntity;
  }

  @Override
  public PointEntity findPointByUserEntity(UserEntity userEntity) {
    return findPointByUserId(userEntity.getId());
  }
}