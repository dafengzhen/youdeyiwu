package com.youdeyiwu.service.point.impl;

import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.point.PointMapper;
import com.youdeyiwu.model.dto.point.SavePointAutoRuleDto;
import com.youdeyiwu.model.dto.point.SavePointRuleDto;
import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointAutoRuleEntityVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.model.vo.point.PointRuleEntityVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointAutoRuleRepository;
import com.youdeyiwu.repository.point.PointHistoryRepository;
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

  private final PointAutoRuleRepository pointAutoRuleRepository;

  private final PointRuleRepository pointRuleRepository;

  private final PointHistoryRepository pointHistoryRepository;

  private final UserRepository userRepository;

  private final ConfigRepository configRepository;

  private final PointMapper pointMapper;

  private final PointCoreService pointCoreService;

  @Transactional
  @Override
  public void save(SavePointAutoRuleDto dto) {
    PointAutoRuleEntity pointAutoRuleEntity = pointAutoRuleRepository
        .findByAutoRuleName(dto.autoRuleName())
        .orElseGet(PointAutoRuleEntity::new);

    pointAutoRuleEntity.setAutoRuleName(dto.autoRuleName());
    if (Objects.nonNull(dto.requiredPoints())) {
      pointAutoRuleEntity.setRequiredPoints(dto.requiredPoints());
    }

    pointAutoRuleRepository.save(pointAutoRuleEntity);
  }

  @Transactional
  @Override
  public void save(SavePointRuleDto dto) {
    PointRuleEntity pointRuleEntity = pointRuleRepository
        .findByRuleName(dto.ruleName())
        .orElseGet(PointRuleEntity::new);

    pointRuleEntity.setRuleName(dto.ruleName());
    if (Objects.nonNull(dto.requiredPoints())) {
      pointRuleEntity.setRequiredPoints(dto.requiredPoints());
    }

    pointRuleRepository.save(pointRuleEntity);
  }

  @Override
  public List<PointAutoRuleEntityVo> queryAutoRules() {
    return StreamSupport.stream(pointAutoRuleRepository.findAll().spliterator(), false)
        .map(pointMapper::entityToVo)
        .toList();
  }

  @Override
  public List<PointRuleEntityVo> queryRules() {
    return StreamSupport.stream(pointRuleRepository.findAll().spliterator(), false)
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