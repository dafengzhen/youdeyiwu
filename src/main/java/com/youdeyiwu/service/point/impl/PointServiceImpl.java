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
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.repository.point.PointPermissionRuleRepository;
import com.youdeyiwu.repository.point.PointRuleRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import java.util.List;
import java.util.Objects;
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

  private final PointRuleRepository pointRuleRepository;

  private final PointPermissionRuleRepository pointPermissionRuleRepository;

  private final PointHistoryRepository pointHistoryRepository;

  private final UserRepository userRepository;

  private final PointMapper pointMapper;

  private final PointCoreService pointCoreService;

  @Transactional
  @Override
  public void save(SavePointRuleDto dto) {
    PointRuleEntity pointRuleEntity = pointRuleRepository
        .findByRuleName(dto.ruleName())
        .orElseGet(PointRuleEntity::new);
    pointMapper.dtoToEntity(dto, pointRuleEntity);
    pointRuleRepository.save(pointRuleEntity);
  }

  @Transactional
  @Override
  public void save(SavePointPermissionRuleDto dto) {
    PointPermissionRuleEntity pointPermissionRuleEntity = pointPermissionRuleRepository
        .findByPermissionRuleName(dto.permissionRuleName())
        .orElseGet(PointPermissionRuleEntity::new);
    pointMapper.dtoToEntity(dto, pointPermissionRuleEntity);
    pointPermissionRuleRepository.save(pointPermissionRuleEntity);
  }

  @Override
  public List<PointRuleEntityVo> queryRules() {
    return pointRuleRepository.findAll().stream()
        .map(pointMapper::entityToVo)
        .toList();
  }

  @Override
  public List<PointPermissionRuleEntityVo> queryPermissionRules() {
    return pointPermissionRuleRepository.findAll()
        .stream()
        .map(pointMapper::entityToVo)
        .toList();
  }

  @Override
  public PageVo<PointHistoryEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(pointHistoryRepository.findAll(pageable).map(pointMapper::entityToVo));
  }

  @Override
  public PageVo<PointHistoryEntityVo> queryAllHistoryByUserId(Long userId, Pageable pageable) {
    UserEntity userEntity = userRepository.findById(userId)
        .orElseThrow(UserNotFoundException::new);
    return new PageVo<>(
        pointHistoryRepository.findAllByUser(userEntity, pageable).map(pointMapper::entityToVo)
    );
  }

  @Override
  public PointEntity findPointByUserId(Long userId) {
    UserEntity userEntity = userRepository.findById(userId)
        .orElseThrow(UserNotFoundException::new);

    if (Objects.isNull(userEntity.getPoint())) {
      return pointCoreService.create(userEntity);
    } else {
      return userEntity.getPoint();
    }
  }

  @Override
  public PointEntity findPointByUserEntity(UserEntity userEntity) {
    return findPointByUserId(userEntity.getId());
  }
}