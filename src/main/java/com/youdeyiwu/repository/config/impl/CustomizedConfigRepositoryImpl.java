package com.youdeyiwu.repository.config.impl;

import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.repository.config.CustomizedConfigRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.NoResultException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * config.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Repository
public class CustomizedConfigRepositoryImpl implements CustomizedConfigRepository {

  private final EntityManager entityManager;

  @Override
  public ConfigEntity saveByTypeAndName(ConfigTypeEnum type, String name, String value) {
    try {
      ConfigEntity configEntity = entityManager.createQuery(
              "select c from ConfigEntity c where c.type = :type and c.name = :name",
              ConfigEntity.class
          )
          .setParameter("type", type)
          .setParameter("name", name)
          .getSingleResult();
      configEntity.setValue(value);
      return configEntity;
    } catch (NoResultException e) {
      ConfigEntity configEntity = new ConfigEntity();
      configEntity.setType(type);
      configEntity.setName(name);
      configEntity.setValue(value);
      entityManager.persist(configEntity);
      return configEntity;
    }
  }
}
