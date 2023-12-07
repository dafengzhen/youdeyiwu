package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.enums.forum.SectionStateEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * section.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class SectionEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * cover.
   */
  private String cover;

  /**
   * overview.
   */
  private String overview;

  /**
   * content.
   */
  private String content;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * states.
   */
  private Set<SectionStateEnum> states;

  /**
   * admins.
   */
  private Set<UserEntityVo> admins;

  /**
   * allows.
   */
  private Set<UserEntityVo> allows;

  /**
   * blocks.
   */
  private Set<UserEntityVo> blocks;

  /**
   * accessKey.
   */
  private String accessKey;

  /**
   * tagGroups.
   */
  private Set<TagGroupEntityVo> tagGroups;

  /**
   * tags.
   */
  private Set<TagEntityVo> tags;

  /**
   * sectionGroups.
   */
  private Set<SectionGroupEntityVo> sectionGroups;

}