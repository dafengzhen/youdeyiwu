package com.youdeyiwu.model.vo;

import java.util.List;
import lombok.Data;
import org.springframework.data.domain.Page;

/**
 * page.
 *
 * @author dafengzhen
 */
@Data
public class PageVo<T> {

  /**
   * content.
   */
  private List<T> content;

  /**
   * pageable.
   */
  private PageableVo pageable;

  /**
   * PageVo.
   *
   * @param page page
   */
  public PageVo(Page<T> page) {
    content = page.getContent();
    pageable = create(page);
  }

  /**
   * create.
   *
   * @param page page
   * @return PageableVo
   */
  private PageableVo create(Page<T> page) {
    PageableVo vo = new PageableVo();
    vo.setSize(page.getSize());
    vo.setPage(page.getNumber());
    vo.setPages(page.getTotalPages());
    vo.setNext(page.hasNext());
    vo.setPrevious(page.hasPrevious());
    return vo;
  }
}