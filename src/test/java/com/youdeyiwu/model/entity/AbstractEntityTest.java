package com.youdeyiwu.model.entity;

import com.youdeyiwu.model.entity.user.UserEntity;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * base.
 *
 * @author dafengzhen
 */
@SpringBootTest
class AbstractEntityTest {

    @Test
    void testEquals() {
        AbstractEntity entity1 = new UserEntity();
        entity1.setId(1L);

        AbstractEntity entity2 = new UserEntity();
        entity2.setId(1L);

        assertEquals(entity1, entity2);
    }
}