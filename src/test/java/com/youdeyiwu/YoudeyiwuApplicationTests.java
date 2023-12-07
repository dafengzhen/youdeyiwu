package com.youdeyiwu;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

/**
 * youdeyiwu.
 *
 * @author dafengzhen
 */
@AutoConfigureMockMvc
@SpringBootTest
class YoudeyiwuApplicationTests {

  @Autowired
  private MockMvc mockMvc;

  @Test
  void contextLoads() throws Exception {
    mockMvc.perform(get("/actuator/health").accept(APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().json(
            """
                {
                  "status": "UP"
                }
                """
        ));
  }

}
