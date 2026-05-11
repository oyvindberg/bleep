package com.example.myapp;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest
@AutoConfigureMockMvc
@ActiveProfiles("test")
class ItemControllerTest {
  @Autowired MockMvc mvc;

  @Test
  void postCreatesAndGetListsItem() throws Exception {
    mvc.perform(
            post("/items")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"name\":\"apple\",\"quantity\":12}"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.name").value("apple"))
        .andExpect(jsonPath("$.quantity").value(12));

    mvc.perform(get("/items"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$[?(@.name=='apple')].quantity").value(12));
  }

  @Test
  void invalidPostReturnsBadRequest() throws Exception {
    mvc.perform(
            post("/items")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{\"name\":\"\",\"quantity\":1}"))
        .andExpect(status().isBadRequest());
  }
}
