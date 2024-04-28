<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue';
import { Navigation, Pagination, Scrollbar, A11y } from 'swiper/modules';
import { Swiper, SwiperSlide } from 'swiper/vue';
import { withBase } from 'vitepress';

import 'swiper/css';
import 'swiper/css/pagination';
import 'swiper/css/navigation';

const isDark = ref(false);
let darkEl;

function vPSwitchAppearanceFn() {
  isDark.value = !isDark.value;
}

onMounted(() => {
  isDark.value = document.documentElement.classList.contains('dark');
  darkEl = document.querySelector('.VPSwitchAppearance');
  darkEl?.addEventListener('click', vPSwitchAppearanceFn);
});

onUnmounted(() => {
  darkEl?.removeEventListener('click', vPSwitchAppearanceFn);
});

const images = [
  'index',
  'post',
  'post2',
  'posts',
  'section',
  'sections',
  'user',
  'users',
  'messages',
  'section-admin',
  'login',
  'register',
];

const modules = [Navigation, Pagination, Scrollbar, A11y];
</script>

<template>
  <div class="text-center shadow user-select-none">
    <swiper
      :modules="modules"
      navigation
      :pagination="{ clickable: true } as any"
      :space-between="50"
      class="swiper"
    >
      <swiper-slide v-for="(image, i) in images" :key="i">
        <img
          v-if="isDark"
          class="img-fluid"
          :src="withBase(`/images/${image}-dark.png`)"
          alt="index"
        />

        <img
          v-else
          class="img-fluid"
          :src="withBase(`/images/${image}.png`)"
          alt="index"
        />
      </swiper-slide>
    </swiper>
  </div>
</template>

<style lang="scss" scoped>
@import 'bootstrap';
</style>
