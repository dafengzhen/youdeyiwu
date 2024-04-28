<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue';
import { A11y, Navigation, Pagination, Scrollbar } from 'swiper/modules';
import { Swiper, SwiperSlide } from 'swiper/vue';
import Viewer from 'viewerjs';
import { withBase } from 'vitepress';

import 'swiper/css';
import 'swiper/css/pagination';
import 'swiper/css/navigation';
import 'viewerjs/dist/viewer.css';

let viewerRef = ref<Viewer>();
const isDark = ref(false);
let darkEl;

function vPSwitchAppearanceFn() {
  isDark.value = !isDark.value;
}

function onClickImage() {
  viewerRef.value?.show();
}

onMounted(() => {
  isDark.value = document.documentElement.classList.contains('dark');
  darkEl = document.querySelector('.VPSwitchAppearance');
  darkEl?.addEventListener('click', vPSwitchAppearanceFn);

  viewerRef.value = new Viewer(document.getElementById('images'), {
    toolbar: false,
    navbar: false,
    title: false,
    initialCoverage: 1,
    viewed() {
      viewerRef.value.zoomTo(1);
    },
  });
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
  <div id="images" class="text-center shadow user-select-none">
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
          v-on:click="onClickImage"
          class="image img-fluid"
          style="cursor: zoom-in"
          :src="withBase(`/images/${image}-dark.png`)"
          alt="index"
        />

        <img
          v-else
          v-on:click="onClickImage"
          class="image img-fluid"
          style="cursor: zoom-in"
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
