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
let appViewerRef = ref<Viewer>();
const isDark = ref(false);
let darkEl: HTMLElement;

function vPSwitchAppearanceFn() {
  isDark.value = !isDark.value;
}

function onClickImage() {
  viewerRef.value?.show();
}

function onClickAppImage() {
  appViewerRef.value?.show();
}

onMounted(() => {
  isDark.value = document.documentElement.classList.contains('dark');
  darkEl = document.querySelector('.VPSwitchAppearance');
  darkEl?.addEventListener('click', vPSwitchAppearanceFn);

  // web
  viewerRef.value = new Viewer(document.getElementById('images'), {
    toolbar: false,
    navbar: false,
    title: false,
    initialCoverage: 1,
    viewed() {
      viewerRef.value.zoomTo(1);
    },
  });

  // app
  appViewerRef.value = new Viewer(document.getElementById('app-images'), {
    toolbar: false,
    navbar: false,
    title: false,
    initialCoverage: 1,
    viewed() {
      appViewerRef.value.zoomTo(1);
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

const appImages = [
  'index',
  'post-id',
  'post-id-2',
  'post-id-comment',
  'post-edit',
  'post-edit-2',
  'sections',
  'section-id',
  'messages',
  'user',
];

const modules = [Navigation, Pagination, Scrollbar, A11y];
</script>

<template>
  <div id="images" class="text-center shadow-sm user-select-none">
    <!-- web -->
    <swiper
      :modules="modules"
      :navigation="true as any"
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
          alt="web"
        />

        <img
          v-else
          v-on:click="onClickImage"
          class="image img-fluid"
          style="cursor: zoom-in"
          :src="withBase(`/images/${image}.png`)"
          alt="web"
        />
      </swiper-slide>
    </swiper>
  </div>

  <div class="my-5"></div>

  <!-- app -->
  <div id="app-images" class="text-center user-select-none">
    <swiper
      :modules="modules"
      :navigation="true as any"
      :pagination="{ clickable: true } as any"
      :space-between="50"
      class="swiper"
    >
      <swiper-slide v-for="(image, i) in appImages" :key="i" class="d-flex justify-content-center">
        <img
          v-if="isDark"
          v-on:click="onClickAppImage"
          class="image img-fluid"
          style="cursor: zoom-in"
          :src="withBase(`/images/app/${image}-dark.png`)"
          alt="app"
        />

        <img
          v-else
          v-on:click="onClickAppImage"
          class="image img-fluid"
          style="cursor: zoom-in"
          :src="withBase(`/images/app/${image}.png`)"
          alt="app"
        />
      </swiper-slide>
    </swiper>
  </div>

  <p class="small text-center text-secondary" style="margin-top: 4rem;">
    Tip: You can switch to dark mode using the top right corner to view images in dark mode.
  </p>
</template>

<style lang="scss" scoped>
@import 'bootstrap';
</style>
