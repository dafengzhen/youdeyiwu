<script setup lang="ts">
import { onMounted, onUnmounted, ref } from 'vue';

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
</script>

<template>
  <div class="mt-5">
    <div class="text-center shadow">
      <img
        v-if="isDark"
        class="img-fluid"
        src="/images/index-dark.png"
        alt="index"
      />

      <img v-else class="img-fluid" src="/images/index.png" alt="index" />
    </div>
  </div>
</template>

<style lang="scss" scoped>
@import 'bootstrap';
</style>
