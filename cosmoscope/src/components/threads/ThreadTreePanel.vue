<script setup lang="ts">
import Button from 'primevue/button'
import Tree from 'primevue/tree'
import type { TreeNode } from 'primevue/treenode'

defineProps<{ nodes: TreeNode[] }>()
const emit = defineEmits<{ select: [node: TreeNode] }>()
const selectedKeys = defineModel<Record<string, boolean>>('selectedKeys', { required: true })
const expandedKeys = defineModel<Record<string, boolean>>('expandedKeys', { required: true })
const focused = defineModel<boolean>('focused', { required: true })
const zoom = defineModel<number>('zoom', { required: true })
</script>

<template>
  <section class="thread-tree-panel">
    <header>
      <span>Reply tree</span>
      <span class="thread-tree-tools">
        <Button
          icon="pi pi-search-minus"
          text
          rounded
          size="small"
          aria-label="Zoom reply tree out"
          title="Zoom out"
          :disabled="zoom <= 60"
          @click="zoom -= 10"
        />
        <small>{{ zoom }}%</small>
        <Button
          icon="pi pi-search-plus"
          text
          rounded
          size="small"
          aria-label="Zoom reply tree in"
          title="Zoom in"
          :disabled="zoom >= 150"
          @click="zoom += 10"
        />
        <Button
          :icon="focused ? 'pi pi-window-minimize' : 'pi pi-window-maximize'"
          text
          rounded
          size="small"
          :aria-label="focused ? 'Exit focused reply tree' : 'Focus reply tree'"
          :title="focused ? 'Show context' : 'Focus tree'"
          @click="focused = !focused"
        />
      </span>
    </header>
    <div class="thread-tree-viewport">
      <Tree
        v-model:selection-keys="selectedKeys"
        v-model:expanded-keys="expandedKeys"
        :value="nodes"
        :style="{ zoom: zoom / 100 }"
        selection-mode="single"
        @node-select="emit('select', $event)"
      />
    </div>
  </section>
</template>
