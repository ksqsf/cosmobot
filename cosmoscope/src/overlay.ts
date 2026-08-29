import { computed, onScopeDispose, ref, watch, type ComputedRef, type Ref } from 'vue'
import { useConfirm } from 'primevue/useconfirm'
import type { ConfirmationOptions } from 'primevue/confirmationoptions'

const activeLayers = ref<symbol[]>([])

interface OverlayLayer {
  readonly isTop: ComputedRef<boolean>
  readonly show: () => void
  readonly hide: () => void
}

interface LayeredConfirm {
  readonly require: (options: ConfirmationOptions) => void
  readonly close: () => void
}

export function useOverlayLayer(visible?: Readonly<Ref<boolean>>): OverlayLayer {
  const id = Symbol('overlay')
  const isTop = computed(() => activeLayers.value.at(-1) === id)
  const show = (): void => {
    if (!activeLayers.value.includes(id)) activeLayers.value = [...activeLayers.value, id]
  }
  const hide = (): void => { activeLayers.value = activeLayers.value.filter((layer) => layer !== id) }

  if (visible !== undefined) watch(visible, (value) => { if (value) show(); else hide() }, { immediate: true })
  onScopeDispose(hide)
  return { isTop, show, hide }
}

export function useLayeredConfirm(): LayeredConfirm {
  const confirm = useConfirm()
  const layer = useOverlayLayer()
  return {
    require(options: ConfirmationOptions): void {
      confirm.require({
        ...options,
        onShow: () => { layer.show(); options.onShow?.() },
        onHide: () => { layer.hide(); options.onHide?.() },
        accept: () => { layer.hide(); options.accept?.() },
        reject: () => { layer.hide(); options.reject?.() },
      })
    },
    close(): void { layer.hide(); confirm.close() },
  }
}
