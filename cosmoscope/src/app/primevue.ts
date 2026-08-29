import { definePreset } from '@primeuix/themes'
import Aura from '@primeuix/themes/aura'

export const CosmoscopePreset = definePreset(Aura, {
  semantic: {
    primary: {
      50: '#f4f1ff', 100: '#e9e4ff', 200: '#d5ccff', 300: '#b9aaff', 400: '#a191ff',
      500: '#8b7cf6', 600: '#7564e8', 700: '#6756df', 800: '#5141b9', 900: '#433797', 950: '#292440',
    },
    colorScheme: {
      light: {
        surface: { 0: '#ffffff', 50: '#fcfcfd', 100: '#f7f8fa', 200: '#edf0f4', 300: '#dfe2e8', 400: '#656d7a', 500: '#59616e', 600: '#505763', 700: '#414752', 800: '#30343d', 900: '#1b1d24', 950: '#111319' },
        primary: { color: '{primary.700}', contrastColor: '#ffffff', hoverColor: '{primary.800}', activeColor: '{primary.900}' },
      },
      dark: {
        surface: { 0: '#ffffff', 50: '#f0f2f7', 100: '#d8dce5', 200: '#c4c9d3', 300: '#aeb5c3', 400: '#929aaa', 500: '#858d9e', 600: '#4a5160', 700: '#303642', 800: '#191d26', 900: '#14171e', 950: '#0c0e13' },
        primary: { color: '{primary.400}', contrastColor: '#0c0e13', hoverColor: '{primary.300}', activeColor: '{primary.200}' },
      },
    },
  },
  components: {
    button: { root: { borderRadius: '8px', paddingX: '0.8rem', paddingY: '0.52rem', gap: '0.42rem', label: { fontWeight: '650' } } },
    card: { root: { borderRadius: '12px' }, body: { padding: '1rem' }, title: { fontSize: '0.9rem' } },
    datatable: { headerCell: { padding: '0.58rem 0.85rem' }, bodyCell: { padding: '0.62rem 0.85rem' } },
    inputtext: { root: { paddingX: '0.7rem', paddingY: '0.52rem', borderRadius: '7px' } },
    select: { root: { borderRadius: '7px' }, option: { padding: '0.52rem 0.7rem' } },
    tag: { root: { borderRadius: '5px', padding: '0.15rem 0.4rem', fontSize: '0.62rem', fontWeight: '700' } },
    dialog: { root: { borderRadius: '12px' }, header: { padding: '1rem' }, content: { padding: '0 1rem 1rem' } },
  },
})
