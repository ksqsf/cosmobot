import js from '@eslint/js'
import vue from 'eslint-plugin-vue'
import { vueTsConfigs, withVueTs } from '@vue/eslint-config-typescript'

export default withVueTs(
  { rootDir: import.meta.dirname, allowComponentTypeUnsafety: true },
  { ignores: ['dist/**', 'playwright-report/**', 'test-results/**', '*.config.js', '*.config.d.ts', '*.tsbuildinfo'] },
  js.configs.recommended,
  vue.configs['flat/recommended'],
  vueTsConfigs.strictTypeChecked,
  vueTsConfigs.stylisticTypeChecked,
  {
    rules: {
      'no-undef': 'off',
      '@typescript-eslint/consistent-type-exports': 'error',
      '@typescript-eslint/consistent-type-imports': ['error', { prefer: 'type-imports', fixStyle: 'inline-type-imports' }],
      '@typescript-eslint/explicit-function-return-type': ['error', { allowExpressions: true, allowConciseArrowFunctionExpressionsStartingWithVoid: true }],
      '@typescript-eslint/no-confusing-void-expression': ['error', { ignoreArrowShorthand: true }],
      '@typescript-eslint/no-import-type-side-effects': 'error',
      '@typescript-eslint/no-unnecessary-condition': 'error',
      '@typescript-eslint/switch-exhaustiveness-check': 'error',
      'vue/block-order': ['error', { order: ['script', 'template', 'style'] }],
      'vue/component-api-style': ['error', ['script-setup']],
      'vue/component-name-in-template-casing': ['error', 'PascalCase'],
      'vue/define-macros-order': 'error',
      'vue/html-self-closing': ['error', { html: { void: 'always', normal: 'always', component: 'always' } }],
      'vue/no-bare-strings-in-template': 'off',
      'vue/no-ref-object-reactivity-loss': 'error',
      'vue/no-undef-components': 'error',
      'vue/no-unused-properties': ['error', { groups: ['props', 'data', 'computed', 'methods', 'setup'] }],
      'vue/prefer-define-options': 'error',
      'vue/require-default-prop': 'off',
      'vue/require-typed-ref': 'error',
    },
  },
)
