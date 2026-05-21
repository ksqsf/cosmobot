import App from './App.svelte';
import './styles/global.css';

const target = document.getElementById('app');

if (target === null) {
  throw new Error('Missing #app mount point');
}

new App({ target });
