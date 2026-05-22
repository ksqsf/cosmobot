import React from 'react';
import { createRoot } from 'react-dom/client';
import { Provider } from 'jotai';
import { appStore } from './lib/stores';
import App from './App';
import './styles/global.css';

const root = document.getElementById('root');

if (root === null) {
  throw new Error('Missing #root element');
}

createRoot(root).render(
  <React.StrictMode>
    <Provider store={appStore}>
      <App />
    </Provider>
  </React.StrictMode>
);
