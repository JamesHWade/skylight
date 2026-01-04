// Skylight Calendar - Service Worker
// Provides offline caching for PWA functionality

const CACHE_NAME = 'skylight-v1';
const STATIC_ASSETS = [
  '/',
  '/www/styles.css',
  '/www/custom.js',
  '/www/logo.svg',
  '/www/icon-192.png',
  '/www/icon-512.png',
  '/www/manifest.json'
];

// Install event - cache static assets
self.addEventListener('install', function(event) {
  event.waitUntil(
    caches.open(CACHE_NAME).then(function(cache) {
      console.log('[SW] Caching static assets');
      return cache.addAll(STATIC_ASSETS);
    }).catch(function(error) {
      console.log('[SW] Cache failed:', error);
    })
  );
  // Activate immediately
  self.skipWaiting();
});

// Activate event - clean up old caches
self.addEventListener('activate', function(event) {
  event.waitUntil(
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          if (cacheName !== CACHE_NAME) {
            console.log('[SW] Removing old cache:', cacheName);
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
  // Take control of all clients immediately
  self.clients.claim();
});

// Fetch event - serve from cache, fallback to network
self.addEventListener('fetch', function(event) {
  const request = event.request;
  const url = new URL(request.url);

  // Skip non-GET requests
  if (request.method !== 'GET') {
    return;
  }

  // Skip WebSocket connections (Shiny uses these)
  if (url.protocol === 'ws:' || url.protocol === 'wss:') {
    return;
  }

  // Skip Shiny internal requests
  if (url.pathname.includes('__sockjs__') ||
      url.pathname.includes('websocket') ||
      url.pathname.includes('/session/')) {
    return;
  }

  // Network-first for API calls and dynamic content
  if (url.pathname.includes('/api/') ||
      url.hostname.includes('openweathermap') ||
      url.hostname.includes('googleapis')) {
    event.respondWith(
      fetch(request).catch(function() {
        return caches.match(request);
      })
    );
    return;
  }

  // Cache-first for static assets
  event.respondWith(
    caches.match(request).then(function(cachedResponse) {
      if (cachedResponse) {
        // Return cached version, but also update cache in background
        event.waitUntil(
          fetch(request).then(function(networkResponse) {
            if (networkResponse.ok) {
              caches.open(CACHE_NAME).then(function(cache) {
                cache.put(request, networkResponse);
              });
            }
          }).catch(function() {
            // Network failed, that's fine - we have cache
          })
        );
        return cachedResponse;
      }

      // Not in cache, fetch from network
      return fetch(request).then(function(networkResponse) {
        // Cache successful responses for static assets
        if (networkResponse.ok && isStaticAsset(url.pathname)) {
          const responseClone = networkResponse.clone();
          caches.open(CACHE_NAME).then(function(cache) {
            cache.put(request, responseClone);
          });
        }
        return networkResponse;
      }).catch(function() {
        // Network failed and not in cache - return offline page if available
        if (request.mode === 'navigate') {
          return caches.match('/');
        }
        return new Response('Offline', { status: 503, statusText: 'Offline' });
      });
    })
  );
});

// Helper to identify static assets worth caching
function isStaticAsset(pathname) {
  return pathname.match(/\.(css|js|png|jpg|jpeg|gif|svg|ico|woff|woff2|ttf|eot)$/i) ||
         pathname === '/' ||
         pathname.startsWith('/www/');
}
