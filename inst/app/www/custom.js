// Skylight Calendar - Custom JavaScript

(function() {
  'use strict';

  // =========================================================================
  // Chat Toggle Handler
  // =========================================================================

  document.addEventListener('click', function(e) {
    const chatSidebar = document.getElementById('chat_container');
    const toggleBtn = e.target.closest('#toggle_chat');

    if (toggleBtn) {
      // Toggle button clicked - open/close chat
      if (chatSidebar) {
        chatSidebar.classList.toggle('expanded');
        chatSidebar.classList.toggle('collapsed');
      }
      return;
    }

    // Click outside chat panel - close if open
    if (chatSidebar && chatSidebar.classList.contains('expanded')) {
      const clickedInsideChat = e.target.closest('#chat_container');
      if (!clickedInsideChat) {
        chatSidebar.classList.remove('expanded');
        chatSidebar.classList.add('collapsed');
      }
    }
  });

  // =========================================================================
  // Chat Input - Enter Key Handler
  // =========================================================================

  document.addEventListener('keydown', function(e) {
    // Find chat input by looking for the specific input
    const chatInput = document.querySelector('[id$="-user_input"]');

    if (chatInput && e.target === chatInput && e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();

      // Find and click the send button
      const sendButton = document.querySelector('[id$="-send_message"]');
      if (sendButton) {
        sendButton.click();
      }
    }
  });

  // =========================================================================
  // Smooth Scroll for Chat Messages
  // =========================================================================

  const scrollChatToBottom = function() {
    const chatContainer = document.querySelector('.chat-messages');
    if (chatContainer) {
      chatContainer.scrollTop = chatContainer.scrollHeight;
    }
  };

  // Observe chat messages for new content
  const observeChatMessages = function() {
    const chatContainer = document.querySelector('.chat-messages');
    if (!chatContainer) return;

    const observer = new MutationObserver(function(mutations) {
      scrollChatToBottom();
    });

    observer.observe(chatContainer, {
      childList: true,
      subtree: true
    });
  };

  // =========================================================================
  // Touch Gestures for Week Navigation
  // =========================================================================

  let touchStartX = 0;
  let touchEndX = 0;
  const minSwipeDistance = 50;

  const handleSwipe = function() {
    const swipeDistance = touchEndX - touchStartX;

    if (Math.abs(swipeDistance) < minSwipeDistance) return;

    // Find navigation buttons
    const prevButton = document.querySelector('[id$="-prev_week"], [id$="-prev_day"]');
    const nextButton = document.querySelector('[id$="-next_week"], [id$="-next_day"]');

    if (swipeDistance > 0 && prevButton) {
      // Swipe right - go to previous
      prevButton.click();
    } else if (swipeDistance < 0 && nextButton) {
      // Swipe left - go to next
      nextButton.click();
    }
  };

  document.addEventListener('touchstart', function(e) {
    touchStartX = e.changedTouches[0].screenX;
  }, { passive: true });

  document.addEventListener('touchend', function(e) {
    touchEndX = e.changedTouches[0].screenX;

    // Only handle swipes on calendar areas
    const target = e.target.closest('.week-grid, .day-schedule, .agenda-list');
    if (target) {
      handleSwipe();
    }
  }, { passive: true });

  // =========================================================================
  // Keyboard Navigation
  // =========================================================================

  document.addEventListener('keydown', function(e) {
    // Ignore if typing in an input
    if (e.target.matches('input, textarea, [contenteditable]')) return;

    switch (e.key) {
      case 'ArrowLeft':
        // Previous week/day
        const prevBtn = document.querySelector('[id$="-prev_week"], [id$="-prev_day"]');
        if (prevBtn) prevBtn.click();
        break;

      case 'ArrowRight':
        // Next week/day
        const nextBtn = document.querySelector('[id$="-next_week"], [id$="-next_day"]');
        if (nextBtn) nextBtn.click();
        break;

      case 't':
      case 'T':
        // Jump to today
        const todayBtn = document.querySelector('[id$="-today"]');
        if (todayBtn) todayBtn.click();
        break;

      case 'c':
      case 'C':
        // Toggle chat
        const chatPanel = document.getElementById('chat_container');
        if (chatPanel) {
          chatPanel.classList.toggle('expanded');
          chatPanel.classList.toggle('collapsed');
        }
        break;

      case 'Escape':
        // Close chat if open
        const openChat = document.querySelector('.chat-sidebar.expanded');
        if (openChat) {
          openChat.classList.remove('expanded');
          openChat.classList.add('collapsed');
        }
        break;
    }
  });

  // =========================================================================
  // Auto-refresh Indicator
  // =========================================================================

  const showRefreshIndicator = function() {
    let indicator = document.getElementById('refresh-indicator');

    if (!indicator) {
      indicator = document.createElement('div');
      indicator.id = 'refresh-indicator';
      indicator.className = 'refresh-indicator';

      // Build indicator content safely using DOM methods
      const spinner = document.createElement('span');
      spinner.className = 'spinner-border spinner-border-sm';

      const text = document.createTextNode(' Refreshing...');

      indicator.appendChild(spinner);
      indicator.appendChild(text);
      document.body.appendChild(indicator);
    }

    indicator.classList.add('visible');
    setTimeout(function() {
      indicator.classList.remove('visible');
    }, 1000);
  };

  // Listen for Shiny refresh events
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('refresh-indicator', function(message) {
      showRefreshIndicator();
    });
  }

  // =========================================================================
  // Visibility API - Pause/Resume Refresh
  // =========================================================================

  document.addEventListener('visibilitychange', function() {
    if (document.visibilityState === 'visible') {
      // Trigger refresh when page becomes visible
      const refreshBtn = document.getElementById('refresh_calendar');
      if (refreshBtn) {
        // Small delay to let the page settle
        setTimeout(function() {
          refreshBtn.click();
        }, 500);
      }
    }
  });

  // =========================================================================
  // Browser Geolocation for Weather Widget
  // =========================================================================

  const LOCATION_CACHE_KEY = 'skylight_user_location';
  const LOCATION_CACHE_DURATION = 24 * 60 * 60 * 1000; // 24 hours

  const getCachedLocation = function() {
    try {
      const cached = localStorage.getItem(LOCATION_CACHE_KEY);
      if (!cached) return null;

      const data = JSON.parse(cached);
      const now = Date.now();

      // Check if cache is still valid
      if (data.timestamp && (now - data.timestamp) < LOCATION_CACHE_DURATION) {
        return data;
      }

      // Cache expired
      localStorage.removeItem(LOCATION_CACHE_KEY);
      return null;
    } catch (e) {
      return null;
    }
  };

  const cacheLocation = function(lat, lon, source) {
    try {
      localStorage.setItem(LOCATION_CACHE_KEY, JSON.stringify({
        lat: lat,
        lon: lon,
        source: source,
        timestamp: Date.now()
      }));
    } catch (e) {
      console.warn('Failed to cache location:', e);
    }
  };

  const sendLocationToShiny = function(lat, lon, source) {
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('browser_geolocation', {
        lat: lat,
        lon: lon,
        source: source,
        timestamp: Date.now()
      });
    }
  };

  const requestGeolocation = function() {
    // First check cache
    const cached = getCachedLocation();
    if (cached) {
      sendLocationToShiny(cached.lat, cached.lon, 'cache');
      return;
    }

    // Check if geolocation is supported
    if (!navigator.geolocation) {
      console.log('Geolocation not supported');
      sendLocationToShiny(null, null, 'unsupported');
      return;
    }

    // Request location
    navigator.geolocation.getCurrentPosition(
      function(position) {
        const lat = position.coords.latitude;
        const lon = position.coords.longitude;

        // Cache the location
        cacheLocation(lat, lon, 'browser');

        // Send to Shiny
        sendLocationToShiny(lat, lon, 'browser');
      },
      function(error) {
        console.log('Geolocation error:', error.message);
        sendLocationToShiny(null, null, 'denied');
      },
      {
        enableHighAccuracy: false,
        timeout: 10000,
        maximumAge: 300000 // 5 minutes
      }
    );
  };

  // Request geolocation when Shiny is ready
  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:connected', function() {
      // Small delay to let the app initialize
      setTimeout(requestGeolocation, 1000);
    });
  }

  // Listen for location refresh requests from Shiny
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('refresh-geolocation', function(message) {
      // Clear cache and re-request
      localStorage.removeItem(LOCATION_CACHE_KEY);
      requestGeolocation();
    });
  }

  // =========================================================================
  // Service Worker Registration (PWA)
  // =========================================================================

  if ('serviceWorker' in navigator) {
    window.addEventListener('load', function() {
      navigator.serviceWorker.register('/www/sw.js')
        .then(function(registration) {
          console.log('[Skylight] Service Worker registered');

          // Check for updates periodically
          setInterval(function() {
            registration.update();
          }, 60 * 60 * 1000); // Check every hour
        })
        .catch(function(err) {
          console.log('[Skylight] Service Worker registration failed:', err);
        });
    });
  }

  // =========================================================================
  // Remove Hamburger Toggler (Aggressive - MutationObserver)
  // =========================================================================

  const removeNavbarToggler = function() {
    // Remove ALL navbar togglers - both Bootstrap 3/4 (.navbar-toggle) and 5 (.navbar-toggler)
    const togglers = document.querySelectorAll('.navbar-toggler, .navbar-toggle');
    togglers.forEach(function(toggler) {
      toggler.remove();
    });
  };

  // Watch for dynamically added togglers and remove them immediately
  const observeAndRemoveTogglers = function() {
    const observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        mutation.addedNodes.forEach(function(node) {
          if (node.nodeType === 1) { // Element node
            // Check both Bootstrap 3/4 and Bootstrap 5 class names
            if (node.classList && (node.classList.contains('navbar-toggler') || node.classList.contains('navbar-toggle'))) {
              node.remove();
            }
            // Also check children
            const childTogglers = node.querySelectorAll ? node.querySelectorAll('.navbar-toggler, .navbar-toggle') : [];
            childTogglers.forEach(function(toggler) {
              toggler.remove();
            });
          }
        });
      });
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    // Initial removal
    removeNavbarToggler();
  };

  // =========================================================================
  // Event Details Modal Handler
  // =========================================================================

  // Track current event for countdown toggle
  let currentModalEvent = null;

  const handleEventCardClick = function(e) {
    const eventCard = e.target.closest('[data-event-id]');
    if (!eventCard) return;

    const eventData = eventCard.getAttribute('data-event');
    if (!eventData) return;

    try {
      const event = JSON.parse(eventData);
      showEventModal(event);
    } catch (err) {
      console.error('Failed to parse event data:', err);
    }
  };

  // Toggle countdown for current event
  window.toggleEventCountdown = function() {
    if (!currentModalEvent) return;

    const toggleBtn = document.getElementById('event-countdown-toggle');
    const isCurrentlyCountdown = toggleBtn && toggleBtn.classList.contains('is-countdown');

    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('toggle_event_countdown', {
        event_id: currentModalEvent.id,
        title: currentModalEvent.title,
        target_date: currentModalEvent.start.split('T')[0], // Extract date portion
        action: isCurrentlyCountdown ? 'remove' : 'add',
        nonce: Math.random()
      });

      // Optimistically update button state
      if (isCurrentlyCountdown) {
        toggleBtn.classList.remove('is-countdown', 'btn-primary');
        toggleBtn.classList.add('btn-outline-primary');
        toggleBtn.querySelector('.countdown-icon-add').classList.remove('d-none');
        toggleBtn.querySelector('.countdown-icon-remove').classList.add('d-none');
      } else {
        toggleBtn.classList.add('is-countdown', 'btn-primary');
        toggleBtn.classList.remove('btn-outline-primary');
        toggleBtn.querySelector('.countdown-icon-add').classList.add('d-none');
        toggleBtn.querySelector('.countdown-icon-remove').classList.remove('d-none');
      }
    }
  };

  const showEventModal = function(event) {
    // Store current event for countdown toggle
    currentModalEvent = event;
    // Format date/time for display
    const startDate = new Date(event.start);
    const endDate = new Date(event.end);

    const dateOptions = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
    const timeOptions = { hour: 'numeric', minute: '2-digit' };

    const dateStr = startDate.toLocaleDateString('en-US', dateOptions);
    const startTimeStr = startDate.toLocaleTimeString('en-US', timeOptions);
    const endTimeStr = endDate.toLocaleTimeString('en-US', timeOptions);

    const timeDisplay = event.all_day
      ? 'All Day'
      : startTimeStr + ' - ' + endTimeStr;

    // Build modal content
    const modal = document.getElementById('event-detail-modal');
    if (!modal) {
      console.error('Event detail modal not found');
      return;
    }

    // Update modal content
    const titleEl = modal.querySelector('.event-modal-title');
    const dateEl = modal.querySelector('.event-modal-date');
    const timeEl = modal.querySelector('.event-modal-time');
    const locationEl = modal.querySelector('.event-modal-location');
    const locationRow = modal.querySelector('.event-modal-location-row');
    const descriptionEl = modal.querySelector('.event-modal-description');
    const descriptionRow = modal.querySelector('.event-modal-description-row');
    const calendarEl = modal.querySelector('.event-modal-calendar');
    const colorIndicator = modal.querySelector('.event-modal-color');

    if (titleEl) titleEl.textContent = event.title;
    if (dateEl) dateEl.textContent = dateStr;
    if (timeEl) timeEl.textContent = timeDisplay;
    if (colorIndicator) colorIndicator.style.backgroundColor = event.color;
    if (calendarEl) calendarEl.textContent = event.calendar_name || 'Calendar';

    // Show/hide location
    if (locationRow) {
      if (event.location && event.location.trim()) {
        locationEl.textContent = event.location;
        locationRow.style.display = '';
      } else {
        locationRow.style.display = 'none';
      }
    }

    // Show/hide description
    if (descriptionRow) {
      if (event.description && event.description.trim()) {
        descriptionEl.textContent = event.description;
        descriptionRow.style.display = '';
      } else {
        descriptionRow.style.display = 'none';
      }
    }

    // Update countdown button state
    const countdownBtn = document.getElementById('event-countdown-toggle');
    if (countdownBtn) {
      // Check if this event is a countdown via Shiny
      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('check_event_countdown', {
          event_id: event.id,
          nonce: Math.random()
        });
      }
      // Reset to default state initially (will be updated by Shiny callback)
      countdownBtn.classList.remove('is-countdown', 'btn-primary');
      countdownBtn.classList.add('btn-outline-primary');
      countdownBtn.querySelector('.countdown-icon-add').classList.remove('d-none');
      countdownBtn.querySelector('.countdown-icon-remove').classList.add('d-none');
    }

    // Show the modal using Bootstrap
    const bsModal = new bootstrap.Modal(modal);
    bsModal.show();
  };

  // Listen for countdown status updates from Shiny
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('update-countdown-button', function(data) {
      const countdownBtn = document.getElementById('event-countdown-toggle');
      if (!countdownBtn) return;

      if (data.is_countdown) {
        countdownBtn.classList.add('is-countdown', 'btn-primary');
        countdownBtn.classList.remove('btn-outline-primary');
        countdownBtn.querySelector('.countdown-icon-add').classList.add('d-none');
        countdownBtn.querySelector('.countdown-icon-remove').classList.remove('d-none');
      } else {
        countdownBtn.classList.remove('is-countdown', 'btn-primary');
        countdownBtn.classList.add('btn-outline-primary');
        countdownBtn.querySelector('.countdown-icon-add').classList.remove('d-none');
        countdownBtn.querySelector('.countdown-icon-remove').classList.add('d-none');
      }
    });
  }

  // Event delegation for event cards
  document.addEventListener('click', handleEventCardClick);

  // Also handle keyboard activation (Enter/Space)
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Enter' || e.key === ' ') {
      const eventCard = e.target.closest('[data-event-id]');
      if (eventCard) {
        e.preventDefault();
        handleEventCardClick(e);
      }
    }
  });

  // =========================================================================
  // Quick Add Event - Day Click Handler
  // =========================================================================

  const handleDayClick = function(e) {
    // Don't trigger if clicking on an event card
    if (e.target.closest('[data-event-id]')) return;

    // Find the day column or day cell
    const dayElement = e.target.closest('.day-column, .month-day, .day-cell');
    if (!dayElement) return;

    // Extract date from the element
    const dateAttr = dayElement.getAttribute('data-date');
    if (!dateAttr) return;

    // Trigger the quick add modal via Shiny
    if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
      Shiny.setInputValue('quick_add-quick_add_trigger', {
        date: dateAttr,
        timestamp: Date.now()
      });
    }
  };

  // Double-click to add event (more intentional than single click)
  document.addEventListener('dblclick', handleDayClick);

  // Also support a dedicated add button if present
  document.addEventListener('click', function(e) {
    const addBtn = e.target.closest('.quick-add-btn');
    if (addBtn) {
      e.stopPropagation();
      const dayElement = addBtn.closest('.day-column, .month-day, .day-cell');
      if (dayElement) {
        const dateAttr = dayElement.getAttribute('data-date');
        if (dateAttr && typeof Shiny !== 'undefined') {
          Shiny.setInputValue('quick_add-quick_add_trigger', {
            date: dateAttr,
            timestamp: Date.now()
          });
        }
      }
    }
  });

  // =========================================================================
  // Initialize on DOM Ready
  // =========================================================================

  const initSkylight = function() {
    observeChatMessages();
    observeAndRemoveTogglers();
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initSkylight);
  } else {
    initSkylight();
  }

  // Note: MutationObserver handles dynamic toggler additions.
  // No setInterval needed - it would waste CPU cycles.

})();
