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
  // Service Worker Registration (PWA) - Disabled until sw.js is created
  // =========================================================================

  // if ('serviceWorker' in navigator) {
  //   window.addEventListener('load', function() {
  //     navigator.serviceWorker.register('/www/sw.js')
  //       .then(function(registration) {
  //         console.log('ServiceWorker registration successful');
  //       })
  //       .catch(function(err) {
  //         console.log('ServiceWorker registration failed:', err);
  //       });
  //   });
  // }

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

  const showEventModal = function(event) {
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

    // Show the modal using Bootstrap
    const bsModal = new bootstrap.Modal(modal);
    bsModal.show();
  };

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
