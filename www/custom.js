// The native file input already covers each upload zone. A delegated click
// handler previously called the Bootstrap file button recursively and could
// lock the browser event loop. No JavaScript forwarding is required.
