# Gardian of Memory

It is said that ulimit does not work on memory limitation any more. And Linux goes crazy when memory exhausted. So made this tool.

The logic is pretty simple, scan current user's process every second. If RSS excceeds soft limit, sends libnotify notification. If RSS excceeds hard limit, sends notification and kill it. No termination yet.

# TODO

Support configurations, per command limitations.
