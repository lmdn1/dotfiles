#!/bin/bash

case "${1}" in
lock)
  i3lock -c 008080
  ;;
suspend)
  systemctl suspend
  ;;
hibernate)
  systemctl hibernate
  ;;
reboot)
  systemctl reboot
  ;;
shutdown)
  systemctl shutdown
  ;;
*)
  echo "Usage: ${0} {lock|logout|suspend|hibernate|reboot|shutdown}"
  exit 2
  ;;
esac

exit 0
