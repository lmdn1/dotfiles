#!/bin/bash
current_workspace=$(
  i3-msg -t get_workspaces | jq -r '.[] | select(.focused).name'
)

new_name=$(
  rofi -dmenu -p "Rename workspace (${current_workspace}) to"
)

[ -z "${new_name}" ] && {
  exit 0
}

i3-msg "rename workspace \"${current_workspace}\" to \"${current_workspace}: ${new_name}\""
