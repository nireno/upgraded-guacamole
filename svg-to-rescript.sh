#!/bin/bash

# Function to display help message
display_help() {
  echo "Usage: $0 <svg_dir> <res_dir>"
  echo "  <svg_dir>  Directory containing SVG files"
  echo "  <res_dir>  Directory to output ReScript components"
  exit 1
}

# Read the paths from the command line
svg_dir="$1"
res_dir="$2"

# Check if svg_dir and res_dir are supplied
if [[ -z "$svg_dir" || -z "$res_dir" ]]; then
  echo "Error: Missing required arguments."
  display_help
fi

# Check if svg_dir is a valid directory
if [[ ! -d "$svg_dir" ]]; then
  echo "Error: svg_dir '$svg_dir' is not a valid directory."
  display_help
fi

# Check if res_dir is a valid directory
if [[ ! -d "$res_dir" ]]; then
  echo "Error: res_dir '$res_dir' is not a valid directory."
  display_help
fi

# Function to convert SVG file name to ReScript component name
convert_name() {
  local svg_name="$1"
  local base_name="${svg_name%.*}"
  echo "Svg_${base_name//-/_}.res"
}

# Function to convert kebab-case to camelCase
kebab_to_camel() {
  echo "$1" | sed -r 's/-([a-z])/\U\1/g'
}

# Function to update the SVG content
update_svg_content() {
  local svg_content="$1"
  local width height viewBox

  width=$(echo "$svg_content" | grep -oP '(?<=<svg[^>]*\s)width="\K[\d.]+' || echo "")
  height=$(echo "$svg_content" | grep -oP '(?<=<svg[^>]*\s)height="\K[\d.]+' || echo "")

  if [[ -n "$width" && -n "$height" ]]; then
    viewBox="viewBox=\"0 0 $width $height\""
    svg_content=$(echo "$svg_content" | sed -E "s/<svg/<svg $viewBox preserveAspectRatio=\"xMidYMid meet\"/")
  fi

  # Convert kebab-case properties to camelCase
  svg_content=$(echo "$svg_content" | sed -r 's/([a-zA-Z]+)-([a-zA-Z]+)/\1\U\2/g')

  echo "$svg_content"
}

# Function to create ReScript component content
create_rescript_component() {
  local svg_content="$1"
  local component_name="$2"
  echo "@react.component
let make = () => {
  $svg_content
}"
}

# Process each SVG file
for svg_file in "$svg_dir"/*.svg; do
  if [[ -f "$svg_file" ]]; then
    svg_content=$(<"$svg_file")
    component_name=$(convert_name "$(basename "$svg_file")")
    rescript_content=$(create_rescript_component "$svg_content" "$component_name")

    rescript_path="$res_dir/$component_name"
    echo "$rescript_content" > "$rescript_path"
  fi
done

echo "SVG to ReScript component transformation complete."