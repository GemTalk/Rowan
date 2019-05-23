private
_sh_realpath_source

	"https://github.com/mkropat/sh-realpath/blob/master/realpath.sh"

	"all on one line because that's what our perform on server call wants - I think:)"

	^'realpath() { canonicalize_path "$(resolve_symlinks "$1")"; }; resolve_symlinks() { local dir_context path ; path=$(readlink -- "$1"); if [ $? -eq 0 ]; then dir_context=$(dirname -- "$1"); resolve_symlinks "$(_prepend_path_if_relative "$dir_context" "$path")"; else printf ''%s\n'' "$1"; fi; }; _prepend_path_if_relative() { case "$2" in /* ) printf ''%s\n'' "$2" ;; * ) printf ''%s\n'' "$1/$2" ;; esac; }; canonicalize_path() { if [ -d "$1" ]; then _canonicalize_dir_path "$1"; else _canonicalize_file_path "$1"; fi; }; _canonicalize_dir_path() { (cd "$1" 2>/dev/null && pwd -P); }; _canonicalize_file_path() { local dir file; dir=$(dirname -- "$1"); file=$(basename -- "$1"); (cd "$dir" 2>/dev/null && printf ''%s/%s\n'' "$(pwd -P)" "$file"); }'