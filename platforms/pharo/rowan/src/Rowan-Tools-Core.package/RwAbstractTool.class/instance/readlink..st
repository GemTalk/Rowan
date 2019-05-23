bash utilities
readlink: filepath

	"resolve (possible) symbolic links in filepath and return an absolute path"
	"NOTE: may need alternate solution on OSX"

	| command |
	command := self _sh_realpath_source, '; realpath ' , filepath.
	^Rowan gitTools performOnServer: command logging: true