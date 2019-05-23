package reading
_packageNameFromPackageDir: packageDir

	"this is not really correct, but it works as a fallback (filetree does not have independent package name)"

	^ (self _readObjectFrom: (packageDir / 'package', 'st')) at: #name