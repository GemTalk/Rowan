private
_repositoryFormatFor: packagesRoot

	^ (self _repositoryPropertyDictFor: packagesRoot) at: #format ifAbsent: [ self defaultPackageFormat ]