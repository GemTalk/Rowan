private
_repositoryConventionFor: packagesRoot

	^ (self _repositoryPropertyDictFor: packagesRoot) at: #convention ifAbsent: [ 'RowanHybrid'  ]