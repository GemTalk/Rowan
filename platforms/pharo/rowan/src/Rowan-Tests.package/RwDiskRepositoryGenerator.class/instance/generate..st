generator
generate: myLoadSpecification

	| myPackageSet |
	myLoadSpecification repositoryUrl
		ifNil: [ 
			| url |
			url := myLoadSpecification projectUrl asRwUrl.
			url scheme = 'file'
				ifFalse: [ 
					self
						error:
							'if ' , myLoadSpecification specName printString
								,
									' has not been attached to a cloned repository (repositoryUrl nonNil), then the projectUrl must be a file: url: '
								, url printString ] ].
	myPackageSet := self generatePackageSet.
	Rowan projectTools write
		writePackageSet: myPackageSet
		specification: myLoadSpecification