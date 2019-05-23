generator
generateForUrl: specNameOrUrl

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme = 'file'
		ifFalse: [ 
			self
				error:
					'Must use a file: url for spec, since generator will write to the disk repository' ].
	^ self generate: (RwSpecification fromUrl: url)