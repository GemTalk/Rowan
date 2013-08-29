Accessing - digests
repositoryDigests

	^self repositories collect: [:each | self digestFor: each url]