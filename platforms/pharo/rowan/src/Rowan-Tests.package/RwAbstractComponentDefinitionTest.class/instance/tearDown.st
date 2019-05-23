running
tearDown

	repositoryRoot ifNotNil: [ repositoryRoot ensureDelete ].
	super tearDown