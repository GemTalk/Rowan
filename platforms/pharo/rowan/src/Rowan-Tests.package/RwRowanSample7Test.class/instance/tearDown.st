running
tearDown

	self _repositoryRoot ifNotNil: [:root | root ensureDelete ].
	super tearDown