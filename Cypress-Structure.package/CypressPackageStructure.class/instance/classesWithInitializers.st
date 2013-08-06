accessing
classesWithInitializers

	^self classesInFileInOrder
		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]