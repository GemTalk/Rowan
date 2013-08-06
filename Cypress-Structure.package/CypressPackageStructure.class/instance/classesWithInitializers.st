"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
classesWithInitializers

	^self classesInFileInOrder
		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]