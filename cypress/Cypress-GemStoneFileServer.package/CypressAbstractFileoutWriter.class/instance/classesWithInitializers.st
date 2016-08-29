"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
classesWithInitializers

	^self classesInDependencyOrder
		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]