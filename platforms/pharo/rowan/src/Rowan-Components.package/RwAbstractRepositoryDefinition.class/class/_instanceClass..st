private
_instanceClass: defaultInstanceClass
	"default class only needed when receiver is the abstract class"

	self == RwAbstractRepositoryDefinition
		ifTrue: [ ^ defaultInstanceClass ].
	^ self