Handling
methodErrorDo: methodBlock classErrorDo: classBlock
	"helper method for use in exception handling block ... avoid isKindOf:"

	self subclassResponsibility: #methodErrorDo:classErrorDo: