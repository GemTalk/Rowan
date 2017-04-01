"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
createClassesFromWorklist: workList andClassesByName: newClassesByName
	"Pick one class or class version from the workList and create it, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| className patch superclassName |
	className := self anyElementOfCollection: workList.
	className ifNil: [self error: 'Empty WorkList.'].
	self
		createClassNamed: className
		fromWorkList: workList
		andClassesByName: newClassesByName