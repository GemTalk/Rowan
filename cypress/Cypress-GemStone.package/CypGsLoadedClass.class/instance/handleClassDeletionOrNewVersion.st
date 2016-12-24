"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
handleClassDeletionOrNewVersion
	"The class is deleted if both:
	* its name no longer resolves to a class in the classHistory of the class I remember.
	* no class in the classHistory of the class I remember is accessible under its name.
	If my name no longer resolves to my class, but another class in its classHistory does resolve,
	consider it a class re-version or rename."

	| resolved thoseResolving |
	resolved := self resolveClassNamed: name.
	resolved == handle ifTrue: [^self].
	"Has been deleted, renamed, or a new version under the same name. But which?"
	thoseResolving := handle classHistory reverse
				select: [:aClass | aClass == self resolveClassNamed: aClass name].
	thoseResolving size = 0 ifTrue: [^self handleClassDeletion].
	thoseResolving size > 1
		ifTrue: 
			[Notification
				signal: 'Class history with multiple "current" members found. One associated name is '
						, name].
	self handleClassUpdateToVersion: thoseResolving first