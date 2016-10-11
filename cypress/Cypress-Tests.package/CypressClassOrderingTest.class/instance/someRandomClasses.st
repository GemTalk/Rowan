"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: support
"
someRandomClasses

	| cutoff random someClasses randomlyOrderedClasses |
	cutoff := 0.5.
	random := HostRandom new.
	someClasses := Globals
				select: [:aGlobal | aGlobal isBehavior and: [random float > cutoff]].
	randomlyOrderedClasses := Dictionary new: someClasses size.
	someClasses
		do: [:aClass | randomlyOrderedClasses at: random integer put: aClass].
	^randomlyOrderedClasses