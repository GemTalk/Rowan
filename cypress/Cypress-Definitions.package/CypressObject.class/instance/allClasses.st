"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
allClasses

	| classes |
	classes := Array new.
	System myUserProfile symbolList
		do: [:dict | classes addAll: (dict select: [:each | each isBehavior])].
	^classes