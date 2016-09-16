"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Updating
"
removeRepository: aRepository

	self repositories removeKey: aRepository url ifAbsent: [].
	self digests removeKey: aRepository url ifAbsent: [].
	self updateDigestsFromImage.
