"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
updatePackage: aPackage withSnapshot: aSnapshot

	| patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]
