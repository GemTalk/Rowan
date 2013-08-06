"
notice: This work is protected by copyright. All rights reserved.
category: initializing - private
"
initialize

	directoryPackageMap := Dictionary new.
	diskTimestamps := Dictionary new.
	diskSnapshots := Dictionary new.
	imageSnapshots := Dictionary new.
	snapshotDifferences := Dictionary new
		at: 'newer' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		at: 'older' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		yourself.
