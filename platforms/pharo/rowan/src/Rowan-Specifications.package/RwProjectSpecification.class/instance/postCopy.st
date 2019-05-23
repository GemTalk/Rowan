copying
postCopy
  "don't forget to copy any compound state objects"

  | oldPlatformSpec |
  super postCopy.
  oldPlatformSpec := platformSpec.
  platformSpec := Dictionary new.
  oldPlatformSpec
    keysAndValuesDo: [ :key :value | platformSpec at: key put: value copy ].
  repoSpec := repoSpec copy.
  imageSpec := imageSpec copy