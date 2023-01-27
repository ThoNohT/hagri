#!/usr/bin/env sh

if [ $1 = "release" ]; then
    # Builds executable and strips it.

    echo "Removing build cache..."
    rm -r ./dist-newstyle/
    rm ./hagri

    echo "Building project...\n"
    OUTPUT=`cabal build`
    echo $OUTPUT

    OUTPUT_FILE=`echo "${OUTPUT}" | tail -n1 | sed 's/Linking //' | sed 's/ ...//'`

    echo "\nStripping ${OUTPUT_FILE} to ./hagri...\n"
    strip -p --strip-unneeded --remove-section=.comment -o hagri "${OUTPUT_FILE}"

elif [ $1 = "build" ]; then
    # Build haskell project.
    cabal build

elif [ $1 = "run" ]; then
    # Run haskell project.
    cabal run

else
    echo "Invalid command."

fi

