#!/bin/sh

# This ensures that each exercise's stack resolver has the same version.

differing_stack=""
expected_stack=$(grep 'RESOLVER.*CURRENT' .travis.yml | head -1 | cut -d'"' -f2)
echo "All exercises should have resolver $expected_stack"
for exercise in ${TRAVIS_BUILD_DIR}/exercises/*/ ; do
    # This might allow lts-xyz for expected_stack=x.z, but hopefully `stack` fails in that case!
    # Not a mistake we expect people to make
    if grep -v "^resolver: ${expected_stack}\$" $exercise/stack.yaml; then
      differing_stack="$differing_stack $(basename "$exercise")"
    fi
done
if [ -n "$differing_stack" ]; then
    echo "The following exercises have a different stack.yaml resolver:$differing_stack"
    echo "They should instead be $expected_stack"
    exit 1
fi

