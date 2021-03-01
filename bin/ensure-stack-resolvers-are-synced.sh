#!/bin/sh

# This ensures that each exercise's stack resolver has the same version.

differing_stack=""
first_stack_yaml=$(ls -1 exercises/*/stack.yaml | head -1)
expected_stack=$(yq read "$first_stack_yaml" resolver)
echo "All exercises should have resolver $expected_stack"
for exercise in $(git rev-parse --show-toplevel)/exercises/*/ ; do
    exercise_stack=$(yq read "$exercise/stack.yaml" resolver)
    if ! [ "$exercise_stack" = "$expected_stack" ]; then
      differing_stack="$differing_stack $(basename "$exercise")"
    fi
done
if [ -n "$differing_stack" ]; then
    echo "The following exercises have a different stack.yaml resolver:$differing_stack"
    echo "They should instead be $expected_stack"
    exit 1
fi
