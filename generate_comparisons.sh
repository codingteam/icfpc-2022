#!/bin/sh

set -e

tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT

get_solution_cost() {
    problem=$1
    solution=$2

    stack run -- evaluateSolution $problem $solution
}

stack build

for problem_no in `seq 1 25`
do
    problem="problems/${problem_no}.png"
    solution="solutions/${problem_no}.isl"
    comparison="solutions/${problem_no}.png"
    echo "Generating a comparison image for problem ${problem_no}..."
    stack run -- evaluateSolution "$problem" "$solution" "$comparison"
done
