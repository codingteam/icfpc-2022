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

if [ $# -eq 0 ]; then
    set -- $(seq 1 30)
fi

for problem_no in "$@"
do
    problem="problems/${problem_no}.png"
    config="problems/${problem_no}.initial.json"
    solution="solutions/${problem_no}.isl"
    comparison="solutions/${problem_no}.png"
    echo "Generating a comparison image for problem ${problem_no}..."
    [ -f "$config" ] || config=""
    stack run -- evaluateSolution "$problem" "$solution" "$comparison" $config
done
