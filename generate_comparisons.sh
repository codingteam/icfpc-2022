#!/bin/sh

set -e

tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT

get_solution_cost() {
    problem=$1
    solution=$2

    "$icfpc2022exe" evaluateSolution $problem $solution
}

stack build
icfpc2022exe=$(stack path --local-install-root)/bin/icfpc2022-exe

if [ $# -eq 0 ]; then
    set -- $(seq 1 35)
fi

for problem_no in "$@"
do
    problem="problems/${problem_no}.png"
    config="problems/${problem_no}.initial.json"
    solution="solutions/${problem_no}.isl"
    comparison="solutions/${problem_no}.png"
    echo "Generating a comparison image for problem ${problem_no}..."
    [ -f "$config" ] || config=""
    "$icfpc2022exe" evaluateSolution "$problem" "$solution" "$comparison" $config
done
