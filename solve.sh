#!/bin/sh

set -e

tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT

get_solution_cost() {
    local problem=$1 solution=$2 cfg=$3
    [ -f "$cfg" ] || cfg=

    "$icfpc2022exe" evaluateSolution "$problem" "$solution" "" $cfg
}

info() {
    if [ "$2" ]
    then
        printf "    %-20s %s\n" "$1:" "$2"
    else
        printf "    %-20s %s\n" "$1"
    fi
}

run_solver() {
    local name=$1
    shift
    local isl=$(mktemp -p "$tmp")
    $icfpc2022exe "$@" > "$isl"
    local cost=$(get_solution_cost "$problem" "$isl" "$initial_config")
    info "$name" "$cost"
    if [ "$cost" -lt "$best_cost" ]
    then
        cp -vf "$isl" "$existing_isl"
        best_cost=$cost
    fi

}

stack build
icfpc2022exe=$(stack path --local-install-root)/bin/icfpc2022-exe

for problem_no in `seq 1 30`
do
    problem="problems/${problem_no}.png"

    echo "Solving problem #$problem_no"

    existing_isl="solutions/${problem_no}.isl"
    initial_config="problems/${problem_no}.initial.json"
    existing_cost=$(get_solution_cost "$problem" "$existing_isl" "$initial_config")
    info "existing solution" "$existing_cost"

    best_cost=$existing_cost

    if ! [ -f "$initial_config" ]
    then
        # run_solver "default solver"     "$problem"
        # run_solver "spiral sovler"      spiral "$problem"
        run_solver   "average solver"     average "$problem"
        run_solver   "average4 solver"    average4 "$problem"
        # run_solver "search4 solver"     search4 "$problem"
        info         "quads solver"
        for level in $(seq 5); do
            run_solver "    level $level" quads $level "$problem"
        done
        info         "quads+merge solver"
        for level in $(seq 5); do
            run_solver "    level $level" quadsMerge $level "$problem"
        done
        info         "quads_reset solver"
        for level in $(seq 5); do
            run_solver "    level $level" quads-reset $level "$problem"
        done
        run_solver   "recursive solver"   recursive "$problem"
        run_solver   "billboard solver"   billboard "$problem"
    else
        run_solver  "dumb solver"         dumbFromInitial "$initial_config" "$problem"
        run_solver  "merge solver"        mergeFromInitial "$initial_config" "$problem"
    fi
done
