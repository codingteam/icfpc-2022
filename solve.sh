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

    echo "Solving problem #$problem_no"

    existing_isl="solutions/${problem_no}.isl"
    existing_cost=$(get_solution_cost $problem $existing_isl)
    echo "\texisting solution:\t$existing_cost"

    best_cost=$existing_cost

    #default_isl=$(mktemp -p "$tmp")
    #stack run -- "$problem" > "$default_isl"
    #default_cost=$(get_solution_cost $problem $default_isl)
    #echo "\tdefault solver:\t$default_cost"
    #if [ $default_cost -lt $best_cost ]
    #then
    #    cp -vf $default_isl $existing_isl
    #    best_cost=$default_cost
    #fi

    #spiral_isl=$(mktemp -p "$tmp")
    #stack run -- spiral "$problem" > "$spiral_isl"
    #spiral_cost=$(get_solution_cost $problem $spiral_isl)
    #echo "\tspiral solver:\t$spiral_cost"
    #if [ $spiral_cost -lt $best_cost ]
    #then
    #    cp -vf $spiral_isl $existing_isl
    #    best_cost=$spiral_cost
    #fi

    average_isl=$(mktemp -p "$tmp")
    stack run -- average "$problem" > "$average_isl"
    average_cost=$(get_solution_cost $problem $average_isl)
    echo "\taverage solver:\t$average_cost"
    if [ $average_cost -lt $best_cost ]
    then
        cp -vf $average_isl $existing_isl
        best_cost=$average_cost
    fi

    average4_isl=$(mktemp -p "$tmp")
    stack run -- average4 "$problem" > "$average4_isl"
    average4_cost=$(get_solution_cost $problem $average4_isl)
    echo "\taverage4 solver:\t$average4_cost"
    if [ $average4_cost -lt $best_cost ]
    then
        cp -vf $average4_isl $existing_isl
        best_cost=$average4_cost
    fi

    #search4_isl=$(mktemp -p "$tmp")
    #stack run -- search4 "$problem" > "$search4_isl"
    #search4_cost=$(get_solution_cost $problem $search4_isl)
    #echo "\tsearch4 solver:\t$search4_cost"
    #if [ $search4_cost -lt $best_cost ]
    #then
    #    cp -vf $search4_isl $existing_isl
    #    best_cost=$search4_cost
    #fi

    echo '\tquads solver'
    for level in `seq 5`
    do
        quads_isl=$(mktemp -p "$tmp")
        stack run -- quads $level "$problem" > "$quads_isl"
        quads_cost=$(get_solution_cost $problem $quads_isl)
        echo "\t\tlevel $level:\t$quads_cost"
        if [ $quads_cost -lt $best_cost ]
        then
            cp -vf $quads_isl $existing_isl
            best_cost=$quads_cost
        fi
    done

    echo '\tquads_reset solver'
    for level in `seq 5`
    do
        quads_reset_isl=$(mktemp -p "$tmp")
        stack run -- quads-reset $level "$problem" > "$quads_reset_isl"
        quads_reset_cost=$(get_solution_cost $problem $quads_reset_isl)
        echo "\t\tlevel $level:\t$quads_reset_cost"
        if [ $quads_reset_cost -lt $best_cost ]
        then
            cp -vf $quads_reset_isl $existing_isl
            best_cost=$quads_reset_cost
        fi
    done

    recursive_isl=$(mktemp -p "$tmp")
    stack run -- recursive "$problem" > "$recursive_isl"
    recursive_cost=$(get_solution_cost $problem $recursive_isl)
    echo "\trecursive solver:\t$recursive_cost"
    if [ $recursive_cost -lt $best_cost ]
    then
        cp -vf $recursive_isl $existing_isl
        best_cost=$recursive_cost
    fi

    echo '\tbillboard solver'
    for tolerance in `seq 0 5`
    do
        billboard_isl=$(mktemp -p "$tmp")
        stack run -- billboard "$problem" "$tolerance" > "$billboard_isl"
        billboard_cost=$(get_solution_cost $problem $billboard_isl)
        echo "\t\ttolerance $tolerance:\t$billboard_cost"
        if [ $billboard_cost -lt $best_cost ]
        then
            cp -vf $billboard_isl $existing_isl
            best_cost=$billboard_cost
        fi
    done
done
