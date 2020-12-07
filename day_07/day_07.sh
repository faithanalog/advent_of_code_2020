#!/bin/sh

# Specialized to gawk because some awks implement local arrays statically, which breaks recursion.
# Given i'm using gawk anyway, i also use the gnu extension that adds length() for arrays
cat input.txt \
    | sed '
        s/ bags\?//g
        s/,//g
        s/\.$//
        s/ contain//
        s/no other/NONE/
    ' \
    | gawk '
        $3 != "NONE" {
            parent = $1 $2

            for (i = 3; i < NF; i += 3) {
                parents[$(i + 1) $(i + 2)] = parents[$(i + 1) $(i + 2)] "," parent
                children[parent] = children[parent] "," $i " " $(i + 1) $(i + 2)
            }
        }

        # Doesnt actually return anything, but sets keys in target_parents to indicate parent
        # presence, essentially using it as a set
        function get_parents_of(target,         parents_of) {
            # substr to chop off leading comma, then split on comma
            split(substr(parents[target], 2), parents_of, /,/)
            for (i in parents_of) {
                bag = parents_of[i]
                target_parents[bag] = "1"
                get_parents_of(bag)
            }
        }

        function count_children_of(target,        children_of, val_parts, n) {
            # substr to chop off leading comma, then split on comma
            split(substr(children[target], 2), children_of, /,/)

            n = 0
            for (i in children_of) {
                split(children_of[i], val_parts, / /)
                count = val_parts[1]
                bag = val_parts[2]
                n += count + count * count_children_of(bag)
            }
            return n
        }

        END {
            get_parents_of("shiny" "gold")
            print "Part 1: " length(target_parents)
            print "Part 2: " count_children_of("shiny" "gold")
        }
    '