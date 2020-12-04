#!/bin/bash

awk -F '[ :]' '
    function validate() {
        has_keys = "byr" in pass &&
            "iyr" in pass &&
            "eyr" in pass &&
            "hgt" in pass &&
            "hcl" in pass &&
            "ecl" in pass &&
            "pid" in pass

        if (has_keys) {
            part1++

            valid = 1
            valid = valid && pass["byr"] >= 1920 && pass["byr"] <= 2002
            valid = valid && pass["iyr"] >= 2010 && pass["iyr"] <= 2020
            valid = valid && pass["eyr"] >= 2020 && pass["eyr"] <= 2030

            match(pass["hgt"], /^([0-9]+)(cm|in)$/, hgt)
            valid = valid && (hgt[2] == "cm" || hgt[2] == "in")
            if (hgt[2] == "cm") {
                valid = valid && hgt[1] >= 150 && hgt[1] <= 193
            }
            if (hgt[2] == "in") {
                valid = valid && hgt[1] >= 59 && hgt[1] <= 76
            }

            valid = valid && match(pass["hcl"], /^#[0-9a-f]{6}$/) > 0
            valid = valid && match(pass["ecl"], /^(amb|blu|brn|gry|grn|hzl|oth)$/) > 0
            valid = valid && match(pass["pid"], /^[0-9]{9}$/) > 0

            if (valid) {
                part2++
            }
        }

        delete pass
    }

    {
        for (i = 1; i < NF; i += 2) {
            pass[$i] = $(i + 1)
        }
    }

    /^$/ {
        validate()
    }

    END {
        validate()
        print "Part 1: " part1
        print "Part 2: " part2
    }
' < input.txt