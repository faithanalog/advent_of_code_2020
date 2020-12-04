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

            hgt_val = hgt_unit = ""
            if (pass["hgt"] ~ /^([0-9]+)(cm|in)$/) {
                hgt_val = substr(pass["hgt"], 1, length(pass["hgt"]) - 2)
                hgt_unit = substr(pass["hgt"], length(pass["hgt"]) - 1)
            }
            valid = valid && (hgt_unit == "cm" || hgt_unit == "in")
            if (hgt_unit == "cm") {
                valid = valid && hgt_val >= 150 && hgt_val <= 193
            }
            if (hgt_unit == "in") {
                valid = valid && hgt_val >= 59 && hgt_val <= 76
            }

            valid = valid && pass["hcl"] ~ /^#[0-9a-f]{6}$/
            valid = valid && pass["ecl"] ~ /^(amb|blu|brn|gry|grn|hzl|oth)$/
            valid = valid && pass["pid"] ~ /^[0-9]{9}$/

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