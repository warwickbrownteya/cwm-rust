#!/bin/bash
# Compare cwm-rust output with EYE reasoner

CWM=/Users/nonroot/src/localhost/cwm-rust/target/release/cwm
EYE=/Users/nonroot/.local/bin/eye

TESTS_DIR="$(dirname "$0")/n3"
PASS=0
FAIL=0

echo "========================================"
echo "CWM-Rust vs EYE Comparison Tests"
echo "========================================"
echo ""

for test_file in "$TESTS_DIR"/*.n3; do
    test_name=$(basename "$test_file")
    echo "--- Test: $test_name ---"

    # Run cwm-rust
    echo "Running cwm-rust..."
    cwm_output=$("$CWM" "$test_file" --think --filter 2>&1)
    cwm_status=$?

    # Run EYE
    echo "Running EYE..."
    eye_output=$("$EYE" --nope --quiet "$test_file" 2>&1)
    eye_status=$?

    # Count inferred triples (rough comparison)
    cwm_lines=$(echo "$cwm_output" | grep -v "^@prefix" | grep -v "^$" | wc -l)
    eye_lines=$(echo "$eye_output" | grep -v "^@prefix" | grep -v "^$" | wc -l)

    echo "  cwm-rust: exit=$cwm_status, output lines=$cwm_lines"
    echo "  EYE:      exit=$eye_status, output lines=$eye_lines"

    if [ $cwm_status -eq 0 ]; then
        echo "  Status: PASS (cwm-rust parsed successfully)"
        ((PASS++))
    else
        echo "  Status: FAIL (cwm-rust error)"
        ((FAIL++))
    fi
    echo ""
done

echo "========================================"
echo "Summary: $PASS passed, $FAIL failed"
echo "========================================"
