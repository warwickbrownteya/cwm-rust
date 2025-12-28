#!/bin/bash
# Comprehensive test suite for cwm-rust

set -e

CWM="cargo run --quiet --"
TESTS_DIR="$(dirname "$0")"
PASSED=0
FAILED=0
SKIPPED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "================================="
echo "CWM-Rust Comprehensive Test Suite"
echo "================================="
echo ""

# Build first
echo "Building cwm-rust..."
cargo build --quiet

# Function to run a test
run_test() {
    local name="$1"
    local cmd="$2"
    local expected_pattern="$3"

    echo -n "Testing $name... "

    output=$(eval "$cmd" 2>&1) || true

    if echo "$output" | grep -q "$expected_pattern"; then
        echo -e "${GREEN}PASS${NC}"
        ((PASSED++))
    else
        echo -e "${RED}FAIL${NC}"
        echo "  Expected pattern: $expected_pattern"
        echo "  Output: $output" | head -5
        ((FAILED++))
    fi
}

# Function to run inference test
run_n3_test() {
    local file="$1"
    local description="$2"

    echo -n "Testing $description... "

    if $CWM "$file" --think > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        ((PASSED++))
    else
        echo -e "${RED}FAIL${NC}"
        ((FAILED++))
    fi
}

echo "=== N3 Reasoning Tests ==="

# Run all N3 test files
for file in "$TESTS_DIR"/n3/*.n3; do
    name=$(basename "$file" .n3)
    run_n3_test "$file" "$name"
done

echo ""
echo "=== SPARQL Tests ==="

# Test SPARQL SELECT
echo -n "Testing SPARQL SELECT... "
result=$($CWM "$TESTS_DIR/sparql/data.n3" --sparql "$TESTS_DIR/sparql/select-basic.rq" 2>&1)
if echo "$result" | grep -q "Alice Smith"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test SPARQL FILTER
echo -n "Testing SPARQL FILTER... "
result=$($CWM "$TESTS_DIR/sparql/data.n3" --sparql "$TESTS_DIR/sparql/select-filter.rq" 2>&1)
if echo "$result" | grep -q "Alice Smith" && echo "$result" | grep -q "Charlie Brown" && ! echo "$result" | grep -q "Bob Jones"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test SPARQL ASK
echo -n "Testing SPARQL ASK... "
result=$($CWM "$TESTS_DIR/sparql/data.n3" --sparql "$TESTS_DIR/sparql/ask.rq" 2>&1)
if echo "$result" | grep -q "true"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test SPARQL CONSTRUCT
echo -n "Testing SPARQL CONSTRUCT... "
result=$($CWM "$TESTS_DIR/sparql/data.n3" --sparql "$TESTS_DIR/sparql/construct.rq" 2>&1)
if echo "$result" | grep -q "hasLabel"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test SPARQL OPTIONAL
echo -n "Testing SPARQL OPTIONAL... "
result=$($CWM "$TESTS_DIR/sparql/data.n3" --sparql "$TESTS_DIR/sparql/optional.rq" 2>&1)
if echo "$result" | grep -q "David Lee"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

echo ""
echo "=== CLI Feature Tests ==="

# Test --diff
echo -n "Testing --diff... "
result=$($CWM "$TESTS_DIR/n3/02-simple-rule.n3" --think --diff 2>&1)
if echo "$result" | grep -q "addition" && echo "$result" | grep -q "Mortal"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test --filter
echo -n "Testing --filter... "
result=$($CWM "$TESTS_DIR/n3/02-simple-rule.n3" --think --filter 2>&1)
if echo "$result" | grep -q "Mortal" && ! echo "$result" | grep -q "Human"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test --purge-rules
echo -n "Testing --purge-rules... "
result=$($CWM "$TESTS_DIR/n3/02-simple-rule.n3" --purge-rules 2>&1)
if ! echo "$result" | grep -q "implies"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test output formats
echo -n "Testing --format ntriples... "
result=$($CWM "$TESTS_DIR/n3/01-basic-triples.n3" --format ntriples 2>&1)
if echo "$result" | grep -q "<http://"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

# Test JSON-LD output
echo -n "Testing --format jsonld... "
result=$($CWM "$TESTS_DIR/n3/01-basic-triples.n3" --format jsonld 2>&1)
if echo "$result" | grep -q "@context"; then
    echo -e "${GREEN}PASS${NC}"
    ((PASSED++))
else
    echo -e "${RED}FAIL${NC}"
    ((FAILED++))
fi

echo ""
echo "================================="
echo "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}, ${YELLOW}$SKIPPED skipped${NC}"
echo "================================="

# Exit with error if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi
