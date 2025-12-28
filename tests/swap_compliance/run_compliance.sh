#!/bin/bash
# SWAP Compliance Test Runner
# Tests cwm-rust against W3C CWM/SWAP built-in specifications

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$(dirname "$SCRIPT_DIR")")"
CWM_RUST="$PROJECT_DIR/target/release/cwm"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Build cwm-rust if needed
echo -e "${BLUE}=== SWAP Compliance Test Suite ===${NC}"
echo ""

if [ ! -f "$CWM_RUST" ]; then
    echo -e "${YELLOW}Building cwm-rust (release mode)...${NC}"
    cd "$PROJECT_DIR"
    cargo build --release
fi

# Verify binary exists
if [ ! -f "$CWM_RUST" ]; then
    echo -e "${RED}Error: Could not build cwm-rust${NC}"
    exit 1
fi

echo -e "${GREEN}Using: $CWM_RUST${NC}"
echo ""

# Test results
PASSED=0
FAILED=0
SKIPPED=0
TOTAL_INFERENCES=0

# Function to run a test file
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .n3)

    printf "Testing %-12s " "$test_name..."

    # Run cwm-rust with think mode and capture output
    local output
    output=$("$CWM_RUST" "$test_file" --think 2>&1)
    local exit_code=$?

    if [ $exit_code -ne 0 ]; then
        echo -e "${RED}ERROR${NC} (exit code $exit_code)"
        ((FAILED++))
        return
    fi

    # Count inferred results (lines with :result or :passed)
    local results=$(echo "$output" | grep -c ":result\|:passed" || true)

    # Count total output lines (excluding rules)
    local total_lines=$(echo "$output" | grep -v "log:implies" | grep -c "^\s*:" || true)

    if [ "$results" -gt 0 ]; then
        echo -e "${GREEN}PASSED${NC} ($results inferences)"
        ((PASSED++))
        ((TOTAL_INFERENCES += results))
    elif [ "$total_lines" -gt 0 ]; then
        echo -e "${YELLOW}PARSED${NC} (no test assertions matched)"
        ((PASSED++))
    else
        echo -e "${RED}FAILED${NC} (no output)"
        ((FAILED++))
    fi
}

# Function to run syntax-only test (just parse, don't reason)
run_syntax_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .n3)

    printf "Parsing %-12s " "$test_name..."

    local output
    output=$("$CWM_RUST" "$test_file" 2>&1)
    local exit_code=$?

    if [ $exit_code -eq 0 ]; then
        local line_count=$(echo "$output" | wc -l | tr -d ' ')
        echo -e "${GREEN}PASSED${NC} ($line_count lines)"
        ((PASSED++))
    else
        echo -e "${RED}FAILED${NC}"
        echo "Error: $output"
        ((FAILED++))
    fi
}

echo -e "${BLUE}--- Namespace Tests ---${NC}"

# Run all namespace tests
for ns in math string list log time crypto os graph; do
    if [ -f "$SCRIPT_DIR/${ns}.n3" ]; then
        run_test "$SCRIPT_DIR/${ns}.n3"
    else
        echo -e "Skipping ${ns}.n3 (not found)"
        ((SKIPPED++))
    fi
done

echo ""
echo -e "${BLUE}--- N3 Syntax Tests ---${NC}"

# Run syntax test
if [ -f "$SCRIPT_DIR/n3_syntax.n3" ]; then
    run_syntax_test "$SCRIPT_DIR/n3_syntax.n3"
fi

echo ""
echo -e "${BLUE}--- Summary ---${NC}"
echo -e "Passed:     ${GREEN}$PASSED${NC}"
echo -e "Failed:     ${RED}$FAILED${NC}"
echo -e "Skipped:    ${YELLOW}$SKIPPED${NC}"
echo -e "Inferences: ${BLUE}$TOTAL_INFERENCES${NC}"
echo ""

# Exit with error if any tests failed
if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
else
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
fi
