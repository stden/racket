#!/bin/bash

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║           RACKET MULTITHREADING TEST SUITE                   ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_DIR="$SCRIPT_DIR"

# Find racket executable (try common locations)
# Path from tests/ -> thread/ -> src/ -> racket/ -> bin/racket
RACKET_RELATIVE="$SCRIPT_DIR/../../../bin/racket"

if [ -x "$RACKET_RELATIVE" ]; then
    RACKET="$RACKET_RELATIVE"
elif command -v racket &> /dev/null; then
    RACKET=racket
else
    echo "Error: racket not found. Please add racket to PATH or build Racket first."
    exit 1
fi

PASSED=0
FAILED=0

run_test() {
    local name=$1
    local file=$2
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "▶ $name"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    timeout 120 $RACKET "$TESTS_DIR/$file" 2>&1
    
    if [ $? -eq 0 ]; then
        echo "✅ PASSED: $name"
        ((PASSED++))
    else
        echo "❌ FAILED: $name"
        ((FAILED++))
    fi
    echo ""
}

START_TIME=$(date +%s)

# Run all tests
run_test "Basic Thread Tests" "thread_tests.rkt"
run_test "Thread Stress Tests" "thread_stress_tests.rkt"
run_test "Hardcore Benchmarks" "hardcore_benchmarks.rkt"
run_test "Extended Benchmarks" "extended_benchmarks.rkt"
run_test "Ultra Benchmarks" "ultra_benchmarks.rkt"
run_test "Mega Benchmarks" "mega_benchmarks.rkt"
run_test "Multicore Proof" "multicore_proof.rkt"
run_test "Detailed Profile" "detailed_profile.rkt"

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║                    TEST RESULTS SUMMARY                      ║"
echo "╠══════════════════════════════════════════════════════════════╣"
printf "║  ✅ Passed: %-48s ║\n" "$PASSED"
printf "║  ❌ Failed: %-48s ║\n" "$FAILED"
printf "║  ⏱  Time:   %-48s ║\n" "${ELAPSED}s"
echo "╚══════════════════════════════════════════════════════════════╝"
