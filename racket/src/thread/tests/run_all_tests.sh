#!/bin/bash

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║           RACKET MULTITHREADING TEST SUITE                   ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

RACKET=/srv/racket/racket/bin/racket
TESTS_DIR=/srv/racket/racket/src/thread/tests
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
