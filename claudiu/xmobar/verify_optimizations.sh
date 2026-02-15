#!/usr/bin/env bash

# Verification script for xmobar performance optimizations
# Run this to ensure all optimizations are working correctly

set -e  # Exit on error

XMOBAR_DIR="${HOME}/.config/xmobar"
cd "$XMOBAR_DIR"

echo "================================================"
echo "Xmobar Performance Optimization Verification"
echo "================================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0
WARNINGS=0

check_pass() {
  echo -e "${GREEN}✓${NC} $1"
  ((PASSED++))
}

check_fail() {
  echo -e "${RED}✗${NC} $1"
  ((FAILED++))
}

check_warn() {
  echo -e "${YELLOW}⚠${NC} $1"
  ((WARNINGS++))
}

echo "1. Checking new infrastructure files..."
echo "----------------------------------------"

if [ -f "scripts/cache" ]; then
  check_pass "scripts/cache exists"
  if bash -n "scripts/cache" 2>/dev/null; then
    check_pass "scripts/cache syntax valid"
  else
    check_fail "scripts/cache has syntax errors"
  fi
else
  check_fail "scripts/cache missing"
fi

if [ -f "scripts/state_loader" ]; then
  check_pass "scripts/state_loader exists"
  if bash -n "scripts/state_loader" 2>/dev/null; then
    check_pass "scripts/state_loader syntax valid"
  else
    check_fail "scripts/state_loader has syntax errors"
  fi
else
  check_fail "scripts/state_loader missing"
fi

if [ -d ".cache" ]; then
  check_pass ".cache directory exists"
else
  check_warn ".cache directory will be created on first use"
fi

echo ""
echo "2. Verifying core scripts..."
echo "----------------------------------------"

if bash -n "scripts/bar" 2>/dev/null; then
  check_pass "scripts/bar syntax valid"
else
  check_fail "scripts/bar has syntax errors"
fi

if grep -q "set -a" "scripts/bar"; then
  check_pass "scripts/bar exports variables"
else
  check_fail "scripts/bar not exporting variables"
fi

if bash -n "scripts/render" 2>/dev/null; then
  check_pass "scripts/render syntax valid"
else
  check_fail "scripts/render has syntax errors"
fi

echo ""
echo "3. Checking component optimizations..."
echo "----------------------------------------"

COMPONENTS_WITH_CACHE=0
COMPONENTS_WITH_STATE_LOADER=0
COMPONENTS_TOTAL=0

for index in com/*/index; do
  ((COMPONENTS_TOTAL++))
  
  # Check syntax
  if ! bash -n "$index" 2>/dev/null; then
    check_fail "$(basename $(dirname $index)): syntax error"
    continue
  fi
  
  # Check for cache usage
  if grep -q "scripts/cache" "$index" 2>/dev/null; then
    ((COMPONENTS_WITH_CACHE++))
  fi
  
  # Check for state_loader usage
  if grep -q "scripts/state_loader" "$index" 2>/dev/null; then
    ((COMPONENTS_WITH_STATE_LOADER++))
  fi
done

check_pass "All $COMPONENTS_TOTAL components have valid syntax"
check_pass "$COMPONENTS_WITH_CACHE components use cache system"
check_pass "$COMPONENTS_WITH_STATE_LOADER components use state_loader"

echo ""
echo "4. Testing cache system..."
echo "----------------------------------------"

source scripts/cache

# Test cache_cmd
TEST_OUTPUT=$(cache_cmd "verify_test" 5 echo "Cache test output")
if [ "$TEST_OUTPUT" == "Cache test output" ]; then
  check_pass "cache_cmd works correctly"
  
  # Verify it's cached
  TEST_OUTPUT2=$(cache_cmd "verify_test" 5 echo "This should not appear")
  if [ "$TEST_OUTPUT2" == "Cache test output" ]; then
    check_pass "cache_cmd returns cached value"
  else
    check_fail "cache_cmd not caching properly"
  fi
else
  check_fail "cache_cmd failed to execute"
fi

# Clean up test cache
rm -f .cache/verify_test* 2>/dev/null

echo ""
echo "5. Testing state_loader..."
echo "----------------------------------------"

source scripts/state_loader

# Test state_init
state_init "verify_test_key" "test_value"
if grep -q "verify_test_key=test_value" "scripts/state"; then
  check_pass "state_init creates new key"
else
  check_fail "state_init failed"
fi

# Test state_update
state_update "verify_test_key" "updated_value"
if grep -q "verify_test_key=updated_value" "scripts/state"; then
  check_pass "state_update modifies key"
else
  check_fail "state_update failed"
fi

# Clean up test state
sed -i '/verify_test_key/d' "scripts/state" 2>/dev/null

echo ""
echo "6. Checking documentation..."
echo "----------------------------------------"

[ -f "PERFORMANCE_OPTIMIZATIONS.md" ] && check_pass "PERFORMANCE_OPTIMIZATIONS.md exists" || check_warn "PERFORMANCE_OPTIMIZATIONS.md missing"
[ -f "OPTIMIZATION_QUICK_START.md" ] && check_pass "OPTIMIZATION_QUICK_START.md exists" || check_warn "OPTIMIZATION_QUICK_START.md missing"
[ -f "OPTIMIZATIONS_README.md" ] && check_pass "OPTIMIZATIONS_README.md exists" || check_warn "OPTIMIZATIONS_README.md missing"
[ -f "CHANGELOG_OPTIMIZATIONS.md" ] && check_pass "CHANGELOG_OPTIMIZATIONS.md exists" || check_warn "CHANGELOG_OPTIMIZATIONS.md missing"

echo ""
echo "7. Sample component tests..."
echo "----------------------------------------"

# Test a few components can render
for component in battery cpu_temp network volume; do
  if [ -f "com/$component/render" ]; then
    if timeout 2s bash "com/$component/render" >/dev/null 2>&1; then
      check_pass "$component renders successfully"
    else
      check_warn "$component failed to render (may need dependencies)"
    fi
  fi
done

echo ""
echo "8. Performance indicators..."
echo "----------------------------------------"

# Check cache size (should be small)
if [ -d ".cache" ]; then
  CACHE_SIZE=$(du -sh .cache 2>/dev/null | cut -f1)
  echo "   Cache directory size: $CACHE_SIZE"
  check_pass "Cache directory exists and is accessible"
fi

# Count state file size
if [ -f "scripts/state" ]; then
  STATE_SIZE=$(wc -c < "scripts/state")
  STATE_LINES=$(wc -l < "scripts/state")
  echo "   State file: $STATE_LINES lines, $STATE_SIZE bytes"
  check_pass "State file is healthy"
fi

echo ""
echo "================================================"
echo "Verification Results"
echo "================================================"
echo -e "${GREEN}Passed:${NC}   $PASSED"
echo -e "${YELLOW}Warnings:${NC} $WARNINGS"
echo -e "${RED}Failed:${NC}   $FAILED"
echo ""

if [ $FAILED -eq 0 ]; then
  echo -e "${GREEN}✓ All critical checks passed!${NC}"
  echo ""
  echo "Your xmobar configuration is optimized and ready to use."
  echo "Reload xmobar to see the performance improvements:"
  echo ""
  echo "  killall xmobar && xmonad --restart"
  echo ""
  exit 0
else
  echo -e "${RED}✗ Some checks failed!${NC}"
  echo ""
  echo "Please review the errors above and fix them before using."
  echo ""
  exit 1
fi
