// envkc — store/retrieve secrets in the macOS data-protection Keychain behind a
// TouchID-gated access control (BiometryCurrentSet: strict, no passcode fallback,
// invalidated if the enrolled fingerprint set changes).
//
// These items live in the data-protection keychain and are scoped to this
// binary's keychain-access-group, so the legacy `security` CLI cannot read them
// and every `get` forces a physical TouchID. Requires the binary to be signed
// with an Apple Developer ID cert + a <TeamID>.<group> keychain-access-groups
// entitlement (see build-envkc.sh) — AMFI kills it otherwise.
//
//	envkc set -p <project> -k <key>     # secret read from stdin
//	envkc get -p <project> -k <key>     # prints secret (TouchID prompt)
//	envkc has -p <project> -k <key>     # exit 0 if present (no prompt)
//	envkc rm  -p <project> -k <key>
package main

/*
#cgo CFLAGS: -x objective-c -Wno-deprecated-declarations
#cgo LDFLAGS: -framework CoreFoundation -framework Security
#include <CoreFoundation/CoreFoundation.h>
#include <Security/Security.h>

static CFStringRef cfstr(const char *s) {
  return CFStringCreateWithCString(kCFAllocatorDefault, s, kCFStringEncodingUTF8);
}

// Common base query: generic password, service+account, data-protection keychain.
// Caller owns svc/acc and must release the returned mutable dict.
static CFMutableDictionaryRef base_query(CFStringRef svc, CFStringRef acc) {
  CFMutableDictionaryRef q = CFDictionaryCreateMutable(kCFAllocatorDefault, 0,
      &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
  CFDictionarySetValue(q, kSecClass, kSecClassGenericPassword);
  CFDictionarySetValue(q, kSecAttrService, svc);
  CFDictionarySetValue(q, kSecAttrAccount, acc);
  CFDictionarySetValue(q, kSecUseDataProtectionKeychain, kCFBooleanTrue);
  return q;
}

// Store value behind a BiometryCurrentSet access control. Deletes any existing
// item first so re-import overwrites. Returns OSStatus (0 == ok).
static int kc_set(const char *service, const char *account,
                  const void *data, int dataLen) {
  CFStringRef svc = cfstr(service), acc = cfstr(account);
  CFDataRef val = CFDataCreate(kCFAllocatorDefault, data, dataLen);

  CFErrorRef acErr = NULL;
  SecAccessControlRef ac = SecAccessControlCreateWithFlags(kCFAllocatorDefault,
      kSecAttrAccessibleWhenUnlockedThisDeviceOnly,
      kSecAccessControlBiometryCurrentSet, &acErr);
  if (ac == NULL) {
    if (acErr) CFRelease(acErr);
    CFRelease(svc); CFRelease(acc); CFRelease(val);
    return -1;
  }

  CFMutableDictionaryRef del = base_query(svc, acc);
  SecItemDelete(del);
  CFRelease(del);

  CFMutableDictionaryRef add = base_query(svc, acc);
  CFDictionarySetValue(add, kSecValueData, val);
  CFDictionarySetValue(add, kSecAttrAccessControl, ac);
  CFDictionarySetValue(add, kSecAttrLabel,
      cfstr("envkc")); // namespacing label; access-group already isolates us
  OSStatus st = SecItemAdd(add, NULL);

  CFRelease(add); CFRelease(ac); CFRelease(svc); CFRelease(acc); CFRelease(val);
  return (int)st;
}

// Retrieve value (triggers TouchID). On success mallocs *out (caller frees).
static int kc_get(const char *service, const char *account, const char *prompt,
                  void **out, int *outLen) {
  CFStringRef svc = cfstr(service), acc = cfstr(account), pr = cfstr(prompt);
  CFMutableDictionaryRef q = base_query(svc, acc);
  CFDictionarySetValue(q, kSecReturnData, kCFBooleanTrue);
  CFDictionarySetValue(q, kSecMatchLimit, kSecMatchLimitOne);
  CFDictionarySetValue(q, kSecUseOperationPrompt, pr);

  CFTypeRef res = NULL;
  OSStatus st = SecItemCopyMatching(q, &res);
  CFRelease(q); CFRelease(svc); CFRelease(acc); CFRelease(pr);

  if (st == errSecSuccess && res != NULL) {
    CFDataRef d = (CFDataRef)res;
    CFIndex n = CFDataGetLength(d);
    *out = malloc(n);
    memcpy(*out, CFDataGetBytePtr(d), n);
    *outLen = (int)n;
    CFRelease(res);
  }
  return (int)st;
}

// Existence check — returns attributes only, so no TouchID prompt.
static int kc_has(const char *service, const char *account) {
  CFStringRef svc = cfstr(service), acc = cfstr(account);
  CFMutableDictionaryRef q = base_query(svc, acc);
  CFDictionarySetValue(q, kSecReturnAttributes, kCFBooleanTrue);
  CFDictionarySetValue(q, kSecMatchLimit, kSecMatchLimitOne);
  CFTypeRef res = NULL;
  OSStatus st = SecItemCopyMatching(q, &res);
  if (res) CFRelease(res);
  CFRelease(q); CFRelease(svc); CFRelease(acc);
  return (int)st;
}

static int kc_delete(const char *service, const char *account) {
  CFStringRef svc = cfstr(service), acc = cfstr(account);
  CFMutableDictionaryRef q = base_query(svc, acc);
  OSStatus st = SecItemDelete(q);
  CFRelease(q); CFRelease(svc); CFRelease(acc);
  return (int)st;
}
*/
import "C"

import (
	"flag"
	"fmt"
	"io"
	"os"
	"unsafe"
)

func main() {
	if len(os.Args) < 2 {
		usage()
	}
	cmd := os.Args[1]
	fs := flag.NewFlagSet(cmd, flag.ExitOnError)
	project := fs.String("p", "", "project (keychain account)")
	key := fs.String("k", "", "secret name (keychain service)")
	fs.Parse(os.Args[2:])
	if *project == "" || *key == "" {
		fmt.Fprintln(os.Stderr, "envkc: -p <project> and -k <key> are required")
		os.Exit(2)
	}

	cKey, cProj := C.CString(*key), C.CString(*project)
	defer C.free(unsafe.Pointer(cKey))
	defer C.free(unsafe.Pointer(cProj))

	switch cmd {
	case "set":
		data, err := io.ReadAll(os.Stdin)
		if err != nil {
			fatal("read stdin: %v", err)
		}
		var p unsafe.Pointer
		if len(data) > 0 {
			p = unsafe.Pointer(&data[0])
		}
		check("set", C.kc_set(cKey, cProj, p, C.int(len(data))))
		fmt.Fprintf(os.Stderr, "stored %s/%s (TouchID-gated; re-enrolling a fingerprint invalidates it)\n", *project, *key)
	case "get":
		prompt := C.CString(fmt.Sprintf("Unlock %s for %s", *key, *project))
		defer C.free(unsafe.Pointer(prompt))
		var out unsafe.Pointer
		var n C.int
		check("get", C.kc_get(cKey, cProj, prompt, &out, &n))
		os.Stdout.Write(C.GoBytes(out, n))
		C.free(out)
	case "has":
		os.Exit(boolToCode(C.kc_has(cKey, cProj) == C.errSecSuccess))
	case "rm":
		check("rm", C.kc_delete(cKey, cProj))
	default:
		usage()
	}
}

func check(op string, st C.int) {
	if st != C.errSecSuccess {
		fatal("%s failed: OSStatus %d", op, int(st))
	}
}

func fatal(format string, a ...any) {
	fmt.Fprintf(os.Stderr, "envkc: "+format+"\n", a...)
	os.Exit(1)
}

func boolToCode(b bool) int {
	if b {
		return 0
	}
	return 1
}

func usage() {
	fmt.Fprintln(os.Stderr, "usage: envkc <set|get|has|rm> -p <project> -k <key>")
	os.Exit(2)
}
