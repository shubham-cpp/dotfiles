// Linked only into the test executable. Production always links system libpam.
#define _GNU_SOURCE
#include <security/pam_appl.h>
#include <pwd.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
static struct pam_conv callback;
static const char *mode(void) { const char *m = getenv("QS_TEST_CASE"); return m ? m : "success"; }
int pam_start(const char *service, const char *user, const struct pam_conv *conv, pam_handle_t **handle) {
    if (strcmp(service, "system-auth") || strcmp(user, getpwuid(getuid())->pw_name)) return PAM_SYSTEM_ERR;
    callback = *conv; *handle = (pam_handle_t *)&callback;
    return strcmp(mode(), "start-fail") ? PAM_SUCCESS : PAM_SYSTEM_ERR;
}
int pam_authenticate(pam_handle_t *handle, int flags) {
    (void)handle;
    if (!(flags & PAM_DISALLOW_NULL_AUTHTOK)) return PAM_SYSTEM_ERR;
    const struct pam_message message = {PAM_PROMPT_ECHO_OFF, "Password:"};
    const struct pam_message *messages[] = {&message};
    struct pam_response *response = NULL;
    int result = callback.conv(1, messages, &response, callback.appdata_ptr);
    if (result != PAM_SUCCESS) return result;
    int correct = response[0].resp && !strcmp(response[0].resp, "fixture");
    explicit_bzero(response[0].resp, strlen(response[0].resp)); free(response[0].resp); free(response);
    if (!strcmp(mode(), "multi-prompt")) return callback.conv(1, messages, &response, callback.appdata_ptr);
    return correct && strcmp(mode(), "auth-fail") ? PAM_SUCCESS : PAM_AUTH_ERR;
}
int pam_acct_mgmt(pam_handle_t *handle, int flags) {
    (void)handle; (void)flags;
    if (!strcmp(mode(), "auth-fail")) _exit(99);
    return strcmp(mode(), "account-fail") ? PAM_SUCCESS : PAM_ACCT_EXPIRED;
}
int pam_end(pam_handle_t *handle, int status) { (void)handle; (void)status; return PAM_SUCCESS; }
