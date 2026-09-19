// Unprivileged, one-attempt PAM client. Secrets travel only over stdin.
#define _GNU_SOURCE
#include <security/pam_appl.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <unistd.h>

struct credentials {
    char password[4096];
    const char *user;
    int answered;
};

static int converse(int count, const struct pam_message **messages,
                    struct pam_response **out, void *data) {
    struct credentials *credentials = data;
    if (count < 1 || count > PAM_MAX_NUM_MSG)
        return PAM_CONV_ERR;
    struct pam_response *responses = calloc((size_t)count, sizeof(*responses));
    if (!responses)
        return PAM_BUF_ERR;
    for (int i = 0; i < count; i++) {
        switch (messages[i]->msg_style) {
        case PAM_PROMPT_ECHO_OFF:
            // This UI supports one password, not an implicit OTP/password-change flow.
            if (credentials->answered++)
                goto fail;
            responses[i].resp = strdup(credentials->password);
            explicit_bzero(credentials->password, sizeof(credentials->password));
            if (!responses[i].resp)
                goto fail;
            break;
        case PAM_PROMPT_ECHO_ON:
            responses[i].resp = strdup(credentials->user);
            if (!responses[i].resp)
                goto fail;
            break;
        case PAM_TEXT_INFO:
        case PAM_ERROR_MSG:
            break;
        default:
            goto fail;
        }
    }
    *out = responses;
    return PAM_SUCCESS;
fail:
    for (int i = 0; i < count; i++) {
        if (responses[i].resp) {
            explicit_bzero(responses[i].resp, strlen(responses[i].resp));
            free(responses[i].resp);
        }
    }
    free(responses);
    return PAM_CONV_ERR;
}

int main(void) {
    const struct rlimit no_core = {0, 0};
    if (setrlimit(RLIMIT_CORE, &no_core) || prctl(PR_SET_DUMPABLE, 0))
        return 2;
    // Avoid a second password copy lingering in stdio's input buffer.
    if (setvbuf(stdin, NULL, _IONBF, 0))
        return 2;
    struct passwd *account = getpwuid(getuid());
    if (!account || getuid() != geteuid())
        return 2;
    struct credentials credentials = {.user = account->pw_name};
    size_t length = 0;
    int byte;
    while ((byte = getchar()) != EOF && byte != '\n') {
        if (byte == 0 || length >= sizeof(credentials.password) - 1) {
            explicit_bzero(credentials.password, sizeof(credentials.password));
            return 2;
        }
        credentials.password[length++] = (char)byte;
    }
    if (!length || byte != '\n') {
        explicit_bzero(credentials.password, sizeof(credentials.password));
        return 2;
    }
    const struct pam_conv conversation = {converse, &credentials};
    pam_handle_t *handle = NULL;
    int result = pam_start("system-auth", credentials.user, &conversation, &handle);
    if (result == PAM_SUCCESS)
        result = pam_authenticate(handle, PAM_DISALLOW_NULL_AUTHTOK);
    if (result == PAM_SUCCESS)
        result = pam_acct_mgmt(handle, 0);
    explicit_bzero(credentials.password, sizeof(credentials.password));
    if (handle)
        pam_end(handle, result);
    if (result == PAM_SUCCESS) {
        puts("QS_AUTH_SUCCESS");
        return 0;
    }
    if (result == PAM_AUTH_ERR || result == PAM_USER_UNKNOWN ||
        result == PAM_MAXTRIES || result == PAM_PERM_DENIED) {
        puts("QS_AUTH_REJECTED");
        return 1;
    }
    puts("QS_AUTH_UNAVAILABLE");
    return 2;
}
