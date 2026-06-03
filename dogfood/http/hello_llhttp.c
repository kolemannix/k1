#include <stdint.h>
#include "stdio.h"
#include "llhttp.h"
#include "string.h"

int handle_on_message_complete(llhttp_t* parser) {
	fprintf(stdout, "Message completed!\n");
	return 0;
}

int main() {
	llhttp_t parser;
	llhttp_settings_t settings;

	/*Initialize user callbacks and settings */
	llhttp_settings_init(&settings);

	/*Set user callback */
	settings.on_message_complete = handle_on_message_complete;

	/*Initialize the parser in HTTP_BOTH mode, meaning that it will select between
	*HTTP_REQUEST and HTTP_RESPONSE parsing automatically while reading the first
	*input.
	*/
	llhttp_init(&parser, HTTP_BOTH, &settings);

	/*Parse request! */
	const char* request = "GET / HTTP/1.1\r\n\r\n";
	int request_len = strlen(request);

	enum llhttp_errno err = llhttp_execute(&parser, request, request_len);
	if (err == HPE_OK) {
		fprintf(stdout, "Successfully parsed!\n");
	} else {
		fprintf(stderr, "Parse error: %s %s\n", llhttp_errno_name(err), llhttp_get_error_reason(&parser));
	}
}
