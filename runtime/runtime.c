#include <stdio.h>
#include <string.h>
#include <stdlib.h>

extern int builtin_read() {
	int d;
	printf("> ");
	scanf("%d", &d);
	return d;
}

extern void builtin_write(int x) {
	printf("%d\n", x);
}

typedef struct {
	int length;
	char buffer[0];
} string_t;
typedef string_t *string;

static string stralloc(int len) {
	string s = (string) malloc(sizeof(int) + len * sizeof(char));
	if (s == NULL) {
		// PANIC
		exit(1);
	}
	s->length = len;
	return s;
}

static void strfree(string s) {
	free(s);
}

extern string builtin_strmake(int n, int c) {
	string s = stralloc(n);
	for (int i = 0; i != n; ++i)
		s->buffer[i] = c;
	return s;
}

extern string builtin_strset(string s, int i, int c) {
	s->buffer[i] = c;
	return s;
}

extern int builtin_strget(string s, int i) {
	return s->buffer[i];
}

extern string builtin_strdup(string s) {
	string res = stralloc(s->length);
	strncpy(res->buffer, s->buffer, s->length);
	return res;
}

extern string builtin_strcat(string s1, string s2) {
	string res = stralloc(s1->length + s2->length);
	strncpy(res->buffer + 0, s1->buffer, s1->length);
	strncpy(res->buffer + s1->length, s2->buffer, s2->length);
	return res;
}

extern int builtin_strcmp(string s1, string s2) {
	int len = s1->length;
	if (len > s2->length)
		len = s2->length;
	int res = strncmp(s1->buffer, s2->buffer, len);
	if (res != 0)
		return res;
	return s1->length - s2->length;
}

extern int builtin_strlen(string s) {
	return s->length;
}

extern string builtin_strsub(string s, int i, int l) {
	string res = stralloc(l);
	memcpy(res->buffer, s->buffer + i, l);
	return res;
}
