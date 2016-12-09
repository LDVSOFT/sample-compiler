#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <algorithm>

#define BUILTIN(result, name, ...) \
	extern "C" result builtin_ ## name(__VA_ARGS__)

#define PRIVATE_BUILTIN(result, name, ...) \
	BUILTIN(result, __ ## name, __VA_ARGS__)

BUILTIN(int, read) {
	int d;
	std::printf("> ");
	std::scanf("%d", &d);
	return d;
}

BUILTIN(void, write, int x) {
	std::printf("%d\n", x);
}

enum class tag_t: int {
	STRING = 0,
	BOXED_ARRAY = 1,
	UNBOXED_ARRAY = 2
};

struct string {
	tag_t const tag = tag_t::STRING;
	int length;
	char buffer[0];

	char& operator [](int index) {
		return buffer[index];
	}

	static string* alloc(int len) {
		string* s((string*)std::malloc(sizeof(tag_t) + sizeof(int) + len * sizeof(char)));
		if (s == NULL)
			exit(1);
		s->length = len;
		return s;
	}

	static void free(string* s) {
		std::free(s);
	}
};

BUILTIN(string*, strmake, int n, int c) {
	string* s(string::alloc(n));
	for (int i(0); i != n; ++i)
		(*s)[i] = c;
	return s;
}

BUILTIN(string*, strset, string* s, int i, int c) {
	(*s)[i] = c;
	return s;
}

BUILTIN(int, strget, string* s, int i) {
	return (*s)[i];
}

BUILTIN(string*, strdup, string* s) {
	string* res(string::alloc(s->length));
	std::strncpy(res->buffer, s->buffer, s->length);
	return res;
}

BUILTIN(string*, strcat, string* s1, string* s2) {
	string* res(string::alloc(s1->length + s2->length));
	std::strncpy(res->buffer + 0, s1->buffer, s1->length);
	std::strncpy(res->buffer + s1->length, s2->buffer, s2->length);
	return res;
}

BUILTIN(int, strcmp, string* s1, string* s2) {
	int len(std::min(s1->length, s2->length));
	int res(strncmp(s1->buffer, s2->buffer, len));
	if (res != 0)
		return res;
	return s1->length - s2->length;
}

BUILTIN(int, strlen, string* s) {
	return s->length;
}

BUILTIN(string*, strsub, string* s, int i, int l) {
	string* res(string::alloc(l));
	std::memcpy(res->buffer, s->buffer + i, l);
	return res;
}

struct array {
	tag_t tag;
	int length;
	void* buffer[0];

	void*& operator [](int index) {
		return buffer[index];
	}

	static array* alloc(int len, bool boxed) {
		array* a((array*)std::malloc(sizeof(tag_t) + sizeof(int) + len * sizeof(void*)));
		if (a == NULL)
			exit(1);
		a->length = len;
		a->tag = boxed ? tag_t::BOXED_ARRAY : tag_t::UNBOXED_ARRAY;
		return a;
	}

	static void free(array* s) {
		std::free(s);
	}
};

static array* arrmake(int n, int v, bool boxed) {
	array* a(array::alloc(n, boxed));
	for (int i(0); i != n; ++i)
		a->buffer[i] = (void*)v;
	return a;
}

BUILTIN(array*, arrmake, int n, int v) {
	return arrmake(n, v, false);
}

BUILTIN(array*, Arrmake, int n, int v) {
	return arrmake(n, v, true);
}

BUILTIN(int, arrlen, array* a) {
	return a->length;
}

PRIVATE_BUILTIN(int, arrget, array* a, int n) {
	return (int)(*a)[n];
}

PRIVATE_BUILTIN(array*, arrset, array* a, int n, int v) {
	(*a)[n] = (void*)v;
	return a;
}
