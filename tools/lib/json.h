/*
 * Reading JSON with jsmn (vendored, MIT): parse once into tokens, then walk
 * them. Header-only.
 */
#ifndef WELLBEING_JSON_H
#define WELLBEING_JSON_H

#define JSMN_STATIC
#include "jsmn.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    const char *text;
    jsmntok_t *tokens;
    int count;
} Json;

/* 0 if text isn't JSON. Free with json_free. */
static int json_parse(const char *text, Json *json) {
    jsmn_parser parser;
    jsmn_init(&parser);
    int count = jsmn_parse(&parser, text, strlen(text), NULL, 0);
    json->text = text;
    json->count = 0;
    json->tokens = NULL;
    if (count <= 0) return 0;
    json->tokens = malloc(sizeof(jsmntok_t) * (size_t)count);
    jsmn_init(&parser);
    json->count = jsmn_parse(&parser, text, strlen(text), json->tokens, (unsigned)count);
    return json->count > 0;
}

static void json_free(Json *json) {
    free(json->tokens);
    json->tokens = NULL;
    json->count = 0;
}

/* The index just past token i and everything inside it. */
static int json_skip(const Json *json, int i) {
    int end = i + 1;
    if (json->tokens[i].type == JSMN_OBJECT)
        for (int child = 0; child < json->tokens[i].size; child++) end = json_skip(json, json_skip(json, end));
    else if (json->tokens[i].type == JSMN_ARRAY)
        for (int child = 0; child < json->tokens[i].size; child++) end = json_skip(json, end);
    return end;
}

static int json_equals(const Json *json, int i, const char *s) {
    const jsmntok_t *t = &json->tokens[i];
    size_t length = strlen(s);
    return t->type == JSMN_STRING && (size_t)(t->end - t->start) == length && strncmp(json->text + t->start, s, length) == 0;
}

/* The value of key in the object at token i, or -1. */
static int json_get(const Json *json, int i, const char *key) {
    if (i < 0 || i >= json->count || json->tokens[i].type != JSMN_OBJECT) return -1;
    int at = i + 1;
    for (int child = 0; child < json->tokens[i].size; child++) {
        if (json_equals(json, at, key)) return at + 1;
        at = json_skip(json, at + 1);
    }
    return -1;
}

/* The n-th element of the array at token i, or -1. */
static int json_at(const Json *json, int i, int n) {
    if (i < 0 || json->tokens[i].type != JSMN_ARRAY || n >= json->tokens[i].size) return -1;
    int at = i + 1;
    while (n-- > 0) at = json_skip(json, at);
    return at;
}

static int json_size(const Json *json, int i) {
    return i >= 0 && (json->tokens[i].type == JSMN_ARRAY || json->tokens[i].type == JSMN_OBJECT) ? json->tokens[i].size : 0;
}

static double json_number(const Json *json, int i, double fallback) {
    if (i < 0 || json->tokens[i].type != JSMN_PRIMITIVE) return fallback;
    char c = json->text[json->tokens[i].start];
    return c == '-' || (c >= '0' && c <= '9') ? atof(json->text + json->tokens[i].start) : fallback;
}

static int json_bool(const Json *json, int i) {
    return i >= 0 && json->tokens[i].type == JSMN_PRIMITIVE && json->text[json->tokens[i].start] == 't';
}

/* A string token, unescaped into UTF-8; "" if i isn't a string. */
static void json_string(const Json *json, int i, char *out, size_t size) {
    size_t n = 0;
    if (i >= 0 && json->tokens[i].type == JSMN_STRING)
        for (const char *c = json->text + json->tokens[i].start, *end = json->text + json->tokens[i].end; c < end && n + 4 < size; c++) {
            if (*c != '\\' || c + 1 >= end) {
                out[n++] = *c;
                continue;
            }
            c++;
            if (*c == 'n') out[n++] = '\n';
            else if (*c == 't') out[n++] = '\t';
            else if (*c == 'r') out[n++] = '\r';
            else if (*c == 'b' || *c == 'f') out[n++] = ' ';
            else if (*c == 'u' && c + 4 < end) {
                unsigned code = (unsigned)strtoul((char[5]){c[1], c[2], c[3], c[4], 0}, NULL, 16);
                c += 4;
                /* Surrogate pairs come as two escapes; enough for names. */
                if (code >= 0xD800 && code <= 0xDBFF && c + 6 < end && c[1] == '\\' && c[2] == 'u') {
                    unsigned low = (unsigned)strtoul((char[5]){c[3], c[4], c[5], c[6], 0}, NULL, 16);
                    code = 0x10000 + ((code - 0xD800) << 10) + (low - 0xDC00);
                    c += 6;
                }
                if (code < 0x80) out[n++] = (char)code;
                else if (code < 0x800) out[n++] = (char)(0xC0 | code >> 6), out[n++] = (char)(0x80 | (code & 0x3F));
                else if (code < 0x10000) out[n++] = (char)(0xE0 | code >> 12), out[n++] = (char)(0x80 | ((code >> 6) & 0x3F)), out[n++] = (char)(0x80 | (code & 0x3F));
                else if (n + 4 < size) out[n++] = (char)(0xF0 | code >> 18), out[n++] = (char)(0x80 | ((code >> 12) & 0x3F)), out[n++] = (char)(0x80 | ((code >> 6) & 0x3F)), out[n++] = (char)(0x80 | (code & 0x3F));
            }
            else out[n++] = *c; /* \" \\ \/ */
        }
    out[n] = 0;
}

/* s as a JSON string body (no quotes). */
static void json_escape(const char *s, char *out, size_t size) {
    size_t n = 0;
    for (; *s && n + 7 < size; s++) {
        unsigned char c = (unsigned char)*s;
        if (c == '"' || c == '\\') out[n++] = '\\', out[n++] = (char)c;
        else if (c < 0x20) n += (size_t)snprintf(out + n, size - n, "\\u%04x", c);
        else out[n++] = (char)c;
    }
    out[n] = 0;
}

#endif
