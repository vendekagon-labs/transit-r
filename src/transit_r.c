/*
 * Copyright 2026 Vendekagon Labs LLC.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* Glue between transit-format-c and R: converts transit values to and from
 * exactly the R values the pure R implementation reads and writes. */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "transit.h"
#include "transit_c_version.h"

static SEXP cls_keyword, cls_symbol, cls_uuid, cls_uri, cls_decimal, cls_bigint, cls_int64, cls_map, cls_set,
    cls_list, cls_tagged, cls_link, cls_posixct, names_map, names_tagged, names_link, str_utc;

/* Documents are held by an external pointer, so they're freed even if R
 * signals an error part way through a conversion. */

static void doc_finalizer(SEXP ptr) {
    transit_doc *doc = (transit_doc *)R_ExternalPtrAddr(ptr);
    if (doc) {
        transit_doc_free(doc);
        R_ClearExternalPtr(ptr);
    }
}

static SEXP new_doc_ptr(transit_doc **doc) {
    SEXP ptr;
    *doc = transit_doc_new();
    if (!*doc) Rf_error("out of memory");
    ptr = PROTECT(R_MakeExternalPtr(*doc, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, doc_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

/* Reading: transit values to R */

static SEXP mkstr(const char *s, size_t len) {
    if (memchr(s, '\0', len)) Rf_error("R strings can't contain NUL, which this transit string does");
    return Rf_mkCharLenCE(s, (int)len, CE_UTF8);
}

static SEXP scalar_str(const char *s, size_t len) {
    SEXP x = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(x, 0, mkstr(s, len));
    UNPROTECT(1);
    return x;
}

static SEXP classed_str(const char *s, size_t len, SEXP cls) {
    SEXP x = PROTECT(scalar_str(s, len));
    Rf_setAttrib(x, R_ClassSymbol, cls);
    UNPROTECT(1);
    return x;
}

/* Canonical integer text (no '+', no leading zeros, "0" not "-0"). */
static SEXP canonical_int(const char *s, size_t len, SEXP cls) {
    char buf[512];
    size_t i = 0, n = 0;
    int neg = 0;
    if (len && (s[0] == '-' || s[0] == '+')) {
        neg = s[0] == '-';
        i = 1;
    }
    while (i + 1 < len && s[i] == '0') i++;
    if (len - i + 2 > sizeof(buf)) return classed_str(s, len, cls);
    if (neg && !(len - i == 1 && s[i] == '0')) buf[n++] = '-';
    memcpy(buf + n, s + i, len - i);
    n += len - i;
    return classed_str(buf, n, cls);
}

static SEXP int_value(int64_t v) {
    char buf[24];
    if (v > -2147483648LL && v <= 2147483647LL) return Rf_ScalarInteger((int)v);
    snprintf(buf, sizeof(buf), "%lld", (long long)v);
    return classed_str(buf, strlen(buf), cls_int64);
}

static SEXP posixct(double ms) {
    SEXP x = PROTECT(Rf_ScalarReal(ms / 1000));
    Rf_setAttrib(x, R_ClassSymbol, cls_posixct);
    Rf_setAttrib(x, Rf_install("tzone"), str_utc);
    UNPROTECT(1);
    return x;
}

static SEXP to_r(const transit_value *v);

static SEXP coll_to_r(const transit_value *v, SEXP cls) {
    size_t i, n = v->u.coll.count;
    SEXP x = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)n));
    for (i = 0; i < n; i++) SET_VECTOR_ELT(x, (R_xlen_t)i, to_r(v->u.coll.items[i]));
    if (cls != R_NilValue) Rf_setAttrib(x, R_ClassSymbol, cls);
    UNPROTECT(1);
    return x;
}

static SEXP map_to_r(const transit_value *v) {
    size_t i, n = v->u.map.count;
    SEXP m = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP keys = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)n));
    SEXP values = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)n));
    for (i = 0; i < n; i++) {
        SET_VECTOR_ELT(keys, (R_xlen_t)i, to_r(v->u.map.keys[i]));
        SET_VECTOR_ELT(values, (R_xlen_t)i, to_r(v->u.map.values[i]));
    }
    SET_VECTOR_ELT(m, 0, keys);
    SET_VECTOR_ELT(m, 1, values);
    Rf_setAttrib(m, R_NamesSymbol, names_map);
    Rf_setAttrib(m, R_ClassSymbol, cls_map);
    UNPROTECT(3);
    return m;
}

static const transit_value *map_lookup(const transit_value *m, const char *key) {
    size_t i, n = strlen(key);
    for (i = 0; i < m->u.map.count; i++) {
        const transit_value *k = m->u.map.keys[i];
        if (k->type == TRANSIT_STRING && k->u.str.len == n && memcmp(k->u.str.data, key, n) == 0) return m->u.map.values[i];
    }
    return NULL;
}

/* A link's representation is a map with string keys; as transit_link(). */
static SEXP link_to_r(const transit_value *rep) {
    static const char *fields[] = {"href", "rel", "name", "prompt", "render"};
    SEXP x = PROTECT(Rf_allocVector(VECSXP, 5));
    int i;
    for (i = 0; i < 5; i++) {
        const transit_value *f = map_lookup(rep, fields[i]);
        SEXP value = R_NilValue;
        if (f) {
            if (i == 0 && f->type == TRANSIT_URI) value = classed_str(f->u.str.data, f->u.str.len, cls_uri);
            else if (i == 0 && f->type == TRANSIT_STRING) value = classed_str(f->u.str.data, f->u.str.len, cls_uri);
            else if (f->type == TRANSIT_STRING || f->type == TRANSIT_URI) value = scalar_str(f->u.str.data, f->u.str.len);
            else if (f->type != TRANSIT_NIL) {
                UNPROTECT(1);
                return R_NilValue;   /* not a link we understand: keep it tagged */
            }
        }
        SET_VECTOR_ELT(x, i, value);
    }
    Rf_setAttrib(x, R_NamesSymbol, names_link);
    Rf_setAttrib(x, R_ClassSymbol, cls_link);
    UNPROTECT(1);
    return x;
}

static SEXP to_r(const transit_value *v) {
    char text[40];
    SEXP x;
    switch (v->type) {
    case TRANSIT_NIL: return R_NilValue;
    case TRANSIT_BOOL: return Rf_ScalarLogical(v->u.boolean);
    case TRANSIT_INT: return int_value(v->u.integer);
    case TRANSIT_BIGINT: return canonical_int(v->u.str.data, v->u.str.len, cls_bigint);
    case TRANSIT_FLOAT: return Rf_ScalarReal(v->u.number);
    case TRANSIT_DECIMAL: return classed_str(v->u.str.data, v->u.str.len, cls_decimal);
    case TRANSIT_STRING: return scalar_str(v->u.str.data, v->u.str.len);
    case TRANSIT_KEYWORD: return classed_str(v->u.str.data, v->u.str.len, cls_keyword);
    case TRANSIT_SYMBOL: return classed_str(v->u.str.data, v->u.str.len, cls_symbol);
    case TRANSIT_URI: return classed_str(v->u.str.data, v->u.str.len, cls_uri);
    case TRANSIT_BYTES:
        x = Rf_allocVector(RAWSXP, (R_xlen_t)v->u.str.len);
        if (v->u.str.len) memcpy(RAW(x), v->u.str.data, v->u.str.len);
        return x;
    case TRANSIT_TIME: return posixct((double)v->u.integer);
    case TRANSIT_UUID: {
        static const char hex[] = "0123456789abcdef";
        int i, n = 0;
        for (i = 0; i < 16; i++) {
            if (i == 4 || i == 6 || i == 8 || i == 10) text[n++] = '-';
            text[n++] = hex[v->u.uuid[i] >> 4];
            text[n++] = hex[v->u.uuid[i] & 15];
        }
        return classed_str(text, (size_t)n, cls_uuid);
    }
    case TRANSIT_ARRAY: return coll_to_r(v, R_NilValue);
    case TRANSIT_LIST: return coll_to_r(v, cls_list);
    case TRANSIT_SET: return coll_to_r(v, cls_set);
    case TRANSIT_MAP: return map_to_r(v);
    case TRANSIT_TAGGED:
        if (v->u.tagged.tag_len == 4 && memcmp(v->u.tagged.tag, "link", 4) == 0 && v->u.tagged.rep->type == TRANSIT_MAP) {
            x = link_to_r(v->u.tagged.rep);
            if (x != R_NilValue) return x;
        }
        x = PROTECT(Rf_allocVector(VECSXP, 2));
        SET_VECTOR_ELT(x, 0, scalar_str(v->u.tagged.tag, v->u.tagged.tag_len));
        SET_VECTOR_ELT(x, 1, to_r(v->u.tagged.rep));
        Rf_setAttrib(x, R_NamesSymbol, names_tagged);
        Rf_setAttrib(x, R_ClassSymbol, cls_tagged);
        UNPROTECT(1);
        return x;
    }
    return R_NilValue;
}

static transit_format format_arg(SEXP format) {
    int f = Rf_asInteger(format);
    return f == 0 ? TRANSIT_JSON : f == 1 ? TRANSIT_JSON_VERBOSE : TRANSIT_MSGPACK;
}

/* .Call entry: data is a string or raw vector. */
SEXP C_from_transit(SEXP data, SEXP format) {
    transit_doc *doc;
    SEXP ptr = PROTECT(new_doc_ptr(&doc)), result;
    transit_error err;
    const void *bytes;
    size_t len;
    transit_value *v;
    if (TYPEOF(data) == RAWSXP) {
        bytes = RAW(data);
        len = (size_t)XLENGTH(data);
    } else if (TYPEOF(data) == STRSXP && XLENGTH(data) == 1) {
        SEXP s = STRING_ELT(data, 0);
        bytes = CHAR(s);
        len = (size_t)LENGTH(s);
    } else {
        Rf_error("expected a string or raw vector");
    }
    v = transit_read(doc, bytes, len, format_arg(format), &err);
    if (!v) {
        char message[200];
        snprintf(message, sizeof(message), "%s", err.message);
        doc_finalizer(ptr);
        Rf_error("%s", message);
    }
    result = PROTECT(to_r(v));
    doc_finalizer(ptr);
    UNPROTECT(2);
    return result;
}

/* Writing: R values to transit. Returns NULL (and C_to_transit returns R
 * NULL) for values the pure R encoder should handle instead. */

typedef struct {
    transit_doc *doc;
    int unsupported;
} to_c_state;

static int is_a(SEXP x, const char *cls) { return Rf_inherits(x, cls); }

static transit_value *str_value(to_c_state *st, transit_value *(*make)(transit_doc *, const char *, size_t), SEXP x) {
    SEXP s;
    const char *p;
    if (XLENGTH(x) != 1 || STRING_ELT(x, 0) == NA_STRING) {
        st->unsupported = 1;
        return NULL;
    }
    s = STRING_ELT(x, 0);
    p = Rf_translateCharUTF8(s);
    return make(st->doc, p, strlen(p));
}

static int hexval(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

static transit_value *to_c(to_c_state *st, SEXP x);

static transit_value *time_value(to_c_state *st, double ms) {
    return transit_time(st->doc, (int64_t)ms);
}

/* One element of an atomic vector, as a scalar. */
static transit_value *atomic_elt(to_c_state *st, SEXP x, R_xlen_t i) {
    switch (TYPEOF(x)) {
    case LGLSXP:
        return LOGICAL(x)[i] == NA_LOGICAL ? transit_nil(st->doc) : transit_bool(st->doc, LOGICAL(x)[i]);
    case INTSXP:
        return INTEGER(x)[i] == NA_INTEGER ? transit_nil(st->doc) : transit_int(st->doc, INTEGER(x)[i]);
    case REALSXP: {
        double d = REAL(x)[i];
        if (R_IsNA(d)) return transit_nil(st->doc);
        return transit_float(st->doc, d);
    }
    case STRSXP: {
        SEXP s = STRING_ELT(x, i);
        const char *p;
        if (s == NA_STRING) return transit_nil(st->doc);
        p = Rf_translateCharUTF8(s);
        return transit_string(st->doc, p, strlen(p));
    }
    case RAWSXP:
        return transit_int(st->doc, RAW(x)[i]);
    default:
        st->unsupported = 1;
        return NULL;
    }
}

static transit_value *list_value(to_c_state *st, SEXP x, transit_value *coll) {
    R_xlen_t i, n = XLENGTH(x);
    for (i = 0; i < n; i++) {
        transit_value *item = to_c(st, VECTOR_ELT(x, i));
        if (!item || transit_push(st->doc, coll, item)) return NULL;
    }
    return coll;
}

static transit_value *named_map(to_c_state *st, SEXP x, SEXP names) {
    transit_value *m = transit_map(st->doc);
    R_xlen_t i, n = XLENGTH(x);
    for (i = 0; i < n; i++) {
        SEXP nm = STRING_ELT(names, i);
        transit_value *k, *v;
        if (nm == NA_STRING) k = transit_nil(st->doc);   /* as the pure R encoder writes an NA name */
        else {
            const char *p = Rf_translateCharUTF8(nm);
            k = transit_string(st->doc, p, strlen(p));
        }
        v = TYPEOF(x) == VECSXP ? to_c(st, VECTOR_ELT(x, i)) : atomic_elt(st, x, i);
        if (!v || transit_map_put(st->doc, m, k, v)) return NULL;
    }
    return m;
}

static transit_value *to_c(to_c_state *st, SEXP x) {
    SEXP names;
    if (st->unsupported) return NULL;
    if (x == R_NilValue) return transit_nil(st->doc);
    if (Rf_isObject(x)) {
        if (is_a(x, "transit_keyword")) return str_value(st, transit_keyword, x);
        if (is_a(x, "transit_symbol")) return str_value(st, transit_symbol, x);
        if (is_a(x, "transit_uri")) return str_value(st, transit_uri, x);
        if (is_a(x, "transit_decimal")) return str_value(st, transit_decimal, x);
        if (is_a(x, "transit_bigint")) return str_value(st, transit_bigint, x);
        if (is_a(x, "transit_int64")) {
            const char *p = CHAR(STRING_ELT(x, 0));
            return transit_int(st->doc, (int64_t)strtoll(p, NULL, 10));
        }
        if (is_a(x, "transit_uuid")) {
            const char *p = CHAR(STRING_ELT(x, 0));
            unsigned char u[16];
            int i, n = 0;
            for (i = 0; p[i] && n < 16; i++) {
                if (p[i] == '-') continue;
                u[n++] = (unsigned char)(hexval(p[i]) * 16 + hexval(p[i + 1]));
                i++;
            }
            return transit_uuid(st->doc, u);
        }
        if (is_a(x, "transit_tagged")) {
            SEXP tag = VECTOR_ELT(x, 0);
            const char *t = Rf_translateCharUTF8(STRING_ELT(tag, 0));
            transit_value *rep = to_c(st, VECTOR_ELT(x, 1));
            return rep ? transit_tagged(st->doc, t, strlen(t), rep) : NULL;
        }
        if (is_a(x, "transit_map")) {
            SEXP keys = VECTOR_ELT(x, 0), values = VECTOR_ELT(x, 1);
            transit_value *m = transit_map(st->doc);
            R_xlen_t i, n = XLENGTH(keys);
            for (i = 0; i < n; i++) {
                transit_value *k = to_c(st, VECTOR_ELT(keys, i)), *v;
                if (!k || !(v = to_c(st, VECTOR_ELT(values, i))) || transit_map_put(st->doc, m, k, v)) return NULL;
            }
            return m;
        }
        if (is_a(x, "transit_set")) return list_value(st, x, transit_set(st->doc));
        if (is_a(x, "transit_list")) return list_value(st, x, transit_list(st->doc));
        if (is_a(x, "transit_link")) {
            static const char *fields[] = {"href", "rel", "name", "prompt", "render"};
            transit_value *m = transit_map(st->doc);
            int i;
            for (i = 0; i < 5; i++) {
                SEXP f = VECTOR_ELT(x, i);
                transit_value *v;
                if (f == R_NilValue && i >= 2) continue;
                v = to_c(st, f);
                if (!v || transit_map_put(st->doc, m, transit_string(st->doc, fields[i], strlen(fields[i])), v)) return NULL;
            }
            return transit_tagged(st->doc, "link", 4, m);
        }
        if (is_a(x, "POSIXct") && TYPEOF(x) == REALSXP) {
            R_xlen_t i, n = XLENGTH(x);
            if (n == 1) {
                double s = REAL(x)[0];
                return ISNAN(s) ? transit_nil(st->doc) : time_value(st, nearbyint(s * 1000));
            } else {
                transit_value *a = transit_array(st->doc);
                for (i = 0; i < n; i++) {
                    double s = REAL(x)[i];
                    if (transit_push(st->doc, a, ISNAN(s) ? transit_nil(st->doc) : time_value(st, nearbyint(s * 1000)))) return NULL;
                }
                return a;
            }
        }
        if (is_a(x, "Date") && XLENGTH(x) == 1) {
            double d = Rf_asReal(x);
            return ISNAN(d) ? transit_nil(st->doc) : time_value(st, d * 86400000.0);
        }
        if (is_a(x, "factor") || is_a(x, "POSIXlt") || is_a(x, "Date")) {
            st->unsupported = 1;   /* the pure R encoder converts these */
            return NULL;
        }
        if (TYPEOF(x) != VECSXP && TYPEOF(x) != LGLSXP && TYPEOF(x) != INTSXP && TYPEOF(x) != REALSXP &&
            TYPEOF(x) != STRSXP && TYPEOF(x) != RAWSXP) {
            st->unsupported = 1;
            return NULL;
        }
    }
    if (TYPEOF(x) == RAWSXP) return transit_bytes(st->doc, RAW(x), (size_t)XLENGTH(x));
    names = Rf_getAttrib(x, R_NamesSymbol);
    if (TYPEOF(x) == VECSXP) {
        if (names != R_NilValue) return named_map(st, x, names);
        return list_value(st, x, transit_array(st->doc));
    }
    if (TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP || TYPEOF(x) == STRSXP) {
        R_xlen_t i, n = XLENGTH(x);
        transit_value *a;
        if (names != R_NilValue) return named_map(st, x, names);
        if (n == 1) return atomic_elt(st, x, 0);
        a = transit_array(st->doc);
        for (i = 0; i < n; i++) {
            transit_value *item = atomic_elt(st, x, i);
            if (!item || transit_push(st->doc, a, item)) return NULL;
        }
        return a;
    }
    st->unsupported = 1;
    return NULL;
}

/* .Call entry: returns a string (JSON) or raw vector (msgpack), or NULL if
 * the pure R encoder should write x. */
SEXP C_to_transit(SEXP x, SEXP format) {
    to_c_state st;
    SEXP ptr = PROTECT(new_doc_ptr(&st.doc)), result;
    transit_buffer out = {0};
    transit_error err;
    transit_value *v;
    transit_format f = format_arg(format);
    st.unsupported = 0;
    v = to_c(&st, x);
    if (!v) {
        doc_finalizer(ptr);
        UNPROTECT(1);
        return R_NilValue;
    }
    if (transit_write(v, f, &out, &err) != 0) {
        char message[200];
        snprintf(message, sizeof(message), "%s", err.message);
        transit_buffer_free(&out);
        doc_finalizer(ptr);
        Rf_error("%s", message);
    }
    doc_finalizer(ptr);
    if (f == TRANSIT_MSGPACK) {
        result = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t)out.len));
        if (out.len) memcpy(RAW(result), out.data, out.len);
    } else {
        result = PROTECT(Rf_allocVector(STRSXP, 1));
        SET_STRING_ELT(result, 0, Rf_mkCharLenCE((const char *)out.data, (int)out.len, CE_UTF8));
    }
    transit_buffer_free(&out);
    UNPROTECT(2);
    return result;
}

/* Registration */

/* Class and name vectors, shared by every value made here, so never modified. */
static SEXP preserve_strs(int n, ...) {
    SEXP x = Rf_allocVector(STRSXP, n);
    va_list ap;
    int i;
    R_PreserveObject(x);
    va_start(ap, n);
    for (i = 0; i < n; i++) SET_STRING_ELT(x, i, Rf_mkChar(va_arg(ap, const char *)));
    va_end(ap);
    MARK_NOT_MUTABLE(x);
    return x;
}

/* .Call entry: the transit-c commit the C sources came from. */
SEXP C_transit_c_version(void) { return Rf_mkString(TRANSIT_C_COMMIT); }

static const R_CallMethodDef call_methods[] = {
    {"C_from_transit", (DL_FUNC)&C_from_transit, 2},
    {"C_to_transit", (DL_FUNC)&C_to_transit, 2},
    {"C_transit_c_version", (DL_FUNC)&C_transit_c_version, 0},
    {NULL, NULL, 0}};

void attribute_visible R_init_transit(DllInfo *dll) {
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
    cls_keyword = preserve_strs(1, "transit_keyword");
    cls_symbol = preserve_strs(1, "transit_symbol");
    cls_uuid = preserve_strs(1, "transit_uuid");
    cls_uri = preserve_strs(1, "transit_uri");
    cls_decimal = preserve_strs(1, "transit_decimal");
    cls_bigint = preserve_strs(1, "transit_bigint");
    cls_int64 = preserve_strs(1, "transit_int64");
    cls_map = preserve_strs(1, "transit_map");
    cls_set = preserve_strs(1, "transit_set");
    cls_list = preserve_strs(1, "transit_list");
    cls_tagged = preserve_strs(1, "transit_tagged");
    cls_link = preserve_strs(1, "transit_link");
    cls_posixct = preserve_strs(2, "POSIXct", "POSIXt");
    names_map = preserve_strs(2, "keys", "values");
    names_tagged = preserve_strs(2, "tag", "rep");
    names_link = preserve_strs(5, "href", "rel", "name", "prompt", "render");
    str_utc = preserve_strs(1, "UTC");
}
