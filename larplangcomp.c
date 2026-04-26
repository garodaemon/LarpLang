#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#include <stdint.h>
#include <unistd.h>

// dict
#define MSYM 1000
#define MBUF 1024
#define VSYM 8192
#define LDEP 128

char mn[MSYM][32];
char mb[MSYM][MBUF];
int  mc = 0, rec = 0;
char cb[MBUF] = "";

// st
int dv = 0, va = 0, ln = 1;
int ic = 0, ia = 0;

// lbl
int lc = 0, ls[LDEP], lt = -1;
int le[LDEP]; // 1 = this IF already got an ELSE

void p(char *src, int d);

// err
void er(const char *m) { fprintf(stderr, "ERR (L%d): %s\n", ln, m); exit(1); }

// stk guards
void chku() { if (lt < 0)      er("Unmatched ctrl"); }
void chko() { if (lt >= LDEP-1) er("Label stack OF"); }

// cb append token (space-sep)
static void cbadd(const char *s) {
    size_t cl = strlen(cb), sl = strlen(s);
    if (cl + sl + 2 >= MBUF) er("Macro too long");
    strcat(cb, s);
    strcat(cb, " ");
}
// cb append raw bytes (no trailing space, for string chars)
static void cbraw(const char *s, size_t n) {
    size_t cl = strlen(cb);
    if (cl + n + 1 >= MBUF) er("Macro too long");
    memcpy(cb + cl, s, n);
    cb[cl + n] = '\0';
}

void ev(char *t, int d) {
    if (!t || !*t) return;
    if (d > 200) er("Recursion limit");

    // def start
    if (t[0] == ',') {
        if (mc >= MSYM) er("Dict full");
        strncpy(mn[mc], &t[1], 31); mn[mc][31] = '\0';
        rec = 1; cb[0] = '\0'; return;
    }
    // def
    if (rec) {
        if (!strcmp(t, ";")) {
            // check dict limit at save time
            if (mc >= MSYM) er("Dict full");
            strncpy(mb[mc], cb, MBUF-1); mb[mc][MBUF-1] = '\0';
            mc++; rec = 0;
        } else {
            // ( is handled in lex; just skip stray tokens named "("
            if (!strcmp(t, "(")) return;
            cbadd(t);
        }
        return;
    }

    // C
    if (!strcmp(t, "C{"))  { ic = 1; return; }
    if (!strcmp(t, "}C"))  { ic = 0; printf("\n"); return; }
    if (ic) { printf("%s ", t); return; }

    // inline ASM — escape quotes
    if (!strcmp(t, "ASM{")) { ia = 1; printf("    __asm__ volatile(\""); return; }
    if (!strcmp(t, "}ASM")) { ia = 0; printf("\\n\");\n"); return; }
    if (ia) {
        // escape " in asm tokens
        for (char *c = t; *c; c++) {
            if (*c == '"') printf("\\\"");
            else putchar(*c);
        }
        putchar(' ');
        return;
    }

    // var
    if (dv) {
        if (mc >= MSYM) er("Dict full");
        if (va >= VSYM) er("Var limit");
        strncpy(mn[mc], t, 31); mn[mc][31] = '\0';
        snprintf(mb[mc], MBUF, "\x01    PUSH((intptr_t)&m[%d]);\n", va++);
        mc++; dv = 0; return;
    }
    if (!strcmp(t, "VARIABLE")) { dv = 1; return; }

    // lookup
    for (int i = mc-1; i >= 0; i--) {
        if (!strcmp(t, mn[i])) {
            if (mb[i][0] == '\x01') { printf("%s", mb[i]+1); return; } // raw emit
            char tmp[MBUF];
            strncpy(tmp, mb[i], MBUF-1); tmp[MBUF-1] = '\0';
            p(tmp, d+1);
            return;
        }
    }

    // flow
    if (!strcmp(t, "BEGIN"))  { chko(); ls[++lt] = ++lc; printf("Ls_%d:\n", ls[lt]); return; }
    if (!strcmp(t, "UNTIL"))  { chku(); printf("    if(!POP()) goto Ls_%d;\n", ls[lt--]); return; }
    if (!strcmp(t, "WHILE"))  { chku(); printf("    if(!POP()) goto Le_%d;\n", ls[lt]); return; }
    if (!strcmp(t, "REPEAT")) { chku(); printf("    goto Ls_%d;\nLe_%d:\n", ls[lt], ls[lt]); lt--; return; }
    if (!strcmp(t, "AGAIN"))  { chku(); printf("    goto Ls_%d;\n", ls[lt--]); return; }

    // look or loop idk
    if (!strcmp(t, "DO")) {
        chko(); ls[++lt] = ++lc;
        printf("    { intptr_t _i=POP(), _l=POP(); R_PUSH(_l); R_PUSH(_i); }\nLs_%d:\n", ls[lt]);
        return;
    }
    if (!strcmp(t, "LOOP")) {
        chku();
        printf("    rs[rt]++; if(rs[rt]<rs[rt-1]) goto Ls_%d;\n", ls[lt]);
        printf("Le_%d:\n    R_POP(); R_POP();\n", ls[lt--]);
        return;
    }
    if (!strcmp(t, "+LOOP")) {
        chku();
        printf("    { intptr_t _s=POP(); rs[rt]+=_s;"
               " if(_s>0 ? rs[rt]<rs[rt-1] : rs[rt]>rs[rt-1]) goto Ls_%d; }\n", ls[lt]);
        printf("Le_%d:\n    R_POP(); R_POP();\n", ls[lt--]);
        return;
    }
    if (!strcmp(t, "LEAVE")) { chku(); printf("    goto Le_%d;\n", ls[lt]); return; }
    if (!strcmp(t, "I"))     { printf("    { if(rt<0) UFLOW(); PUSH(rs[rt]); }\n"); return; }
    if (!strcmp(t, "J"))     { printf("    { if(rt<2) UFLOW(); PUSH(rs[rt-2]); }\n"); return; }

    // cond
    if (!strcmp(t, "IF"))   { chko(); ls[++lt] = ++lc; le[lt] = 0; printf("    if(!POP()) goto Lf_%d;\n", ls[lt]); return; }
    if (!strcmp(t, "ELSE")) {
        chku();
        printf("    goto Le_%d;\nLf_%d:\n", ls[lt], ls[lt]);
        le[lt] = 1;
        return;
    }
    if (!strcmp(t, "THEN")) {
        chku();
        if (!le[lt]) printf("Lf_%d:\n", ls[lt]); // only if no ELSE
        printf("Le_%d:\n", ls[lt]);
        lt--;
        return;
    }

    // sys
    if (!strcmp(t, "EXIT"))   { printf("    return 0;\n"); return; }
    if (!strcmp(t, "BYE"))    { printf("    exit(0);\n"); return; }
    if (!strcmp(t, "UNLOOP")) { printf("    R_POP(); R_POP();\n"); return; } // clean rstack before EXIT in loop
    if (!strcmp(t, "KEY"))    { printf("    PUSH((intptr_t)getchar());\n"); return; }
    if (!strcmp(t, "TIME"))   { printf("    PUSH((intptr_t)time(NULL));\n"); return; }

    // dstk
    if (!strcmp(t, "DUP"))   { printf("    PUSH(s[st]);\n"); return; }
    if (!strcmp(t, "DROP"))  { printf("    POP();\n"); return; }
    if (!strcmp(t, "SWAP"))  { printf("    { intptr_t a=POP(),b=POP(); PUSH(a); PUSH(b); }\n"); return; }
    if (!strcmp(t, "OVER"))  { printf("    { if(st<1) UFLOW(); PUSH(s[st-1]); }\n"); return; }
    if (!strcmp(t, "ROT"))   { printf("    { intptr_t a=POP(),b=POP(),c=POP(); PUSH(b); PUSH(a); PUSH(c); }\n"); return; }
    if (!strcmp(t, "DEPTH")) { printf("    PUSH((intptr_t)(st+1));\n"); return; }
    if (!strcmp(t, "NIP"))   { printf("    { intptr_t a=POP(); POP(); PUSH(a); }\n"); return; }
    if (!strcmp(t, "TUCK"))  { printf("    { intptr_t a=POP(),b=POP(); PUSH(a); PUSH(b); PUSH(a); }\n"); return; }
    if (!strcmp(t, "2DUP"))  { printf("    { if(st<1) UFLOW(); intptr_t b=s[st],a=s[st-1]; PUSH(a); PUSH(b); }\n"); return; }

    // rstk
    if (!strcmp(t, ">R"))  { printf("    R_PUSH(POP());\n"); return; }
    if (!strcmp(t, "R>"))  { printf("    PUSH(R_POP());\n"); return; }
    if (!strcmp(t, "R@"))  { printf("    { if(rt<0) UFLOW(); PUSH(rs[rt]); }\n"); return; }

    // math 1+1=3
    if (!strcmp(t, "+"))   { printf("    { intptr_t b=POP(),a=POP(); PUSH(a+b); }\n"); return; }
    if (!strcmp(t, "-"))   { printf("    { intptr_t b=POP(),a=POP(); PUSH(a-b); }\n"); return; }
    if (!strcmp(t, "*"))   { printf("    { intptr_t b=POP(),a=POP(); PUSH(a*b); }\n"); return; }
    if (!strcmp(t, "/"))   { printf("    { intptr_t b=POP(),a=POP(); if(!b) DIVZ(); PUSH(a/b); }\n"); return; }
    if (!strcmp(t, "MOD")) { printf("    { intptr_t b=POP(),a=POP(); if(!b) DIVZ(); PUSH(a%%b); }\n"); return; }
    if (!strcmp(t, "1+"))  { printf("    s[st]++;\n"); return; }
    if (!strcmp(t, "1-"))  { printf("    s[st]--;\n"); return; }
    if (!strcmp(t, "2*"))  { printf("    s[st]<<=1;\n"); return; }
    if (!strcmp(t, "2/"))  { printf("    s[st]>>=1;\n"); return; }
    if (!strcmp(t, "NEGATE")) { printf("    s[st]=-s[st];\n"); return; }
    if (!strcmp(t, "ABS"))    { printf("    { intptr_t v=s[st]; if(v<0) s[st]=-v; }\n"); return; }
    if (!strcmp(t, "MAX"))    { printf("    { intptr_t b=POP(),a=POP(); PUSH(a>b?a:b); }\n"); return; }
    if (!strcmp(t, "MIN"))    { printf("    { intptr_t b=POP(),a=POP(); PUSH(a<b?a:b); }\n"); return; }

    // log
    if (!strcmp(t, "="))   { printf("    { intptr_t b=POP(),a=POP(); PUSH(a==b?-1:0); }\n"); return; }
    if (!strcmp(t, ">"))   { printf("    { intptr_t b=POP(),a=POP(); PUSH(a>b?-1:0); }\n"); return; }
    if (!strcmp(t, "<"))   { printf("    { intptr_t b=POP(),a=POP(); PUSH(a<b?-1:0); }\n"); return; }
    if (!strcmp(t, "<>"))  { printf("    { intptr_t b=POP(),a=POP(); PUSH(a!=b?-1:0); }\n"); return; }
    if (!strcmp(t, "0="))  { printf("    { intptr_t a=POP(); PUSH(a==0?-1:0); }\n"); return; }
    if (!strcmp(t, "0<"))  { printf("    { intptr_t a=POP(); PUSH(a<0?-1:0); }\n"); return; }
    if (!strcmp(t, "0>"))  { printf("    { intptr_t a=POP(); PUSH(a>0?-1:0); }\n"); return; }
    if (!strcmp(t, "AND")) { printf("    { intptr_t b=POP(),a=POP(); PUSH(a&b); }\n"); return; }
    if (!strcmp(t, "OR"))  { printf("    { intptr_t b=POP(),a=POP(); PUSH(a|b); }\n"); return; }
    if (!strcmp(t, "XOR")) { printf("    { intptr_t b=POP(),a=POP(); PUSH(a^b); }\n"); return; }
    if (!strcmp(t, "NOT")) { printf("    s[st]=~s[st];\n"); return; }
    if (!strcmp(t, "INVERT")) { printf("    s[st]=~s[st];\n"); return; }
    if (!strcmp(t, "LSHIFT")) { printf("    { intptr_t n=POP(),a=POP(); PUSH(a<<n); }\n"); return; }
    if (!strcmp(t, "RSHIFT")) { printf("    { intptr_t n=POP(),a=POP(); PUSH((uintptr_t)a>>n); }\n"); return; }

    // mem
    if (!strcmp(t, "@"))    { printf("    { intptr_t a=POP(); PUSH(*(intptr_t*)CHKP(a,sizeof(intptr_t))); }\n"); return; }
    if (!strcmp(t, "!"))    { printf("    { intptr_t a=POP(),v=POP(); *(intptr_t*)CHKP(a,sizeof(intptr_t))=v; }\n"); return; }
    if (!strcmp(t, "C@"))   { printf("    { intptr_t a=POP(); PUSH(*(uint8_t*)CHKP(a,1)); }\n"); return; }
    if (!strcmp(t, "C!"))   { printf("    { intptr_t a=POP(),v=POP(); *(uint8_t*)CHKP(a,1)=(uint8_t)v; }\n"); return; }
    if (!strcmp(t, "FILL")) { printf("    { intptr_t c=POP(),n=POP(),a=POP(); memset((void*)CHKP(a,n),(int)c,(size_t)n); }\n"); return; }
    if (!strcmp(t, "MOVE")) { printf("    { intptr_t n=POP(),d=POP(),s=POP(); memmove((void*)CHKP(d,n),(void*)CHKP(s,n),(size_t)n); }\n"); return; }

    // hw
    if (!strcmp(t, "HW@")) { printf("    { uintptr_t a=(uintptr_t)POP(); PUSH(*(volatile intptr_t*)a); }\n"); return; }
    if (!strcmp(t, "HW!")) { printf("    { uintptr_t a=(uintptr_t)POP(); intptr_t v=POP(); *(volatile intptr_t*)a=v; }\n"); return; }

    // io
    if (!strcmp(t, "EMIT"))  { printf("    { int c=(int)POP(); if(c>=0&&c<=255) putchar(c); }\n"); return; }
    if (!strcmp(t, "."))     { printf("    printf(\"%%ld \",(long)POP());\n"); return; }
    if (!strcmp(t, "CR"))    { printf("    putchar('\\n');\n"); return; }
    if (!strcmp(t, "SPACE")) { printf("    putchar(' ');\n"); return; }
    if (!strcmp(t, "SPACES")){ printf("    { intptr_t n=POP(); while(n-->0) putchar(' '); }\n"); return; }
    if (!strcmp(t, ".S"))    { printf("    { for(int _i=0;_i<=st;_i++) printf(\"%%ld \",(long)s[_i]); printf(\"<-Top\\n\"); }\n"); return; }
    if (!strcmp(t, "U."))    { printf("    printf(\"%%lu \",(unsigned long)POP());\n"); return; }
    if (!strcmp(t, "HEX."))  { printf("    printf(\"%%lx \",(long)POP());\n"); return; }

    // lit
    {
        const char *q = t;
        int neg = (q[0]=='-');
        if (neg) q++;
        int is_bin = (!strncmp(q,"0b",2)||!strncmp(q,"0B",2));
        int is_hex = (!strncmp(q,"0x",2)||!strncmp(q,"0X",2));
        int is_dec = isdigit((unsigned char)q[0]);

        if (is_bin || is_hex || is_dec) {
            if (is_bin) {
                intptr_t v = 0;
                for (const char *c = q+2; *c; c++) {
                    if (*c!='0'&&*c!='1') er("Bad binary lit");
                    v = (v<<1)|(*c-'0');
                }
                if (neg) v = -v;
                printf("    PUSH(%ld);\n", (long)v);
            } else {
                printf("    PUSH(%s);\n", t);
            }
            return;
        }
    }

    er(t);
}

// lex
void p(char *src, int d) {
    char t[512];
    char *ptr = src;

    while (*ptr) {
        while (*ptr && isspace((unsigned char)*ptr)) { if (*ptr=='\n') ln++; ptr++; }
        if (!*ptr) break;

        // cmt
        if (*ptr=='(' && ptr[1] && isspace((unsigned char)ptr[1])) {
            while (*ptr && *ptr!=')') { if (*ptr=='\n') ln++; ptr++; }
            if (*ptr==')') ptr++;
            continue;
        }
        // line
        if (*ptr=='\\' && (!ptr[1]||ptr[1]=='\n'||ptr[1]==' ')) {
            while (*ptr && *ptr!='\n') ptr++;
            continue;
        }

        // str
        if (!strncmp(ptr,".\"",2)) {
            ptr += 2;
            while (*ptr && isspace((unsigned char)*ptr)) { if (*ptr=='\n') ln++; ptr++; }
            if (!rec) printf("    fputs(\"");
            else cbadd(".\"");

            while (*ptr && *ptr!='"') {
                if (*ptr=='\\' && ptr[1]=='"') {
                    if (!rec) printf("\\\""); else cbraw("\\\"", 2);
                    ptr += 2; continue;
                }
                if (!rec) {
                    if (*ptr=='\n') { printf("\\n"); ln++; }
                    else if (*ptr!='\r') {
                        if (*ptr=='"'||*ptr=='\\') putchar('\\');
                        putchar(*ptr);
                    }
                } else {
                    if (*ptr=='\n') { cbraw("\\n", 2); ln++; }
                    else if (*ptr!='\r') {
                        char esc[3]; int ei = 0;
                        if (*ptr=='"'||*ptr=='\\') esc[ei++]='\\';
                        esc[ei++]=*ptr; esc[ei]='\0';
                        cbraw(esc, ei);
                    }
                }
                ptr++;
            }
            if (!*ptr) er("Unclosed string");
            if (!rec) printf("\", stdout);\n"); else cbadd("\"");
            ptr++; // skip closing "
            continue;
        }

        // tok
        int i = 0;
        while (*ptr && !isspace((unsigned char)*ptr) && i < 511) t[i++] = *ptr++;
        t[i] = '\0';
        // skipping ads
        while (*ptr && !isspace((unsigned char)*ptr)) ptr++;
        if (i) ev(t, d);
    }
}

int main(int ac, char **av) {
    char *src = NULL;

    if (ac > 1) {
        FILE *f = fopen(av[1], "r");
        if (!f) er("File IO");
        fseek(f,0,SEEK_END); long fz=ftell(f); fseek(f,0,SEEK_SET);
        src = malloc(fz+1);
        if (!src) er("OOM");
        fread(src,1,fz,f); src[fz]='\0'; fclose(f);
    } else if (!isatty(0)) {
        // stdin pipe
        size_t cap = 4096, sz = 0;
        src = malloc(cap);
        if (!src) er("OOM");
        int c;
        while ((c = getchar()) != EOF) {
            if (sz+2 >= cap) { cap *= 2; src = realloc(src, cap); if (!src) er("OOM"); }
            src[sz++] = (char)c;
        }
        src[sz] = '\0';
    } else {
        src = strdup(",test VARIABLE X 5 X ! X @ . CR BYE test");
    }

    // code is with tung rn
    printf("#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n"
           "#include <time.h>\n#include <stdint.h>\n\n");
    printf("static intptr_t s[1024]; static int st=-1;\n"
           "static intptr_t rs[1024]; static int rt=-1;\n"
           "static intptr_t m[%d];\n\n", VSYM);

    // macros
    printf("#define PUSH(v)   (st>=1023?(fprintf(stderr,\"Stack OF\\n\"),exit(1),0):(s[++st]=(v)))\n");
    printf("#define POP()     (st<0   ?(fprintf(stderr,\"Stack UF\\n\"),exit(1),(intptr_t)0):s[st--])\n");
    printf("#define R_PUSH(v) (rt>=1023?(fprintf(stderr,\"RStack OF\\n\"),exit(1),0):(rs[++rt]=(v)))\n");
    printf("#define R_POP()   (rt<0   ?(fprintf(stderr,\"RStack UF\\n\"),exit(1),(intptr_t)0):rs[rt--])\n");
    printf("#define UFLOW()   (fprintf(stderr,\"Stack UF\\n\"),exit(1))\n");
    printf("#define DIVZ()    (fprintf(stderr,\"Div/0\\n\"),exit(1))\n");
    // tung tung tung sahur
    printf("#define CHKP(a,n) (((intptr_t)(a)<(intptr_t)&m[0]||(intptr_t)(a)+(intptr_t)(n)>(intptr_t)&m[%d])?"
           "(fprintf(stderr,\"Mem OOB\\n\"),exit(1),(intptr_t)0):(a))\n\n", VSYM);

    printf("int main(void) {\n");
    p(src, 0);

    // ewror
    if (lt >= 0) er("Unclosed block");
    if (rec)     er("Unclosed definition");

    printf("    return 0;\n}\n");

    free(src);  
    return 0;
}
